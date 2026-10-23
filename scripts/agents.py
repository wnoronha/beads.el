#!/usr/bin/env python3
"""Sole-user harness: poll `bd ready`, spin at most one Cursor Agent, loop.

Fish-friendly: run this script with Python (no shell-escaped prompt). The
composed agent prompt is written to a temp file named
``beads-{role}-{issue}-{pid}-{token}.prompt.txt``; the agent argv uses
``@/path/to/file`` (Cursor SIGKILLs inline argv prompts over ~1000 chars).
subprocess uses shell=False so Fish quoting never enters the path.

Actor attribution uses BEADS_ACTOR only. bd has no BEADS_USER; harness
resolution is: --actor > role actor= in the single agents.ini >
$BEADS_ACTOR > [default] actor (+ optional actor_suffix). This process
sets BEADS_ACTOR for itself and for the spawned agent child env.

Prompt temp cleanup: delete on agent exit 0; retain on non-zero for
post-mortem. --keep-prompts retains always. Dry-run returns 0 so the
temp file is deleted after the prompt is printed to stderr (use
--keep-prompts to inspect the exact @file on disk).

Selection: among ready *leaf* issues (task/bug/chore/feature — never epic),
pick the highest priority (lowest priority number), breaking ties by list
order from ``bd ready --json``. On non-zero agent exit, sleep
``--agent-fail-backoff`` (default 30s) before the next poll so failures do
not tight-loop.

Configuration: prompts, roles, and harnesses live in an INI file loaded via
stdlib configparser (see scripts/agents.ini). Override with --config or
$BEADS_AGENTS_CONFIG.
"""

from __future__ import annotations

import argparse
import configparser
import json
import logging
import os
import shlex
import signal
import subprocess
import sys
import tempfile
import time
import uuid
from dataclasses import dataclass, field
from logging.handlers import RotatingFileHandler
from pathlib import Path
from typing import Any

DEFAULT_ACTOR = "cursor-agent"
MAX_CONCURRENCY = 1  # hardcoded; multi-agent is out of scope
# Executable leaf types; epics skipped unless role select=ready_any.
EXECUTABLE_ISSUE_TYPES = frozenset({"task", "bug", "chore", "feature"})
READY_ANY_ISSUE_TYPES = EXECUTABLE_ISSUE_TYPES | frozenset({"epic"})
DEFAULT_AGENT_FAIL_BACKOFF = 30.0
ENV_CONFIG = "BEADS_AGENTS_CONFIG"
# CLI --role aliases (INI section names are canonical).
ROLE_ALIASES = {
    "code": "code",
    "verify": "verify",
    "agent-verify": "verify",
    "planner": "planner",
    "agent-planner": "planner",
}
SELECT_POLICIES = frozenset({"ready_leaf", "ready_any", "closed_recent"})


class ConfigError(Exception):
    """Raised when agents.ini is missing keys or has an invalid schema."""


@dataclass
class HarnessConfig:
    name: str
    command: str
    args_template: str
    prompt_mode: str = "atfile"


@dataclass
class RoleConfig:
    name: str
    harness: str
    sections: list[str]
    actor: str = ""
    actor_suffix: str = ""
    select: str = "ready_leaf"
    labels: str = ""
    prompt_sections: dict[str, str] = field(default_factory=dict)


@dataclass
class AgentsConfig:
    path: Path
    role: str
    harness: str
    actor: str
    interval: float
    agent_fail_backoff: float
    post_agent_sleep: float
    harnesses: dict[str, HarnessConfig]
    roles: dict[str, RoleConfig]

    def active_role(self) -> RoleConfig:
        if self.role not in self.roles:
            raise ConfigError(
                f"default role {self.role!r} not found; "
                f"known roles: {sorted(self.roles)}"
            )
        return self.roles[self.role]

    def harness_for_role(
        self,
        role: RoleConfig | None = None,
        override: str | None = None,
    ) -> HarnessConfig:
        """Resolve harness: CLI override > role.harness > [default] harness."""
        role = role or self.active_role()
        name = (override or "").strip() or role.harness or self.harness
        if name not in self.harnesses:
            raise ConfigError(
                f"harness {name!r} for role {role.name!r} not found; "
                f"known harnesses: {sorted(self.harnesses)}"
            )
        return self.harnesses[name]


def default_config_path() -> Path:
    return Path(__file__).resolve().parent / "agents.ini"


def resolve_config_path(cli_path: Path | None = None) -> Path:
    if cli_path is not None:
        return cli_path.expanduser()
    env = os.environ.get(ENV_CONFIG, "").strip()
    if env:
        return Path(env).expanduser()
    return default_config_path()


def _require(section: str, key: str, mapping: configparser.SectionProxy) -> str:
    if key not in mapping or not str(mapping[key]).strip():
        raise ConfigError(
            f"missing required key [{section}] {key}= in agents config"
        )
    return mapping[key].strip()


def _parse_harness(name: str, sec: configparser.SectionProxy) -> HarnessConfig:
    section = f"harness.{name}"
    command = _require(section, "command", sec)
    args_template = _require(section, "args", sec)
    prompt_mode = sec.get("prompt_mode", "atfile").strip().lower() or "atfile"
    if prompt_mode not in {"atfile", "argv", "stdin"}:
        raise ConfigError(
            f"[{section}] prompt_mode must be atfile|argv|stdin, got {prompt_mode!r}"
        )
    return HarnessConfig(
        name=name,
        command=command,
        args_template=args_template,
        prompt_mode=prompt_mode,
    )


def _parse_role(
    name: str,
    sec: configparser.SectionProxy,
    parser: configparser.ConfigParser,
) -> RoleConfig:
    section = f"role.{name}"
    harness = _require(section, "harness", sec)
    sections_raw = _require(section, "sections", sec)
    sections = [s.strip() for s in sections_raw.split(",") if s.strip()]
    if not sections:
        raise ConfigError(f"[{section}] sections= is empty")
    actor = sec.get("actor", "").strip()
    actor_suffix = sec.get("actor_suffix", "").strip()
    select = sec.get("select", "ready_leaf").strip().lower() or "ready_leaf"
    if select not in SELECT_POLICIES:
        raise ConfigError(
            f"[{section}] select must be "
            f"{'|'.join(sorted(SELECT_POLICIES))}, got {select!r}"
        )
    labels = sec.get("labels", "").strip()

    prompt_sections: dict[str, str] = {}
    for part in sections:
        prompt_sec = f"prompt.{name}.{part}"
        if prompt_sec not in parser:
            raise ConfigError(
                f"role {name!r} lists section {part!r} but "
                f"[{prompt_sec}] is missing"
            )
        text = parser[prompt_sec].get("text")
        if text is None or not str(text).strip():
            raise ConfigError(
                f"[{prompt_sec}] missing required key text="
            )
        # configparser keeps indent on continuation lines; normalize lightly
        prompt_sections[part] = _normalize_multiline(text)

    return RoleConfig(
        name=name,
        harness=harness,
        sections=sections,
        actor=actor,
        actor_suffix=actor_suffix,
        select=select,
        labels=labels,
        prompt_sections=prompt_sections,
    )


def _normalize_multiline(text: str) -> str:
    """Strip trailing spaces per line; keep intentional blank lines."""
    lines = text.replace("\r\n", "\n").split("\n")
    return "\n".join(line.rstrip() for line in lines).strip("\n") + "\n"


def load_config(path: Path) -> AgentsConfig:
    """Load and validate agents.ini. Raises ConfigError on schema problems."""
    path = path.expanduser().resolve()
    if not path.is_file():
        raise ConfigError(f"config file not found: {path}")

    # No inline_comment_prefixes: prompt bodies contain markdown `#` / `###`.
    # Whole-line comments still work via default comment_prefixes ("#" / ";").
    parser = configparser.ConfigParser(interpolation=None)
    try:
        read_ok = parser.read(path, encoding="utf-8")
    except configparser.Error as exc:
        raise ConfigError(f"invalid INI syntax in {path}: {exc}") from exc
    if not read_ok:
        raise ConfigError(f"config file not readable: {path}")

    if "default" not in parser:
        raise ConfigError(f"missing required section [default] in {path}")

    default = parser["default"]
    role = _require("default", "role", default)
    actor = _require("default", "actor", default)
    harness = default.get("harness", "").strip() or "cursor"

    def _float(key: str, fallback: float) -> float:
        raw = default.get(key)
        if raw is None or not str(raw).strip():
            return fallback
        try:
            return float(raw)
        except ValueError as exc:
            raise ConfigError(
                f"[default] {key}= must be a number, got {raw!r}"
            ) from exc

    interval = _float("interval", 30.0)
    agent_fail_backoff = _float("agent_fail_backoff", DEFAULT_AGENT_FAIL_BACKOFF)
    post_agent_sleep = _float("post_agent_sleep", 2.0)

    harnesses: dict[str, HarnessConfig] = {}
    roles: dict[str, RoleConfig] = {}

    for section in parser.sections():
        if section.startswith("harness."):
            name = section[len("harness.") :]
            if not name:
                raise ConfigError("empty harness name in section [harness.]")
            harnesses[name] = _parse_harness(name, parser[section])
        elif section.startswith("role."):
            name = section[len("role.") :]
            if not name:
                raise ConfigError("empty role name in section [role.]")
            roles[name] = _parse_role(name, parser[section], parser)

    if not harnesses:
        raise ConfigError(f"no [harness.*] sections in {path}")
    if not roles:
        raise ConfigError(f"no [role.*] sections in {path}")
    if role not in roles:
        raise ConfigError(
            f"[default] role={role!r} has no [role.{role}] section; "
            f"known: {sorted(roles)}"
        )

    cfg = AgentsConfig(
        path=path,
        role=role,
        harness=harness,
        actor=actor,
        interval=interval,
        agent_fail_backoff=agent_fail_backoff,
        post_agent_sleep=post_agent_sleep,
        harnesses=harnesses,
        roles=roles,
    )
    # Validate default role → harness wiring early
    cfg.harness_for_role()
    return cfg


def resolve_role_name(raw: str, roles: dict[str, RoleConfig]) -> str:
    """Map CLI/INI role name (and aliases) to a known [role.*] key."""
    key = raw.strip().lower()
    canonical = ROLE_ALIASES.get(key, key)
    if canonical not in roles:
        raise ConfigError(
            f"unknown role {raw!r} (resolved {canonical!r}); "
            f"known: {sorted(roles)}; aliases: {sorted(ROLE_ALIASES)}"
        )
    return canonical


def role_actor(cfg: AgentsConfig, role: RoleConfig, base_actor: str) -> str:
    """Resolve BEADS_ACTOR for a role: role.actor > base+suffix > base."""
    if role.actor:
        return role.actor
    if role.actor_suffix:
        return f"{base_actor}{role.actor_suffix}"
    return base_actor


def compose_prompt(
    issue_id: str,
    role: RoleConfig,
) -> str:
    """Join named prompt sections; inject harness-chosen issue id into tasks."""
    secs = dict(role.prompt_sections)
    # Role-agnostic inject — role TASK/RULES say what to do with the id.
    inject = f"""\

### HARNESS SELECTION:
The harness selected issue `{issue_id}`.
Follow your role TASK / RULES / GATES for ONLY `{issue_id}`.
Do not start any other issue.
"""
    tasks = secs.get("tasks", "") + inject
    parts = []
    for name in role.sections:
        if name == "tasks":
            parts.append(tasks)
        else:
            if name not in secs:
                raise ConfigError(
                    f"role {role.name!r} missing prompt section {name!r}"
                )
            parts.append(secs[name])
    return "\n\n".join(p.strip("\n") for p in parts)


@dataclass
class LogContext:
    """Mutable fields injected into every log record (one stream/file)."""

    actor: str = "-"
    role: str = "-"
    harness: str = "-"
    issue_id: str = "-"
    run_id: str = "-"

    def bind(
        self,
        *,
        actor: str | None = None,
        role: str | None = None,
        harness: str | None = None,
        issue_id: str | None = None,
        run_id: str | None = None,
    ) -> None:
        if actor is not None:
            self.actor = actor
        if role is not None:
            self.role = role
        if harness is not None:
            self.harness = harness
        if issue_id is not None:
            self.issue_id = issue_id
        if run_id is not None:
            self.run_id = run_id

    def clear_cycle(self) -> None:
        """Reset per-cycle fields; keep process-level actor/role/harness."""
        self.issue_id = "-"
        self.run_id = "-"


def new_run_id() -> str:
    """Per-cycle token: time + pid + short uuid (correlates spawn ↔ exit)."""
    return f"{int(time.time())}-{os.getpid()}-{uuid.uuid4().hex[:8]}"


def sanitize_fs_component(value: str, *, fallback: str = "x") -> str:
    """Make a single path component safe for temp filenames.

    Keeps letters, digits, ``._-``; maps everything else to ``_``. Collapses
    runs of ``_``. Empty / all-unsafe input becomes ``fallback``.
    """
    out: list[str] = []
    prev_us = False
    for ch in value.strip():
        if ch.isalnum() or ch in "._-":
            out.append(ch)
            prev_us = False
        elif not prev_us:
            out.append("_")
            prev_us = True
    cleaned = "".join(out).strip("._")
    return cleaned or fallback


def prompt_temp_prefix(role: str, issue_id: str) -> str:
    """Prefix for NamedTemporaryFile: ``beads-{role}-{issue}-{pid}-``.

    Final name is ``beads-{role}-{issue}-{pid}-{token}.prompt.txt`` where
    ``token`` is the random suffix from ``tempfile`` (collision-safe across
    concurrent processes).
    """
    safe_role = sanitize_fs_component(role, fallback="role")
    safe_issue = sanitize_fs_component(issue_id, fallback="issue")
    return f"beads-{safe_role}-{safe_issue}-{os.getpid()}-"


def cleanup_prompt_temp(
    prompt_path: Path,
    exit_code: int,
    *,
    keep_prompts: bool,
    logger: logging.Logger,
    run_id: str,
) -> None:
    """Apply prompt-temp cleanup policy after a spawn/dry-run cycle.

    Policy (single INI / bel-wjm.8):
    - Exit 0 (including dry-run): delete the temp file unless ``keep_prompts``.
    - Non-zero: retain for post-mortem (``--keep-prompts`` is redundant then).
    Dry-run prints the full prompt on stderr, so deleting the file avoids
    orphans; pass ``--keep-prompts`` to leave the exact ``@file`` on disk.
    """
    if keep_prompts or exit_code != 0:
        logger.info(
            "prompt file retained path=%s exit=%s keep_prompts=%s run_id=%s",
            prompt_path,
            exit_code,
            keep_prompts,
            run_id,
        )
        return
    try:
        prompt_path.unlink(missing_ok=True)
        logger.debug(
            "prompt file deleted path=%s exit=%s run_id=%s",
            prompt_path,
            exit_code,
            run_id,
        )
    except OSError as exc:
        logger.warning(
            "prompt file delete failed path=%s err=%s run_id=%s",
            prompt_path,
            exc,
            run_id,
        )


def setup_logging(
    log_path: Path, debug: bool, actor: str
) -> tuple[logging.Logger, LogContext]:
    """Configure one stderr + rotating file stream with structured cycle fields."""
    logger = logging.getLogger("agents")
    logger.setLevel(logging.DEBUG if debug else logging.INFO)
    logger.handlers.clear()
    fmt = logging.Formatter(
        "%(asctime)s %(levelname)s "
        "[actor=%(actor)s role=%(role)s harness=%(harness)s "
        "issue_id=%(issue_id)s run_id=%(run_id)s] %(message)s",
        datefmt="%Y-%m-%dT%H:%M:%S",
    )
    ctx = LogContext(actor=os.environ.get("BEADS_ACTOR", actor))

    class CycleFilter(logging.Filter):
        def filter(self, record: logging.LogRecord) -> bool:
            if not hasattr(record, "actor"):
                record.actor = ctx.actor
            if not hasattr(record, "role"):
                record.role = ctx.role
            if not hasattr(record, "harness"):
                record.harness = ctx.harness
            if not hasattr(record, "issue_id"):
                record.issue_id = ctx.issue_id
            if not hasattr(record, "run_id"):
                record.run_id = ctx.run_id
            return True

    cycle_filter = CycleFilter()

    sh = logging.StreamHandler(sys.stderr)
    sh.setLevel(logging.DEBUG if debug else logging.INFO)
    sh.setFormatter(fmt)
    sh.addFilter(cycle_filter)
    logger.addHandler(sh)

    log_path.parent.mkdir(parents=True, exist_ok=True)
    fh = RotatingFileHandler(
        log_path, maxBytes=2_000_000, backupCount=5, encoding="utf-8"
    )
    fh.setLevel(logging.DEBUG)
    fh.setFormatter(fmt)
    fh.addFilter(cycle_filter)
    logger.addHandler(fh)
    return logger, ctx


def run_bd_ready(workspace: Path, actor: str) -> list[dict[str, Any]]:
    """Return ready issues from `bd ready --json`, or [] on empty/failure."""
    env = os.environ.copy()
    env["BEADS_ACTOR"] = actor
    try:
        proc = subprocess.run(
            ["bd", "ready", "--json"],
            cwd=str(workspace),
            env=env,
            capture_output=True,
            text=True,
            check=False,
        )
    except FileNotFoundError as exc:
        raise RuntimeError("bd not found on PATH") from exc

    if proc.returncode != 0:
        raise RuntimeError(
            f"bd ready failed (exit {proc.returncode}): {proc.stderr.strip()}"
        )

    raw = proc.stdout.strip()
    if not raw or raw == "[]":
        return []
    data = json.loads(raw)
    if not isinstance(data, list):
        raise RuntimeError(f"unexpected bd ready --json shape: {type(data)}")
    return data


def filter_for_role(
    issues: list[dict[str, Any]],
    role: RoleConfig,
) -> list[dict[str, Any]]:
    """Apply role selection policy to bd ready results (v1).

    ready_leaf / closed_recent: leaf types only (verify still needs --issue
    for recently closed work — closed_recent is not auto-fetched in v1).
    ready_any: leaves + epics (planner).
    """
    allowed = (
        READY_ANY_ISSUE_TYPES
        if role.select == "ready_any"
        else EXECUTABLE_ISSUE_TYPES
    )
    kept: list[dict[str, Any]] = []
    for issue in issues:
        itype = str(issue.get("issue_type") or issue.get("type") or "").lower()
        if itype in allowed:
            kept.append(issue)
    return kept


# Back-compat alias for callers/tests that used the old name.
def filter_executable(issues: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """Drop epics and other non-leaf types; keep task/bug/chore/feature."""
    stub = RoleConfig(
        name="_", harness="_", sections=[], select="ready_leaf"
    )
    return filter_for_role(issues, stub)


def pick_issue(issues: list[dict[str, Any]]) -> dict[str, Any]:
    """Highest priority (lowest number); ties keep bd ready list order."""
    return min(
        enumerate(issues),
        key=lambda pair: (int(pair[1].get("priority", 99)), pair[0]),
    )[1]


def expand_args_template(
    harness: HarnessConfig,
    workspace: Path,
    prompt_file: str,
) -> str:
    """Fill ``{workspace}`` / ``{prompt_file}`` in the harness args template."""
    try:
        return harness.args_template.format(
            workspace=str(workspace),
            prompt_file=prompt_file,
        )
    except KeyError as exc:
        raise ConfigError(
            f"harness {harness.name!r} args template has unknown placeholder "
            f"{{{exc.args[0]}}}; only {{workspace}} and {{prompt_file}} are valid"
        ) from exc


def build_agent_cmd(
    harness: HarnessConfig,
    workspace: Path,
    prompt_path: Path,
) -> list[str]:
    """Build agent argv from harness template.

    ``prompt_path`` is the temp file holding the composed prompt. For
    ``prompt_mode=atfile``, the template should include ``@{prompt_file}``
    (Cursor SIGKILLs inline argv prompts over ~1000 characters).
    Never pass long prompt bodies as raw argv for cursor — use @file.
    """
    filled = expand_args_template(harness, workspace, str(prompt_path))
    # shlex keeps @{path} as one token when no spaces; path must not need quotes
    argv_tail = shlex.split(filled)
    return [harness.command, *argv_tail]


def format_cmdline(cmd: list[str]) -> str:
    """Shell-ish display of argv (for logs / dry-run)."""
    return shlex.join(cmd)


def spawn_agent(
    workspace: Path,
    prompt: str,
    actor: str,
    harness: HarnessConfig,
    logger: logging.Logger,
    dry_run: bool,
    *,
    role: str,
    issue_id: str,
    run_id: str,
    keep_prompts: bool = False,
) -> int:
    """Write prompt to a temp file; spawn agent with harness argv, wait.

    Spawn path is entirely driven by ``[harness.*]`` config (command, args
    template, prompt_mode). Cursor must keep ``prompt_mode=atfile`` with
    ``@{prompt_file}`` to avoid SIGKILL on long inline argv prompts.

    ``role`` / ``issue_id`` / ``run_id`` are logged on spawn and exit so
    dry-run and live cycles can be correlated in the same log stream.

    Temp prompt path form: ``beads-{role}-{issue}-{pid}-{token}.prompt.txt``
    (issue id sanitized for the filesystem). Still passed as ``@<path>``.
    Cleanup: delete on exit 0 unless ``keep_prompts``; retain on failure.
    """
    with tempfile.NamedTemporaryFile(
        mode="w",
        suffix=".prompt.txt",
        prefix=prompt_temp_prefix(role, issue_id),
        delete=False,
        encoding="utf-8",
    ) as tf:
        tf.write(prompt)
        prompt_path = Path(tf.name)

    if harness.prompt_mode == "stdin":
        raise ConfigError(
            f"harness {harness.name!r} prompt_mode=stdin not implemented yet; "
            "set prompt_mode=atfile|argv in [harness.%s] or fix the stub"
            % harness.name
        )
    if harness.prompt_mode == "argv":
        # Inline body — only for tiny prompts; kept for schema completeness.
        # Do not use for cursor (SIGKILL over ~1000 chars).
        if harness.name == "cursor":
            raise ConfigError(
                "harness cursor must use prompt_mode=atfile with @{prompt_file}; "
                "inline argv prompts are SIGKILL'd over ~1000 chars"
            )
        filled = expand_args_template(harness, workspace, prompt)
        cmd = [harness.command, *shlex.split(filled)]
    else:
        cmd = build_agent_cmd(harness, workspace, prompt_path)

    env = os.environ.copy()
    env["BEADS_ACTOR"] = actor
    cmdline = format_cmdline(cmd)

    logger.info(
        "spawn agent role=%s harness=%s issue_id=%s run_id=%s "
        "prompt_mode=%s cmdline=%s prompt_file=%s BEADS_ACTOR=%s",
        role,
        harness.name,
        issue_id,
        run_id,
        harness.prompt_mode,
        cmdline,
        prompt_path,
        actor,
    )

    if dry_run:
        logger.info(
            "dry-run: skipping spawn role=%s harness=%s issue_id=%s run_id=%s",
            role,
            harness.name,
            issue_id,
            run_id,
        )
        print(
            f"=== HARNESS: {harness.name} (prompt_mode={harness.prompt_mode}) ===",
            file=sys.stderr,
        )
        print(
            f"role={role} harness={harness.name} issue_id={issue_id} run_id={run_id}",
            file=sys.stderr,
        )
        print(f"template: {harness.command} {harness.args_template}", file=sys.stderr)
        print(f"cmdline:  {cmdline}", file=sys.stderr)
        print(f"prompt_file: {prompt_path}", file=sys.stderr)
        print("=== COMPOSED PROMPT ===", file=sys.stderr)
        print(prompt, file=sys.stderr)
        cleanup_prompt_temp(
            prompt_path,
            0,
            keep_prompts=keep_prompts,
            logger=logger,
            run_id=run_id,
        )
        return 0

    t0 = time.monotonic()
    code = 127
    try:
        proc = subprocess.run(
            cmd,
            cwd=str(workspace),
            env=env,
            check=False,
        )
        code = proc.returncode
    except FileNotFoundError:
        logger.error(
            "harness %s: %s not found on PATH (fix [harness.%s] command=) "
            "role=%s issue_id=%s run_id=%s",
            harness.name,
            harness.command,
            harness.name,
            role,
            issue_id,
            run_id,
        )
        code = 127
    duration = time.monotonic() - t0

    logger.info(
        "agent exited role=%s harness=%s issue_id=%s run_id=%s "
        "code=%s duration=%.1fs prompt_file=%s",
        role,
        harness.name,
        issue_id,
        run_id,
        code,
        duration,
        prompt_path,
    )
    cleanup_prompt_temp(
        prompt_path,
        code,
        keep_prompts=keep_prompts,
        logger=logger,
        run_id=run_id,
    )
    return code


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    p = argparse.ArgumentParser(
        prog="agents.py",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description="Poll bd ready and run at most one Cursor Agent at a time.",
        epilog=f"""\
Fish-friendly usage (no prompt escaping; Python + @file, not shell quotes):

  python3 scripts/agents.py --workspace ~/code/beads.el

  # dry-run one cycle (compose + print, no spawn)
  python3 scripts/agents.py --dry-run --once
  make agents-dry-run
  make agents-dry-run ROLE=verify ISSUE=bel-wjm.4

  # role-specific dry-run (code default / verify / planner)
  python3 scripts/agents.py --role code --dry-run --once --issue bel-wjm.4
  python3 scripts/agents.py --role verify --dry-run --once --issue bel-wjm.4
  python3 scripts/agents.py --role planner --dry-run --once --issue bel-wjm.4

  # harness override (INI stubs: cursor, agy, codex, opencode)
  python3 scripts/agents.py --harness cursor --dry-run --once --issue bel-wjm.4
  python3 scripts/agents.py --harness codex --dry-run --once --issue bel-wjm.4

  # custom config + force compose for a specific id
  python3 scripts/agents.py --config scripts/agents.ini --dry-run --once --issue bel-wjm.4

Migration: scripts/beads_agent_loop.py → scripts/agents.py (same loop;
config/roles/harnesses now live in agents.ini).

Config (INI via configparser):
  Default: {default_config_path()}  (committed example)
  Override: --config PATH  or  ${ENV_CONFIG}
  Schema: see comments at top of agents.ini
  Add a role/harness: edit agents.ini (see header comments there)

Roles (--role; default from [default] role=code):
  code              Implement + close; may edit code / commit
  verify            Validate acceptance/tests/handoff; no features
  planner           Create/refine bd issues only; no product code
  Aliases: agent-verify→verify, agent-planner→planner

Harnesses (--harness; default: role harness= then [default] harness):
  cursor            Working: agent -p --trust -f … @{{prompt_file}}
  agy, codex, opencode
                    Experimental INI stubs — fix argv via config, not code

Selection (per-role select= in INI; v1):
  ready_leaf        Leaf types only (code default)
  ready_any         Leaves + epics (planner)
  closed_recent     Same leaf filter for now; pass --issue for closed work
  Highest priority (lowest number) wins among filtered ready issues.

Agent argv:
  Spawn is config-driven ([harness.*] command + args template). For cursor,
  the composed prompt is written to a temp *.prompt.txt and passed as
  @/path (not inline). Inline argv prompts over ~1000 chars get SIGKILL.
  Dry-run prints harness name, template, and expanded cmdline.

Prompt temp cleanup:
  Delete on agent exit 0; retain on non-zero for post-mortem.
  --keep-prompts retains always. Dry-run exits 0 so the temp file is
  deleted after the prompt is printed to stderr (no orphan) unless
  --keep-prompts (inspect the exact @file on disk).

Environment:
  BEADS_ACTOR   Actor for bd audit trail. Precedence: --actor >
                role actor= in single agents.ini > $BEADS_ACTOR >
                [default] actor. bd has no BEADS_USER — use BEADS_ACTOR.
  {ENV_CONFIG}  Path to agents.ini (overridden by --config).
  BEADS_LOOP_WORKSPACE, BEADS_LOOP_INTERVAL, BEADS_LOOP_LOG
                Defaults for --workspace / --interval / --log.
""",
    )
    p.add_argument(
        "--config",
        type=Path,
        default=None,
        help=(
            f"Path to agents.ini (default: ${ENV_CONFIG} or "
            f"{default_config_path()})"
        ),
    )
    p.add_argument(
        "--role",
        default=None,
        metavar="NAME",
        help=(
            "Agent role: code|verify|planner "
            "(aliases: agent-verify, agent-planner; default: from config)"
        ),
    )
    p.add_argument(
        "--harness",
        default=None,
        metavar="NAME",
        help=(
            "Harness backend override (cursor|agy|codex|opencode|…); "
            "overrides role harness= / [default] harness"
        ),
    )
    p.add_argument(
        "--workspace",
        type=Path,
        default=Path(
            os.environ.get("BEADS_LOOP_WORKSPACE", os.getcwd())
        ).expanduser(),
        help="Repo workspace for bd and cursor agent (default: cwd / BEADS_LOOP_WORKSPACE)",
    )
    p.add_argument(
        "--interval",
        type=float,
        default=None,
        help="Seconds to sleep when no ready work (default: from config / 30 / BEADS_LOOP_INTERVAL)",
    )
    p.add_argument(
        "--log",
        type=Path,
        default=Path(
            os.environ.get("BEADS_LOOP_LOG", "beads-agent-loop.log")
        ).expanduser(),
        help="Append/rotating log file path (also logs to stderr)",
    )
    p.add_argument(
        "--actor",
        default=None,
        help=f"BEADS_ACTOR for harness + child (default: from config / {DEFAULT_ACTOR})",
    )
    p.add_argument(
        "--dry-run",
        action="store_true",
        help="Compose prompt and print; do not spawn cursor agent",
    )
    p.add_argument(
        "--keep-prompts",
        action="store_true",
        help=(
            "Retain prompt temp files always (default: delete on exit 0, "
            "retain on failure for post-mortem; dry-run deletes unless set)"
        ),
    )
    p.add_argument(
        "--once",
        action="store_true",
        help="Run a single poll cycle then exit (smoke / dry-run)",
    )
    p.add_argument(
        "--issue",
        metavar="ID",
        help="Skip bd ready; compose/spawn for this issue id (useful with --dry-run)",
    )
    p.add_argument(
        "--debug",
        action="store_true",
        help="DEBUG logging to stderr",
    )
    p.add_argument(
        "--post-agent-sleep",
        type=float,
        default=None,
        help="Brief sleep after successful agent exit before next poll (default: from config / 2)",
    )
    p.add_argument(
        "--agent-fail-backoff",
        type=float,
        default=None,
        help=(
            "Seconds to sleep after non-zero agent exit before next poll "
            f"(default: from config / {DEFAULT_AGENT_FAIL_BACKOFF:g})"
        ),
    )
    return p.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    workspace = args.workspace.resolve()
    if not workspace.is_dir():
        print(f"error: workspace not a directory: {workspace}", file=sys.stderr)
        return 2

    try:
        cfg = load_config(resolve_config_path(args.config))
    except ConfigError as exc:
        print(f"error: agents config: {exc}", file=sys.stderr)
        return 2

    # CLI / env override config defaults
    if args.interval is not None:
        interval = args.interval
    elif "BEADS_LOOP_INTERVAL" in os.environ:
        interval = float(os.environ["BEADS_LOOP_INTERVAL"])
    else:
        interval = cfg.interval

    try:
        if args.role is not None:
            cfg.role = resolve_role_name(args.role, cfg.roles)
        role = cfg.active_role()
    except ConfigError as exc:
        print(f"error: agents config: {exc}", file=sys.stderr)
        return 2

    # Actor: --actor > role.actor (INI) > $BEADS_ACTOR > [default] actor (+suffix)
    # Role actor wins over ambient env so `--role verify` is not stuck on a
    # leftover cursor-agent from a prior code session in the same shell.
    if args.actor is not None:
        actor = args.actor
    elif role.actor:
        actor = role.actor
    elif "BEADS_ACTOR" in os.environ:
        actor = os.environ["BEADS_ACTOR"]
    else:
        actor = role_actor(cfg, role, cfg.actor)

    post_agent_sleep = (
        args.post_agent_sleep
        if args.post_agent_sleep is not None
        else cfg.post_agent_sleep
    )
    agent_fail_backoff = (
        args.agent_fail_backoff
        if args.agent_fail_backoff is not None
        else cfg.agent_fail_backoff
    )

    try:
        harness = cfg.harness_for_role(role, override=args.harness)
    except ConfigError as exc:
        print(f"error: agents config: {exc}", file=sys.stderr)
        return 2

    os.environ["BEADS_ACTOR"] = actor
    logger, log_ctx = setup_logging(
        args.log if args.log.is_absolute() else workspace / args.log,
        args.debug,
        actor,
    )
    log_ctx.bind(actor=actor, role=role.name, harness=harness.name)

    logger.info(
        "start workspace=%s interval=%s dry_run=%s actor=%s concurrency=%s "
        "config=%s role=%s select=%s harness=%s prompt_mode=%s",
        workspace,
        interval,
        args.dry_run,
        actor,
        MAX_CONCURRENCY,
        cfg.path,
        role.name,
        role.select,
        harness.name,
        harness.prompt_mode,
    )

    stopping = False

    def _handle_sig(_signum: int, _frame: Any) -> None:
        nonlocal stopping
        stopping = True
        logger.info("signal received; will exit after current cycle")

    signal.signal(signal.SIGINT, _handle_sig)
    signal.signal(signal.SIGTERM, _handle_sig)

    while not stopping:
        code = 0
        run_id = new_run_id()
        log_ctx.clear_cycle()
        log_ctx.bind(run_id=run_id)
        try:
            if args.issue:
                issue_id = args.issue
                log_ctx.bind(issue_id=issue_id)
                logger.info(
                    "forced issue id=%s run_id=%s (skipping bd ready)",
                    issue_id,
                    run_id,
                )
            else:
                issues = run_bd_ready(workspace, actor)
                # closed_recent: v1 still filters ready leaves; prefer --issue
                # for recently closed work (documented in agents.ini).
                executable = filter_for_role(issues, role)
                skipped = len(issues) - len(executable)
                logger.info(
                    "poll ready count=%s selectable=%s skipped=%s select=%s "
                    "run_id=%s",
                    len(issues),
                    len(executable),
                    skipped,
                    role.select,
                    run_id,
                )
                if not executable:
                    logger.info(
                        "no selectable ready issues for role=%s harness=%s "
                        "run_id=%s; sleeping %.1fs",
                        role.name,
                        harness.name,
                        run_id,
                        interval,
                    )
                    if args.once:
                        break
                    _sleep_interruptible(interval, lambda: stopping)
                    continue
                chosen = pick_issue(executable)
                issue_id = str(chosen["id"])
                log_ctx.bind(issue_id=issue_id)
                logger.info(
                    "chose id=%s type=%s priority=%s title=%r run_id=%s",
                    issue_id,
                    chosen.get("issue_type"),
                    chosen.get("priority"),
                    chosen.get("title"),
                    run_id,
                )

            prompt = compose_prompt(issue_id, role)
            code = spawn_agent(
                workspace,
                prompt,
                actor,
                harness,
                logger,
                dry_run=args.dry_run,
                role=role.name,
                issue_id=issue_id,
                run_id=run_id,
                keep_prompts=args.keep_prompts,
            )
            logger.info(
                "cycle done role=%s harness=%s issue_id=%s run_id=%s "
                "agent_exit=%s",
                role.name,
                harness.name,
                issue_id,
                run_id,
                code,
            )

        except ConfigError as exc:
            logger.error("config error: %s", exc)
            return 2
        except Exception as exc:  # noqa: BLE001 — harness must keep looping
            logger.exception("cycle failure: %s", exc)
            code = -1

        if args.once or args.issue:
            break
        if stopping:
            break
        if code != 0:
            backoff = max(agent_fail_backoff, post_agent_sleep)
            logger.warning(
                "agent non-zero exit=%s run_id=%s; backing off %.1fs "
                "before next poll",
                code,
                run_id,
                backoff,
            )
            _sleep_interruptible(backoff, lambda: stopping)
        else:
            _sleep_interruptible(post_agent_sleep, lambda: stopping)

    log_ctx.clear_cycle()
    logger.info("exit")
    return 0


def _sleep_interruptible(seconds: float, should_stop) -> None:
    end = time.monotonic() + max(0.0, seconds)
    while time.monotonic() < end:
        if should_stop():
            return
        time.sleep(min(0.5, end - time.monotonic()))


if __name__ == "__main__":
    sys.exit(main())
