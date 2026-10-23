# bd CLI contract

Beads.el calls `bd` through `beads-cli-bd-contract` in
`lisp/beads-cli.el`. Every invocation adds these base flags:

```text
--json --no-color -C DIR
```

Optional global: `--actor NAME` when `beads-actor` is set.

## Subcommands and flags

| Subcommand | Flags this package uses |
|------------|-------------------------|
| `list` | `--status`, `--assignee` |
| `ready` | `--exclude-type`, `--limit` |
| `human` | `list` (subcommand) |
| `show` | `--include-comments`, `--include-dependents` |
| `create` | `--title`, `--description`, `--priority`, `--parent` |
| `close` | `--reason` |
| `update` | `--claim`, `--status`, `--title`, `--parent` |
| `comment` | `--file` |
| `comments` | (none) |
| `version` | probe only (`bd --version` or `bd version`); not via `beads-cli-call` |

Source of truth: `beads-cli-bd-contract` in `lisp/beads-cli.el`.
