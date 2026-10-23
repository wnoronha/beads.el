EMACS ?= emacs
ROOT := $(abspath .)
LISP := $(ROOT)/lisp

# Offline gate (no live bd project required):
#   make compile ert
#   make verify
# Package release hygiene (regenerate + assert clean autoloads):
#   make autoloads check-autoloads
# Agent loop dry-run (no spawn); optional; not part of verify:
#   make agents-dry-run
#   make agents-dry-run ROLE=verify ISSUE=bel-wjm.4
# Local bd smoke (requires bd on PATH); not part of verify:
#   make smoke-bd

.PHONY: ert compile verify agents-dry-run autoloads check-autoloads smoke-bd

ROLE ?= code
ISSUE ?=
agents-dry-run:
	python3 $(ROOT)/scripts/agents.py --workspace $(ROOT) --role $(ROLE) --dry-run --once \
	  $(if $(ISSUE),--issue $(ISSUE),)

smoke-bd:
	command -v bd; bd --version; bd list --limit 1 >/dev/null

ert:
	$(EMACS) -Q --batch --eval '(setq load-prefer-newer t)' -l $(ROOT)/test/beads-ert.el

# Byte-compile every lisp/*.el module (single load-path: LISP).
# Skips generated beads-autoloads.el (no-byte-compile).
compile:
	$(EMACS) -Q --batch \
	  --eval '(setq byte-compile-error-on-warn t load-prefer-newer t)' \
	  --eval '(add-to-list '\''load-path "$(LISP)")' \
	  --eval '(dolist (f (directory-files "$(LISP)" t "\\.el$$")) (unless (string-suffix-p "-autoloads.el" f) (byte-compile-file f)))'

# Scrapes ;;;###autoload from lisp/*.el into lisp/beads-autoloads.el.
autoloads:
	$(EMACS) -Q --batch \
	  --eval '(require '\''loaddefs-gen)' \
	  --eval '(loaddefs-generate "$(LISP)" "$(LISP)/beads-autoloads.el" '\''("beads-autoloads.el") nil nil t)'

# Fail if autoloads are stale relative to ;;;###autoload cookies.
check-autoloads: autoloads
	@git diff --exit-code -- $(LISP)/beads-autoloads.el \
	  || (echo "lisp/beads-autoloads.el is stale; commit make autoloads output" && exit 1)

# Offline: compile + ert. Autoload freshness is a release/CI check, not required for local ert.
# agents-dry-run is optional and not part of verify.
verify: compile ert
