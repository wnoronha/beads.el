# Architecture (short)

## Mutations

Write paths (`update --claim`, `comment`, `close`, `create`) go through
`beads-actions.el` (UI) or thin `beads-cli-*` wrappers. List/detail only
call `beads-cli-call(-async)` for reads (`list`, `show`, `comments`,
`ready`, `human`).

## List ↔ detail ↔ follow

See Commentary in `lisp/beads-list.el`. Layout placement lives in
`lisp/beads-layout.el`.

## Soft deps among list-* modules (`declare-function`)

| From | Soft deps (file) |
|------|------------------|
| `beads-list-data` | redraw, id-at-point, goto-id (`render`); schedule-follow (`follow`); tree-seed-collapsed (`tree`) |
| `beads-list-filter` | redraw, id-at-point (`render`); refresh (`data`) |
| `beads-list-render` | apply-filters (`filter`); header (`header`); schedule-follow (`follow`) |
| `beads-list-header` | filter toggles (`filter`); cycle-sort (`render`); toggle-follow (`follow`) |
| `beads-list-follow` | id-at-point, goto-id (`render`); detail-display / goto-section / shown-id (`detail`); detail heading helpers (`detail-render`) |
| `beads-list` (coordinator) | requires the modules above; no `declare-function` |

`beads-keymap` declares many list/detail/actions commands without loading
those modules at keymap definition time.
