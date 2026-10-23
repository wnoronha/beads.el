# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Versioning

- `0.1.x` — packaging, docs, and hygiene.
- `0.2.0` — expand the write surface further (edit flows and related UI).

## [Unreleased]

## [0.1.0] - 2026-10-30

Initial public release of beads.el.

### Added

- List and detail buffers for Beads issues via `bd`
- Write commands: claim, comment, create, close, set-status, set-title, reparent
- Emacs and BTU keymaps; standalone, stacked, and side-by-side layouts
- Offline `make verify` (byte-compile + ERT), autoload hygiene, and CI on Emacs 31
- Docs, README screenshots, and `bd` CLI contract notes
