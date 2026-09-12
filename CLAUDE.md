# CLAUDE.md

Instructions for Claude Code when working in this repository. See `README.org` for full project documentation.

## Project Overview

Orgist is an Emacs Lisp package for bidirectional sync between Todoist and Org-Mode. Read path is complete; write-back runs in dry-run mode.

## Commands

```bash
# Offline replay tests (primary — no API token needed)
./test-run.sh                     # All projects
./test-run.sh Orgtest             # One project

# Record fresh API responses (needs TODOIST_API_TOKEN or `pass`)
./test-run.sh record Orgtest

# Live API tests (needs TODOIST_API_TOKEN or `pass`)
./test-run.sh live                # All projects
./test-run.sh live Orgtest        # One project
```

## File Structure

- `orgist.el` — Main source (all functionality)
- `orgist-confirm.el` — Write-back confirmation buffer (`orgist-confirm-mode`)
- `README.org` — Project documentation, architecture, configuration reference
- `test-harness.el` — Offline test infrastructure (request mock, isolation, lifecycle runner)
- `test-run.sh` — Shell wrapper for replay, record, and live test modes
- `test-sync.el` — Live API sync test script
- `test-due-string.el` — ERT regression tests for hand-edited `TODOIST_DUE_STRING` write-back
- `test-region-guard.el` — ERT regression tests for pull-side org command guards (active-region smear, todo-dependency blocking, done-keyword preservation)
- `test-sync-token.el` — ERT regression tests ensuring command-only writes cannot advance the read sync cursor
- `test-id-cache.el` — ERT regression tests for stale ID-cache markers (rescan instead of duplicating the heading on pull)
- `test-body-spacing.el` — ERT regression tests for body spacing normalization (blank line after a LOGBOOK drawer)
- `test-new-item-order.el` — ERT regression tests for new-task ordering (item_add child_order, sibling reorder in the same batch)
- `test-capture.el` — Batch-output capture shim (works around the Emacs 30.2 Windows `--batch` stderr bug; see `test-run.sh`)
- `test-data/` — Shared cached API responses (`full-sync.json`, `incremental-sync.json`)
- `test-data-move/` — Synthetic fixture for the cross-project `move` test (invented projects/tasks, no live data)

## Dependencies

- `request` (Emacs HTTP library)
- `org` (Org-Mode)
- `pandoc` (optional, for markdown → org conversion)
