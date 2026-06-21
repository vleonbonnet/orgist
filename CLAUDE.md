# CLAUDE.md

Instructions for Claude Code when working in this repository. See `orgist.org` for full project documentation.

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
- `orgist.org` — Project documentation, architecture, configuration reference
- `test-harness.el` — Offline test infrastructure (request mock, isolation, lifecycle runner)
- `test-run.sh` — Shell wrapper for replay, record, and live test modes
- `test-sync.el` — Live API sync test script
- `test-due-string.el` — ERT regression tests for hand-edited `TODOIST_DUE_STRING` write-back
- `test-capture.el` — Batch-output capture shim (works around the Emacs 30.2 Windows `--batch` stderr bug; see `test-run.sh`)
- `test-data/` — Shared cached API responses (`full-sync.json`, `incremental-sync.json`)
- `test-data-move/` — Synthetic fixture for the cross-project `move` test (invented projects/tasks, no live data)

## Dependencies

- `request` (Emacs HTTP library)
- `org` (Org-Mode)
- `pandoc` (optional, for markdown → org conversion)
