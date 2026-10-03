# CLAUDE.md

Instructions for Claude Code when working in this repository. See `README.org` for full project documentation.

## Project Overview

Orgist is an Emacs Lisp package for bidirectional sync between Todoist and Org-Mode. Read path is complete; write-back runs in dry-run mode.

## Commands

```bash
# Offline replay tests (primary — no API token needed)
./test-run.sh                     # All projects, then ERT suites
./test-run.sh Orgtest             # One project
./test-run.sh ert                 # Standalone ERT regression suites only

# Record fresh API responses (needs TODOIST_API_TOKEN or `pass`)
./test-run.sh record Orgtest

# Live API tests (needs TODOIST_API_TOKEN or `pass`), in a throwaway base dir
./test-run.sh live                # Pull all projects
./test-run.sh live Orgtest        # Pull one project
./test-run.sh live-writeback      # Write-back round trips on Orgtest (also needs pandoc)
```

Never run orgist batch code against the real `orgist-base-dir`: in batch, write-back executes without the review buffer for every file there.  Any run on real data, or a copy of it, sets `orgist-read-only`, which refuses every remote write by operation.

## File Structure

- `orgist.el` — Main source (all functionality)
- `orgist-confirm.el` — Write-back review adapter: builds the `org-sync-confirm` tree, fetches live Todoist state, executes the confirmed subset
- `org-sync-safety.el` — Tracker-agnostic safety net: shadow-git history (copies fallback), removal journal, trash, region and cycle guards; orgist's use lives in its `;;; Safety net` section
- `README.org` — Project documentation, architecture, configuration reference
- `test-harness.el` — Offline test infrastructure (request mock, isolation, lifecycle runner)
- `test-run.sh` — Shell wrapper for replay, record, and live test modes
- `test-sync.el` — Live API pull test script (throwaway base dir)
- `test-writeback-live.el` — Live write-back round trips on the Orgtest project (identity, description sub-headings); cleans up its tasks
- `test-due-string.el` — ERT regression tests for hand-edited `TODOIST_DUE_STRING` write-back
- `test-region-guard.el` — ERT regression tests for pull-side org command guards (active-region smear, todo-dependency blocking, done-keyword preservation)
- `test-sync-token.el` — ERT regression tests ensuring command-only writes cannot advance the read sync cursor
- `test-id-cache.el` — ERT regression tests for stale ID-cache markers (rescan instead of duplicating the heading on pull)
- `test-body-spacing.el` — ERT regression tests for body spacing normalization (blank line after a LOGBOOK drawer; stable under repeated passes)
- `test-log-notes.el` — ERT regression tests for log notes written in the body (`org-log-into-drawer` nil): the description goes below them, extraction and clearing leave them out
- `test-new-item-order.el` — ERT regression tests for new-task ordering (item_add child_order, sibling reorder in the same batch)
- `test-confirm.el` — ERT regression tests for the review adapter (tree building, live remote values, partial selection)
- `test-local-links.el` — ERT regression tests for local Org links across Markdown conversion
- `test-element-identity.el` — ERT regression tests for element identity (a pre-existing org-id survives the push; Todoist ID in `TODOIST_ID`)
- `test-description-subtree.el` — ERT regression tests for task descriptions (non-task child headings travel in the description; unchanged descriptions keep the org body; nested tasks are never deleted)
- `test-safety.el` — ERT tests for `org-sync-safety` (history backends, journal, trash, guards)
- `test-safety-orgist.el` — ERT tests for orgist's safety net (journaled deletions, sub-project deletion, rollback on smear or lost element, user hooks exempt, safe reset, journal restore)
- `test-seams.el` — ERT tests for the M1 seams (project-file registry, element lookup, remote operations and `orgist-read-only`, file policy, subprocess snapshot hand-back)
- `test-isolation.el` — Required first by every test file: sandboxes `user-emacs-directory`, so orgist defaults, org-id and org-persist never write into `~/.emacs.d`
- `test-capture.el` — Batch-output capture shim (works around the Emacs 30.2 Windows `--batch` stderr bug; see `test-run.sh`)
- `test-data/` — Shared cached API responses (`full-sync.json`, `incremental-sync.json`)
- `test-data-move/` — Synthetic fixture for the cross-project `move` test (invented projects/tasks, no live data)

## Dependencies

- `request` (Emacs HTTP library)
- `org-sync-confirm` (review buffer; `~/.emacs.d/elpaca/sources/org-sync-confirm`)
- `org` (Org-Mode)
- `pandoc` (optional, for markdown → org conversion)
