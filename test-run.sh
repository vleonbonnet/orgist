#!/bin/bash
# Test runner for orgist.
#
# Usage:
#   ./test-run.sh                    # Replay all cached projects, then ERT suites
#   ./test-run.sh Orgtest            # Replay one project
#   ./test-run.sh ert                # Standalone ERT regression suites only
#   ./test-run.sh record Orgtest     # Record API responses (needs token)
#   ./test-run.sh live               # Live API test, all projects (needs token)
#   ./test-run.sh live Orgtest       # Live API test, one project (needs token)
#   ORGIST_TEST_TASK_ID=<id> ./test-run.sh live-attachments
#                                      # Live attachment CRUD (needs token)
#
# Output is saved to test-run.log alongside this script.
#
# NOTE (Windows): native Emacs 30.2 in --batch does NOT write message/stderr
# output to redirected pipes or files, so test-run.log and any captured stdout
# come back EMPTY even on a successful run. The process EXIT CODE is still
# correct (0 = all passed), so rely on that. This is an Emacs bug fixed in 31.1;
# once upgraded, normal redirection works and this note can be removed. To see
# per-assertion output before upgrading, preload test-capture.el (it tees
# `message' to the file named by the $ORGIST_TEST_CAPTURE env var).
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
LOG_FILE="$SCRIPT_DIR/test-run.log"

# Find emacs
EMACS="${EMACS:-$(command -v emacs 2>/dev/null || echo /ucrt64/bin/emacs)}"
if [ ! -x "$EMACS" ]; then
    echo "Error: emacs not found. Set EMACS=/path/to/emacs"
    exit 1
fi

# Resolve API token for record/live modes.
# pass may live in MSYS2 /usr/bin which isn't on the PATH in MINGW/UCRT shells.
PASS="$(command -v pass 2>/dev/null || true)"
if [ -z "$PASS" ] && [ -x /c/tools/msys64/usr/bin/pass ]; then
    PASS=/c/tools/msys64/usr/bin/pass
fi
if [ -z "$TODOIST_API_TOKEN" ] && [ -n "$PASS" ]; then
    # Extract 'token' field from multi-field pass entry
    TODOIST_API_TOKEN="$("$PASS" show web/todoist.com/valentin@leon.click 2>/dev/null \
        | awk '/^token:/{print $2}' || true)"
fi

# Per-project timeout (seconds); "all" mode runs sequentially
PROJECT_TIMEOUT="${PROJECT_TIMEOUT:-300}"

run_harness() {
    local mode="$1"
    shift
    local t="$PROJECT_TIMEOUT"
    # "all" mode needs much more time
    [ "$mode" = "all" ] && t=3600
    TODOIST_API_TOKEN="$TODOIST_API_TOKEN" \
        HOME="${HOME:-/c/Users/$USER}" \
        timeout "$t" "$EMACS" --batch \
        --chdir "$SCRIPT_DIR" \
        -l "$SCRIPT_DIR/test-harness.el" -- "$mode" "$@"
}

run_live() {
    local project="$1"
    local t="$PROJECT_TIMEOUT"
    local args=()
    if [ -n "$project" ]; then
        args=(-- "$project")
    else
        t=3600
        args=(-- "--full")
    fi
    TODOIST_API_TOKEN="$TODOIST_API_TOKEN" \
        HOME="${HOME:-/c/Users/$USER}" \
        timeout "$t" "$EMACS" --batch \
        --chdir "$SCRIPT_DIR" \
        -l "$SCRIPT_DIR/test-sync.el" "${args[@]}"
}

require_token() {
    if [ -z "$TODOIST_API_TOKEN" ]; then
        echo "Error: TODOIST_API_TOKEN not set and pass/gpg fallback failed."
        echo "Export TODOIST_API_TOKEN or set up password-store."
        exit 1
    fi
}

run_ert() {
    # Standalone ERT regression suites (exit code is the failure count)
    local file rc=0
    for file in test-due-string.el test-region-guard.el test-sync-token.el \
                test-id-cache.el test-body-spacing.el test-new-item-order.el \
                test-confirm.el test-local-links.el; do
        echo "Running ERT suite $file..."
        if ! timeout "$PROJECT_TIMEOUT" "$EMACS" --batch \
             --chdir "$SCRIPT_DIR" -L "$SCRIPT_DIR" \
             -l "$SCRIPT_DIR/test-capture.el" -l ert \
             -l "$SCRIPT_DIR/$file" -f ert-run-tests-batch-and-exit; then
            echo "ERT suite $file FAILED"
            rc=1
        fi
    done
    return "$rc"
}

main() {
    if [ $# -eq 0 ]; then
        # No args: replay all cached projects, then the ERT suites
        run_harness all
        run_ert
    elif [ "$1" = "record" ]; then
        if [ -z "$2" ]; then
            echo "Usage: $0 record <ProjectName>"
            exit 1
        fi
        require_token
        run_harness record "$2"
    elif [ "$1" = "live" ]; then
        require_token
        run_live "$2"
    elif [ "$1" = "ert" ]; then
        run_ert
    elif [ "$1" = "move" ]; then
        run_harness move
    elif [ "$1" = "state-log" ]; then
        run_harness state-log
    elif [ "$1" = "subprocess" ]; then
        run_harness subprocess
    elif [ "$1" = "format" ]; then
        run_harness format
    elif [ "$1" = "comments" ]; then
        run_harness comments
    elif [ "$1" = "features" ]; then
        run_harness features
    elif [ "$1" = "encoding" ]; then
        run_harness encoding
    elif [ "$1" = "completed" ]; then
        run_harness completed
    elif [ "$1" = "archived" ]; then
        run_harness archived
    elif [ "$1" = "metadata" ]; then
        run_harness metadata
    elif [ "$1" = "attachments" ]; then
        run_harness attachments
    elif [ "$1" = "live-attachments" ]; then
        require_token
        if [ -z "${ORGIST_TEST_TASK_ID:-}" ]; then
            echo "Error: ORGIST_TEST_TASK_ID must name a disposable Todoist task."
            exit 1
        fi
        local t="$PROJECT_TIMEOUT"
        TODOIST_API_TOKEN="$TODOIST_API_TOKEN" \
            HOME="${HOME:-/c/Users/$USER}" \
            timeout "$t" "$EMACS" --batch \
            --chdir "$SCRIPT_DIR" \
            -l "$SCRIPT_DIR/test-attachments-live.el"
    elif [ "$1" = "quick-add" ]; then
        require_token
        run_harness quick-add
    elif [ "$1" = "record-comments" ]; then
        if [ -z "$2" ]; then
            echo "Usage: $0 record-comments <ProjectName>"
            exit 1
        fi
        require_token
        run_harness record-comments "$2"
    else
        # Single project replay
        run_harness replay "$1"
    fi
}

# Run main, saving all output to the log file while still showing it.
main "$@" 2>&1 | tee "$LOG_FILE"
exit "${PIPESTATUS[0]}"
