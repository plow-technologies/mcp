#!/usr/bin/env bash
# Run Claude in a loop until no beads are ready for a given epic
#
# Usage: run-epic.sh <epic-id>
#
# Environment variables:
#   CLAUDE_MODEL    - Model to use (default: opus)
#   SESSION_LOG     - Log file path (default: ../session-log.jsonl)
#   CLAUDE_PROMPT   - Override the default prompt

set -euo pipefail

EPIC_ID="${1:-}"

if [[ -z "$EPIC_ID" ]]; then
    echo "Usage: $0 <epic-id>" >&2
    echo "Environment: CLAUDE_MODEL, SESSION_LOG, CLAUDE_PROMPT" >&2
    exit 1
fi

# Configuration (edit these or set via environment)
CLAUDE_MODEL="${CLAUDE_MODEL:-opus}"
SESSION_LOG="${SESSION_LOG:-../session-log.jsonl}"

CLAUDE_PROMPT="${CLAUDE_PROMPT:-/speckit.implement claim next bead from the epic that is currently in_progress and use the commit skill when closed and STOP. Ensure by running tests that no regressions have been introduced BEFORE closing. Use the efficient-subagent-orchestration skill for main agent context-management. Use the beads-project-tracking skill to track progress. When the current epic is finished, close the bead, use the commit skill and emit a message with a SINGLE line with a single word: DONE, otherwise a single line with a single NOTDONE}"

# Function to count ready beads for epic (includes epic itself and all children)
count_ready_beads() {
    bd ready --json 2>/dev/null | jq --arg epic "$EPIC_ID" '
        [.[] | select(.id == $epic or (.id | startswith($epic + ".")))] | length
    '
}

# Main loop
iteration=0
while true; do
    ready_count=$(count_ready_beads)
    iteration=$((iteration + 1))

    if [[ "$ready_count" -eq 0 ]]; then
        echo "[$(date -Iseconds)] No ready beads for epic $EPIC_ID. Done after $iteration iteration(s)!"
        exit 0
    fi

    echo "[$(date -Iseconds)] Iteration $iteration: $ready_count ready bead(s) for epic $EPIC_ID"
    echo "[$(date -Iseconds)] Running Claude (model=$CLAUDE_MODEL, log=$SESSION_LOG)..."

    # Run Claude with streaming JSON output, log to file
    claude -p "$CLAUDE_PROMPT" \
        --dangerously-skip-permissions \
        --model "$CLAUDE_MODEL" \
        --output-format stream-json \
        --verbose \
        | tee -a "$SESSION_LOG"

    echo ""  # Newline after Claude output
    sleep 2
done
