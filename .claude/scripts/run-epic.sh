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

# Default prompt using heredoc for multi-line support
if [[ -z "${CLAUDE_PROMPT:-}" ]]; then
    read -r -d '' CLAUDE_PROMPT <<'PROMPT_EOF' || true
/speckit.implement claim next bead from the epic that is currently in_progress and use the commit skill when closed and STOP. Ensure by running tests that no regressions have been introduced BEFORE closing. Use the efficient-subagent-orchestration skill for main agent context-management. Use the beads-project-tracking skill to track progress.

CONTEXT MONITORING (CRITICAL): Continuously monitor your context usage. When you reach 50% context capacity, you MUST:
1. Leave the current bead as in_progress (do NOT close it)
2. Update the bead description to include: work completed, work remaining, and why you are handing off
3. Use the commit skill to commit any completed work
4. STOP immediately to allow a fresh context to continue

TECHNICAL DEBT TRACKING (MANDATORY):
- When you discover or create technical debt, create a 'discovered-from' bead to track it (unless already tracked)
- ALL TODO, FIXME, XXX, and HACK comments MUST include a bead ID that tracks the work. Format: TODO(bead-id): description
- Never leave implicit work—make it explicit in beads

BEAD SELECTION PRIORITY (CRITICAL):
1. FIRST: Check for any in_progress child beads (bd list --status in_progress) - these are handoff continuations, resume them
2. THEN: If no in_progress children, claim the next ready bead from bd ready
3. The root epic being in_progress is expected - look for its CHILDREN to continue work

ONE BEAD PER SESSION (CRITICAL):
- Work on exactly ONE bead per session
- Once you close a bead (bd close), STOP IMMEDIATELY
- Do NOT loop to the next bead - the shell wrapper script handles iteration
- After closing: commit, then STOP
PROMPT_EOF
fi

# Function to count ready beads for epic (includes epic itself and all children)
count_ready_beads() {
    bd ready --json 2>/dev/null | jq --arg epic "$EPIC_ID" '
        [.[] | select(.id == $epic or (.id | startswith($epic + ".")))] | length
    '
}

# Claim the epic if not already claimed
claim_epic_if_needed() {
    local status output

    if ! output=$(bd show "$EPIC_ID" --json 2>&1); then
        echo "[$(date -Iseconds)] ERROR: Failed to query epic $EPIC_ID: $output" >&2
        exit 1
    fi

    status=$(echo "$output" | jq -r '.[0].status // "unknown"')

    case "$status" in
        ready|open)
            echo "[$(date -Iseconds)] Claiming epic $EPIC_ID..."
            if ! bd update "$EPIC_ID" --status in_progress; then
                echo "[$(date -Iseconds)] ERROR: Failed to claim epic $EPIC_ID" >&2
                exit 1
            fi
            echo "[$(date -Iseconds)] Epic $EPIC_ID claimed successfully"
            ;;
        in_progress)
            echo "[$(date -Iseconds)] Epic $EPIC_ID already in_progress"
            ;;
        completed)
            echo "[$(date -Iseconds)] Epic $EPIC_ID is already completed. Nothing to do."
            exit 0
            ;;
        unknown)
            echo "[$(date -Iseconds)] ERROR: Epic $EPIC_ID not found or has invalid status" >&2
            exit 1
            ;;
        *)
            echo "[$(date -Iseconds)] Epic $EPIC_ID has status: $status (cannot proceed)"
            exit 1
            ;;
    esac
}

# Ensure epic is claimed before starting
claim_epic_if_needed

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
