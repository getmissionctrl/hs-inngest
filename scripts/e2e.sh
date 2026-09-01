#!/usr/bin/env bash
# Headless end-to-end runner: start the Inngest dev server, run the example
# (which serves the app, registers, sends an event, and asserts completion),
# then tear the dev server down. Run inside the dev shell:
#
#   nix develop .#dev --command bash scripts/e2e.sh
set -uo pipefail
cd "$(dirname "$0")/.."

LOG=/tmp/inngest-e2e.log
echo "Starting inngest dev (logs: $LOG) ..."
inngest dev >"$LOG" 2>&1 &
IPID=$!
cleanup() { kill "$IPID" 2>/dev/null || true; }
trap cleanup EXIT

ok=0
for _ in $(seq 1 40); do
  if curl -sf http://127.0.0.1:8288/health >/dev/null 2>&1; then ok=1; break; fi
  sleep 1
done
if [ "$ok" -ne 1 ]; then
  echo "inngest dev did not become healthy"; tail -20 "$LOG"; exit 1
fi
echo "inngest dev healthy."

echo "Running example (cabal run) ..."
cabal run -v0 hs-inngest-example
rc=$?
echo "example exit code: $rc"
exit $rc
