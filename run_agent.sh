#!/usr/bin/env bash
set -euo pipefail

project_root="$(cd "$(dirname "$0")" && pwd)"
cd "$project_root"

python_bin="$project_root/.venv/bin/python"
if [[ ! -x "$python_bin" ]]; then
	python_bin="python3"
fi

if [[ "${1-}" == "compare" ]]; then
	echo "Note: Compare mode non è ancora implementato in wumpus.py"
	echo "Eseguendo agentA..."
	"$python_bin" wumpus/wumpus.py -m wumpus/map1.json -a agentA/agent.clp
else
	"$python_bin" wumpus/wumpus.py -m wumpus/map1.json -a agentA/agent.clp
fi
