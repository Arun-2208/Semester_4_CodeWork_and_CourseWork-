# FPA Generator — Complete End-to-End Script

Files created:
- `fpa_generator_complete.py` — end-to-end runner with mock and OpenAI LLM modes.
- Output CSV: `/mnt/data/fpa_results.csv` (after running experiments).

## Modes

### Mock mode (recommended first)
Simulates abstraction bias: the LLM prediction is simulated by assuming the original pattern output.
Run locally without API keys:
```
python fpa_generator_complete.py --mode mock --iterations 10 --out /mnt/data/fpa_results.csv
```

### OpenAI mode (real LLM; requires API key)
Install OpenAI Python package and set your API key in the environment:
```
pip install openai
export OPENAI_API_KEY="sk-..."
python fpa_generator_complete.py --mode openai --openai-model gpt-4o --iterations 20 --out /mnt/data/fpa_results.csv
```

Notes:
- The OpenAI wrapper expects the `openai` package and uses ChatCompletion.create in this script.
- The script asks the LLM to return a JSON object containing the variable `V` (or the variable asked for) to simplify parsing.
- The MockLLM is intentionally biased (returns original pattern outputs) to simulate the abstraction bias so you can reproduce mismatch cases without external calls.

## Extending patterns & perturbations
- Add a new `Pattern` to PATTERNS with `code_template` that sets `V` to the value of interest.
- Add a new `Perturbation` with a mutate function that transforms code_template reproducibly.
- The runner will attempt P' for each perturbation and record cases where runtime values change and how the LLM predicts.

## Ethics & safety
Use only on code you own or have explicit permission to test. Record meta-data and keep experiments isolated from production systems.

