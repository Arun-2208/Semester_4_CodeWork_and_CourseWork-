# FPA Demo & Generator Skeleton

This folder contains:
- `FPA_paper_summary_and_repro_plan.pptx` — a slide deck summarising the paper and an implementation plan.
- `fpa_demo.py` — a tiny runnable demo showing how a deception pattern can gate logic.
- `fpa_generator_skeleton.py` — scaffolding for a black-box generator + evaluator. Plug in an LLM API.

## Quick start

```bash
python fpa_demo.py
python fpa_generator_skeleton.py
```

In `fpa_generator_skeleton.py`, fill in `llm_predict_output` and (optionally) `llm_rewrite_code`
with your provider of choice to run full experiments. The script keeps all runtime checks local.
