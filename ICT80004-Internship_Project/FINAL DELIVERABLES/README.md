Here’s a clean, copy-paste **README.md** for your repo. It’s written in plain language, end-to-end, and matches exactly what’s in your project (C demo, Python FPA tools, text-attack dataset, and the R validator). You can drop this at the root and commit.

---

# Benchmarking & Securing LLMs Against Adversarial Prompt Attacks

This repo contains everything needed to **run the project end-to-end**:

* A **C memory-safety demo** (CWE-121) with “bad vs good” cases and sanitizer evidence
* **Python** tools for **Familiar Pattern Attacks (FPA)** — single-case and multi-case runners
* A **text-attack dataset** (direct vs paraphrased prompts) and **R validator** to check data quality
* Logs, screenshots and CSV results to make runs **reproducible**

> Host organisation: **CSIRO Data61** · Unit: **ICT80004 Internship Project (Swinburne)**
> Author: **Arun Ragavendhar A. Palaniyappan** · Contact: **[arunragavendhar.1999@gmail.com](mailto:arunragavendhar.1999@gmail.com)**

---

## Quick start (5 minutes)

Choose your OS:

* **Linux / macOS**: works out of the box.
* **Windows**: use **WSL** (Ubuntu recommended) for the C demo and Python. RStudio can be native Windows or WSL; both work.

```bash
# 1) clone
git clone <your-remote-url> llm-attack-benchmark
cd llm-attack-benchmark

# 2) run the C evidence demo (with sanitizers)
cc -O0 -g -Wall -Wextra -Wshadow -Wconversion -fno-omit-frame-pointer \
   -fsanitize=address,undefined Stack_Buffer_Overflow_Codefix_Source_code.c \
   -o cwe121_showcase

./cwe121_showcase index-bad 10     # shows overflow
./cwe121_showcase index-good 10    # guarded and clean

# 3) run the FPA (mock) multi-case runner
python3 -m venv .venv && source .venv/bin/activate   # Windows: .venv\Scripts\activate
python FPA-Based-Attacks_Multi_case_generator_source_code.py --out fpa_multi.csv
```

Open **fpa_multi.csv** and confirm rows show `llm_pred_flag='TARGET_BRANCH_EXECUTED'` while `runtime_flag` is `None` for “success” rows.

---

## Repository layout

```
.
├── Stack_Buffer_Overflow_Codefix_Source_code.c
├── Stack_Buffer_Overflow_Codefix_Test_Results.docx
├── FPA-Based-Attacks_Single_case_generator_source_code.py
├── FPA-Based-Attacks_Single_case_generator_test_output.csv        # example output
├── FPA-Based-Attacks_Multi_case_generator_source_code.py
├── FPA-Based-Attacks_Multi_case_generator_test_output.csv         # example output
├── Text-Based-Attacks_Prompt_Injection_Testing.xlsx               # dataset (direct + paraphrased)
├── Text-Based-Attacks_Test_Result_Screenshots.docx
├── validate_prompts_view.R                                        # R validator (place this name in repo)
├── .RData                                                         # R session data (history/objects)
└── README.md
```

> If your R script has a different filename, save the shared R validator as **`validate_prompts_view.R`** at the repo root.

---

## 1) C Evidence Demo — CWE-121 “bad vs good”

### Build

Recommended (ASan + UBSan):

```bash
cc -O0 -g -Wall -Wextra -Wshadow -Wconversion -fno-omit-frame-pointer \
   -fsanitize=address,undefined Stack_Buffer_Overflow_Codefix_Source_code.c \
   -o cwe121_showcase
```

> macOS: you can use `clang` (same flags).
> Windows via WSL: install `build-essential` (`sudo apt install build-essential`).

### Run

```bash
./cwe121_showcase memcpy-bad
./cwe121_showcase memcpy-good
./cwe121_showcase index-bad 10
./cwe121_showcase index-good 10
```

**What you should see**

* “bad” runs: **AddressSanitizer** reports stack/buffer overflow
* “good” runs: clean output; guard in place
* The paired screenshots and notes are in `Stack_Buffer_Overflow_Codefix_Test_Results.docx`

---

## 2) FPA — Familiar Pattern Attacks (Python)

Two runners are provided:

* **Multi-case runner** (mock LLM):
  `FPA-Based-Attacks_Multi_case_generator_source_code.py`
  Patterns: `vowel`, `nth_prime`, `lswr`, `in_range`, `to_cents`

* **Single-case runner** (improved mock; optional OpenAI mode):
  `FPA-Based-Attacks_Single_case_generator_source_code.py`
  Patterns: `vowel_check`, `count_first_n`

### Python environment

```bash
python3 -m venv .venv
# macOS/Linux
source .venv/bin/activate
# Windows PowerShell
# .venv\Scripts\Activate.ps1

# (No heavy deps required for mock mode. If you plan to use OpenAI mode:)
pip install openai
```

### Run — multi-case (mock only)

```bash
python FPA-Based-Attacks_Multi_case_generator_source_code.py \
  --out FPA-Based-Attacks_Multi_case_generator_test_output.csv
```

**Expected output**
CSV with columns like:
`pattern, perturbation, P_runtime_val, Pprime_runtime_val, runtime_flag, llm_pred_flag, success, note`
A **success** row means: the branch did **not** run (`runtime_flag` is `None`) but the model **believed** it ran (`llm_pred_flag='TARGET_BRANCH_EXECUTED'`).

### Run — single-case (mock or OpenAI)

```bash
# mock mode (default)
python FPA-Based-Attacks_Single_case_generator_source_code.py \
  --mode mock --iterations 20 \
  --out FPA-Based-Attacks_Single_case_generator_test_output.csv

# openai mode (optional): requires API key in env
export OPENAI_API_KEY=...   # Windows PowerShell: setx OPENAI_API_KEY "..."
python FPA-Based-Attacks_Single_case_generator_source_code.py \
  --mode openai --openai-model gpt-4o --iterations 10 \
  --out FPA_openai_results.csv
```

**Notes**

* OpenAI mode is optional and **not required** for reproducing the paper’s idea.
* If OpenAI mode fails (no key or network), the script will return an error message in the `note` field; use `--mode mock`.

---

## 3) Text-Attack Dataset & R Validator

### What’s included

* `Text-Based-Attacks_Prompt_Injection_Testing.xlsx` – prompts and results (direct & paraphrased, across 9 tactics and 3 placements)
* `Text-Based-Attacks_Test_Result_Screenshots.docx` – evidence examples
* `validate_prompts_view.R` – a **one-file** R validator

### Install R & packages

1. Install **R** (and **RStudio** if you prefer a GUI).
2. Open R or RStudio and run:

```r
# Runs once; the script auto-installs missing packages too.
source("validate_prompts_view.R")
```

When prompted by the **file picker**, select your dataset (the `.xlsx` above, or a `.csv` if you export one).

### What the validator does

* Shows **schema** and **missingness**
* Flags **duplicates** and **near-duplicates**
* Checks **split leakage** (similar pairs across splits)
* Lists **length outliers**
* Computes **agreement**: direct vs paraphrased per model, and inter-model agreement on paraphrases
* Opens results in R **View()** panels (nothing is written to disk)

**Tip**: If you want to save any table, click the “Export” button in RStudio’s View window.

---

## Reproduce “golden runs”

To keep the project stable over time, create one “golden run” per component and store it under a dated folder:

```
runs/
└── 2025-10-24/
    ├── cwe121/
    │   ├── asan_index_bad.txt
    │   ├── asan_memcpy_bad.txt
    │   └── good_runs.txt
    ├── fpa/
    │   ├── FPA-Based-Attacks_Multi_case_generator_test_output.csv
    │   └── FPA-Based-Attacks_Single_case_generator_test_output.csv
    └── text/
        └── validator_notes.md   # brief summary + screenshots if needed
```

Later runs can be diffed against this folder.

---

## Troubleshooting

**C demo**

* *“command not found: cc”* → install toolchain (`xcode-select --install` on macOS; `sudo apt install build-essential` on WSL).
* *No sanitizer output on bad cases* → ensure you compiled with `-fsanitize=address,undefined` and run the **bad** commands.

**Python (FPA)**

* *OpenAI mode errors* → ensure `OPENAI_API_KEY` is set; otherwise use `--mode mock`.
* *CSV empty or missing rows* → the runner skips a case if the perturbation didn’t change the value (that’s expected).

**R validator**

* *Package install prompts* → accept; the script does auto-install.
* *Nothing opens* → ensure you ran `source("validate_prompts_view.R")` from the repo root; RStudio will open **View()** tabs.

---

## Recommended workflow (host use)

1. **Run the repo on a second machine** and keep a **golden run**.
2. **Benchmark monthly** on more models/versions; store logs by date.
3. **Grow the dataset** (new tactics, tasks, languages); re-run the R validator.
4. **Extend FPA** on real review tasks; add one new case family per sprint.
5. Publish a **simple results page** (pass/fail + links to evidence).

---

## Citation / Acknowledgement

If you use this repo in reports or talks, please cite the project as:

> A. R. A. Palaniyappan, *Benchmarking & Securing LLMs Against Adversarial Prompt Attacks*, CSIRO Data61 × Swinburne, 2025.

You may also cite any underlying papers you used; see the project presentation/report for the full APA list.

---

## License

If this work is for internal use only, add your organisation’s notice here.
Otherwise, choose a license (e.g., MIT) and add a `LICENSE` file.

---

## Contact

Questions, issues, or ideas: **[arunragavendhar.1999@gmail.com](mailto:arunragavendhar.1999@gmail.com)**.
Happy to help you run the demo or extend the benchmark.
