# fpa_generator_skeleton.py
"""
Skeleton for a black-box FPA generator + evaluator.

You will need to plug in your LLM API of choice (e.g., OpenAI/Anthropic/Google)
for the two TODO functions:
  - llm_predict_output(code_snippet: str) -> str
  - llm_rewrite_code(code_snippet: str) -> str

This keeps runtime equivalence checks local (exec) and uses the LLM only
for static 'interpretation' tasks.

Ethics: Use responsibly. Keep tests in controlled environments.
"""

from dataclasses import dataclass
from typing import Callable, List, Optional, Tuple
import ast
import textwrap
import traceback

# --------------------------- Utilities ---------------------------

def run_code_and_capture_value(code: str, var_name: str) -> Tuple[bool, Optional[object], str]:
    """
    Execute 'code' in a fresh globals/locals, then extract a variable value.
    Return (ok, value, error_message).
    """
    g, l = {}, {}
    try:
        exec(code, g, l)
        return True, l.get(var_name, g.get(var_name, None)), ""
    except Exception as e:
        return False, None, traceback.format_exc()

def safe_strip(code: str) -> str:
    return textwrap.dedent(code).strip() + "\n"

# --------------------------- LLM Stubs ---------------------------

def llm_predict_output(code_snippet: str) -> str:
    """
    TODO: Call your LLM to 'predict' the output/value.
    For example, prompt: 'What is the value of variable V after running this code? <code>'
    """
    raise NotImplementedError("Plug in your LLM API here.")

def llm_rewrite_code(code_snippet: str) -> str:
    """
    TODO: Ask the LLM to rewrite code to 'different author style' while preserving behavior.
    Used in plagiarism anti-clone scenario.
    """
    raise NotImplementedError("Plug in your LLM API here.")

# --------------------------- Patterns ---------------------------

@dataclass
class Pattern:
    name: str
    code_template: str      # A small function P and a call that assigns to V
    expected_var: str = "V" # Variable to read for ground-truth comparison

    def concrete(self) -> str:
        return safe_strip(self.code_template)

@dataclass
class Perturbation:
    name: str
    mutate: Callable[[str], str]   # maps code -> code

# --------------------------- Library ---------------------------

# Example pattern: vowel-check (familiar idiom)
VOWEL_PATTERN = Pattern(
    name="vowel_check",
    code_template="""
def is_vowel(c):
    return c in "aeiouAEIOU"
V = is_vowel('u')
"""
)

# Example perturbation: drop 'u' (deception pattern for 'u')
def drop_u_mutation(code: str) -> str:
    return code.replace('"aeiouAEIOU"', '"aeioAEIOU"')

DROP_U = Perturbation("drop_u", drop_u_mutation)

# --------------------------- Core Pipeline ---------------------------

def validate_pattern_with_llm(P: Pattern) -> bool:
    """
    Ensure P is both executable AND the LLM can correctly predict V.
    """
    ok, v, err = run_code_and_capture_value(P.concrete(), P.expected_var)
    if not ok:
        return False
    try:
        y = llm_predict_output(P.concrete())
    except NotImplementedError:
        # For dry runs, accept as True so you can focus on wiring.
        return True
    return str(v).strip() in str(y)

def generate_deception(P: Pattern, pert: Perturbation) -> Optional[str]:
    """
    Produce P' from P using a tiny valid edit; ensure runtime output differs.
    """
    P_code = P.concrete()
    P_prime = safe_strip(pert.mutate(P_code))

    ok1, v1, _ = run_code_and_capture_value(P_code, P.expected_var)
    ok2, v2, _ = run_code_and_capture_value(P_prime, P.expected_var)
    if not (ok1 and ok2):
        return None
    if v1 == v2:
        return None
    return P_prime

def embed_gate(host_program: str, P_prime: str, condition: str, target_block: str) -> str:
    """
    Embed P' and guard a block 't' with a condition (string of Python code: e.g., 'if V is True: ...').
    'target_block' should be valid indented code (e.g., print('target')).
    """
    host = safe_strip(host_program)
    gate = safe_strip(P_prime) + "\n" + condition + ":\n" + textwrap.indent(target_block, "    ")
    return host + "\n" + gate + "\n"

def example_host_program() -> str:
    return safe_strip("""
# Host program that does something benign
def main():
    x = 1 + 1
    return x
R = main()
""")

def evaluate_attack(x_prime: str, check_predicate: Callable[[dict], bool]) -> bool:
    """
    Execute x′ locally to ensure runtime behavior remains as intended (predicate satisfied).
    """
    g, l = {}, {}
    try:
        exec(x_prime, g, l)
    except Exception:
        return False
    return check_predicate({**g, **l})

# --------------------------- Scenarios ---------------------------

def scenario_static_output():
    P = VOWEL_PATTERN
    if not validate_pattern_with_llm(P):
        print("[info] Pattern not accepted by LLM in this dry-run (ok for now).")
    P_prime = generate_deception(P, DROP_U)
    assert P_prime is not None, "Failed to generate deception P′."
    x = example_host_program()
    # Gate behavior: if V is True, execute target
    condition = "if V is True"
    t_block = "FLAG = 'TARGET_BRANCH_EXECUTED'"
    x_prime = embed_gate(x, P_prime, condition, t_block)

    # Runtime predicate: ensure FLAG does NOT exist when c='u' in deception
    def predicate(env):
        return env.get("FLAG") is None and env.get("R") == 2

    ok = evaluate_attack(x_prime, predicate)
    print(f"[static-output scenario] runtime_ok={ok}")
    return x_prime

if __name__ == "__main__":
    print("Generating an example x′ for the static-output scenario...")
    x_prime = scenario_static_output()
    print("--- x′ (attack sample) ---")
    print(x_prime)
