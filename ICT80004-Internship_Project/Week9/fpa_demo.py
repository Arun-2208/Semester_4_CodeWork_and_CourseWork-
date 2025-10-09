# fpa_demo.py
"""
Minimal demo of a Familiar Pattern Attack (FPA) idea in Python.

This does NOT query an LLM. It just shows how a tiny, valid bug (P')
can gate a target branch t, while runtime behavior stays consistent.
In an LLM static analysis setting, the model often 'assumes' the familiar
pattern is correct and misreads the control flow.
"""

def is_vowel_correct(c: str) -> bool:
    # Familiar idiom
    return c in "aeiouAEIOU"

def is_vowel_deceptive(c: str) -> bool:
    # Deception pattern: missing 'u' (tiny, valid, deterministic difference)
    return c in "aeioAEIOU"

def target_behavior():
    # This is the branch the LLM is supposed to believe runs
    return "TARGET_BRANCH_EXECUTED"

def program_with_guard(char: str, use_deception: bool = True) -> str:
    """
    Gate the 'target_behavior' behind the familiar function output.
    For a character 'u', the correct function returns True; deceptive returns False.
    """
    vowel_fn = is_vowel_deceptive if use_deception else is_vowel_correct
    if vowel_fn(char):             # The LLM may assume this is the standard vowel check
        return target_behavior()   # ...and 'see' this branch as taken
    return "NORMAL_PATH"

def main():
    for ch in ["a", "e", "i", "o", "u", "x"]:
        res_correct = program_with_guard(ch, use_deception=False)
        res_decept  = program_with_guard(ch, use_deception=True)
        
        print(f"char={ch!r}  correct={res_correct:>20} deceptive={res_decept:>20}")

if __name__ == "__main__":
    main()



'''res_decept  = program_with_guard(ch, use_deception=True)

deceptive={res_decept:>20}'''