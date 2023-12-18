#!/usr/bin/env python
from subprocess import CompletedProcess, run
from sys import argv
from termcolor import colored
from os import path
from difflib import unified_diff as diff
from glob import glob
import re

def get_test_paths() -> list[str]:
    folder = path.dirname(path.realpath(__file__))
    return glob(folder + '/src/**/*.lsp', recursive=True)


def run_command(test_path: str, binary: str) -> CompletedProcess[str]:
    return run(f'{binary} < {test_path} ; echo Exit status:$?', shell=True, capture_output=True, text=True)


def disp_err(output: CompletedProcess[str], expected_output: str, expected_error: str) -> None:
    if output.stdout != expected_output:
        print("stdout:")
        delta = diff(output.stdout.split(), expected_output.split(), "glados", "test")
        for line in delta:
            print(line, end="")
        print()
    if output.stderr != expected_error:
        print("stderr:")
        delta = diff(output.stderr.split(), expected_error.split(), "glados", "test")
        for line in delta:
            print(line, end="")
        print()


def run_test(test_path: str, is_debug: bool, is_full_log: bool, has_color: bool) -> bool:
    output = run_command(test_path, "./glados")
    expected_output, expected_error = '', ''

    if path.exists(test_path.replace(".lsp", ".out")):
        with open(test_path.replace(".lsp", ".out")) as file:
            expected_output = file.read()

    if path.exists(test_path.replace(".lsp", ".err")):
        with open(test_path.replace(".lsp", ".err")) as file:
            expected_error = file.read()

    name = re.findall('/(\S+).lsp$', test_path)[0]

    if (output.stdout != expected_output) or (output.stderr != expected_error) or (expected_error != '' and output.returncode != 84):
        if is_debug:
            disp_err(output, expected_output, expected_error)
        err = f'Test "{name}" failed.'
        print(colored(err, "red") if has_color else err)
        return False
    if is_full_log:
        success = f'Test "{name}" passed.'
        print(colored(success, "green") if has_color else success)
    return True


if __name__ == "__main__":
    nb_passed, nb_failed = 0, 0
    tests = get_test_paths()
    nb_tests = len(tests)
    is_full_log = (len(argv) == 2 and "a" in argv[1])
    is_debug = (len(argv) == 2 and "d" in argv[1])
    has_color = (len(argv) == 2 and "c" in argv[1])

    for test in tests:
        passed = run_test(test, is_debug, is_full_log, has_color)
        nb_passed += 1 if passed else 0
        nb_failed += 1 if not passed else 0
    print(f"Ran {nb_tests} tests ({nb_passed} passed and {nb_failed} failed).")
    exit(nb_tests - nb_passed)
