#!/usr/bin/env python
from subprocess import CompletedProcess, run
from sys import argv
from termcolor import colored
from os import path
from difflib import unified_diff as diff
from glob import glob
import re

LANGUAGES = [("Lisp", ".lsp", "lisp", "lisp", ""), ("Dreamberd Compile + Execute", ".db4", "dreamberd", "compile", ";./glados execute a.out"), ("Dreamberd run", ".db4", "dreamberd", "run", "")]

def get_test_paths(folder_name: str, extension: str) -> list[str]:
    folder = path.dirname(path.realpath(__file__))
    return glob(folder + f'/src/{folder_name}/*{extension}', recursive=True)

def run_command(test_path: str, binary: str, after_command: str) -> CompletedProcess[str]:
    return run(f'{binary} {test_path} {after_command}', shell=True, capture_output=True, text=True)

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

def run_test(test_path: str, extension: str, flag: str, after_command: str, is_debug: bool, is_full_log: bool, has_color: bool) -> bool:
    output = run_command(test_path, f"./glados {flag}", after_command)
    expected_output, expected_error = '', ''

    if path.exists(test_path.replace(extension, ".out")):
        with open(test_path.replace(extension, ".out")) as file:
            expected_output = file.read()

    if path.exists(test_path.replace(extension, ".err")):
        with open(test_path.replace(extension, ".err")) as file:
            expected_error = file.read()

    name = re.findall('[^/]*$', test_path)[0]

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
    is_full_log = (len(argv) == 2 and "a" in argv[1])
    is_debug = (len(argv) == 2 and "d" in argv[1])
    has_color = (len(argv) == 2 and "c" in argv[1])

    total_tests, total_passed = 0, 0

    for name, extension, folder, flag, after_command in LANGUAGES:
         print(f"\n\n===================================================================================================================" + (len(name) - 6) * "=")
         print(f"================================================== TEST [{name}] ==================================================")
         print(f"===================================================================================================================" + (len(name) - 6) * "=" + "\n")
         nb_passed, nb_failed = 0, 0
         test_passed, test_failed = [], []
         tests = get_test_paths(folder_name=folder, extension=extension)
         nb_tests = len(tests)

         for test in tests:
             name = re.findall('[^/]*$', test)[0]
             passed = run_test(test, extension, flag, after_command, is_debug, is_full_log, has_color)
             nb_passed += 1 if passed else 0
             test_passed.append(name) if passed else test_failed.append(name)
             nb_failed += 1 if not passed else 0
         print(f"\n##################################################################################################################")
         print(f"#================================================= RESULTS ======================================================#")
         print(f"#================================================================================================================#")
         print(f"#                                              Ran {nb_tests} tests")
         print(f"# {nb_passed} passed :")
         for test in test_passed:
            print(f"#   - {test}")
         print(f"#\n# {nb_failed} failed :")
         for test in test_failed:
            print(f"#   - {test}")
         print(f"#\n##################################################################################################################\n")
         total_tests += nb_tests
         total_passed += nb_passed

    exit(nb_tests - nb_passed)