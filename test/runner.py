import subprocess, pathlib, sys, difflib, time

COMPILER = "../target/debug/tahuc"

GREEN = "\033[92m"
RED = "\033[91m"
RESET = "\033[0m"

def compiler_command(input_file: pathlib.Path):
    if sys.platform == "win32":
        cmd = COMPILER + ".exe"
    else:
        cmd = COMPILER
    
    return [cmd, "--test", input_file]

def run_test_expected(src: pathlib.Path, expected: pathlib.Path, is_error: False):
    proc = subprocess.run(compiler_command(src), capture_output=True, text=True)
    actual = proc.stdout
    result = test_result(src, proc.returncode, is_error, proc.stderr)
    
    if result != 0:
        return result

    with open(expected, "r") as f:
        expected_output = f.read()

    if actual != expected_output:
        print(f"    Test failed: Output mismatch for {src}")
        print(f"    Expected:\n{expected_output}")
        print(f"    Actual:\n{actual}")

        print(f"    {RED}Test failed: Output mismatch for {src}{RESET}")
        diff = difflib.unified_diff(
            expected_output.splitlines(),
            actual.splitlines(),
            fromfile="expected",
            tofile="actual",
            lineterm=""
        )
        diff_text = "\n".join(diff)
        print(diff_text)
        return 1

    return 0
    

def run_test(src: pathlib.Path, is_error: False):
    proc = subprocess.run(compiler_command(src), capture_output=True, text=True)
    return test_result(src, proc.returncode, is_error, proc.stderr)
    

def test_result(src: pathlib.Path, return_code: int, is_error: bool, stderr: str):
    if is_error:
        if return_code == 0:
            msg = f"Expected error, but got success"
            print(f"    {RED}Test failed: {msg} for {src}{RESET}")
            return 1
    else:
        if return_code != 0:
            msg = f"Expected success, but got error\nStderr:\n{stderr}"
            print(f"    {RED}Test failed: {msg} for {src}{RESET}")
            return 1
    return 0


def main():
    start_time = time.time()
    faileds = []
    passed = []
    current_dir = pathlib.Path(__file__).parent
    files = [x for x in current_dir.rglob("*.tahu")]

    print("run:", files.__len__(), "test case")

    for tahu_file in files:
        is_error = tahu_file.stem.endswith("_error")
        expected_file = tahu_file.with_suffix(".expected")

        if expected_file.exists():
            result = run_test_expected(tahu_file, expected_file, is_error)
        else:
            result = run_test(tahu_file, is_error)

        if result == 0:
            passed.append(tahu_file)
        else:
            faileds.append(tahu_file)

    object = [x for x in current_dir.rglob("*.o")]
    for obj in object:
        obj.unlink()
            
    passed_count = len(passed)
    failed_count = len(faileds)

    if failed_count == 0:
        status_msg = f"{GREEN}OK{RESET}"
    else:
        status_msg = f"{RED}FAIL{RESET}"

    print(f"Result: {status_msg} — {passed_count} passed, {failed_count} failed")
    print(f"Finish: {time.time() - start_time}s")

    if faileds.__len__() != 0:
        return 1
    else:
        return 0


if __name__ == "__main__":
    sys.exit(main())