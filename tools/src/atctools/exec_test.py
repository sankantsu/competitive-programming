import sys
import re
import pathlib
import subprocess
from difflib import unified_diff


def exec_test(args):
    task_id = args.task_id
    base_dir = pathlib.Path("testcases") / task_id
    infiles = sorted(base_dir.glob("in-*.txt"))
    for infile in infiles:
        num = re.search(r"in-(.*).txt", str(infile)).group(1)
        outfile = base_dir / f"out-{num}.txt"
        print(f"Running test {num}...")
        with infile.open() as in_f:
            cmd = [args.prog]
            proc = subprocess.run(cmd, stdin=in_f, capture_output=True, text=True)
            out = proc.stdout
        with outfile.open() as out_f:
            expected = out_f.readlines()
            actual = out.splitlines(keepends=True)
            diff_gen = unified_diff(expected, actual, fromfile="expected", tofile="actual")
            diff = "".join(diff_gen)
            if diff:
                print(diff, end="")
            else:
                print("Passed!")
