import argparse
from .gen import new_contest
from .exec_test import exec_test


def main():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers()

    # "new" subcommand
    subparser_new = subparsers.add_parser("new", help="Create a new directory for a contest.")
    subparser_new.add_argument("contest_id", type=str, help="Contest id")
    subparser_new.set_defaults(func=new_contest)

    # "test" subcommand
    subparser_test = subparsers.add_parser("test", help="Execute all testcases.")
    subparser_test.add_argument("task_id", type=str, help="Task id to run tests.")
    subparser_test.add_argument("prog", type=str, help="Executable program")
    subparser_test.add_argument("args", type=str, nargs="*", help="Arguments for executable")
    subparser_test.set_defaults(func=exec_test)

    args = parser.parse_args()
    if getattr(args, "func", None):
        args.func(args)
    else:
        parser.print_usage()
