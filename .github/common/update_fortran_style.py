import argparse
import re
from contextlib import nullcontext
from itertools import repeat
from pathlib import Path
from typing import Iterator, Optional
from warnings import warn

from fprettify.fparse_utils import InputStream

INTENT_PATTERN = re.compile(r".*(intent\(.+\)).*")


def get_intent(s) -> Optional[str]:
    result = INTENT_PATTERN.match(s)
    return result.group(1) if result else None


def get_param(s) -> bool:
    return "parameter" in s


def get_comments(comments) -> Iterator[str]:
    for comment in comments:
        if not any(comment):
            continue
        yield comment.rstrip()


class Transforms:
    @staticmethod
    def separate_lines(path, overwrite=False):
        """Variables defined on separate lines"""

        flines = []
        with open(path, "r") as f:
            stream = InputStream(f)
            while 1:
                line, comments, lines = stream.next_fortran_line()
                if not lines:
                    break
                line = line.rstrip()
                parts = line.rpartition("::")
                comments = " " + "".join(get_comments(comments))
                if not parts[1] or "procedure" in parts[0]:
                    for l in lines:
                        flines.append(l.rstrip())
                    continue

                nspaces = len(lines[0]) - len(lines[0].lstrip())
                prefix = "".join(repeat(" ", nspaces))
                vtype = parts[0].split(",")[0].strip()
                split = parts[2].split(",")
                intent = get_intent(parts[0])
                param = get_param(parts[0])

                if not line:
                    continue
                if (len(parts[0]) == 0 and len(parts[1]) == 0) or (
                    "(" in parts[2] or ")" in parts[2]
                ):
                    flines.append(prefix + line + comments)
                elif len(split) == 1:
                    flines.append(prefix + line + comments)
                elif param:
                    flines.append(prefix + line + comments)
                else:
                    for s in split:
                        if s.strip() == "&":
                            continue
                        l = prefix + vtype
                        if intent:
                            l += f", {intent}"
                        l += f" :: {s.strip()}"
                        flines.append(l + comments)

        with open(path, "w") if overwrite else nullcontext() as f:
            def write(line):
                if overwrite:
                    f.write(line + "\n")
                else:
                    print(line)

            for line in flines:
                write(line)

    @staticmethod
    def no_return_statements(path, overwrite=False):
        """Remove return statements at the end of routines"""
        # todo
        pass

    @staticmethod
    def no_empty_comments(path, overwrite=False):
        """Remove comments on lines with only whitespace"""
        # todo
        pass


def reformat(path, overwrite, separate_lines, no_return_statements, no_empty_comments):
    if separate_lines:
        Transforms.separate_lines(path, overwrite=overwrite)
    if no_return_statements:
        Transforms.no_return_statements(path, overwrite=overwrite)
        warn("--no-return not implemented yet")
    if no_empty_comments:
        Transforms.no_empty_comments(path, overwrite=overwrite)
        warn("--no-empty-comments not implemented yet")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        """
        Modify MODFLOW 6 Fortran source code, either writing to stdout or
        overwriting the input file. Options are provided for several code
        styles.
        """
    )
    parser.add_argument(
        "-i", "--input", help="path to input file"  # todo: or directory
    )
    parser.add_argument(
        "-f",
        "--force",
        action="store_true",
        default=False,
        required=False,
        help="overwrite/reformat files",
    )
    parser.add_argument(
        "--separate-lines",
        action="store_true",
        default=True,
        required=False,
        help="define variables on separate lines",
    )
    parser.add_argument(
        "--no-return_statements",
        action="store_true",
        default=False,
        required=False,
        help="no return statements at the end of routines",
    )
    parser.add_argument(
        "--no-empty-comments",
        action="store_true",
        default=False,
        required=False,
        help="no empty comments",
    )
    args = parser.parse_args()
    reformat(
        path=Path(args.input).expanduser().absolute(),
        overwrite=args.force,
        separate_lines=args.separate_lines,
        no_return_statements=args.no_return_statements,
        no_empty_comments=args.no_empty_comments,
    )
