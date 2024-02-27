import argparse
import string
from itertools import repeat
from pathlib import Path
from warnings import warn

from fprettify.fparse_utils import InputStream

ALPHANUMERICS = set(string.ascii_letters + string.digits)


def join_comments(comments) -> str:
    return "".join([c for c in comments if any(c)])


class Transforms:
    @staticmethod
    def separate_lines(path, check, diff):
        """Variables defined on separate lines"""

        flines = []
        with open(path, "r") as f:
            stream = InputStream(f)
            while 1:
                line, comment, lines = stream.next_fortran_line()
                if not lines:
                    break
                line = line.rstrip()
                parts = line.rpartition("::")
                comment = join_comments(comment)

                if not parts[1] or "procedure" in parts[0]:
                    for l in lines:
                        flines.append(l.rstrip())
                    continue

                nspaces = len(lines[0]) - len(lines[0].lstrip())
                prefix = "".join(repeat(" ", nspaces))
                quals = [q.strip() for q in parts[0].split(",")]
                vars = [v.strip() for v in parts[2].split(",")]

                if not line:
                    continue
                if (
                    (len(parts[0]) == 0 and len(parts[1]) == 0)
                    or ("(" in parts[2] or ")" in parts[2])
                    or len(vars) == 1
                    or "parameter" in parts[0]
                ):
                    flines.extend(lines)
                else:
                    for s in vars:
                        if s == "&":
                            continue
                        l = prefix + ", ".join(quals)
                        l += f" :: {s}"
                        flines.append(l + comment)

        if check:
            warn("Check mode not implemented yet")
        elif diff:
            warn("Diff mode not implemented yet")
        else:
            with open(path, "w") as f:
                for line in flines:
                    f.write(line.rstrip() + "\n")

    @staticmethod
    def no_trailing_returns(path, check, diff):
        """Remove return statements at the end of routines"""

        flines = []
        with open(path, "r") as f:
            stream = InputStream(f)
            while 1:
                line, comment, lines = stream.next_fortran_line()
                if not lines:
                    break
                line = line.rstrip()
                comment = join_comments(comment)

                if comment.strip().lower().replace("-", "").replace(" ", "") in [
                    "!return"
                ]:
                    continue
                elif "return" in line:
                    continue
                else:
                    flines.extend(lines)

        if check:
            warn("Check mode not implemented yet")
        elif diff:
            warn("Diff mode not implemented yet")
        else:
            with open(path, "w") as f:
                for line in flines:
                    f.write(line.rstrip() + "\n")

    @staticmethod
    def no_empty_comments(path, check, diff):
        """Remove comments on lines with only whitespace"""

        flines = []
        with open(path, "r") as f:
            stream = InputStream(f)
            while 1:
                line, comment, lines = stream.next_fortran_line()
                if not lines:
                    break
                line = line.rstrip()
                comment = join_comments(comment)
                nspaces = len(lines[0]) - len(lines[0].lstrip())
                prefix = "".join(repeat(" ", nspaces))

                if not any(line):
                    c = comment.strip().replace(" ", "")
                    if c in ["!!", "!<"]:
                        flines.extend(lines)
                    elif any(set(c) & ALPHANUMERICS):
                        flines.append(prefix + comment)
                    elif "-" in c:
                        continue
                    else:
                        flines.append("")
                else:
                    flines.extend(lines)

        if check:
            warn("Check mode not implemented yet")
        elif diff:
            warn("Diff mode not implemented yet")
        else:
            with open(path, "w") as f:
                for line in flines:
                    f.write(line.rstrip() + "\n")

    @staticmethod
    def no_double_dashes(path, check, diff):
        """Remove '--' from the beginnings of comments"""

        flines = []
        with open(path, "r") as f:
            stream = InputStream(f)
            while 1:
                line, comment, lines = stream.next_fortran_line()
                if not lines:
                    break
                line = line.rstrip()
                comment = join_comments(comment)
                nspaces = len(lines[0]) - len(lines[0].lstrip())
                prefix = "".join(repeat(" ", nspaces))

                if not any(line):
                    flines.append(prefix + comment.strip().replace(" -- ", " "))
                else:
                    flines.extend(lines)

        if check:
            warn("Check mode not implemented yet")
        elif diff:
            warn("Diff mode not implemented yet")
        else:
            with open(path, "w") as f:
                for line in flines:
                    f.write(line.rstrip() + "\n")


def reformat(
    path,
    check,
    diff,
    separate_lines,
    no_return_statements,
    no_empty_comments,
    no_double_dashes,
):
    if separate_lines:
        Transforms.separate_lines(path, check, diff)
    if no_return_statements:
        Transforms.no_trailing_returns(path, check, diff)
    if no_empty_comments:
        Transforms.no_empty_comments(path, check, diff)
    if no_double_dashes:
        Transforms.no_double_dashes(path, check, diff)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        """
        Modify MODFLOW 6 Fortran source code, either writing to stdout or
        overwriting the input file. Options are provided for several code
        styles.
        """
    )
    parser.add_argument("path")
    parser.add_argument(
        "--check",
        action="store_true",
        default=False,
        required=False,
        help="Don't write the files back, just return the status. Return code 0 means nothing would change. Return code 1 means some files would be reformatted.",
    )
    parser.add_argument(
        "--diff",
        action="store_true",
        default=False,
        required=False,
        help="Don't write the files back, just print a diff to stdout to indicate what changes would have been made.",
    )
    parser.add_argument(
        "--separate-lines",
        action="store_true",
        default=True,
        required=False,
        help="Define dummy arguments and local variables on separate lines.",
    )
    parser.add_argument(
        "--no-trailing-returns",
        action="store_true",
        default=True,
        required=False,
        help="Remove return statements at the end of routines.",
    )
    parser.add_argument(
        "--no-empty-comments",
        action="store_true",
        default=True,
        required=False,
        help="Remove empty comments (containing only '!' or '!' followed by some number of '-' or '=').",
    )
    parser.add_argument(
        "--no-double-dashes",
        action="store_true",
        default=True,
        required=False,
        help="Remove double dashes from beginnings of comments (e.g., '! -- comment' becomes '! comment').",
    )
    args = parser.parse_args()
    reformat(
        path=Path(args.path).expanduser().absolute(),
        check=args.check,
        diff=args.diff,
        separate_lines=args.separate_lines,
        no_return_statements=args.no_trailing_returns,
        no_empty_comments=args.no_empty_comments,
        no_double_dashes=args.no_double_dashes,
    )
