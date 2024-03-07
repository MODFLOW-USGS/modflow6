import argparse
import string
from itertools import repeat
from multiprocessing import cpu_count, Pool
from pathlib import Path

from fprettify.fparse_utils import InputStream

ALPHANUMERICS = set(string.ascii_letters + string.digits)


def join_comments(comments) -> str:
    return "".join([c for c in comments if any(c)])


class Rules:
    @staticmethod
    def separate_lines(path):
        """Define dummy arguments, local variables, and procedure declarations on separate lines."""

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

                if (
                    not parts[1]
                    or "procedure" in parts[0]
                    or parts[0].strip().startswith("use")
                ):
                    flines.extend(lines)
                    continue

                indent = "".join(repeat(" ", len(lines[0]) - len(lines[0].lstrip())))
                quals = [q.strip() for q in parts[0].split(",")]  # qualifiers
                vars = [v.strip() for v in parts[2].split(",")]  # variable names

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
                        l = indent + ", ".join(quals)
                        l += f" :: {s}"
                        flines.append(l + comment)

        with open(path, "w") as f:
            for line in flines:
                f.write(line.rstrip() + "\n")

    @staticmethod
    def trailing_returns(path):
        """Remove return statements (and corresponding comments) at the end of routines."""

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
                elif "end subroutine" in line or "end function" in line:
                    for i, fl in enumerate(reversed(flines)):
                        l = fl.strip()
                        if not any(l):
                            continue
                        elif l == "return":
                            del flines[len(flines) - i - 1]
                        break
                flines.extend(lines)

        with open(path, "w") as f:
            for line in flines:
                f.write(line.rstrip() + "\n")

    @staticmethod
    def cleanup_comments(path):
        """
        Remove empty comments (containing only '!', or '!' followed by some number of '-' or '='),
        remove double dashes from beginnings of comments (e.g., '! -- comment' becomes '! comment'),
        remove 'SPECIFICATION' comment lines, and make internal comment spacing consistent (one space
        after '!' before text begins).
        """

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
                indent = "".join(repeat(" ", nspaces))

                if comment.startswith("#"):
                    # preprocessor directives
                    flines.extend(lines)
                elif not any(line):
                    if any(pattern in comment for pattern in ["!!", "!<", "!>"]):
                        flines.extend(lines)
                    elif "SPECIFICATIONS" in comment:
                        continue
                    elif any(set(comment) & ALPHANUMERICS):
                        comment = comment.strip().replace("--", "")
                        i = 0
                        for c in comment:
                            if c.isdigit() or c.isalnum():
                                break
                            i += 1
                        comment = f"! {comment[i:]}"
                        flines.append(indent + comment)
                    elif "-" in comment or "*" in comment:
                        continue
                    else:
                        flines.append("")
                else:
                    flines.extend(lines)

        with open(path, "w") as f:
            for line in flines:
                f.write(line.rstrip() + "\n")


def reformat(
    path,
    separate_lines,
    trailing_returns,
    cleanup_comments,
):
    if separate_lines:
        Rules.separate_lines(path)
    if trailing_returns:
        Rules.trailing_returns(path)
    if cleanup_comments:
        Rules.cleanup_comments(path)


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
        "--separate-lines",
        action="store_true",
        default=True,
        required=False,
        help="Define dummy arguments, local variables, and procedure declarations on separate lines.",
    )
    parser.add_argument(
        "--trailing-returns",
        action="store_true",
        default=True,
        required=False,
        help="Remove return statements (and corresponding comments) at the end of routines.",
    )
    parser.add_argument(
        "--cleanup-comments",
        action="store_true",
        default=True,
        required=False,
        help="Remove empty comments (containing only '!', or '!' followed by some number of '-' or '='), remove double dashes from beginnings of comments (e.g., '! -- comment' becomes '! comment'), remove 'SPECIFICATION' comment lines, and make internal comment spacing consistent (one space after '!' before text begins).",
    )
    args = parser.parse_args()

    # parse path
    path = Path(args.path).expanduser().absolute()
    assert path.exists(), f"Path not found: {path}"

    # parse rules
    sl = args.separate_lines
    tr = args.trailing_returns
    cc = args.cleanup_comments

    # reformat
    if path.is_file():
        reformat(
            path=Path(args.path).expanduser().absolute(),
            separate_lines=sl,
            trailing_returns=tr,
            cleanup_comments=cc,
        )
    else:
        with Pool(cpu_count()) as pool:
            files = [p for p in path.rglob("*.f90") if p.is_file()]
            pool.starmap(reformat, [(f, sl, tr, cc) for f in files])
