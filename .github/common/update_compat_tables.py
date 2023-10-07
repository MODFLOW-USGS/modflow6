"""
Updates compatibility tables in a markdown file.
"""

import re
import sys
from pathlib import Path

name = sys.argv[1]  # name of the table, e.g. "compile", "test"
compat_path = Path(sys.argv[2])  # compatibility report path
update_path = Path(sys.argv[3])  # path to markdown file to update

assert compat_path.is_file()
assert update_path.is_file()

with open(compat_path, "r") as compat:
    table = "".join(compat.readlines())
    r = re.compile(
        r"<!\-\- "
        + name
        + r" compat starts \-\->.*<!\-\- "
        + name
        + r" compat ends \-\->",
        re.DOTALL,
    )
    ct = (
        "<!-- "
        + name
        + " compat starts -->{}<!-- ".format("\n{}\n".format(table))
        + name
        + " compat ends -->"
    )
    readme = update_path.open().read()
    update_path.open("w").write(r.sub(ct, readme))
