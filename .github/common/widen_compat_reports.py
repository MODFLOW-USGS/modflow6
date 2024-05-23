"""
Converts compatibility reports from long to wide format
and makes a markdown table from the wide format report.
"""

import sys
from pathlib import Path

import pandas as pd

ip = Path(sys.argv[1])  # input file path
op = Path(sys.argv[2])  # output file path
assert ip.is_file()
assert ip.suffix == ".csv"
assert op.suffix == ".csv"

# read long CSV
df = pd.read_csv(ip)

# pivot and sort
df = pd.pivot(
    df,
    index="runner",
    columns=["compiler", "version"],
    values="support",
).sort_values(by=["runner"])

# write wide CSV
df.to_csv(op)

# write wide markdown table
with open(op.with_suffix(".md"), "w") as file:
    file.write(
        df.to_markdown()
        .replace("nan", "")
        .replace("(", "")
        .replace(")", "")
        .replace(",", "")
        .replace("'", "")
    )
