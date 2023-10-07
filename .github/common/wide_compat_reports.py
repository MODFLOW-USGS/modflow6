"""
Converts compatibility reports from long to wide format
and makes a markdown table from the wide format report.
"""

from pathlib import Path
import pandas as pd
import sys

ip = Path(sys.argv[1])  # input file path
op = Path(sys.argv[2])  # output file path

assert ip.is_file()
assert ip.suffix == ".csv"
assert op.suffix == ".csv"

df = pd.pivot(
    pd.read_csv(ip),
    index="runner",
    columns=["compiler", "version"],
    values="support",
).sort_values(by=["runner"])
df.to_csv(op)
with open(op.with_suffix(".md"), "w") as file:
    file.write(
        df.to_markdown()
        .replace("nan", "")
        .replace("(", "")
        .replace(")", "")
        .replace(",", "")
        .replace("'", "")
    )
