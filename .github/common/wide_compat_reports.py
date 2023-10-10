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

# read long CSV
df = pd.read_csv(ip)

# workaround until intel compilers are installable on ubuntu-20.04 via setup-fortran
ub20_ifort = df[
    (df["compiler"] == "intel-classic") & (df["runner"] == "macos-12")
].copy()
ub20_ifort["runner"] = "ubuntu-20.04"
ub20_ifort["support"] = "?"
ub20_ifort[
    (ub20_ifort["runner"] == "ubuntu-20.04") & (ub20_ifort["version"] == 2021.7)
]["support"] = "&check;"

ub20_ifx = df[(df["compiler"] == "intel") & (df["runner"] == "ubuntu-22.04")].copy()
ub20_ifx["runner"] = "ubuntu-20.04"
ub20_ifx["support"] = "?"


df = pd.concat([df, ub20_ifort, ub20_ifx])

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
