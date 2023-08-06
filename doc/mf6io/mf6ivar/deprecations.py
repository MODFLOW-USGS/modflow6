import os
from typing import List, Tuple, Optional
from packaging.version import Version
from pathlib import Path


def get_deprecations(dfndir) -> List[Tuple[Path, str, Version, Optional[Version]]]:
    dfns = Path(dfndir).rglob("*.dfn")
    deps = {}
    for dfn in dfns:
        with open(dfn, "r") as f:
            name = None
            for line in f:
                if line.startswith("#"):
                    continue
                keys = ["deprecated", "removed"]
                ikeys = {k: i for i, k in enumerate(keys)}
                for key in keys:
                    if line.startswith("name"):
                        name = line.split()[1]
                    if line.startswith(key):
                        val = deps.get((dfn, key), [None, None])
                        key, ver = line.split()
                        ik = ikeys[key]
                        val[ik] = val[ik] if val[ik] else Version(ver)
                        deps[(dfn, name)] = val

    return [(file, key, dep, rem) for (file, key), (dep, rem) in deps.items()]


def create_deprecations_file(dfndir, mddir, verbose):
    deprecations = get_deprecations(dfndir)
    deps_path = (Path(mddir) / 'deprecations.md').absolute()
    if verbose:
        print(f"Found {len(deprecations)} deprecations, writing {deps_path}")
    with open(deps_path, "w") as f:
        s = "#### Deprecations\n\n"
        s += "The following table lists deprecated options and the versions in which they were deprecated and (optionally) removed.\n\n"
        if any(deprecations):
            s += "| Model-Package | Option | Deprecated | Removed |\n"
            s += "|:--------------|:-------|:-----------|:--------|\n"
            for (file, option, deprecated, removed) in deprecations:
                s += f"| {file.stem} | {option} | {deprecated} | {removed if removed else ''} |\n"
            if len(s) > 0:
                s += "\n"
        f.write(s)


if __name__ == '__main__':
    dfndir = os.path.join('.', 'dfn')
    mddir = os.path.join('.', 'md')
    create_deprecations_file(dfndir, mddir, verbose=True)
