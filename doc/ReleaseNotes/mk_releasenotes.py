# This script converts the release notes TOML file
# to a latex file to include in the release notes.
import argparse
import sys
from pathlib import Path
from warnings import warn

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("path")
    args = parser.parse_args()

    fname = "develop"
    fpath = Path(args.path).expanduser().absolute()
    fnametex = Path(f"{fname}.tex").absolute()
    fnametex.unlink(missing_ok=True)

    if not fpath.is_file():
        warn(f"Release notes TOML file not found: {fpath}")
        sys.exit(0)

    import tomlkit
    from jinja2 import Environment, FileSystemLoader

    loader = FileSystemLoader(fnametex.parent)
    env = Environment(
        loader=loader,
        trim_blocks=True,
        lstrip_blocks=True,
        line_statement_prefix="_",
        keep_trailing_newline=True,
    )
    template = env.get_template(f"{fnametex.name}.jinja")
    with open(fnametex, "w") as f:
        f.write(template.render(tomlkit.load(f"{fname}.toml")))
