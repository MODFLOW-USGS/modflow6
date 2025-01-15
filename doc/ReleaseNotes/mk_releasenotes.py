# This script converts the release notes TOML file
# to a latex file, from which is later built a PDF.
import argparse
import sys
from pathlib import Path
from warnings import warn

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("path")
    args = parser.parse_args()
    toml_path = Path(args.path).expanduser().absolute()
    if not toml_path.is_file():
        warn(f"Release notes TOML file not found: {toml_path}")
        sys.exit(0)

    fname = "develop"
    tex_path = Path(f"{fname}.tex").absolute()
    tex_path.unlink(missing_ok=True)

    import tomli
    from jinja2 import Environment, FileSystemLoader

    loader = FileSystemLoader(tex_path.parent)
    env = Environment(
        loader=loader,
        trim_blocks=True,
        lstrip_blocks=True,
        line_statement_prefix="_",
        keep_trailing_newline=True,
        # since latex uses curly brackets,
        # replace block/var start/end tags
        block_start_string="([",
        block_end_string="])",
        variable_start_string="((",
        variable_end_string="))",
    )
    template = env.get_template(f"{tex_path.name}.jinja")
    with open(tex_path, "w") as tex_file:
        with open(toml_path, "rb") as toml_file:
            notes = tomli.load(toml_file).get("notes")
            if not notes:
                warn("No release notes found, aborting")
                sys.exit(0)
            tex_file.write(template.render(notes=notes))
