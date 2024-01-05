# list of input issues
#
# general
#   auxiliary should be AUXNAMES auxnames(naux)
#   differentiate somehow a block that has repeating records from other block types?
#   should mark time series variables in some way special.  Maybe an *?
#   should we begin all character string variables with a 'c'?
#
# mfsim.nam
#   add dimensions block to sim name file? (NMODELS, NEXCHANGES, NSOLUTIONGROUPS)
#   add NSLNMODELS TO NUMERICAL line so we now how many models to read
#   mxiter is inside solution_group (label the block instead?)
#   change 'NUMERICAL' to 'SMS'
#
# tdis
#   read perlen, nstp, tsmult separately as arrays
#
# gwfgwf
#   change blockname EXCHANGEDATA? DATA or LIST or GWFGWFDATA
#   change auxiliary to auxnames(naux)
#   Move to different part of user guide?
#
# sms
#
# dis
#
# disu
#
# ic
#   no options, so what do we do?  allow empty options blocks?  dummy option?
#
# chd
#   change to OBS8_FILENAME obs8_filename
#   change AUXILIARY to AUXNAMES
#
# wel
#   change to OBS8_FILENAME obs8_filename
#   change AUXILIARY to AUXNAMES
#
# drn
#   change to OBS8_FILENAME obs8_filename
#   change AUXILIARY to AUXNAMES
#
# riv
#   change to OBS8_FILENAME obs8_filename
#   change AUXILIARY to AUXNAMES
#
# ghb
#   change to OBS8_FILENAME obs8_filename
#   change AUXILIARY to AUXNAMES
#
# rch
#   change to OBS8_FILENAME obs8_filename
#   change AUXILIARY to AUXNAMES
#   do not support array-based input for DISU
#   looks like code can be cleaned up quite a bit if we require lists for DISU
#   for array-based input, should arrays be optional?
#
# evt
#   change to OBS8_FILENAME obs8_filename
#   change AUXILIARY to AUXNAMES
#   do not support array-based input for DISU
#   looks like code can be cleaned up quite a bit if we require lists for DISU
#   for array-based input, should arrays be optional?
#   do not support segmented ET for array-based input
#
# maw
#   need to remove the word WELL as the first thing in the WELLS block
#   change to OBS8_FILENAME obs8_filename
#   change AUXILIARY to AUXNAMES
#   suggest renaming ngwfnodes to ncon or nconn
#   change 'WELLS' to 'MAWDATA' (do this consistently throughout SFR, LAK, etc.?)
#   change 'WELL_CONNECTIONS' to 'CONNECTIONS'
#   eliminate 'STEADY-STATE' keyword from period block
#   Change ACTIVE, INACTIVE, and CONSTANT into values for STATUS.
#
# sfr
#   change to OBS8_FILENAME obs8_filename
#   change AUXILIARY to AUXNAMES
#   change from SFR_OUTPUT to SFROUTPUT_FILENAME?  or OUTPUT_FILENAME
#   rno does not correspond to implicit integer definition
#   unit conversion used in sfr, length/time conversion used in lake
#   need to implement STATUS
#
# lak
#   change to OBS8_FILENAME obs8_filename
#   change AUXILIARY to AUXNAMES
#   unit conversion used in sfr, length/time conversion used in lake
#   change time_conversion to timefactor?
#   change LAKE_CONNECTIONS to CONNECTIONS
#   change LAKE_TABLES to TABLES
#   change FILE ctabname to TABLE_FILENAME table_filename
#   STATUS not implemented yet, but it is described in input instructions
#   invert indicates an integer variable.  change to dinvert?
#   time series variables are listed as "real or character ..." should just be double precision
#   capitalize example input file words that are recognized by mf6
#
# uzf
#   change to OBS8_FILENAME obs8_filename
#   change AUXILIARY to AUXNAMES
#   Remove steady-state / transient flag
#   Change DATA block to UZFDATA
#   boundname not implemented.  Should be implemented for uzfdata?
#   aux not implemented.  should be implemented for period block?
#   implement uzfsettings approach?  Or stick with a simple list?
#   example shows 'uzf' keyword as first item in period block
#   combine SIMULATE_ET, LINEAR_GWET, and SQUARE_GWET into a single line?
#
# mvr
#   change maxpackages to npackages
#   Included WEL, DRN, RIV, GHB as providers, though that is not supported in the code yet
#
# oc
#   output control rewritten entirely, and implemented in the code
#

# DEFINITION FILE KEYWORDS
# block :: name of block
# name :: variable name
# in_record :: optional True or False, False if not specified
# type :: recarray, record, keyword, integer, double precision, keystring
# tagged :: optional True or False, True if not specified. If tagged, then keyword comes before value
# shape :: (size), optional, only required for arrays
# valid :: description of valid values
# reader :: urword, readarray, u1dint, ...
# optional :: optional True or False, False if not specified
# longname :: long name for variable
# description :: description for variable, REPLACE tag indicates that description will come from common.dfn


from argparse import ArgumentParser
import os
from pathlib import Path
from collections import OrderedDict
import re
import shutil
from typing import Iterable

mf6ivar_dir = Path(__file__).parent


def parse_mf6var_file(fname):
    f = open(fname, "r")
    lines = f.readlines()
    f.close()

    vardict = OrderedDict()
    vd = {}

    for line in lines:
        # skip blank lines
        if len(line.strip()) == 0:
            if len(vd) > 0:
                name = vd["name"]
                if "block" in vd:
                    block = vd["block"]
                    key = (name, block)
                else:
                    key = name
                if key in vardict:
                    raise Exception("Variable already exists in dictionary: " + name)
                vardict[key] = vd
            vd = {}
            continue

        # skip comments
        if "#" in line.strip()[0]:
            continue

        ll = line.strip().split()
        if len(ll) > 1:
            k = ll[0]
            istart = line.index(" ")
            v = line[istart:].strip()
            if k in vd:
                raise Exception("Attribute already exists in dictionary: " + k)
            vd[k] = v

    if len(vd) > 0:
        name = vd["name"]
        if "block" in vd:
            block = vd["block"]
            key = (name, block)
        else:
            key = name
        if key in vardict:
            raise Exception("Variable already exists in dictionary: " + name)
        vardict[key] = vd
    return vardict


COMMONDESCRIPTIONS = parse_mf6var_file(mf6ivar_dir / "dfn" / "common.dfn")

VALID_TYPES = [
    "integer",
    "double precision",
    "string",
    "keystring",
    "keyword",
    "recarray",
    "record",
]


def block_entry(varname, block, vardict, prefix="  "):
    key = (varname, block)
    v = vardict[key]

    s = "{}".format(varname.upper())
    if "tagged" in v:
        if v["tagged"] == "false":
            s = ""

    # set up the time series marker @
    tsmarker = ""
    if "time_series" in v:
        if v["time_series"] == "true":
            tsmarker = "@"

    # check valid type
    vtype = v["type"]
    if vtype == "double precision":
        pass
    elif " " in vtype:
        vtype = vtype.split(" ", 1)[0]
    if vtype not in VALID_TYPES:
        raise ValueError(
            "{}: {}: {!r} is not a valid type from {}".format(
                fname, key, vtype, VALID_TYPES
            )
        )

    # record or recarray
    if v["type"].startswith("rec"):
        varnames = v["type"].strip().split()[1:]
        s = ""
        for vn in varnames:
            blockentry = block_entry(vn, block, vardict, prefix="")
            s += "{} ".format(blockentry.strip())
        if v["type"].startswith("recarray"):
            s = s.strip()
            s = "{}{}\n{}{}\n{}{}".format("", s, prefix, s, prefix, "...")

    # layered
    elif v["reader"] in ["readarray", "u1ddbl", "u2ddbl", "u1dint"]:
        shape = v["shape"]
        reader = v["reader"].upper()
        layered = ""
        if "layered" in v:
            if v["layered"] == "true":
                layered = " [LAYERED]"
        s = "{}{}\n{}{}<{}{}> -- {}".format(
            s, layered, prefix, prefix, varname, shape, reader
        )

    # keyword
    elif v["type"] != "keyword":
        vtmp = varname
        if "shape" in v:
            shape = v["shape"]
            vtmp += shape
        s = "{} <{}{}{}>".format(s, tsmarker, vtmp, tsmarker)

    # if optional, wrap string in square brackets
    if "optional" in v:
        if v["optional"] == "true":
            s = "[{}]".format(s.strip())

    # prepend with prefix and return string
    s = "{}{}".format(prefix, s)
    return s


def write_block(vardict, block, blk_var_list, varexcludeprefix=None, indent=None):
    if indent is None:
        prepend = ""
    else:
        prepend = indent * " "

    s = prepend + "BEGIN {}".format(block.upper())
    for variable in blk_var_list:
        ts = block_entry(variable[0], block, vardict).strip()
        if variable[1]:
            s = "{} [{}]".format(s, ts)
        else:
            s = "{} {}".format(s, ts)
    s += "\n"
    for iv, key in enumerate(vardict):
        name, b = key
        v = vardict[key]
        if b == block:
            addv = True
            if varexcludeprefix is not None:
                # exclude variables that start with `dev_`.  These are
                # develop options that shouldn't go into the documentation.
                n = name.upper()
                if n.startswith(varexcludeprefix.upper()):
                    addv = False
            if "in_record" in v:
                if v["in_record"] == "true":
                    # do not separately include this variable
                    # because it is part of a record
                    addv = False
            if "block_variable" in v:
                if v["block_variable"] == "true":
                    # do not separately include this variable
                    # because it is part of a record
                    addv = False
            if "deprecated" in v:
                if v["deprecated"] != "":
                    addv = False
            if addv:
                ts = block_entry(name, block, vardict, prefix="  " + prepend)
                s += "{}\n".format(ts)
    s += prepend + "END {}".format(block.upper())
    return s


def get_description(desc):
    """
    Check to see if the description is in common.dfn, and make the text
    substitutions if so.

    """
    if desc.strip().split()[0] == "REPLACE":
        bcoption = desc.strip().split()[1]
        constantstring = COMMONDESCRIPTIONS[bcoption]["description"]
        istart = desc.index("{")
        istop = desc.rfind("}") + 1
        d = eval(desc[istart:istop])
        # d = eval(desc[desc.index('{'):])
        for k in d:
            v = d[k]
            constantstring = constantstring.replace(k, v)
        desc = constantstring + desc[istop:]
    return desc


def write_desc(vardict, block, blk_var_list, varexcludeprefix=None):
    s = ""
    for iv, (name, b) in enumerate(vardict):
        v = vardict[(name, b)]
        if v["block"] == block:
            if "block_variable" in v and v["block_variable"]:
                optional = "optional" in v and v["optional"] == "true"
                blk_var_list.append((v["name"], optional))
            addv = True
            if varexcludeprefix is not None:
                # exclude variables that start with `dev_`.  These are
                # develop options that shouldn't go into the documentation.
                n = name.upper()
                if n.startswith(varexcludeprefix.upper()):
                    addv = False
            if v["type"].startswith("rec"):
                addv = False
            if "deprecated" in v:
                if v["deprecated"] != "":
                    addv = False
            if "removed" in v:
                if v["removed"] != "":
                    addv = False
            if addv:
                if v["type"] == "keyword":
                    n = name.upper()
                else:
                    if "tagged" in v:
                        # could be used in future to write tag and name
                        n = "{}".format(name)
                    else:
                        n = name
                n = n.replace("_", "\\_")
                if "description" in v:
                    desc = get_description(v["description"])
                else:
                    msg = ""
                    for k, v in v.items():
                        msg += "  {}: {}\n".format(k, v)
                    print(msg)
                    raise Exception(msg)
                ss = "\\texttt{" + n + "}---" + desc
                if "time_series" in v:
                    if v["time_series"] == "true":
                        fmt = "\\textcolor{blue}\{\}"
                        ss = "\\textcolor{blue}{" + ss + "}"
                        # \textcolor{declared-color}{text}
                s += "\\item " + ss + "\n\n"

                t = v["type"]
                if t.startswith("keystring"):
                    # s += '\\begin{verbatim}\n'
                    s += "\\begin{lstlisting}[style=blockdefinition]\n"
                    for vn in t.strip().split()[1:]:
                        blockentry = block_entry(vn, block, vardict, "")
                        s += "{}\n".format(blockentry)
                    # s += '\\end{verbatim}\n\n'
                    s += "\\end{lstlisting}\n\n"

    return s


def write_desc_md(vardict, block, blk_var_list, varexcludeprefix=None):
    s = ""
    for iv, (name, b) in enumerate(vardict):
        v = vardict[(name, b)]
        if v["block"] == block:
            if "block_variable" in v and v["block_variable"]:
                optional = "optional" in v and v["optional"] == "true"
                blk_var_list.append((v["name"], optional))
            addv = True
            if varexcludeprefix is not None:
                # exclude variables that start with `dev_`.  These are
                # develop options that shouldn't go into the documentation.
                n = name.upper()
                if n.startswith(varexcludeprefix.upper()):
                    addv = False
            if v["type"].startswith("rec"):
                addv = False
            if "deprecated" in v:
                if v["deprecated"] != "":
                    addv = False
            if "removed" in v:
                if v["removed"] != "":
                    addv = False
            if addv:
                if v["type"] == "keyword":
                    n = name.upper()
                else:
                    if "tagged" in v:
                        # could be used in future to write tag and name
                        n = "{}".format(name)
                    else:
                        n = name
                if "description" in v:
                    desc = get_description(v["description"])
                else:
                    msg = ""
                    for k, v in v.items():
                        msg += "  {}: {}\n".format(k, v)
                    print(msg)
                    raise Exception(msg)
                desc = md_replace(desc)
                ss = "`" + n + "` " + desc
                if "time_series" in v:
                    if v["time_series"] == "true":
                        ss = '<span style="color:blue">' + ss + "</span>"
                s += "  * " + ss + "\n\n"

                t = v["type"]
                if t.startswith("keystring"):
                    for vn in t.strip().split()[1:]:
                        blockentry = md_replace(
                            block_entry(vn, block, vardict, 10 * " ")
                        )
                        s += "{}\n".format(blockentry)

    return s


def md_replace(s):
    # replace specific latex commands
    re_dict = {
        re.compile("\\\\cite{(.*?)\\}"): ("\\cite{{{}}}", None),
        re.compile("\\\\citep{(.*?)\\}"): ("\\citep{{{}}}", None),
        re.compile("\\\\texttt{(.*?)\\}"): ("\\texttt{{{}}}", "`{}`"),
        re.compile("\\$(.*?)\\$"): ("${}$", "{}"),
        re.compile("\\^{(.*?)\\}"): ("^{{{}}}", "<sup>{}</sup>"),
        re.compile("\\^(.*?)\\ "): ("^{:.1}", "<sup>{:.1}</sup>"),
        re.compile("\\``(.*?)\\''"): ("``{}''", '"{}"'),
        re.compile("\\`(.*?)\\'"): ("`{}'", '"{}"'),
    }
    for key, (in_fmt, out_fmt) in re_dict.items():
        for v in key.findall(s):
            src = in_fmt.format(v)
            if out_fmt is None:
                dst = ""
            else:
                dst = out_fmt.format(v)
            s = s.replace(src, dst)

    # replace individual characters
    replace_dict = {
        "\mf": "MODFLOW 6",
        "~": " ",
        "@": "",
        "$": "",
        "\_": "_",
        "&": "|",
        "\le": "&#8804;",
        "\ge": "&#8805;",
        "\\times": "x",
        "\\": "",
    }
    for key, value in replace_dict.items():
        s = s.replace(key, value)
    return s


def get_examples(component):
    pth = mf6ivar_dir / "examples"
    files = [
        filename
        for filename in sorted(os.listdir(pth))
        if component.lower() in filename.lower() and "-obs" not in filename.lower()
    ]
    s = ""
    for idx, filename in enumerate(files):
        if idx == 0:
            s += "#### Example Input File\n"
        if len(files) > 1:
            s += "Example {}\n\n".format(idx + 1)
        fpth = os.path.join(pth, filename)
        with open(fpth, "r") as f:
            lines = f.readlines()
        s += "```\n"
        for line in lines:
            line = line.rstrip()
            s += "    {}\n".format(line)
        s += "```\n\n"
    return s


def get_obs_examples(component):
    pth = mf6ivar_dir / "examples"
    files = [
        filename
        for filename in sorted(os.listdir(pth))
        if component.lower() in filename.lower() and "-obs" in filename.lower()
    ]
    s = ""
    for idx, filename in enumerate(files):
        s += "#### Example Observation Input File\n"
        if len(files) > 1:
            s += "Example {}\n\n".format(idx + 1)
        fpth = os.path.join(pth, filename)
        with open(fpth, "r") as f:
            lines = f.readlines()
        s += "```\n"
        for line in lines:
            line = line.rstrip()
            s += "    {}\n".format(line)
        s += "```\n\n"
    return s


def get_obs_table(component):
    pth = mf6ivar_dir.parent.parent / "Common"
    files = [
        filename
        for filename in sorted(os.listdir(pth))
        if component.lower() in filename.lower()
        and filename.lower().endswith("obs.tex")
    ]
    s = ""
    if files:
        s += "#### Available Observation Types\n\n"
        s += "| Stress Package | Observation Type | ID1 | ID2 | Description |\n"
        s += "|----------------|------------------|-----|-----|-------------|\n"
    for idx, filename in enumerate(files):
        fpth = os.path.join(pth, filename)
        with open(fpth, "r") as f:
            lines = f.readlines()
        for line in lines:
            line = md_replace(line.rstrip())
            save_line = True
            if len(line) < 1:
                save_line = False
            elif line.strip().startswith("%"):
                save_line = False
            if save_line:
                s += "| {} |\n".format(line.replace("&", "|"))
    if len(s) > 0:
        s += "\n\n"
    return s


def write_md_header(f):
    s = "# MODFLOW 6 INPUT VARIABLES\n\n"
    fmd.write(s)
    s = "| {} | {} | {} | {} | {} | {} |\n".format(
        "component", "package", "block", "variable name", "type", "description"
    )
    fmd.write(s)
    s = "| {} | {} | {} | {} | {} | {} |\n".format(
        ":---:", ":---:", ":---:", ":---:", ":---:", "---"
    )
    fmd.write(s)
    return


def write_md(f, vardict, component, package):
    c = component.upper()
    p = package.upper()
    for iv, (name, b) in enumerate(vardict):
        n = name.upper()
        v = vardict[(name, b)]
        b = v["block"].upper()
        t = v["type"].upper()
        s = ""
        if t.startswith("REC"):
            pass
        else:
            if t.startswith("KEYSTRING"):
                t = "KEYSTRING"
            t = "{}".format(t)
            if "shape" in v:
                shape = v["shape"].upper()
                t = "{} {}".format(t, shape)
            d = get_description(v["description"])
            s = "| {} | {} | {} | {} | {} | {} |\n".format(c, p, b, n, t, d)
        f.write(s)
    return


def write_appendix(texdir, allblocks):
    fname = Path(texdir) / "appendixA.tex"
    with open(fname, "w") as f:
        f.write("\\small\n\\begin{longtable}{p{1.5cm} p{1.5cm} p{3cm} c}\n")
        f.write(
            "\\caption{List of block names organized by component and input file "
            "type.  OPEN/CLOSE indicates whether or not the block information "
            "can be contained in separate file} \\tabularnewline \n\n"
        )
        f.write("\\hline\n\\hline\n")
        f.write(
            "\\textbf{Component} & \\textbf{FTYPE} & \\textbf{Blockname} & \\textbf{OPEN/CLOSE} \\\\\n"
        )
        f.write("\\hline\n\\endfirsthead\n\n\n")

        f.write("\captionsetup{textformat=simple}\n")
        f.write(
            "\caption*{\\textbf{Table A--\\arabic{table}.}{\quad}List of block"
            " names organized by component and input file type.  OPEN/CLOSE "
            "indicates whether or not the block information can be contained "
            "in separate file.---Continued} \\tabularnewline\n"
        )

        f.write("\n\\hline\n\\hline\n")
        f.write(
            "\\textbf{Component} & \\textbf{FTYPE} & \\textbf{Blockname} & \\textbf{OPEN/CLOSE} \\\\\n"
        )
        f.write("\\hline\n\\endhead\n\n\\hline\n\\endfoot\n\n\n")

        lastftype = ""
        for b in allblocks:
            l = b.strip().split("-")
            component, ftype, blockname = l
            if lastftype != ftype:
                f.write("\\hline\n")
            oc = "yes"
            if "griddata" in blockname.lower():
                oc = "no"
            if (
                "utl" in component.lower()
                and "tas" in ftype.lower()
                and "time" in blockname.lower()
            ):
                oc = "no"
            s = "{} & {} & {} & {} \\\\ \n".format(
                component.upper(), ftype.upper(), blockname.upper(), oc
            )
            f.write(s)
            lastftype = ftype

        f.write("\n\n\\hline\n\\end{longtable}\n\\label{table:blocks}\n\\normalsize\n")


if __name__ == "__main__":
    parser = ArgumentParser(
        prog="Generate MF6 IO documentation files",
    )
    parser.add_argument(
        "-m",
        "--model",
        required=False,
        action="append",
    )
    parser.add_argument("-e", "--exchange", required=False, action="append")
    parser.add_argument(
        "-v", "--verbose", required=False, default=False, action="store_true"
    )
    args = parser.parse_args()
    models = args.model if args.model else ["gwf", "gwt"]
    exchanges = args.exchange if args.exchange else ["gwf-gwf", "gwt-gwt", "gwf-gwt"]
    verbose = args.verbose

    # define directories
    dfndir = mf6ivar_dir / "dfn"
    texdir = mf6ivar_dir / "tex"
    mddir = mf6ivar_dir / "md"
    docdir = Path(__file__).parents[3] / ".build_rtd_docs" / "_mf6io"

    # regenerate docdir
    if os.path.isdir(docdir):
        shutil.rmtree(docdir)
    os.makedirs(docdir)

    # list for storing all block names
    allblocks = []

    # setup a markdown file
    with open(mddir / "mf6ivar.md", "w") as fmd:
        write_md_header(fmd)

        # construct list of dfn files to process
        def filter(paths: Iterable[Path]) -> Iterable[Path]:
            for p in paths:
                s = p.stem
                if (
                    "sim" in s
                    or "sln" in s
                    or "utl" in s
                    or any(e in s for e in exchanges)
                    or any(m in s for m in models)
                ):
                    yield p
        
        files = sorted(filter(dfndir.glob("*.dfn")))

        for fpath in files:
            component, package = fpath.stem.split("-")[0:2]
            vardict = parse_mf6var_file(fpath)

            # make list of unique block names
            blocks = []
            for k in vardict:
                v = vardict[k]
                b = v["block"]
                if b not in blocks:
                    blocks.append(b)

            # add a full block name to allblocks
            for block in blocks:
                b = "{}-{}-{}".format(component, package, block)
                allblocks.append(b)

            # go through each block and write information
            desc = (
                "% DO NOT MODIFY THIS FILE DIRECTLY.  IT IS CREATED BY mf6ivar.py \n\n"
            )
            for b in blocks:
                blk_var_list = []

                # Write the name of the block to the latex file
                desc += "\item \\textbf{}\n\n".format("{Block: " + b.upper() + "}")

                desc += "\\begin{description}\n"
                desc += write_desc(vardict, b, blk_var_list, varexcludeprefix="dev_")
                desc += "\\end{description}\n"

                fname = texdir / f"{fpath.stem}-{b}.dat"
                f = open(fname, "w")
                s = (
                    write_block(vardict, b, blk_var_list, varexcludeprefix="dev_")
                    + "\n"
                )
                f.write(s)
                if verbose:
                    print(s)
                f.close()
            fname = texdir / f"{fpath.stem}-desc.tex"
            f = open(fname, "w")
            s = desc + "\n"
            f.write(s)
            if verbose:
                print(s)
            f.close()

            # write markdown description
            mdname = fpath.stem
            fname = os.path.join(docdir, mdname + ".md")
            f = open(fname, "w")
            f.write("### {}\n\n".format(mdname.upper()))
            f.write("#### Structure of Blocks\n\n")
            f.write("_FOR EACH SIMULATION_\n\n")
            desc = ""
            for b in blocks:
                blk_var_list = []

                # Write the name of the block to the latex file
                desc += "##### Block: {}\n\n".format(b.upper())

                desc += write_desc_md(vardict, b, blk_var_list, varexcludeprefix="dev_")

                if "period" in b.lower():
                    f.write("\n_FOR ANY STRESS PERIOD_\n\n")
                f.write("```\n")
                s = (
                    md_replace(
                        write_block(
                            vardict, b, blk_var_list, varexcludeprefix="dev_", indent=4
                        )
                    )
                    + "\n"
                )
                f.write(s)
                f.write("```\n")
                if verbose:
                    print(s)

            f.write("\n#### Explanation of Variables\n\n")
            f.write(desc)

            # add examples
            s = get_examples(mdname)
            if len(s) > 0:
                f.write(s)

            # add observation table
            s = get_obs_table(mdname)
            if len(s) > 0:
                f.write(s)

            # add observation examples
            s = get_obs_examples(mdname)
            if len(s) > 0:
                f.write(s)

            # close the markdown file
            f.close()

            # write markdown
            write_md(fmd, vardict, component, package)

        if verbose:
            for b in allblocks:
                print(b)
        write_appendix(texdir, allblocks)
