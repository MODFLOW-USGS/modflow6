import argparse
import sys
import os
from pathlib import Path
from pprint import pprint
from typing import Union
import textwrap
import yaml

MF6_LENVARNAME = 16
F90_LINELEN = 82
PROJ_ROOT = Path(__file__).parents[3]
DEFAULT_DFNS_PATH = Path(__file__).parents[1] / "dfns.txt"
DFN_PATH = PROJ_ROOT / "doc" / "mf6io" / "mf6ivar" / "dfn"
SRC_PATH = PROJ_ROOT / "src"
IDM_PATH = SRC_PATH / "IDM"


class Dfn2F90:
    """generate idm f90 file from dfn file"""

    def __init__(
        self,
        dfnfspec: str = None,
        verbose: bool = False
    ):
        """Dfn290 init"""

        self._dfnfspec = dfnfspec
        self._var_d = {}
        self.component = ""
        self.subcomponent = ""
        self._param_str = ""
        self._aggregate_str = ""
        self._block_str = ""
        self._param_varnames = []
        self._aggregate_varnames = []
        self._warnings = []
        self._multi_package = False
        self._verbose = verbose

        self.component, self.subcomponent = self._dfnfspec.stem.upper().split("-")

        if self._verbose:
            print(f"Processing dfn => {self._dfnfspec}")
        self._set_var_d()
        self._set_param_strs()

    def add_dfn_entry(self, dfn_d=None):
        c_key = f"{self.component.upper()}"
        sc_key = f"{self.subcomponent.upper()}"

        if c_key not in dfn_d:
            dfn_d[c_key] = []

        dfn_d[c_key].append(sc_key)

    def write_f90(self, ofspec=None):
        if self._verbose:
            print(f"Writing file   =>  {ofspec}")
        with open(ofspec, "w") as f:
            # file header
            f.write(self._source_file_header(self.component, self.subcomponent))

            # found type
            f.write(
                f"  type {self.component.capitalize()}"
                f"{self.subcomponent.capitalize()}ParamFoundType\n"
            )
            for var in self._param_varnames:
                varname = var.split(
                    f"{self.component.lower()}{self.subcomponent.lower()}_"
                )[1]
                f.write(f"    logical :: {varname} = .false.\n")
            f.write(
                f"  end type {self.component.capitalize()}"
                f"{self.subcomponent.capitalize()}ParamFoundType\n\n"
            )

            # multi package
            smult = ".false."
            if self._multi_package:
                smult = ".true."
            f.write(
                f"  logical :: {self.component.lower()}_"
                f"{self.subcomponent.lower()}_multi_package = {smult}\n\n"
            )

            # params
            if len(self._param_varnames):
                f.write(self._param_str)
                f.write(self._source_params_header(self.component, self.subcomponent))
                f.write("    " + ", &\n    ".join(self._param_varnames) + " &\n")
                f.write(
                    self._source_list_footer(self.component, self.subcomponent) + "\n"
                )
            else:
                f.write(self._source_params_header(self.component, self.subcomponent))
                f.write(self._param_str.rsplit(",", 1)[0] + " &\n")
                f.write(
                    self._source_list_footer(self.component, self.subcomponent) + "\n"
                )

            # aggregate types
            if len(self._aggregate_varnames):
                f.write(self._aggregate_str)
                f.write(
                    self._source_aggregates_header(self.component, self.subcomponent)
                )
                f.write("    " + ", &\n    ".join(self._aggregate_varnames) + " &\n")
                f.write(
                    self._source_list_footer(self.component, self.subcomponent) + "\n"
                )
            else:
                f.write(
                    self._source_aggregates_header(self.component, self.subcomponent)
                )
                f.write(self._aggregate_str.rsplit(",", 1)[0] + " &\n")
                f.write(
                    self._source_list_footer(self.component, self.subcomponent) + "\n"
                )

            # blocks
            f.write(self._source_blocks_header(self.component, self.subcomponent))
            f.write(self._block_str.rsplit(",", 1)[0] + " &\n")
            f.write(self._source_list_footer(self.component, self.subcomponent) + "\n")

            # file footer
            f.write(self._source_file_footer(self.component, self.subcomponent))

    def get_blocknames(self):
        blocknames = []
        for var, bname in self._var_d:
            if bname not in blocknames:
                blocknames.append(bname)
        return blocknames

    def warn(self):
        if len(self._warnings):
            sys.stderr.write("Warnings:\n")
            for warn in self._warnings:
                sys.stderr.write("  " + warn + "\n")

    def _set_var_d(self):
        f = open(self._dfnfspec, "r")
        lines = f.readlines()
        f.close()

        vardict = {}
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
                    if name in vardict:
                        raise Exception(
                            "Variable already exists in dictionary: " + name
                        )
                    vardict[key] = vd
                vd = {}
                continue

            # skip comments
            if "#" in line.strip()[0]:
                # flopy multi-package
                if "flopy multi-package" in line.strip():
                    self._multi_package = True
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
            if name in vardict:
                raise Exception("Variable already exists in dictionary: " + name)
            vardict[key] = vd

        self._var_d = vardict

    def _construct_f90_block_statement(
        self, blockname, required=False, aggregate=False, block_var=False
    ):
        f90statement = f"    InputBlockDefinitionType( &\n"
        f90statement += f"    '{blockname}', & ! blockname\n"

        if required:
            f90statement += f"    .true., & ! required\n"
        else:
            f90statement += f"    .false., & ! required\n"

        if aggregate:
            f90statement += f"    .true., & ! aggregate\n"
        else:
            f90statement += f"    .false., & ! aggregate\n"

        if block_var:
            f90statement += f"    .true. & ! block_variable\n"
        else:
            f90statement += f"    .false. & ! block_variable\n"

        f90statement += f"    ), &"

        return f90statement

    def _construct_f90_param_statement(
        self, tuple_list, basename, varname, aggregate=False
    ):
        vname = f"{basename.lower()}_{varname.lower()}"

        if aggregate:
            self._aggregate_varnames.append(vname)
        else:
            self._param_varnames.append(vname)

        f90statement = f"  type(InputParamDefinitionType), parameter :: &\n"
        f90statement += f"    {vname} = InputParamDefinitionType &\n"
        f90statement += f"    ( &\n"

        for i, (value, varname) in enumerate(tuple_list):
            comma = ","
            if i + 1 == len(tuple_list):
                comma = ""
            v = f"'{value}'"
            if value in [".false.", ".true."]:
                v = f"{value}"
            f90statement += f"    {v}{comma} & ! {varname}\n"

        f90statement += f"    )\n"

        return f90statement

    def _set_param_strs(self):
        blocknames = self.get_blocknames()

        for b in blocknames:
            self._set_blk_param_strs(b, self.component, self.subcomponent)

        if not self._param_str:
            self._param_str += "    InputParamDefinitionType &\n"
            self._param_str += "    ( &\n"
            self._param_str += "    '', & ! component\n"
            self._param_str += "    '', & ! subcomponent\n"
            self._param_str += "    '', & ! block\n"
            self._param_str += "    '', & ! tag name\n"
            self._param_str += "    '', & ! fortran variable\n"
            self._param_str += "    '', & ! type\n"
            self._param_str += "    '', & ! shape\n"
            self._param_str += "    .false., & ! required\n"
            self._param_str += "    .false., & ! multi-record\n"
            self._param_str += "    .false., & ! preserve case\n"
            self._param_str += "    .false., & ! layered\n"
            self._param_str += "    .false. & ! timeseries\n"
            self._param_str += "    ), &\n"

        if not self._aggregate_str:
            self._aggregate_str += "    InputParamDefinitionType &\n"
            self._aggregate_str += "    ( &\n"
            self._aggregate_str += "    '', & ! component\n"
            self._aggregate_str += "    '', & ! subcomponent\n"
            self._aggregate_str += "    '', & ! block\n"
            self._aggregate_str += "    '', & ! tag name\n"
            self._aggregate_str += "    '', & ! fortran variable\n"
            self._aggregate_str += "    '', & ! type\n"
            self._aggregate_str += "    '', & ! shape\n"
            self._aggregate_str += "    .false., & ! required\n"
            self._aggregate_str += "    .false., & ! multi-record\n"
            self._aggregate_str += "    .false., & ! preserve case\n"
            self._aggregate_str += "    .false., & ! layered\n"
            self._aggregate_str += "    .false. & ! timeseries\n"
            self._aggregate_str += "    ), &\n"

        if not self._block_str:
            self._block_str += "    InputBlockDefinitionType &\n"
            self._block_str += "    ( &\n"
            self._block_str += "    '', & ! blockname\n"
            self._block_str += "    .false., & ! required\n"
            self._block_str += "    .false., & ! aggregate\n"
            self._block_str += "    .false. & ! block_varaible\n"
            self._block_str += "    ), &\n"

    def _set_blk_param_strs(self, blockname, component, subcomponent):
        if self._verbose:
            print("  Processing block params => ", blockname)

        required_l = None
        required_l = []
        has_block_var = False
        is_aggregate_blk = False
        aggregate_required = False

        # comment
        s = f"    ! {component} {subcomponent} {blockname.upper()}\n"

        r = ".true."
        if blockname.upper() == "OPTIONS":
            r = ".false."

        for k in self._var_d:
            varname, bname = k
            if bname != blockname:
                continue

            v = self._var_d[k]

            if "block_variable" in v and v["block_variable"].upper() == "TRUE":
                has_block_var = True
                continue

            c = component
            sc = subcomponent
            b = v["block"].upper()

            # variable name
            vn = v["name"].upper()
            mf6vn = vn
            if "mf6internal" in v:
                mf6vn = v["mf6internal"].upper()

            if len(mf6vn) > MF6_LENVARNAME:
                self._warnings.append(
                    f"MF6_LENVARNAME({MF6_LENVARNAME}) exceeded: "
                    f"{component}-{subcomponent}-{blockname}: {mf6vn}"
                )

            t = v["type"].upper()
            aggregate_t = t and t.startswith("RECARRAY")

            shape = ""
            shapelist = []
            # workaround for Flopy shape issue with exg dfns:
            if c.upper() == "EXG":
                if vn == "CELLIDM1" or vn == "CELLIDM2":
                    v["shape"] = "(ncelldim)"
            if "shape" in v:
                shape = v["shape"]
                shape = shape.replace("(", "")
                shape = shape.replace(")", "")
                shape = shape.replace(",", "")
                shape = shape.upper()
                if shape == "NCOL*NROW; NCPL":
                    # grid array input syntax
                    if mf6vn == "AUXVAR":
                        # for grid, set AUX as DOUBLE2D
                        shape = "NAUX NCPL"
                    else:
                        shape = "NCPL"
                shapelist = shape.strip().split()
            ndim = len(shapelist)

            if t == "DOUBLE PRECISION":
                t = "DOUBLE"
            if shape != "" and not aggregate_t and (t == "DOUBLE" or t == "INTEGER"):
                t = f"{t}{ndim}D"

            inrec = ".false."
            if "in_record" in v:
                if v["in_record"] == "true":
                    inrec = ".true."
                else:
                    inrec = ".false."

            r = ".true."
            if "optional" in v:
                if v["optional"] == "true":
                    r = ".false."
                else:
                    r = ".true."

            preserve_case = ".false."
            if "preserve_case" in v:
                if v["preserve_case"] == "true":
                    preserve_case = ".true."
                else:
                    preserve_case = ".false."

            layered = ".false."
            if "layered" in v:
                if v["layered"] == "true":
                    layered = ".true."
                else:
                    layered = ".false."

            timeseries = ".false."
            if "time_series" in v:
                if v["time_series"] == "true":
                    timeseries = ".true."
                else:
                    timeseries = ".false."

            if inrec == ".false.":
                required_l.append(r)
            tuple_list = [
                (c, "component"),
                (sc, "subcomponent"),
                (b, "block"),
                (vn, "tag name"),
                (mf6vn, "fortran variable"),
                (t, "type"),
                (shape, "shape"),
                (r, "required"),
                (inrec, "multi-record"),
                (preserve_case, "preserve case"),
                (layered, "layered"),
                (timeseries, "timeseries"),
            ]

            if aggregate_t:
                self._aggregate_str += (
                    self._construct_f90_param_statement(
                        tuple_list, f"{component}{subcomponent}", mf6vn, True
                    )
                    + "\n"
                )
                is_aggregate_blk = True
                aggregate_required = r == ".true."
                if not shape:
                    self._warnings.append(
                        f"Aggregate type found with no shape: "
                        f"{component}-{subcomponent}-{blockname}: {mf6vn}"
                    )

            else:
                self._param_str += (
                    self._construct_f90_param_statement(
                        tuple_list, f"{component}{subcomponent}", mf6vn
                    )
                    + "\n"
                )

        if is_aggregate_blk:
            required = aggregate_required
        else:
            required = ".true." in required_l

        if self._block_str == "" and blockname.upper() != "OPTIONS":
            self._block_str += (
                self._construct_f90_block_statement(
                    "OPTIONS",
                    required=False,
                    aggregate=False,
                )
                + "\n"
            )

        self._block_str += (
            self._construct_f90_block_statement(
                blockname.upper(),
                required=required,
                aggregate=is_aggregate_blk,
                block_var=has_block_var,
            )
            + "\n"
        )

    def _source_file_header(self, component, subcomponent):
        s = (
            f"! ** Do Not Modify! MODFLOW 6 system generated file. **\n"
            f"module {component.title()}{subcomponent.title()}InputModule\n"
            f"  use ConstantsModule, only: LENVARNAME\n"
            f"  use InputDefinitionModule, only: InputParamDefinitionType, &\n"
            f"                                   InputBlockDefinitionType\n"
            f"  private\n"
            f"  public {component.lower()}_{subcomponent.lower()}_"
            f"param_definitions\n"
            f"  public {component.lower()}_{subcomponent.lower()}_"
            f"aggregate_definitions\n"
            f"  public {component.lower()}_{subcomponent.lower()}_"
            f"block_definitions\n"
            f"  public {component.capitalize()}{subcomponent.capitalize()}"
            f"ParamFoundType\n"
            f"  public {component.lower()}_{subcomponent.lower()}_"
            f"multi_package\n\n"
        )

        return s

    def _source_params_header(self, component, subcomponent):
        s = (
            f"  type(InputParamDefinitionType), parameter :: &\n"
            f"    {component.lower()}_{subcomponent.lower()}_param_"
            f"definitions(*) = &\n"
            f"    [ &\n"
        )

        return s

    def _source_aggregates_header(self, component, subcomponent):
        s = (
            f"  type(InputParamDefinitionType), parameter :: &\n"
            f"    {component.lower()}_{subcomponent.lower()}_aggregate_"
            f"definitions(*) = &\n"
            f"    [ &\n"
        )

        return s

    def _source_blocks_header(self, component, subcomponent):
        s = (
            f"  type(InputBlockDefinitionType), parameter :: &\n"
            f"    {component.lower()}_{subcomponent.lower()}_block_"
            f"definitions(*) = &\n"
            f"    [ &\n"
        )

        return s

    def _source_list_footer(self, component, subcomponent):
        s = "    ]" + "\n"
        return s

    def _source_file_footer(self, component, subcomponent):
        s = f"end module {component.title()}{subcomponent.title()}InputModule\n"
        return s


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog="Convert DFN files to Fortran source files",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=textwrap.dedent(
            """\
            Generate Fortran source code from DFN files. This script
            converts definition (DFN) files to Fortran source files,
            each representing a parameter set for a particular input
            definition. Fortran files generated by this tool provide
            support for simulations, models or packages described by
            the given DFN files. Each DFN file is transformed into a
            corresponding Fortran file with "idm" and the same stem:
            e.g. gwf-ic.dfn becomes gwf-icidm.f90.
            """
        ),
    )
    parser.add_argument(
        "-d",
        "--dfn",
        required=False,
        default=DEFAULT_DFNS_PATH,
        help="Path to a DFN file, or to a text or YAML file listing DFN files (one per line)",
    )
    parser.add_argument(
        "-o",
        "--outdir",
        required=False,
        help="The directory to write Fortran source files",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action='store_true',
        required=False,
        default=False,
        help="Whether to show verbose output"
    )
    args = parser.parse_args()
    dfn = Path(args.dfn)
    outdir = Path(args.outdir) if args.outdir else Path.cwd()
    verbose = args.verbose
    if dfn.suffix.lower() in [".txt"]:
        dfns = open(dfn, "r").readlines()
        dfns = [l.strip() for l in dfns]
        dfns = [l for l in dfns if not l.startswith("#") and l.lower().endswith(".dfn")]
        if dfn == DEFAULT_DFNS_PATH:
            dfns = [DFN_PATH / p for p in dfns]
        
    elif dfn.suffix.lower() in [".yml", ".yaml"]:
        dfns = yaml.safe_load(open(dfn, "r"))
    elif dfn.suffix.lower() in [".dfn"]:
        dfns = [dfn]

    assert all(
        p.is_file() for p in dfns
    ), f"DFNs not found: {[p for p in dfns if not p.is_file()]}"

    if verbose:
        print("Converting DFNs:")
        pprint(dfns)

    # create DFN files
    dfn_d = {}
    for dfn in dfns:
        converter = Dfn2F90(dfnfspec=dfn, verbose=verbose)
        converter.write_f90(ofspec=outdir / f"{dfn.stem}idm.f90")
        converter.warn()
        converter.add_dfn_entry(dfn_d=dfn_d)

    if verbose:
        print("...done.")
