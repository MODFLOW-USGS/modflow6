import os
import sys
import json
from pathlib import Path
from enum import Enum

MF6_LENVARNAME = 16
F90_LINELEN = 82


class Dfn2F90:
    """generate idm f90 file from dfn file"""

    def __init__(
        self,
        dfnfspec: str = None,
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

        self.component, self.subcomponent = self._dfnfspec.stem.upper().split("-")

        print(f"\nprocessing dfn => {self._dfnfspec}")
        self._set_var_d()
        self._set_param_strs()

    def write_f90(self, ofspec=None):
        with open(ofspec, "w") as f:

            # file header
            f.write(self._source_file_header(self.component, self.subcomponent))

            # found type
            f.write(
                f"  type {self.component.capitalize()}{self.subcomponent.capitalize()}ParamFoundType\n"
            )
            for var in self._param_varnames:
                varname = var.split(
                    f"{self.component.lower()}{self.subcomponent.lower()}_"
                )[1]
                f.write(f"    logical :: {varname} = .false.\n")
            f.write(
                f"  end type {self.component.capitalize()}{self.subcomponent.capitalize()}ParamFoundType\n\n"
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
            self._param_str += "    InputParamDefinitionType ::, &"

        if not self._aggregate_str:
            self._aggregate_str += "    InputParamDefinitionType ::, &"

        if not self._block_str:
            self._aggregate_str += "    InputBlockDefinitionType ::, &"

    def _set_blk_param_strs(self, blockname, component, subcomponent):
        print("  processing block params => ", blockname)

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
                    f"MF6_LENVARNAME({MF6_LENVARNAME}) exceeded: {component}-{subcomponent}-{blockname}: {mf6vn}"
                )

            t = v["type"].upper()
            aggregate_t = t and t.startswith("RECARRAY")

            shape = ""
            shapelist = []
            if "shape" in v:
                shape = v["shape"]
                shape = shape.replace("(", "")
                shape = shape.replace(")", "")
                shape = shape.replace(",", "")
                shape = shape.upper()
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
            ]

            # assumes recarray type appears before and member
            # parameter descriptions in dfn file, adjust
            # if necessary
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
                        f"Aggregate type found with no shape: {component}-{subcomponent}-{blockname}: {mf6vn}"
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
        s = f"module {component.title()}{subcomponent.title()}InputModule" + "\n"
        s += (
            "  use InputDefinitionModule, only: InputParamDefinitionType, &"
            + "\n"
            + "                                   InputBlockDefinitionType"
            + "\n"
        )
        s += "  private" + "\n"
        s += (
            f"  public {component.lower()}_{subcomponent.lower()}_param_definitions"
            + "\n"
        )
        s += (
            f"  public {component.lower()}_{subcomponent.lower()}_aggregate_definitions"
            + "\n"
        )
        s += (
            f"  public {component.lower()}_{subcomponent.lower()}_block_definitions"
            + "\n"
        )
        s += (
            f"  public {component.capitalize()}{subcomponent.capitalize()}ParamFoundType"
            + "\n\n"
        )
        return s

    def _source_params_header(self, component, subcomponent):
        s = (
            f"  type(InputParamDefinitionType), parameter :: &"
            + "\n"
            + f"    {component.lower()}_{subcomponent.lower()}_param_definitions(*) = &"
            + "\n"
        )
        s += "    [ &" + "\n"
        return s

    def _source_aggregates_header(self, component, subcomponent):
        s = (
            f"  type(InputParamDefinitionType), parameter :: &"
            + "\n"
            + f"    {component.lower()}_{subcomponent.lower()}_aggregate_definitions(*) = &"
            + "\n"
        )
        s += "    [ &" + "\n"
        return s

    def _source_blocks_header(self, component, subcomponent):
        s = (
            f"  type(InputBlockDefinitionType), parameter :: &"
            + "\n"
            + f"    {component.lower()}_{subcomponent.lower()}_block_definitions(*) = &"
            + "\n"
        )
        s += "    [ &" + "\n"
        return s

    def _source_list_footer(self, component, subcomponent):
        s = "    ]" + "\n"
        return s

    def _source_file_footer(self, component, subcomponent):
        s = f"end module {component.title()}{subcomponent.title()}InputModule" + "\n"
        return s


if __name__ == "__main__":

    dfns = [
        # list per dfn [dfn relative path, model parent dirname, output filename]
        [
            Path("../../../doc/mf6io/mf6ivar/dfn", "gwf-dis.dfn"),
            Path("../../../src/Model/GroundWaterFlow", "gwf3dis8idm.f90"),
        ],
        [
            Path("../../../doc/mf6io/mf6ivar/dfn", "gwf-disu.dfn"),
            Path("../../../src/Model/GroundWaterFlow", "gwf3disu8idm.f90"),
        ],
        [
            Path("../../../doc/mf6io/mf6ivar/dfn", "gwf-disv.dfn"),
            Path("../../../src/Model/GroundWaterFlow", "gwf3disv8idm.f90"),
        ],
        [
            Path("../../../doc/mf6io/mf6ivar/dfn", "gwf-npf.dfn"),
            Path("../../../src/Model/GroundWaterFlow", "gwf3npf8idm.f90"),
        ],
        [
            Path("../../../doc/mf6io/mf6ivar/dfn", "gwt-dsp.dfn"),
            Path("../../../src/Model/GroundWaterTransport", "gwt1dspidm.f90"),
        ],
    ]

    for dfn in dfns:
        converter = Dfn2F90(dfnfspec=dfn[0])
        converter.write_f90(ofspec=dfn[1])
        converter.warn()

    print("\n...done.")
