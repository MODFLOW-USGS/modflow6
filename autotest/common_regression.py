import os
import shutil
from pathlib import Path
from warnings import warn

ignore_ext = (
    ".hds",
    ".hed",
    ".bud",
    ".cbb",
    ".cbc",
    ".ddn",
    ".ucn",
    ".glo",
    ".lst",
    ".list",
    ".gwv",
    ".mv",
    ".out",
)


def model_setup(namefile, dst, remove_existing=True, extrafiles=None):
    """Setup MODFLOW-based model files for autotests.

    Parameters
    ----------
    namefile : str
        MODFLOW-based model name file.
    dst : str
        destination path for comparison model or file(s)
    remove_existing : bool
        boolean indicating if an existing comparision model or file(s) should
        be replaced (default is True)
    extrafiles : str or list of str
        list of extra files to include in the comparision

    Returns
    -------

    """
    # Construct src pth from namefile or lgr file
    src = os.path.dirname(namefile)

    # Create the destination folder, if required
    create_dir = False
    if os.path.exists(dst):
        if remove_existing:
            print("Removing folder ", dst)
            shutil.rmtree(dst)
            create_dir = True
    else:
        create_dir = True
    if create_dir:
        os.mkdir(dst)

    # determine if a namefile is a lgr control file - get individual
    # name files out of the lgr control file
    namefiles = [namefile]
    ext = os.path.splitext(namefile)[1]
    if ".lgr" in ext.lower():
        lines = [line.rstrip("\n") for line in open(namefile)]
        for line in lines:
            if len(line) < 1:
                continue
            if line[0] == "#":
                continue
            t = line.split()
            if ".nam" in t[0].lower():
                fpth = os.path.join(src, t[0])
                namefiles.append(fpth)

    # Make list of files to copy
    files2copy = []
    for fpth in namefiles:
        files2copy.append(os.path.basename(fpth))
        ext = os.path.splitext(fpth)[1]
        # copy additional files contained in the name file and
        # associated package files
        if ext.lower() == ".nam":
            fname = os.path.abspath(fpth)
            files2copy = files2copy + get_input_files(fname)

    if extrafiles is not None:
        if isinstance(extrafiles, str):
            extrafiles = [extrafiles]
        for fl in extrafiles:
            files2copy.append(os.path.basename(fl))

    # Copy the files
    for f in files2copy:
        srcf = os.path.join(src, f)
        dstf = os.path.join(dst, f)

        # Check to see if dstf is going into a subfolder, and create that
        # subfolder if it doesn't exist
        sf = os.path.dirname(dstf)
        if not os.path.isdir(sf):
            os.makedirs(sf)

        # Now copy the file
        if os.path.exists(srcf):
            print("Copy file '" + srcf + "' -> '" + dstf + "'")
            shutil.copy(srcf, dstf)
        else:
            print(srcf + " does not exist")

    return


def setup_comparison(namefile, dst, remove_existing=True):
    """Setup a comparison model or comparision file(s) for a MODFLOW-based
    model.

    Parameters
    ----------
    namefile : str
        MODFLOW-based model name file.
    dst : str
        destination path for comparison model or file(s)
    remove_existing : bool
        boolean indicating if an existing comparision model or file(s) should
        be replaced (default is True)


    Returns
    -------

    """
    # Construct src pth from namefile
    src = os.path.dirname(namefile)
    action = None
    for root, dirs, files in os.walk(src):
        dl = [d.lower() for d in dirs]
        if any(".cmp" in s for s in dl):
            idx = None
            for jdx, d in enumerate(dl):
                if ".cmp" in d:
                    idx = jdx
                    break
            if idx is not None:
                if "mf2005.cmp" in dl[idx] or "mf2005" in dl[idx]:
                    action = dirs[idx]
                elif "mfnwt.cmp" in dl[idx] or "mfnwt" in dl[idx]:
                    action = dirs[idx]
                elif "mfusg.cmp" in dl[idx] or "mfusg" in dl[idx]:
                    action = dirs[idx]
                elif "mf6.cmp" in dl[idx] or "mf6" in dl[idx]:
                    action = dirs[idx]
                elif "libmf6.cmp" in dl[idx] or "libmf6" in dl[idx]:
                    action = dirs[idx]
                else:
                    action = dirs[idx]
                break
    if action is not None:
        dst = os.path.join(dst, f"{action}")
        if not os.path.isdir(dst):
            try:
                os.mkdir(dst)
            except:
                print("Could not make " + dst)
        # clean directory
        else:
            print("Cleaning ", dst)
            for root, dirs, files in os.walk(dst):
                for f in files:
                    tpth = os.path.join(root, f)
                    print("Removing ", tpth)
                    os.remove(tpth)
                for d in dirs:
                    tdir = os.path.join(root, d)
                    print("Removing ", tdir)
                    shutil.rmtree(tdir)
        # copy files
        cmppth = os.path.join(src, action)
        files = os.listdir(cmppth)
        files2copy = []
        if action.lower() == ".cmp":
            for file in files:
                if ".cmp" in os.path.splitext(file)[1].lower():
                    files2copy.append(os.path.join(cmppth, file))
            for srcf in files2copy:
                f = os.path.basename(srcf)
                dstf = os.path.join(dst, f)
                # Now copy the file
                if os.path.exists(srcf):
                    print("Copy file '" + srcf + "' -> '" + dstf + "'")
                    shutil.copy(srcf, dstf)
                else:
                    print(srcf + " does not exist")
        else:
            for file in files:
                if ".nam" in os.path.splitext(file)[1].lower():
                    files2copy.append(
                        os.path.join(cmppth, os.path.basename(file))
                    )
                    nf = os.path.join(src, action, os.path.basename(file))
                    model_setup(nf, dst, remove_existing=remove_existing)
                    break

    return action


def get_input_files(namefile):
    """Return a list of all the input files in this model.

    Parameters
    ----------
    namefile : str
        path to a MODFLOW-based model name file

    Returns
    -------
    filelist : list
        list of MODFLOW-based model input files

    """
    srcdir = os.path.dirname(namefile)
    filelist = []
    fname = os.path.join(srcdir, namefile)
    with open(fname, "r") as f:
        lines = f.readlines()

    for line in lines:
        ll = line.strip().split()
        if len(ll) < 2:
            continue
        if line.strip()[0] in ["#", "!"]:
            continue
        ext = os.path.splitext(ll[2])[1]
        if ext.lower() not in ignore_ext:
            if len(ll) > 3:
                if "replace" in ll[3].lower():
                    continue
            filelist.append(ll[2])

    # Now go through every file and look for other files to copy,
    # such as 'OPEN/CLOSE'.  If found, then add that file to the
    # list of files to copy.
    otherfiles = []
    for fname in filelist:
        fname = os.path.join(srcdir, fname)
        try:
            f = open(fname, "r")
            for line in f:
                # Skip invalid lines
                ll = line.strip().split()
                if len(ll) < 2:
                    continue
                if line.strip()[0] in ["#", "!"]:
                    continue

                if "OPEN/CLOSE" in line.upper():
                    for i, s in enumerate(ll):
                        if "OPEN/CLOSE" in s.upper():
                            stmp = ll[i + 1]
                            stmp = stmp.replace('"', "")
                            stmp = stmp.replace("'", "")
                            otherfiles.append(stmp)
                            break
        except:
            print(fname + " does not exist")

    filelist = filelist + otherfiles

    return filelist


def get_namefiles(pth, exclude=None):
    """Search through a path (pth) for all .nam files.

    Parameters
    ----------
    pth : str
        path to model files
    exclude : str or lst
        File or list of files to exclude from the search (default is None)

    Returns
    -------
    namefiles : lst
        List of namefiles with paths

    """
    namefiles = []
    for root, _, files in os.walk(pth):
        namefiles += [
            os.path.join(root, file) for file in files if file.endswith(".nam")
        ]
    if exclude is not None:
        if isinstance(exclude, str):
            exclude = [exclude]
        exclude = [e.lower() for e in exclude]
        pop_list = []
        for namefile in namefiles:
            for e in exclude:
                if e in namefile.lower():
                    pop_list.append(namefile)
        for e in pop_list:
            namefiles.remove(e)

    return namefiles


def get_sim_name(namefiles, rootpth=None):
    """Get simulation name.

    Parameters
    ----------
    namefiles : str or list of strings
        path(s) to MODFLOW-based model name files
    rootpth : str
        optional root directory path (default is None)

    Returns
    -------
    simname : list
        list of namefiles without the file extension

    """
    if isinstance(namefiles, str):
        namefiles = [namefiles]
    sim_name = []
    for namefile in namefiles:
        t = namefile.split(os.sep)
        if rootpth is None:
            idx = -1
        else:
            idx = t.index(os.path.split(rootpth)[1])

        # build dst with everything after the rootpth and before
        # the namefile file name.
        dst = ""
        if idx < len(t):
            for d in t[idx + 1 : -1]:
                dst += f"{d}_"

        # add namefile basename without extension
        dst += t[-1].replace(".nam", "")
        sim_name.append(dst)

    return sim_name


def setup_mf6(
    src, dst, mfnamefile="mfsim.nam", extrafiles=None, remove_existing=True
):
    """Copy all of the MODFLOW 6 input files from the src directory to the dst
    directory.

    Parameters
    ----------
    src : src
        directory path with original MODFLOW 6 input files
    dst : str
        directory path that original MODFLOW 6 input files will be copied to
    mfnamefile : str
        optional MODFLOW 6 simulation name file (default is mfsim.nam)
    extrafiles : bool
        boolean indicating if extra files should be included (default is None)
    remove_existing : bool
        boolean indicating if existing file in dst should be removed (default
        is True)

    Returns
    -------
    mf6inp : list
        list of MODFLOW 6 input files
    mf6outp : list
        list of MODFLOW 6 output files

    """

    # Create the destination folder
    create_dir = False
    if os.path.exists(dst):
        if remove_existing:
            print("Removing ", dst)
            shutil.rmtree(dst)
            create_dir = True
    else:
        create_dir = True
    if create_dir:
        os.makedirs(dst)

    # Make list of files to copy
    fname = os.path.join(src, mfnamefile)
    fname = os.path.abspath(fname)
    mf6inp, mf6outp = get_mf6_files(fname)
    files2copy = [mfnamefile] + mf6inp

    # determine if there are any .ex files
    exinp = []
    for f in mf6outp:
        ext = os.path.splitext(f)[1]
        if ext.lower() == ".hds":
            pth = os.path.join(src, f + ".ex")
            if os.path.isfile(pth):
                exinp.append(f + ".ex")
    if len(exinp) > 0:
        files2copy += exinp
    if extrafiles is not None:
        files2copy += extrafiles

    # Copy the files
    for f in files2copy:
        srcf = os.path.join(src, f)
        dstf = os.path.join(dst, f)

        # Check to see if dstf is going into a subfolder, and create that
        # subfolder if it doesn't exist
        sf = os.path.dirname(dstf)
        if not os.path.isdir(sf):
            try:
                os.mkdir(sf)
            except:
                print("Could not make " + sf)

        # Now copy the file
        if os.path.exists(srcf):
            print("Copy file '" + srcf + "' -> '" + dstf + "'")
            shutil.copy(srcf, dstf)
        else:
            print(srcf + " does not exist")

    return mf6inp, mf6outp


def get_mf6_comparison(src):
    """
    Determine the comparison type for a MODFLOW 6 simulation.
    The comparison type is a function of the files present in
    the simulation workspace. Some file types take precedence
    over others.

    Parameters
    ----------
    src : str
        directory path to search for comparison types

    Returns
    -------
    action : str
        comparison type

    """
    # Possible comparison - the order matters
    optcomp = (
        "compare",
        ".cmp",
        "mf2005",
        "mf2005.cmp",
        "mfnwt",
        "mfnwt.cmp",
        "mfusg",
        "mfusg.cmp",
        "mflgr",
        "mflgr.cmp",
        "libmf6",
        "libmf6.cmp",
        "mf6",
        "mf6.cmp",
    )
    # Construct src pth from namefile
    for _, dirs, _ in os.walk(src):
        dl = [d.lower() for d in dirs]
        for oc in optcomp:
            if any(oc in s for s in dl):
                return oc


def setup_mf6_comparison(
    src, dst, action="compare", overwrite=True, verbose=False
):
    """Setup comparison for MODFLOW 6 simulation.

    Parameters
    ----------
    src : path-like
        directory with original MODFLOW 6 input files
    dst : path-like
        directory to copy MODFLOW 6 input files to
    action : str
        type of comparison to use (compare, mf2005, etc)
    overwrite : bool
        whether to overwrite the destination directory if it exists (default is True)

    Returns
    -------
    action : str
        comparison type (also the name of the comparison subdirectory in dst)

    """

    if action is None:
        warn(f"No action provided, aborting")
        return

    # create and/or clean dest dir if needed
    dst = Path(dst) / action
    dst.mkdir(exist_ok=True)
    dls = list(os.walk(dst))
    if overwrite and any(dls):
        if verbose:
            print("Cleaning ", dst)
        for root, dirs, files in dls:
            for f in files:
                tpth = os.path.join(root, f)
                if verbose:
                    print("Removing ", tpth)
                os.remove(tpth)
            for d in dirs:
                tdir = os.path.join(root, d)
                if verbose:
                    print("Removing ", tdir)
                shutil.rmtree(tdir)
    else:
        raise ValueError(f"Destination exists but overwrite disabled: {dst}")

    # copy files
    cmppth = os.path.join(src, action)
    files = os.listdir(cmppth)
    files2copy = []
    if action.lower() == "compare" or action.lower() == ".cmp":
        for file in files:
            if ".cmp" in os.path.splitext(file)[1].lower():
                files2copy.append(os.path.join(cmppth, file))
        for srcf in files2copy:
            f = os.path.basename(srcf)
            dstf = os.path.join(dst, f)
            if os.path.exists(srcf):
                print("Copy file '" + srcf + "' -> '" + dstf + "'")
                shutil.copy(srcf, dstf)
            else:
                print(srcf + " does not exist")
    else:
        if "mf6" in action.lower():
            for file in files:
                if "mfsim.nam" in file.lower():
                    srcf = os.path.join(cmppth, os.path.basename(file))
                    files2copy.append(srcf)
                    srcdir = os.path.join(src, action)
                    setup_mf6(srcdir, dst, remove_existing=overwrite)
                    break
        else:
            for file in files:
                if ".nam" in os.path.splitext(file)[1].lower():
                    srcf = os.path.join(cmppth, os.path.basename(file))
                    files2copy.append(srcf)
                    nf = os.path.join(src, action, os.path.basename(file))
                    model_setup(nf, dst, remove_existing=overwrite)
                    break


def get_mf6_nper(tdisfile):
    """Return the number of stress periods in the MODFLOW 6 model.

    Parameters
    ----------
    tdisfile : str
        path to the TDIS file

    Returns
    -------
    nper : int
        number of stress periods in the simulation

    """
    with open(tdisfile, "r") as f:
        lines = f.readlines()
    line = [line for line in lines if "NPER" in line.upper()][0]
    nper = line.strip().split()[1]
    return nper


def get_mf6_mshape(disfile):
    """Return the shape of the MODFLOW 6 model.

    Parameters
    ----------
    disfile : str
        path to a MODFLOW 6 discretization file

    Returns
    -------
    mshape : tuple
        tuple with the shape of the MODFLOW 6 model.

    """
    with open(disfile, "r") as f:
        lines = f.readlines()

    d = {}
    for line in lines:
        # Skip over blank and commented lines
        ll = line.strip().split()
        if len(ll) < 2:
            continue
        if line.strip()[0] in ["#", "!"]:
            continue

        for key in ["NODES", "NCPL", "NLAY", "NROW", "NCOL"]:
            if ll[0].upper() in key:
                d[key] = int(ll[1])

    if "NODES" in d:
        mshape = (d["NODES"],)
    elif "NCPL" in d:
        mshape = (d["NLAY"], d["NCPL"])
    elif "NLAY" in d:
        mshape = (d["NLAY"], d["NROW"], d["NCOL"])
    else:
        print(d)
        raise Exception("Could not determine model shape")
    return mshape


def get_mf6_files(namefile):
    """Get all MODFLOW 6 input and output files in this simulation.

    Parameters
    ----------
    namefile : pathlike
        path to the MODFLOW 6 simulation name file

    Returns
    -------
    A tuple of lists of paths (input files, output files)
    """

    srcdir = os.path.dirname(namefile)
    mdl_files = []
    pkg_files = []
    out_files = []
    pkg_keys = ["TDIS6", "GWF6", "GWT", "GWF6-GWF6", "GWF-GWT", "IMS6"]
    mdl_keys = ["GWF6", "GWT"]

    # find model and simulation-level package input files in simulation namefile
    for line in open(namefile).readlines():
        # Skip over blank and commented lines
        ll = line.strip().split()
        if len(ll) < 2:
            continue
        if line.strip()[0] in ["#", "!"]:
            continue

        for key in pkg_keys:
            if key in ll[0].upper():
                fname = ll[1]
                pkg_files.append(fname)

        for key in mdl_keys:
            if key in ll[0].upper():
                fname = ll[1]
                mdl_files.append(fname)

    # find model-level package input files in model namefiles
    for namefile in mdl_files:
        fname = os.path.join(srcdir, namefile)
        lines = open(fname, "r").readlines()
        insideblock = False

        for line in lines:
            ll = line.upper().strip().split()
            if len(ll) < 2:
                continue
            if ll[0] in "BEGIN" and ll[1] in "PACKAGES":
                insideblock = True
                continue
            if ll[0] in "END" and ll[1] in "PACKAGES":
                insideblock = False

            if insideblock:
                ll = line.strip().split()
                if len(ll) < 2:
                    continue
                if line.strip()[0] in ["#", "!"]:
                    continue
                pkg_files.append(ll[1])

    # Recurse through package input files and look for input or
    # output file entries, e.g. 'OPEN/CLOSE',  'TIMESERIESFILE'
    # or similar
    flist = pkg_files
    while True:
        olist = []
        flist, olist = _get_mf6_external_files(srcdir, olist, flist)
        pkg_files += flist
        out_files += olist
        # terminate loop if no additional files
        # if len(flist) < 1 and len(olist) < 1:
        if len(flist) < 1:
            break

    return pkg_files, out_files


def _get_mf6_external_files(srcdir, outplist, files):
    """Get list of external files in a MODFLOW 6 simulation.

    Parameters
    ----------
    srcdir : str
        path to a directory containing a MODFLOW 6 simulation
    outplist : list
        list of output files in a MODFLOW 6 simulation
    files : list
        list of MODFLOW 6 name files

    Returns
    -------

    """
    extfiles = []

    for fname in files:
        fname = os.path.join(srcdir, fname)
        try:
            f = open(fname, "r")
            for line in f:
                # Skip invalid lines
                ll = line.strip().split()
                if len(ll) < 2:
                    continue
                if line.strip()[0] in ["#", "!"]:
                    continue

                if "OPEN/CLOSE" in line.upper():
                    for i, s in enumerate(ll):
                        if s.upper() == "OPEN/CLOSE":
                            stmp = ll[i + 1]
                            stmp = stmp.replace('"', "")
                            stmp = stmp.replace("'", "")
                            extfiles.append(stmp)
                            break

                if "TS6" in line.upper():
                    for i, s in enumerate(ll):
                        if s.upper() == "FILEIN":
                            stmp = ll[i + 1]
                            stmp = stmp.replace('"', "")
                            stmp = stmp.replace("'", "")
                            extfiles.append(stmp)
                            break

                if "TAS6" in line.upper():
                    for i, s in enumerate(ll):
                        if s.upper() == "FILEIN":
                            stmp = ll[i + 1]
                            stmp = stmp.replace('"', "")
                            stmp = stmp.replace("'", "")
                            extfiles.append(stmp)
                            break

                if "OBS6" in line.upper():
                    for i, s in enumerate(ll):
                        if s.upper() == "FILEIN":
                            stmp = ll[i + 1]
                            stmp = stmp.replace('"', "")
                            stmp = stmp.replace("'", "")
                            extfiles.append(stmp)
                            break

                if "EXTERNAL" in line.upper():
                    for i, s in enumerate(ll):
                        if s.upper() == "EXTERNAL":
                            stmp = ll[i + 1]
                            stmp = stmp.replace('"', "")
                            stmp = stmp.replace("'", "")
                            extfiles.append(stmp)
                            break

                if "FILE" in line.upper():
                    for i, s in enumerate(ll):
                        if s.upper() == "FILEIN":
                            stmp = ll[i + 1]
                            stmp = stmp.replace('"', "")
                            stmp = stmp.replace("'", "")
                            extfiles.append(stmp)
                            break

                if "FILE" in line.upper():
                    for i, s in enumerate(ll):
                        if s.upper() == "FILEOUT":
                            stmp = ll[i + 1]
                            stmp = stmp.replace('"', "")
                            stmp = stmp.replace("'", "")
                            outplist.append(stmp)
                            break

        except:
            print("could not get a list of external mf6 files")

    return extfiles, outplist


def get_mf6_ftypes(namefile, ftypekeys):
    """Return a list of FTYPES that are in the name file and in ftypekeys.

    Parameters
    ----------
    namefile : str
        path to a MODFLOW 6 name file
    ftypekeys : list
        list of desired FTYPEs

    Returns
    -------
    ftypes : list
        list of FTYPES that match ftypekeys in namefile

    """
    with open(namefile, "r") as f:
        lines = f.readlines()

    ftypes = []
    for line in lines:
        # Skip over blank and commented lines
        ll = line.strip().split()
        if len(ll) < 2:
            continue
        if line.strip()[0] in ["#", "!"]:
            continue

        for key in ftypekeys:
            if ll[0].upper() in key:
                ftypes.append(ll[0])

    return ftypes


def get_mf6_blockdata(f, blockstr):
    """Return list with all non comments between start and end of block
    specified by blockstr.

    Parameters
    ----------
    f : file object
        open file object
    blockstr : str
        name of block to search

    Returns
    -------
    data : list
        list of data in specified block

    """
    data = []

    # find beginning of block
    for line in f:
        if line[0] != "#":
            t = line.split()
            if t[0].lower() == "begin" and t[1].lower() == blockstr.lower():
                break
    for line in f:
        if line[0] != "#":
            t = line.split()
            if t[0].lower() == "end" and t[1].lower() == blockstr.lower():
                break
            else:
                data.append(line.rstrip())
    return data
