import os
import sys
import subprocess
from contextlib import contextmanager


@contextmanager
def cwd(path):
    oldpwd = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(oldpwd)


def test_clean_latex():
    """
    Clean mf6io.nightlybuild files
    """
    # do not build latex on osx
    #    if sys.platform == 'darwin':
    #        return

    pth = os.path.join("..", "doc", "mf6io")

    # remove existing files
    files = [
        "mf6io.nightlybuild.pdf",
        "mf6io.nightlybuild.aux",
        "mf6io.nightlybuild.bbl",
    ]
    delete_files(files, pth, allow_failure=True)
    return


def test_rebuild_from_dfn():
    """
    Rebuild mf6io.nightlybuild TeX files from dfn files
    """
    # do not build latex on osx
    #    if sys.platform == 'darwin':
    #        return

    npth = os.path.join("..", "doc", "mf6io", "mf6ivar")
    pth = "./"

    with cwd(npth):

        # get list of TeX files
        files = [
            f
            for f in os.listdir("tex")
            if os.path.isfile(os.path.join("tex", f))
        ]
        for f in files:
            fpth = os.path.join("tex", f)
            os.remove(fpth)

        # run python
        argv = ["python", "mf6ivar.py"]
        buff, ierr = run_command(argv, pth)
        msg = "\nERROR {}: could not run {} with {}".format(
            ierr, argv[0], argv[1]
        )
        assert ierr == 0, buff + msg

        # get list for dfn files
        dfnfiles = [
            os.path.splitext(f)[0]
            for f in os.listdir("dfn")
            if os.path.isfile(os.path.join("dfn", f))
            and "dfn" in os.path.splitext(f)[1]
        ]
        texfiles = [
            os.path.splitext(f)[0]
            for f in os.listdir("tex")
            if os.path.isfile(os.path.join("tex", f))
            and "tex" in os.path.splitext(f)[1]
        ]
        missing = ""
        icnt = 0
        for f in dfnfiles:
            if "common" in f:
                continue
            fpth = "{}-desc".format(f)
            if fpth not in texfiles:
                icnt += 1
                missing += "  {:3d} {}.tex\n".format(icnt, fpth)
        msg = "\n{} TeX file(s) are missing. ".format(
            icnt
        ) + "Missing files:\n{}".format(missing)
        assert icnt == 0, msg

    return


def test_build_mfio():
    """
    Build mf6io.nightlybuild.pdf from LaTeX files
    """
    # do not build latex on osx
    #    if sys.platform == 'darwin':
    #        return

    # set path to document files
    npth = os.path.join("..", "doc", "mf6io")

    pth = "./"

    with cwd(npth):
        # build pdf
        argv = ["pdflatex", "mf6io.nightlybuild.tex"]
        buff, ierr = run_command(argv, pth)
        msg = "\nERROR {}: could not run {} on {}".format(
            ierr, argv[0], argv[1]
        )
        assert ierr == 0, buff + msg

        argv = ["bibtex", "mf6io.nightlybuild.aux"]
        buff, ierr = run_command(argv, pth)
        msg = "\nERROR {}: could not run {} on {}".format(
            ierr, argv[0], argv[1]
        )
        assert ierr == 0, buff + msg

        argv = ["pdflatex", "mf6io.nightlybuild.tex"]
        buff, ierr = run_command(argv, pth)
        msg = "\nERROR {}: could not run {} on {}".format(
            ierr, argv[0], argv[1]
        )
        assert ierr == 0, buff + msg

        argv = ["pdflatex", "mf6io.nightlybuild.tex"]
        buff, ierr = run_command(argv, pth)
        msg = "\nERROR {}: could not run {} on {}".format(
            ierr, argv[0], argv[1]
        )
        assert ierr == 0, buff + msg

    return


def test_pdf():
    """
    Test if mf6io.nightlybuild.pdf exists
    """
    # do not build latex on osx
    #    if sys.platform == 'darwin':
    #        return

    pth = os.path.join("..", "doc", "mf6io")

    msg = "mf6io.nightlybuild.pdf does not exist"
    assert os.path.isfile(os.path.join(pth, "mf6io.nightlybuild.pdf")), msg


def delete_files(files, pth, allow_failure=False):
    for file in files:
        fpth = os.path.join(pth, file)
        try:
            print("removing...{}".format(file))
            os.remove(fpth)
        except:
            print("could not remove...{}".format(file))
            if not allow_failure:
                return False
    return True


def run_command(argv, pth, timeout=10):
    buff = ""
    ierr = 0
    with subprocess.Popen(
        argv, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, cwd=pth
    ) as process:
        try:
            output, unused_err = process.communicate(timeout=timeout)
            buff = output.decode("utf-8")
        except subprocess.TimeoutExpired:
            process.kill()
            output, unused_err = process.communicate()
            buff = output.decode("utf-8")
            ierr = 100
        except:
            output, unused_err = process.communicate()
            buff = output.decode("utf-8")
            ierr = 101

    return buff, ierr


def main():
    # write message
    tnam = os.path.splitext(os.path.basename(__file__))[0]
    msg = "Running {} test".format(tnam)
    print(msg)

    print("running...test_rebuild_from_dfn()")
    test_rebuild_from_dfn()
    print("running...test_clean_latex()")
    test_clean_latex()
    print("running...test_build_mfio()")
    test_build_mfio()
    print("running...test_pdf()")
    test_pdf()

    return


if __name__ == "__main__":
    print("standalone run of {}".format(os.path.basename(__file__)))

    # run main routine
    main()
