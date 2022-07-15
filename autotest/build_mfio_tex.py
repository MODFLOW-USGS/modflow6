import os
import subprocess
from contextlib import contextmanager

# base name for mf6io LaTeX document
base_name = "mf6io"


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
    Clean mf6io files
    """
    # do not build latex on osx
    #    if sys.platform == 'darwin':
    #        return

    pth = os.path.join("..", "doc", "mf6io")

    # remove existing files
    files = [
        f"{base_name}.pdf",
        f"{base_name}.aux",
        f"{base_name}.bbl",
    ]
    delete_files(files, pth, allow_failure=True)
    return


def test_rebuild_from_dfn():
    """
    Rebuild mf6io TeX files from dfn files
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
        msg = f"\nERROR {ierr}: could not run {argv[0]} with {argv[1]}"
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
            fpth = f"{f}-desc"
            if fpth not in texfiles:
                icnt += 1
                missing += f"  {icnt:3d} {fpth}.tex\n"
        msg = (
            "\n{} TeX file(s) are missing. ".format(icnt)
            + f"Missing files:\n{missing}"
        )
        assert icnt == 0, msg

    return


def test_build_mfio():
    """
    Build mf6io.pdf from LaTeX files
    """
    # do not build latex on osx
    #    if sys.platform == 'darwin':
    #        return

    # set path to document files
    npth = os.path.join("..", "doc", "mf6io")

    pth = "./"

    with cwd(npth):
        # build pdf
        argv = ["pdflatex", f"{base_name}.tex"]
        buff, ierr = run_command(argv, pth)
        msg = f"\nERROR {ierr}: could not run {argv[0]} on {argv[1]}"
        assert ierr == 0, buff + msg

        argv = ["bibtex", f"{base_name}.aux"]
        buff, ierr = run_command(argv, pth)
        msg = f"\nERROR {ierr}: could not run {argv[0]} on {argv[1]}"
        assert ierr == 0, buff + msg

        argv = ["pdflatex", f"{base_name}.tex"]
        buff, ierr = run_command(argv, pth)
        msg = f"\nERROR {ierr}: could not run {argv[0]} on {argv[1]}"
        assert ierr == 0, buff + msg

        argv = ["pdflatex", f"{base_name}.tex"]
        buff, ierr = run_command(argv, pth)
        msg = f"\nERROR {ierr}: could not run {argv[0]} on {argv[1]}"
        assert ierr == 0, buff + msg

    return


def test_pdf():
    """
    Test if mf6io.pdf exists
    """
    # do not build latex on osx
    #    if sys.platform == 'darwin':
    #        return

    pth = os.path.join("..", "doc", "mf6io")

    msg = "mf6io.pdf does not exist"
    assert os.path.isfile(os.path.join(pth, f"{base_name}.pdf")), msg


def delete_files(files, pth, allow_failure=False):
    for file in files:
        fpth = os.path.join(pth, file)
        try:
            print(f"removing...{file}")
            os.remove(fpth)
        except:
            print(f"could not remove...{file}")
            if not allow_failure:
                return False
    return True


def run_command(argv, pth, timeout=10):
    with subprocess.Popen(
        argv, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, cwd=pth
    ) as process:
        try:
            output, unused_err = process.communicate(timeout=timeout)
            buff = output.decode("utf-8")
            ierr = process.returncode
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
    msg = f"Running {tnam} test"
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
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()
