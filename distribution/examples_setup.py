import os
import shutil
from collections import OrderedDict
import pymake


def load_examples_df():

    examples_dfn = 'examples.dfn'
    with open(examples_dfn) as f:
        lines = f.readlines()

    examples_dict = OrderedDict()
    exd = {}
    for line in lines:

        if len(line.strip()) == 0:
            if len(exd) > 0:
                name = exd['distribution_name']
                if name in examples_dict:
                    raise Exception(
                        'Variable already exists in dictionary: ' + name)
                examples_dict[name] = exd
            exd = {}
            continue

        # skip comments
        if '#' in line.strip()[0]:
            continue

        ll = line.strip().split()
        if len(ll) > 1:
            k = ll[0]
            istart = line.index(' ')
            v = line[istart:].strip()
            if k in exd:
                raise Exception('Attribute already exists in dictionary: ' + k)
            exd[k] = v

    return examples_dict


def setup_examples(srcdir, dstdir, win_target_os=True):

    examples_dict = load_examples_df()

    # Create a runall.bat file in examples
    if win_target_os:
        frunallbat = open(os.path.join(dstdir, 'runall.bat'), 'w')
    else:
        frunallbat = None

    for k in examples_dict:
        v = examples_dict[k]

    # For each example, copy the necessary files from the development directory
    # into the distribution directory.
    print('Copying examples')
    for i, exdest in enumerate(examples_dict):

        exd = examples_dict[exdest]
        exsrc = exd['examples_name']  # name in modflow6-testmodels repo
        srcpath = os.path.join(srcdir, exsrc)  # path to modflow6-testmodels repo

        prefix = 'ex{:02d}-'.format(i + 1)
        destfoldername = prefix + exdest
        dstpath = os.path.join(dstdir, prefix + exdest)
        print('  {:<35} ===> {:<20}'.format(exsrc, prefix + exdest))

        # Copy all of the mf6 input from srcpath to dstpath
        pymake.setup_mf6(srcpath, dstpath)

        # Create a description.txt file in the example folder
        fname = os.path.join(dstpath, 'description.txt')
        with open(fname, 'w') as f:
            f.write(exd['description_text'] + '\n')

        if win_target_os:
            # Create a batch file for running the model
            fname = os.path.join(dstpath, 'run.bat')
            with open(fname, 'w') as f:
                s = '@echo off'
                f.write(s + '\n')
                s = r'..\..\bin\mf6.exe'
                f.write(s + '\n')
                s = 'echo.'
                f.write(s + '\n')
                s = 'echo Run complete.  Press any key to continue.'
                f.write(s + '\n')
                s = 'pause>nul'
                f.write(s + '\n')

            if frunallbat is not None:
                frunallbat.write('cd ' + destfoldername + '\n')
                frunallbat.write(r'..\..\bin\mf6.exe' + '\n')
                frunallbat.write('cd ..' + '\n\n')
    print('\n')

    if frunallbat is not None:
        frunallbat.write('pause' + '\n')
        frunallbat.close()

    return


def make_example_items(fname):

    # load examples.df into a dictionary
    examples_dict = load_examples_df()

    # Open the file
    f = open(fname, 'w')

    # Write the latex header
    s = r'''\begin{itemize}'''
    f.write(s)
    f.write('\n')


    for i, exdest in enumerate(examples_dict):

        exd = examples_dict[exdest]
        prefix = 'ex{:02d}-'.format(i + 1)
        exname = prefix + exdest

        s = r'\item {}---'.format(exname) + exd['release_text'] + '\n'
        f.write(s)
        f.write('\n')

    s = r'''\end{itemize}
    '''
    f.write(s)
    f.close()
    return


def make_example_table(fname, examplespath):

    # load examples.df into a dictionary
    examples_dict = load_examples_df()

    # Open the file
    f = open(fname, 'w')

    s = r'''
\small
\begin{longtable}{p{3cm} p{1cm} p{3cm} p{2.5cm}p{4cm}}
\caption{List of example problems and simulation characteristics}\tabularnewline


\hline
\hline
\textbf{Name} & \textbf{NPER} & \textbf{Namefile(s)} & \textbf{Dimensions (NLAY, NROW, NCOL), (NLAY, NCPL) or (NODES)}  & \textbf{Stress Packages} \\
\hline
\endfirsthead

\hline
\hline
\textbf{Name} & \textbf{NPER} & \textbf{Namefile(s)} & \textbf{Dimensions (NLAY, NROW, NCOL) or (NODES)}  & \textbf{Stress Packages} \\
\hline
\endhead

    '''
    f.write(s)
    f.write('\n')


    for i, exdest in enumerate(examples_dict):

        exd = examples_dict[exdest]
        prefix = 'ex{:02d}-'.format(i + 1)
        exname = prefix + exdest
        s = '{} '.format(exname)

        # number of models
        mfnamefile = os.path.join(examplespath, exname, 'mfsim.nam')
        model_files, outfiles = pymake.get_mf6_files(mfnamefile)
        nmodels = len([w for w in model_files if w.lower().endswith('.nam')])
        # s += '& {} '.format(nmodels)

        # Number of stress periods
        tdis = [w for w in model_files if w.upper().endswith('.TDIS')][0]
        nper = pymake.get_mf6_nper(os.path.join(examplespath,
                                                         exname, tdis))
        s += '& {} '.format(nper)

        # Name files
        namefiles = [w for w in model_files if w.lower().endswith('.nam')]
        s += '& '
        cellstring = '\parbox[t]{3cm}{' + ''.join(
            r' {} \\'.format(nf) for nf in namefiles) + '}'
        s += cellstring

        # Model shape
        dis_files = [w for w in model_files if w.upper().endswith('.DIS')
                     or w.upper().endswith('.DISU')
                     or w.upper().endswith('.DISV')]
        s += '& '
        mshapes = []
        for disfile in dis_files:
            mshape = pymake.get_mf6_mshape(
                os.path.join(examplespath, exname, disfile))
            mshapes.append(mshape)
        cellstring = '\parbox[t]{3cm}{' + ''.join(
            r' {} \\'.format(ms) for ms in mshapes) + '}'
        s += cellstring

        # File types
        namefiles = [w for w in model_files if w.lower().endswith('.nam')]
        s += '& '
        lines = []
        for nf in namefiles:
            ftypes = pymake.get_mf6_ftypes(
                os.path.join(examplespath, exname, nf),
                ['CHD6', 'WEL6', 'DRN6', 'RIV6', 'GHB6', 'SFR6', 'RCH6',
                 'EVT6', 'SFR6', 'UZF6', 'MAW6', 'LAK6', 'MVR6'])
            ss = ''
            for st in ftypes:
                if st[:3].upper() not in ss:
                    ss += st[:3].upper() + ' '
            if len(ss) == 0:
                ss = 'none'
            lines.append(ss)
        cellstring = '\parbox[t]{4cm}{' + ''.join(
            r' {} \\'.format(ls) for ls in lines) + '}'
        s += cellstring

        # End the table line for this example
        s = s.replace('_', r'\_')
        s += r'\\'
        f.write(s)
        f.write('\n')

        f.write(r'\hline' + '\n')

    s = r'''\hline
\end{longtable}
\label{table:examples}
\normalsize

    '''
    f.write(s)
    f.close()
    return


if __name__ == '__main__':

    srcdir = os.path.join('..', '..', 'modflow6-testmodels.git', 'mf6')
    assert os.path.isdir(srcdir)

    dstdir = './examples'
    if os.path.isdir(dstdir):
        shutil.rmtree(dstdir)
    os.mkdir(dstdir)

    setup_examples(srcdir, dstdir)

    make_example_items('temp001.tex')

    make_example_table('temp002.tex', dstdir)

