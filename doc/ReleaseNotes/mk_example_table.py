
import os
import sys
import pymake

pth = os.path.join('..', '..')
if pth not in sys.path:
    sys.path.append(pth)


def get_distribution_name(versiontexname):
    dist = None
    fname = versiontexname
    with open(fname) as f:
        lines = f.readlines()
        f.close()
    for line in lines:
        # \newcommand{\modflowversion}{mf6beta0.9.00}
        srchtxt = '\\newcommand{\\modflowversion}'
        istart = line.rfind('{') + 1
        istop = line.rfind('}')
        if 0 < istart < istop:
            dist = line[istart: istop]
            return dist
    return None


# Set up the path to the distribution
fname = os.path.join('..', 'version.tex')
dist = get_distribution_name(fname)
distpth = os.path.join('..', '..', 'distribution', dist)
if not os.path.isdir(distpth):
    raise Exception(distpth + ' does not exist. ')

# Open the file
f = open('example_table.tex', 'w')
    
# Write the latex header

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

expth = os.path.join(distpth, 'examples')
files = os.listdir(expth)
for exname in files:
    
    # Skip if not a directory
    if not os.path.isdir(os.path.join(expth, exname)):
        continue
    
    # example name
    s = '{} '.format(exname)
    
    # number of models
    mfnamefile = os.path.join(expth, exname, 'mfsim.nam')
    model_files, outfiles = pymake.autotest.get_mf6_files(mfnamefile)
    nmodels = len([w for w in model_files if w.lower().endswith('.nam')])
    # s += '& {} '.format(nmodels)


    # Number of stress periods
    tdis = [w for w in model_files if w.upper().endswith('.TDIS')][0]
    nper = pymake.autotest.get_mf6_nper(os.path.join(expth, exname, tdis))
    s += '& {} '.format(nper)


    # Name files    
    namefiles = [w for w in model_files if w.lower().endswith('.nam')]
    s += '& '
    cellstring = '\parbox[t]{3cm}{' + ''.join(r' {} \\'.format(nf) for nf in namefiles) + '}'
    s += cellstring

    # Model shape
    dis_files = [w for w in model_files if w.upper().endswith('.DIS')
                 or w.upper().endswith('.DISU')
                 or w.upper().endswith('.DISV')]
    s += '& '
    mshapes = []
    for disfile in dis_files:
        mshape = pymake.autotest.get_mf6_mshape(os.path.join(expth, exname, disfile))
        mshapes.append(mshape)
    cellstring = '\parbox[t]{3cm}{' + ''.join(r' {} \\'.format(ms) for ms in mshapes) + '}'
    s += cellstring

    # File types
    namefiles = [w for w in model_files if w.lower().endswith('.nam')]
    s += '& '
    lines = []
    for nf in namefiles:
        ftypes = pymake.autotest.get_mf6_ftypes(os.path.join(expth, exname, nf),
                   ['CHD6', 'WEL6', 'DRN6', 'RIV6', 'GHB6', 'SFR6', 'RCH6',
                    'EVT6', 'SFR6', 'UZF6', 'MAW6', 'LAK6', 'MVR6'])
        ss = ''
        for st in ftypes:
            if st[:3] not in ss:
                ss += st[:3] + ' '
        if len(ss) == 0:
            ss = 'none'
        lines.append(ss)
    cellstring = '\parbox[t]{4cm}{' + ''.join(r' {} \\'.format(ls) for ls in lines) + '}'
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

print('done...')
