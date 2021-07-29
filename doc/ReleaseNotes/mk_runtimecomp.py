# Simple script to convert the run time comparison markdown table in the
# distribution folder into a latex table that can be included in the
# release notes

import os

header = r"""
\section{Run-Time Comparison}

Comparison of run times of the current version of MODFLOW 6 to the 
previous version. The distribution example models are used to compare run 
times.  Simulations that fail are indicated by '--'. The percent difference, 
where calculated, is relative to the simulation run time for the previous 
version. Percent differences for example problems with short run times 
(less than 30 seconds) may not be significant.

\small
\begin{longtable}[!htbp]{p{5cm} p{3cm} p{3cm} p{1.5cm}}
\caption{List of run time comparsons}
\label{table:run-time-comparisons}
\tabularnewline

\hline
\hline
\textbf{Example Problem} & \textbf{Current Version} & \textbf{Previous Version} & \textbf{Percent difference}   \\
\hline
\endfirsthead

\hline
\hline
\textbf{Example Problem} & \textbf{Current Version} & \textbf{Previous Version} & \textbf{Percent difference}   \\
\hline
\endhead

"""

data = r"""
ex-gwf-advtidal & 0.592 Seconds & 0.623 Seconds & -4.98 \\
\hline 
ex-gwf-bcf2ss-p01a & 0.020 Seconds & 0.040 Seconds & -50.00 \\
\hline 
"""

footer = r"""
\hline
\end{longtable}
\normalsize
"""

if __name__ == "__main__":

    fname = "../../distribution/run-time-comparison.md"
    fnametex = "run-time-comparison.tex"
    if os.path.isfile(fnametex):
        os.remove(fnametex)

    # if the markdown table exists, then convert it to latex
    if os.path.isfile(fname):
        ftex = open(fnametex, 'w')
        ftex.write(header)
        skipline = True
        with open(fname) as fmd:
            for line in fmd:
                if not skipline:
                    ll = line.strip().split('|')
                    ll = ll[1:-1]
                    linetex = "& ".join(ll)
                    linetex = linetex.replace("\\", "/")
                    linetex += '\\\\' + '\n'
                    linetex = linetex.replace("%", "\\%")
                    ftex.write(linetex)
                    ftex.write("\\hline\n")
                if ":-" in line:
                    skipline = False
        ftex.write(footer)
        ftex.close()
