import os
import sys
from pathlib import Path

path = sys.argv[1]
if path.startswith('/') or path.startswith('\\'):
    print(Path(path).resolve())
elif path.startswith('~'):
    print(Path(os.path.expanduser(path)).resolve())
else:
    print(os.path.realpath(Path(path).resolve()))
