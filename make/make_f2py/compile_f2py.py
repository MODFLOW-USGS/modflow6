"""Compile MF6 as Python extension, using `f2py`.
"""

from os import getcwd, listdir, walk
from os.path import join
from shutil import move
from subprocess import call


CMD = 'f2py'
SRC = '../src'
ORDER = [
    'kind.f90',
    'OpenSpec.f90',
    'HashTable.f90',
    'version.f90',
    'ims8reordering.f90',
    'List.f90',
    'BaseGeometry.f90',
    'Xt3dAlgorithm.f90',
    'Constants.f90',
    'SimVariables.f90',
    'genericutils.f90',
    'ObsOutput.f90',
    'mf6lists.f90',
    'Sparse.f90',
    'SmoothingFunctions.f90',
    'gfortran.f90',
    'Memory.f90',
    'ArrayHandlers.f90',
    'TimeSeriesRecord.f90',
    'MemoryList.f90',
    'StringList.f90',
    'Timer.f90',
    'Sim.f90',
    'Budget.f90',
    'InputOutput.f90',
    'ArrayReaders.f90',
    'comarg.f90',
    'RectangularGeometry.f90',
    'BlockParser.f90',
    'MemoryManager.f90',
    'ListReader.f90',
    'Iunit.f90',
    'RectangularChGeometry.f90',
    'CircularGeometry.f90',
    'ims8linear.f90',
    'ObsOutputList.f90',
    'BaseModel.f90',
    'TimeSeries.f90',
    'DisvGeom.f90',
    'NameFile.f90',
    'PackageMover.f90',
    'TimeSeriesFileList.f90',
    'PrintSaveManager.f90',
    'Connections.f90',
    'BaseSolution.f90',
    'Mover.f90',
    'tdis.f90',
    'sort.f90',
    'TimeSeriesLink.f90',
    'SolutionGroup.f90',
    'BaseExchange.f90',
    'BndUzfKinematic.f90',
    'TimeSeriesManager.f90',
    'DiscretizationBase.f90',
    'NumericalPackage.f90',
    'gwf3dis8.f90',
    'gwf3disu8.f90',
    'gwf3mvr8.f90',
    'TimeArray.f90',
    'TimeArraySeries.f90',
    'gwf3ic8.f90',
    'gwf3sto8.f90',
    'Xt3dInterface.f90',
    'gwf3disv8.f90',
    'Observe.f90',
    'gwf3hfb8.f90',
    'OutputControlData.f90',
    'ObsUtility.f90',
    'TimeArraySeriesLink.f90',
    'TimeArraySeriesManager.f90',
    'ObsContainer.f90',
    'gwf3npf8.f90',
    'OutputControl.f90',
    'Obs3.f90',
    'gwf3oc8.f90',
    'gwf3obs8.f90',
    'BoundaryPackage.f90',
    'gwf3riv8.f90',
    'gwf3rch8.f90',
    'gwf3maw8.f90',
    'gwf3ghb8.f90',
    'gwf3evt8.f90',
    'gwf3sfr8.f90',
    'NumericalModel.f90',
    'gwf3lak8.f90',
    'gwf3uzf8.f90',
    'gwf3drn8.f90',
    'gwf3wel8.f90',
    'gwf3chd8.f90',
    'NumericalExchange.f90',
    'GhostNode.f90',
    'NumericalSolution.f90',
    'gwf3.f90',
    'GwfGwfExchange.f90',
    'SimulationCreate.f90',
    'mf6sub.f90',
    'shared_data.f90',
    'access_memory.f90'
        ]


def compile_ext(src=SRC, cmd=CMD, order=tuple(ORDER)):
    """Compile with `f2py`
    """
    pyf_cmd = [cmd, '--overwrite-signature',
                    '../../src/mf6sub.f90',
                    '../../src/shared_data.f90',
                    '../../src/access_memory.f90',
               #'../../src/Utilities/Constants.f90',
               #'../../src/Model/Geometry/BaseGeometry.f90',
               '-m', 'mf6',
               '-h', 'mf6.pyf']
    compile_cmd = [cmd, '--f90flags=-O2 -fbacktrace', '-c', 'mf6.pyf',
                   '-I../obj_temp/', '../../src/mf6sub.f90',
                   '../../src/shared_data.f90',
                   '../../src/access_memory.f90',
                   ]

    fnames = {}
    for root, _, files in walk(src):
        for fname in files:
            if fname.endswith('.f90'):
                fnames[fname] = join(root, fname)
    sep = 3
    object_files = ['../obj_temp/' + name.split('.f90')[0] + '.o' for name in order[:-sep]]


    compile_cmd.extend(object_files)
    call(pyf_cmd)
    call(compile_cmd)

    for fname in listdir(getcwd()):
        if fname.endswith('.so') or fname.endswith('.pyd'):
            move(fname, f'../../python/{fname}')


if __name__ == '__main__':
    compile_ext()
