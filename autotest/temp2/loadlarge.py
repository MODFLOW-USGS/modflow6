from pathlib import Path
import shutil
from pprint import pprint

import flopy

#def searching_all_files(directory):
#    dirpath = Path(directory)
#    assert dirpath.is_dir()
#    file_list = []
#    for x in dirpath.iterdir():
#        if x.is_file():
#            file_list.append(x)
#        elif x.is_dir():
#            file_list.extend(searching_all_files(x))
#    return file_list

def find_test_dirs(d):
    dirpath = Path(d)
    assert dirpath.is_dir()
    test_dirs = []
    for x in dirpath.iterdir():
        if x.is_dir():
            test_dirs.append(x)
    return test_dirs

p = Path("../../../modflow6-largetestmodels")
#tests = find_test_dirs(p)
#pprint(tests)
tests = ['../../../modflow6-largetestmodels/test1201_gwtbuy-elderRa60']

# my issues
#test1004_mvlake_laksfr_tr has k33 factor in npf like other testmodels test
#test1004_mvlake_lak_tr probably has same issue but need to comfirm
#test1004_mvlake_lak_ss_dev probably has same issue but need to comfirm

count = 0
fails = []
for test in tests:
    #if test == "../../../modflow6-testmodels/mf6/test001h_rch_list4" or \
    #    test == "../../../modflow6-testmodels/mf6/test204_gwtbuy-henryGHBm":
    #    continue
    if "python" in str(test) or \
        "git" in str(test) or \
        "test1002_biscqtg_dev" in str(test) or \
        "test1002_biscqtg_gnc_dev" in str(test):
        continue
    pprint(f"Testing: {test}")
    ws = test
    sim = flopy.mf6.MFSimulation.load(sim_ws=ws, exe_name="../../bin/mf6")
    sim.set_sim_path(path="./ltest")
    sim.write_simulation(write_netcdf=True, silent=True)
    success, buf = sim.run_simulation(silent=True)
    if success:
        print("PASS")
    else:
        print("FAIL")
        fails.append(test)
        exit
    #shutil.rmtree("./ltest")
    count += 1
    print(f"tests run: {count}")
    print(f"tests failed: {len(fails)}")

pprint(fails)
exit(0)
