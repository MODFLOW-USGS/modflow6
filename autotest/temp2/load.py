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

p = Path("../../../modflow6-testmodels/mf6")
tests = find_test_dirs(p)
#pprint(tests)

count = 0
fails = []
for test in tests:
    #if test == "../../../modflow6-testmodels/mf6/test001h_rch_list4" or \
    #    test == "../../../modflow6-testmodels/mf6/test204_gwtbuy-henryGHBm":
    #    continue
    if "test001h_rch_list4" in str(test) or \
        "test204_gwtbuy-henryGHBm" in str(test) or \
        "test027_TimeseriesTest_idomain_dev" in str(test) or \
        "test001h_evt_array3" in str(test) or \
        "test001h_rch_list3" in str(test) or \
        "test001h_rch_array3" in str(test) or \
        "test001h_evt_list2" in str(test) or \
        "test028_sfr_mvr_dev" in str(test) or \
        "test014_NWTP3Low_dev" in str(test) or \
        "python" in str(test) or \
        "test106_tsnodata" in str(test) or \
        "test041_flowdivert_nwt_dev" in str(test) or \
        "test027_TimeseriesTest_dev" in str(test) or \
        "test004_bcfss" in str(test) or \
        "test028_sfr" in str(test) or \
        "test060_gms_ets" in str(test) or \
        "test005_advgw_tidal" in str(test) or \
        "test006_gwf3_disv_ext" in str(test) or \
        "test001a_Tharmonic_tabs" in str(test) or \
        "_drn_list" in str(test) or \
        "evt_" in str(test) or "rch_" in str(test):
        continue
    pprint(f"Testing: {test}")
    ws = test
    sim = flopy.mf6.MFSimulation.load(sim_ws=ws, exe_name="../../bin/mf6")
    sim.set_sim_path(path="./test")
    sim.write_simulation(write_netcdf=True, silent=True)
    success, buf = sim.run_simulation(silent=True)
    if success:
        print("PASS")
    else:
        print("FAIL")
        fails.append(test)
    shutil.rmtree("./test")
    count += 1
    print(f"tests run: {count}")
    print(f"tests failed: {len(fails)}")

pprint(fails)
exit(0)
