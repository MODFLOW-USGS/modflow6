from pathlib import Path
import shutil
from pprint import pprint

import flopy

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
#tests = ['../../../modflow6-testmodels/mf6/test001h_rch_array1']
#tests = ['../temp/lgr_pc']
#test028_sfr

# MY WORK
#test001h_rch_array1
#test004_lpfss_disv THIS is a segfault ../src/Utilities/Idm/netcdf/NCFileGridInput.f90 recharge dim is iper, ncpl
#test059_mvlake_lak_ss
    # npf file k33 has FACTOR set on multiple layers
    # removing factor and running ascii gives exact failure (convergence) and budget at end of sp 1 step 1
    # should netcdf support factor?
#test006_2models simulation.exg has no DIMENSIONS block with required dim, why doesn't this fail in ascii?
    # Adding block with NEXG 36 solves it, looks like this has something to do with the load?
#"test001h_rch_array" in str(test) or \
#"test001h_evt_array" in str(test) or \


# ECLUDED WHY
# TIMESERIES    test027_TimeseriesTest_idomain_dev
# TIMESERIES    test106_tsnodata
# TIMESERIES    test027_TimeseriesTest_dev
# EXC LIST      test004_bcfss
# TIMESERIES    test001h_evt_list3, test001h_evt_list3
# TIMESERIES    test001h_rch_list3, test001h_rch_list4
# EXC LIST      test001a_Tharmonic_tabs
# EXC LIST      test041_flowdivert_nwt_dev
# TIMESERIES    test005_advgw_tidal
# TIMESERIES    test001h_drn_list4
# EXC LIST      test014_NWTP3Low_dev

# reported
#test001h_evt_list2
#test028_sfr_mvr_dev
#test006_gwf3_disv_ext

# additional fail examples (exclude for now)
#test060_gms_ets is another example of flopy not handling evt list with nseg corretly (here nseg should be 3)
#test001h_evt_list1
#test028_sfr this is 10 tests and I think many or all have the same issue as test028_sfr_mvr_dev

count = 0
fails = []
for test in tests:
    if "python" in str(test) or \
        "test001h_evt_array3" in str(test) or \
        "test001h_evt_array4" in str(test) or \
        "test001h_rch_array3" in str(test) or \
        "test001h_rch_array4" in str(test) or \
        "test001h_evt_list1" in str(test) or \
        "test001h_evt_list2" in str(test) or \
        "test028_sfr" in str(test) or \
        "test028_sfr_mvr_dev" in str(test) or \
        "test006_gwf3_disv_ext" in str(test) or \
        "test060_gms_ets" in str(test) or \
        "test027_TimeseriesTest_idomain_dev" in str(test) or \
        "test106_tsnodata" in str(test) or \
        "test027_TimeseriesTest_dev" in str(test) or \
        "test004_bcfss" in str(test) or \
        "test001h_evt_list3" in str(test) or \
        "test001h_evt_list4" in str(test) or \
        "test001h_rch_list3" in str(test) or \
        "test001h_rch_list4" in str(test) or \
        "test001a_Tharmonic_tabs" in str(test) or \
        "test041_flowdivert_nwt_dev" in str(test) or \
        "test005_advgw_tidal" in str(test) or \
        "test001h_drn_list4" in str(test) or \
        "test014_NWTP3Low_dev" in str(test):
        continue
    pprint(f"Testing: {test}")
    ws = test
    sim = flopy.mf6.MFSimulation.load(sim_ws=ws, exe_name="../../bin/mf6")
    sim.set_sim_path(path="./test")
    sim.write_simulation(write_netcdf=True, silent=True)
    #sim.write_simulation(write_netcdf=True)
    #sim.write_simulation()
    #success, buf = sim.run_simulation(silent=True)
    success, buf = sim.run_simulation()
    if success:
        print("PASS")
    else:
        print("FAIL")
        print(buf)
        fails.append(test)
    shutil.rmtree("./test")
    count += 1
    print(f"tests run: {count}")
    print(f"tests failed: {len(fails)}")

pprint(fails)
exit(0)
