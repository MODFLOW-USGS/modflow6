import os
import sys
import numpy as np

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

from flopy.utils.lgrutil import Lgr
from framework import testing_framework
from simulation import Simulation

# Test for the interface model approach.
# It compares the result of a single, strongly anisotropic model 
# with XT3D enabled to the equivalent case where the domain is
# decomposed and joined by a GWF-GWF exchange with XT3D applied.
# The head values should always be indentical. All models are
# part of the same solution for convenience.
# In addition, a check on the x,y,z components of specific discharge
# is present. The values for the rightmost column of the left submodel
# is compared to the 5th column of the full model: they should be identical.
ex = ["ifmod_xt3d02"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))

# global convenience...
mname_ref = 'refmodel'
mname_left = 'leftmodel'
mname_right = 'rightmodel'
hclose_check = 1e-9

useXT3D = True

def get_model(idx, dir):    
    name = ex[idx]    
    
    # parameters and spd
    # tdis
    nper = 1
    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((1., 1, 1))

    # solver data
    nouter, ninner = 100, 300
    hclose, rclose, relax = hclose_check, 1e-3, 0.97

    # model spatial discretization
    nlay = 1
    ncol = 10
    ncol_left = 5
    ncol_right = 5
    nrow = 10

    # cell spacing
    delr = 10.0
    delc = 10.0
    area = delr * delc

    # shift (hor. and vert.)
    shift_x = 5*delr
    shift_y = 0.0

    # top/bot of the aquifer
    tops = [0.0, -5.0]

    # hydraulic conductivity
    k11 = 10.0
    k22 = 0.1
    k_angle = 45.0

    # boundary stress period data
    h_left = -2.
    h_right = -2.

    # initial head
    h_start = -2.

    # well
    well_id = (0,4,4)
    well_rate = -1.0
    
    # This creates the single model, for reference:
    left_chd = [[(0,irow,0), h_left] for irow in range(nrow)]
    right_chd = [[(0,irow,ncol-1), h_right] for irow in range(nrow)]
    chd_data = left_chd + right_chd
    chd_spd = {0: chd_data}    

    sim = flopy.mf6.MFSimulation(sim_name=name, version='mf6',
                                     exe_name='mf6',
                                     sim_ws=dir)

    tdis = flopy.mf6.ModflowTdis(sim, time_units='DAYS',
                                 nper=nper, perioddata=tdis_rc)

    ims = flopy.mf6.ModflowIms(sim,
                               print_option='SUMMARY',
                               outer_hclose=hclose,
                               outer_maximum=nouter,
                               under_relaxation='DBD',
                               inner_maximum=ninner,
                               inner_hclose=hclose, rcloserecord=rclose,
                               linear_acceleration='BICGSTAB',
                               relaxation_factor=relax)

    gwf = flopy.mf6.ModflowGwf(sim, modelname=mname_ref, save_flows=True)

    dis = flopy.mf6.ModflowGwfdis(gwf, nlay=nlay, nrow=nrow, ncol=ncol,
                                  delr=delr, delc=delc,
                                  top=tops[0], botm=tops[1:])

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=h_start)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, 
                                  save_specific_discharge=True,
                                  xt3doptions=useXT3D,
                                  save_flows=True,
                                  icelltype=0,
                                  k=k11, k22=k22, angle1=k_angle)

    # chd file
    chd = flopy.mf6.ModflowGwfchd(gwf, stress_period_data=chd_spd)

    # well
    wel1 = flopy.mf6.ModflowGwfwel(gwf,
                                   stress_period_data=[[well_id, well_rate]],
                                   print_input=True,
                                   print_flows=True,
                                   save_flows=False,
                                   pname='WEL-1')

    # output control
    oc = flopy.mf6.ModflowGwfoc(gwf,
                                head_filerecord='{}.hds'.format(mname_ref),
                                budget_filerecord='{}.cbc'.format(mname_ref),
                                headprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('HEAD', 'LAST'), ('BUDGET', 'LAST')])
        
    # Now create two coupled models with the interface model enabled,
    # to be stored in the same solution as the reference model
    
    # submodel on the left:
    left_chd = [[(0,irow,0), h_left] for irow in range(nrow)]
    chd_spd_left = {0: left_chd}

    gwf = flopy.mf6.ModflowGwf(sim, modelname=mname_left, save_flows=True)
    dis = flopy.mf6.ModflowGwfdis(gwf, nlay=nlay, nrow=nrow, ncol=ncol_left,
                                  delr=delr, delc=delc,
                                  top=tops[0], botm=tops[1:])
    ic = flopy.mf6.ModflowGwfic(gwf, strt=h_start)
    npf = flopy.mf6.ModflowGwfnpf(gwf,
                                  save_specific_discharge=True,
                                  xt3doptions=useXT3D,
                                  save_flows=True, 
                                  icelltype=0,
                                  k=k11, k22=k22, angle1=k_angle)
    chd = flopy.mf6.ModflowGwfchd(gwf, stress_period_data=chd_spd_left)
    oc = flopy.mf6.ModflowGwfoc(gwf,
                                head_filerecord='{}.hds'.format(mname_left),
                                budget_filerecord='{}.cbc'.format(mname_left),
                                headprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('HEAD', 'LAST'), ('BUDGET', 'LAST')])
    wel1 = flopy.mf6.ModflowGwfwel(gwf,
                                   stress_period_data=[[well_id, well_rate]],
                                   print_input=True,
                                   print_flows=True,
                                   save_flows=False,
                                   pname='WEL-1')

    # submodel on the right:
    right_chd = [[(0,irow,ncol_right-1), h_right] for irow in range(nrow)]
    chd_spd_right = {0: right_chd}

    gwf = flopy.mf6.ModflowGwf(sim, modelname=mname_right, save_flows=True)
    dis = flopy.mf6.ModflowGwfdis(gwf, nlay=nlay, nrow=nrow, ncol=ncol_right,
                                  delr=delr, delc=delc,
                                  xorigin=shift_x, yorigin=shift_y,
                                  top=tops[0], botm=tops[1:])
    ic = flopy.mf6.ModflowGwfic(gwf, strt=h_start)
    npf = flopy.mf6.ModflowGwfnpf(gwf,
                                  save_specific_discharge=True,
                                  xt3doptions=useXT3D,
                                  save_flows=True,
                                  icelltype=0,
                                  k=k11, k22=k22, angle1=k_angle)
    chd = flopy.mf6.ModflowGwfchd(gwf, stress_period_data=chd_spd_right)
    oc = flopy.mf6.ModflowGwfoc(gwf,
                                head_filerecord='{}.hds'.format(mname_right),
                                budget_filerecord='{}.cbc'.format(mname_right),
                                headprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('HEAD', 'LAST'), ('BUDGET', 'LAST')])

    # exchangedata
    angldegx = 0.0
    cdist = delr
    gwfgwf_data = [[(0,irow,ncol_left-1), (0,irow,0), 1, delr/2., delr/2., delc, angldegx, cdist] for irow in range(nrow)]
    gwfgwf = flopy.mf6.ModflowGwfgwf(sim, exgtype='GWF6-GWF6',
                                     nexg=len(gwfgwf_data),
                                     exgmnamea=mname_left,
                                     exgmnameb=mname_right,
                                     exchangedata=gwfgwf_data,
                                     auxiliary=["ANGLDEGX", "CDIST"],
                                     xt3d = useXT3D
                                    )
                                    
    return sim


def build_models():
    for idx, dir in enumerate(exdirs):
        sim = get_model(idx, dir)
        sim.write_simulation()
    return

def qxqyqz(fname, nlay, nrow, ncol):
    nodes = nlay * nrow * ncol
    cbb = flopy.utils.CellBudgetFile(fname, precision="double")
    spdis = cbb.get_data(text="DATA-SPDIS")[0]
    qx = np.ones((nodes), dtype=float) * 1.0e30
    qy = np.ones((nodes), dtype=float) * 1.0e30
    qz = np.ones((nodes), dtype=float) * 1.0e30
    n0 = spdis["node"] - 1
    qx[n0] = spdis["qx"]
    qy[n0] = spdis["qy"]
    qz[n0] = spdis["qz"]
    qx = qx.reshape(nlay, nrow, ncol)
    qy = qy.reshape(nlay, nrow, ncol)
    qz = qz.reshape(nlay, nrow, ncol)
    qx = np.ma.masked_equal(qx, 1.0e30)
    qy = np.ma.masked_equal(qy, 1.0e30)
    qz = np.ma.masked_equal(qz, 1.0e30)
    return qx, qy, qz

def compare_to_ref(sim):    
    print("comparing heads and spec. discharge to single model reference...")

    fpth = os.path.join(sim.simpath, "{}.hds".format(mname_ref))
    hds = flopy.utils.HeadFile(fpth)
    heads = hds.get_data()
    fpth = os.path.join(sim.simpath, "{}.cbc".format(mname_ref))
    nlay, nrow, ncol = heads.shape
    qxb, qyb, qzb = qxqyqz(fpth, nlay, nrow, ncol)
    
    fpth = os.path.join(sim.simpath, "{}.hds".format(mname_left))
    hds = flopy.utils.HeadFile(fpth)
    heads_left = hds.get_data()
    fpth = os.path.join(sim.simpath, "{}.cbc".format(mname_left))
    nlay, nrow, ncol = heads_left.shape
    qxb_left, qyb_left, qzb_left = qxqyqz(fpth, nlay, nrow, ncol)
    
    fpth = os.path.join(sim.simpath, "{}.hds".format(mname_right))
    hds = flopy.utils.HeadFile(fpth)
    heads_right = hds.get_data()    
    heads_2models = np.append(heads_left[0],heads_right[0], axis=1)
    
    maxdiff = np.amax(abs(heads - heads_2models))
    assert maxdiff < 10*hclose_check, "Max. head diff. {} should \
                     be within solver tolerance (x10): {}" \
                     .format(maxdiff, 10*hclose_check)

    maxdiff = np.amax(abs(qxb[:,:,0:5] - qxb_left))
    assert maxdiff < 10*hclose_check, "Max. diff. in spec. discharge (x) {} \
                     should be within solver tolerance (x10): {}" \
                     .format(maxdiff, 10*hclose_check)
                    
    print(qyb[0,0:5,4])
    print(qyb_left[0,0:5,4])
    maxdiff = np.amax(abs(qyb[:,:,0:5] - qyb_left))
    assert maxdiff < 10*hclose_check, "Max. diff. in spec. discharge (y) {} \
                     should be within solver tolerance (x10): {}" \
                     .format(maxdiff, 10*hclose_check)
                     
    maxdiff = np.amax(abs(qzb[:,:,0:5] - qzb_left))
    assert maxdiff < 10*hclose_check, "Max. diff. in spec. discharge (z) {} \
                     should be within solver tolerance (x10): {}" \
                     .format(maxdiff, 10*hclose_check)

    return


# - No need to change any code below
def test_mf6model():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        yield test.run_mf6, Simulation(dir, exfunc=compare_to_ref, idxsim=idx)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        sim = Simulation(dir, exfunc=compare_to_ref, idxsim=idx)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print("standalone run of {}".format(os.path.basename(__file__)))

    # run main routine
    main()
