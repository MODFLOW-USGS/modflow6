import os

try:
    import pymake
except:
    msg = "Error. Pymake package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install https://github.com/modflowpy/pymake/zipball/master"
    raise Exception(msg)

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)


def running_on_CI():
    return "TRAVIS" in os.environ or "CI" in os.environ


class testing_framework(object):
    def __init__(self):
        return

    def run_mf6(self, sim):
        """
        Run the MODFLOW 6 simulation and compare to existing head file or
        appropriate MODFLOW-2005, MODFLOW-NWT, MODFLOW-USG, or MODFLOW-LGR run.

        """
        print(os.getcwd())
        sim.set_model(sim.name, testModel=False)
        sim.run()
        sim.compare()
        if sim.exfunc is not None:
            sim.exfunc(sim)
        sim.teardown()
