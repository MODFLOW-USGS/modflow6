import os
import sys

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


def set_teardown_test():
    teardown = True
    for idx, arg in enumerate(sys.argv):
        if arg.lower() == "--keep":
            teardown = False
    return teardown


class testing_framework(object):
    def __init__(self):
        return

    def build_mf6_models(self, build_function, idx, exdir):
        """
        Build base and regression MODFLOW 6 models

        Parameters
        ----------
        build_function : function
            user defined function that builds a base model and optionally
            builds a regression model. If a regression model is not built
            then None must be returned from the function for the regression
            model.
        idx : int
            counter that corresponds to exdir entry
        exdir : str
            path to regression model files
        """
        base, regression = build_function(idx, exdir)
        base.write_simulation()
        if regression is not None:
            if isinstance(regression, flopy.mf6.MFSimulation):
                regression.write_simulation()
            else:
                regression.write_input()

    def build_mf6_models_legacy(self, build_function, idx, exdir):
        """
        Build base and regression for older MODFLOW 6 models

        Parameters
        ----------
        build_function : function
            user defined function that builds a base model and optionally
            builds a regression model. If a regression model is not built
            then None must be returned from the function for the regression
            model.
        idx : int
            counter that corresponds to exdir entry
        exdir : str
            path to regression model files
        """
        base, regression = build_function(idx, exdir)
        base.write_simulation()
        if regression is not None:
            regression.write_input()

    def run_mf6(self, sim):
        """
        Run the MODFLOW 6 simulation and compare to existing head file or
        appropriate MODFLOW-2005, MODFLOW-NWT, MODFLOW-USG, or MODFLOW-LGR run.

        Parameters
        ----------
        sim : Simulation object
            MODFLOW 6 autotest simulation object that runs the base and
            regression models, compares the results, and tears down the
            test if successful.
        """
        print(os.getcwd())
        sim.set_model(sim.name, testModel=False)
        sim.run()
        sim.compare()
        if sim.exfunc is not None:
            sim.exfunc(sim)
        sim.teardown()
