import flopy


class TestFramework:
    # tell pytest this isn't a test class, don't collect it
    __test__ = False

    def build(self, build_function, idx, exdir):
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

    def run(self, sim, workspace=None):
        """
        Run the MODFLOW 6 simulation and compare to existing head file or
        appropriate MODFLOW-2005, MODFLOW-NWT, MODFLOW-USG, or MODFLOW-LGR run.

        Parameters
        ----------
        sim : Simulation object
            MODFLOW 6 autotest simulation object that runs the base and
            regression models, compares the results, and tears down the
            test if successful.
        workspace : str
            The path to the workspace where the test is run.
        """

        sim.set_model(
            sim.name if workspace is None else workspace, testModel=False
        )
        sim.run()
        sim.compare()
        if sim.exfunc is not None:
            sim.exfunc(sim)
