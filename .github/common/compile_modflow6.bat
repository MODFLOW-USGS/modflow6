set FC=ifort
cd "%GITHUB_WORKSPACE%\modflow6"
pixi run setup-release-parallel builddir
pixi run install-build builddir
pixi run test-build builddir
