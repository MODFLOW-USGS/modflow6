set FC=ifort
cd "%GITHUB_WORKSPACE%\modflow6"
pixi run setup -Dparallel=true builddir
pixi run build builddir
pixi run test builddir
