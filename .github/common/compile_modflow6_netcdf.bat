set FC=ifort
cd "%GITHUB_WORKSPACE%\modflow6"
pixi run setup -Dnetcdf=true builddir
pixi run build builddir
