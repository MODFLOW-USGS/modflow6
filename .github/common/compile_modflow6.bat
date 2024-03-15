set FC=ifort
cd "%GITHUB_WORKSPACE%\modflow6"
micromamba run -r "C:\Users\runneradmin\micromamba" -n modflow6 meson setup builddir -Ddebug=false -Dparallel=true --prefix=%CD% --libdir=bin
micromamba run -r "C:\Users\runneradmin\micromamba" -n modflow6 meson install -C builddir
micromamba run -r "C:\Users\runneradmin\micromamba" -n modflow6 meson test --verbose --no-rebuild -C builddir