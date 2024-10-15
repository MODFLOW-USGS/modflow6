cd "%GITHUB_WORKSPACE%\modflow6\autotest"
ldd ..\bin\mf6
pixi run pytest -v --durations=0 --keep-failed .failed --netcdf -k "test_netcdf" -m "%MARKERS%"
