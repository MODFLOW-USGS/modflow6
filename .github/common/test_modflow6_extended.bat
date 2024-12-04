cd "%GITHUB_WORKSPACE%\modflow6\autotest"
pixi run pytest -v --durations=0 --keep-failed .failed --netcdf --parallel -m "%MARKERS%"
