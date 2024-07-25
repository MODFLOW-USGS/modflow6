cd "%GITHUB_WORKSPACE%\modflow6\autotest"
where libpetsc.dll
ldd ..\bin\mf6
pixi run pytest -v --durations=0 --keep-failed .failed -m "%MARKERS%"
