cd "%GITHUB_WORKSPACE%\modflow6\autotest"
where libpetsc.dll
ldd ..\bin\mf6
micromamba run -r "C:\Users\runneradmin\micromamba" -n modflow6 pytest -v -n auto --parallel -k "test_par" --durations 0 --keep-failed .failed