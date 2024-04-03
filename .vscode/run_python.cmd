@echo off

if "%3" == "ifort" (
 call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"
)

call conda activate modflow6

rem run python script
python %1 %2 %3 %4 %5 %6
