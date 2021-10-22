@echo off

if "%4" == "ifort" (
 call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"
)


rem run python script
%1 %2 %3 %4 %5 %6 %7
