@echo off

if "%3" == "ifort" (
 call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"
)


rem run python script
pixi run python %1 %2 %3 %4 %5 %6
