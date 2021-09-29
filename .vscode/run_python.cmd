@echo off

if "%3" == "ifort" (
 call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"
)


call C:\Users\hofer_jn\Anaconda3\condabin\conda.bat activate modflow

rem run python script
%1 %2 %3 %4
