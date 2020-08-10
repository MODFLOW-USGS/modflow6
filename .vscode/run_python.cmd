@echo off
rem there
cd %~dp0\..\pymake

rem activate "your_conda_env" for building the source code with pymake
call C:\Users\russcher\AppData\Local\Continuum\anaconda3\condabin\conda.bat activate your_conda_env

rem build with pymake
python %1 %2 %3 %4 %5

rem ... and back again (BB)
cd %~dp0