@echo off
setlocal ENABLEDELAYEDEXPANSION

echo === TrendR Runner ===

REM Ensure working directory is script location
cd /d "%~dp0"

REM Check for Rscript
where Rscript >nul 2>nul
if %ERRORLEVEL% NEQ 0 (
  echo ERROR: Rscript not found in PATH. Install R from https://cran.r-project.org/
  exit /b 1
)

REM Check for Python
where python >nul 2>nul
if %ERRORLEVEL% NEQ 0 (
  echo WARN: python not found in PATH. Python steps will be skipped.
) else (
  REM Setup Python venv if not exists
  if not exist pmods\.venv (
    echo Creating Python virtual environment...
    python -m venv pmods\.venv
    if %ERRORLEVEL% NEQ 0 (
      echo WARN: Failed to create venv. Continuing with system Python.
    )
  )
  if exist pmods\.venv\Scripts\activate.bat (
    call pmods\.venv\Scripts\activate.bat
    python -m pip install --upgrade pip >nul 2>nul
    echo Installing Python requirements...
    pip install -r pmods\requirements.txt
  )
)

REM Run R unified main orchestrator
Rscript R\main.R
set EXITCODE=%ERRORLEVEL%

if NOT "%VIRTUAL_ENV%"=="" (
  call deactivate
)

if %EXITCODE% NEQ 0 (
  echo ERROR: Pipeline failed with exit code %EXITCODE%.
  exit /b %EXITCODE%
)

echo Pipeline completed successfully.
endlocal
exit /b 0