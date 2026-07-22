@echo off
setlocal
Rscript "%~dp0statlab" %*
exit /b %ERRORLEVEL%
