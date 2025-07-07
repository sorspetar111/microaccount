@echo off
rem backup-files
del /s ..\*.bak 2>nul
del /S ..\*.~* 2>nul
del /S ..\*.dcu 2>nul
del /S ..\*.drc 2>nul
del /S ..\*.map 2>nul
del /s ..\*.tmp 2>nul
rem for /f "delims=" %%i in ('dir /s /b _bak_') do echo "%%i"

rem debuggerinfos
del ..\*.map 2>nul
del ..\*.drc 2>nul
del ..\*.log 2>nul

rem BDS-File
del ..\*.identcache 2>nul
rem fastmm-file
del ..\*.ddp 2>nul

rem delete delphi7-exe
del dfmedit2.exe 2>nul

del /S ..\test_apps\*.zip

echo removing backup folders
for /f "delims=" %%i in ('dir /s /b /a:d ..\*_bak_*') do RMDIR /S /Q "%%i"
