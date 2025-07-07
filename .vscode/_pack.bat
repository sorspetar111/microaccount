@echo off
CALL _del
CALL _config

IF NOT "%verstate%"=="b" SET extrafiles=*.dpl
ECHO.

REM source-package
"%_7za%" u -r -tzip -x!*.ftp -x!*.zip -x!*.php -x!*.exe -x!*.ini -x!*.svn* -x!thumbs.db -x!test_apps\*.* -x!test_dfm\*.* -x!7-zip\*.* -x!backup\*.* -x!*.dpl %dfmedit_src% *.* dcu\
"%_7za%" u %dfmedit_src% testpackage.zip VTWrapper.zip
REM binary-package
"%_7za%" u -tzip -x!7-zip\*.* -x!*.svn* -x!thumbs.db %dfmedit_bin% dfmedit.exe lazres.exe *.dll *.lng *.txt *.obj *.bmp testpackage.zip VTWrapper.zip _debugmode.bat %extrafiles%
"%_7za%" u -r -tzip -x!*.svn* -x!thumbs.db %dfmedit_bin% help\*.*

IF EXIST dfmedit_nopackage.exe GOTO nopackage
GOTO testpackages
:nopackage

REN dfmedit.exe dfmedit_p.exe
REN dfmedit_nopackage.exe dfmedit.exe
"%_7za%" u -tzip -x!7-zip\*.* -x!*.svn* -x!thumbs.db %dfmedit_nop% dfmedit.exe lazres.exe *.dll *.lng *.txt *.obj *.bmp _debugmode.bat
"%_7za%" u -r -tzip -x!*.svn* -x!thumbs.db %dfmedit_nop% help\*.*
REN dfmedit.exe dfmedit_nopackage.exe
REN dfmedit_p.exe dfmedit.exe

:testpackages
IF EXIST test_apps\ "%_7za%" u -r -tzip -x!*.svn* dfmedit_testapps.zip test_apps\
IF EXIST ..\test_apps\ "%_7za%" u -r -tzip -x!*.svn* dfmedit_testapps.zip ..\test_apps\

IF EXIST test_dfm\ "%_7za%" u -r -tzip -x!*.svn* dfmedit_testdfm.zip test_dfm\
IF EXIST ..\test_dfm\ "%_7za%" u -r -tzip -x!*.svn* dfmedit_testdfm.zip ..\test_dfm\
ECHO.
pause