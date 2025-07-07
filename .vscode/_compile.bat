@echo off
CALL _config
del versioninfo.rc

REM sed is a part of the win32-implementation available at http://unxutils.sf.net
type %template% |sed -e "s/%%vDot%%/%vDot%/g;s/%%vComma%%/%vComma%/g;s/%%rev%%/%rev%/g" >versioninfo.rc
IF EXIST "%delphi_brcc%" "%delphi_brcc%" versioninfo.rc
IF EXIST "%delphi_dcc%" GOTO compile_dfmedit
GOTO end

:compile_dfmedit
echo %dcc_cmd%
%dcc_cmd% -LU%package% dfmedit.dpr
%dcc_cmd% dfmedit_noPackage.dpr
:end
