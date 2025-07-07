REM SET verState=b
SET vDot=0.4.0.0
SET vComma=%vDot:.=,%

REM next line is autofilled by svn-client
set REVISION=$LastChangedRevision: 55 $
set REVISION=$Rev: 55 $
REM set rev=%REVISION:~11,-2%
set rev=%REVISION:~6,-2%

set template=v_template.rc

SET delphi_dir=D:\programmierung\delphi3\
set package="vcl30"

SET delphi_dcc=%delphi_dir%bin\dcc32.exe
SET delphi_brcc=%delphi_dir%bin\brcc32.exe
Set curdir=%~dp0

set dcc_cmd="%delphi_dcc%" "-U%curdir%;%delphi_dir%lib;%delphi_dir%slib"

SET _7za=D:\System\7-zip\7za.exe
IF NOT EXIST "%_7za%" SET _7za=%~d0\7-zip\7za.exe
IF NOT EXIST "%_7za%" SET _7za=..\7za.exe

set dfmedit_bin=dfmedit_%vDot%%verstate%.zip
set dfmedit_nop=dfmedit_%vDot%%verstate%_nopackage.zip
set dfmedit_src=dfmedit_%vDot%%verstate%_src.zip
