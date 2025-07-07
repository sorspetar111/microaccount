@ECHO off
SET datestring=%date:~6,4%%date:~3,2%%date:~0,2%
SET backupdir=backup

ECHO creating backup (%backupdir%\%datestring%\)...
IF NOT EXIST %backupdir% MD %backupdir%
IF NOT EXIST %backupdir%\%datestring%\ MD %backupdir%\%datestring%\

COPY dfmedit_*.zip backup\%datestring%\*
