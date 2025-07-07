@ECHO off
CALL _backup
DEL dfmedit_*.zip
pause
CALL _compile
pause
CALL _pack