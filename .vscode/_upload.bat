@ECHO off
call _config.bat
SET file=dfmedit.ftp

%~d0
CD %~p0
SET /p ftp_user=username: 
SET /p ftp_password=password (not hidden): 
CLS
IF "%ftp_user%" =="" GOTO pwerror
IF "%ftp_password%" =="" GOTO pwerror
(
ECHO open www.fw-web.de
ECHO %ftp_user%
ECHO %ftp_password%
ECHO binary
ECHO cd httpdocs/dfmedit

ECHO put changelog.txt
ECHO put todo.txt

REM ~ ECHO cd help
REM ~ ECHO lcd help
REM ~ ECHO mput *.htm

REM ~ ECHO cd images
REM ~ ECHO lcd images
REM ~ ECHO mput *.gif

REM ~ ECHO cd en
REM ~ ECHO lcd en
REM ~ ECHO mput *.gif

REM how to upload folders??
REM ECHO mput help\

ECHO cd /httpdocs/download
ECHO put %dfmedit_bin%
ECHO put %dfmedit_nop%
ECHO put %dfmedit_src%

ECHO quit
)>%file%
ftp -s:%file%
DEL %file% 
GOTO end
:pwerror
ECHO username/password empty
GOTO end

:end
PAUSE
