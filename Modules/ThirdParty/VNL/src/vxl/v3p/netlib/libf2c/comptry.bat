%*
if errorlevel 1 goto nolonglong
exit 0
:nolonglong
%* -DNO_LONG_LONG
if errorlevel 1 goto nossizet
exit 0
:nossizet
%* -DNO_SSIZE_T
if errorlevel 1 goto noboth
exit 0
:noboth
%* -DNO_LONG_LONG -DNO_SSIZE_T
