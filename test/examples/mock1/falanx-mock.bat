@ECHO OFF

del %~dp0\falanx-args.txt

for %%x in (%*) do (
   echo %%~x>> %~dp0\falanx-args.txt
)
