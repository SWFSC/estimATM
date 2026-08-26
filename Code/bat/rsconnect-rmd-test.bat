@echo off
:: Set the path to your Rscript.exe executable
SET RSCRIPT_PATH="C:\Program Files\R\R-4.6.0\bin\x64\Rscript.exe"

:: Set variables for your specific file and server info
SET CONNECT_SERVER="https://test-connect.fisheries.noaa.gov/"
SET CONNECT_API_KEY="secret-code"
SET RMD_DIRECTORY="C:\KLS\CODE\Github\estimATM\2606RL\Doc"
SET RMD_FILENAME="plotSurvey.Rmd"
SET APP_TITLE="plotSurvey-2606RL"

echo Starting RMarkdown deployment to Posit Connect...

%RSCRIPT_PATH% -e "rsconnect::addServer(server='%CONNECT_SERVER%', name='my_connect_server'); rsconnect::connectApiUser(server='my_connect_server', apiKey='%CONNECT_API_KEY%'); rsconnect::deployDoc(doc='%RMD_DIRECTORY%/%RMD_FILENAME%', appTitle='%APP_TITLE%', server='my_connect_server')"

if %ERRORLEVEL% EQU 0 (
  echo Deployment completed successfully!
) else (
  echo Deployment failed with error code %ERRORLEVEL%.
)

pause
