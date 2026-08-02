REM Define the location of the R and Quarto executables
SET R_PATH="C:\Program Files\R\R-4.6.0\bin\x64\Rscript.exe" 
SET Q_PATH="C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe"

REM To install pandoc on Windows, run CMD.exe then winget install JohnMacFarlane.Pandoc

REM Path to where the RMD and QMD files are located
SET WORK_DIR="C:\SURVEY\2606RL\ANALYSIS\estimATM\2606RL\Doc"

CD /d %WORK_DIR%

REM Name of plotSurvey Rmd
SET RMD_FILE="plotSurvey.Rmd"
%R_PATH% -e "rmarkdown::render('%RMD_FILE%', output_format='html_document')"
REM Open resulting file
start "" "%~dp0..\..\Doc\plotSurvey.html"

REM Name of checkTrawls Rmd
SET RMD_FILE="checkTrawls.Rmd"
%R_PATH% -e "rmarkdown::render('%RMD_FILE%', output_format='html_document')"
REM Open resulting file
start "" "%~dp0..\..\Doc\checkTrawls.html"

REM Name of plotTrawls Rmd
SET RMD_FILE="plotTrawls.Rmd"
%R_PATH% -e "rmarkdown::render('%RMD_FILE%', output_format='html_document')"
REM Open resulting file
start "" "%~dp0..\..\Doc\plotTrawls.html"

REM Name of plotCTD Rmd
SET RMD_FILE="plotCTD.Rmd"
%R_PATH% -e "rmarkdown::render('%RMD_FILE%', output_format='html_document')"
REM Open resulting file
start "" "%~dp0..\..\Doc\plotCTD.html"

REM Name of trackSurvey Qmd
SET QMD_FILE="trackSurvey.qmd"
%Q_PATH% render "%QMD_FILE%"
REM Open resulting file
start "" "%~dp0..\..\Doc\trackSurvey.html"

REM Keeps window open to show results
REM pause
