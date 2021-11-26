# Extract ICD-10 Codes associated with Prevention Quality Indicators (PQIs) from AHRQ 

## Introduction and Purpose
The [AHRQ Prevention Quality Indicators (PQIs)](https://qualityindicators.ahrq.gov/modules/pqi_resources.aspx#techspecs) are developed by the [Agency for Healthcare Research and Quality](https://www.ahrq.gov/) to identify inpatient admissions that indicate admission relating to inadequate preventative outpatient measures. Information about PQIs can be found on the AHRQ PQI resources modules in PDF format. The following script was developed in R to extract PQIs from the PDFs and merge it with the most up to date ICD-10 classifications to create an dataset for analysis purposes. The most recent output of the script is saved as an .xlsx file. 
