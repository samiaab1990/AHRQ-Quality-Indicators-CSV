# Libraries 
library(xml2)
library(rvest)
library(pdftools)
library(tesseract)
library(tidyverse)
library(janitor)
library(openxlsx)

# For PQIs
## Pulls most recent PQI zipfile from when script is run
main_pg<-"https://qualityindicators.ahrq.gov/"
ahrq<-read_html('https://qualityindicators.ahrq.gov/modules/pqi_resources.aspx#techspecs')
tech_spec<-ahrq %>% html_nodes(".btn-download") %>% html_attr("href") %>%
  str_subset("TechSpec")
pqi_page<-read_html(paste0(main_pg,tech_spec))

zip_file<-paste0(main_pg,str_remove_all(pqi_page %>% html_nodes(".btn-download") %>% html_attr("href"),
                                        "^[^/]+/"))

pqi_year<-zip_file %>% str_extract('\\d{4}')

temp <- tempfile()
download.file(zip_file, temp, mode="wb")
files<-str_remove_all(unzip(temp),"^[^/]+/")

# For ICD_10 CM description
## Pulls most recent ICD-10 code file 
url2<-"https://www.hcup-us.ahrq.gov/toolssoftware/ccsr/dxccsr.jsp"
ccsr<-read_html(url2)
ref_file<-ccsr %>% html_nodes("a") %>% html_attr("href") %>% str_subset("Reference-File")

ref_file_year<-ref_file %>% str_extract('\\d{4}')

temp2<-tempfile()
download.file(paste0("https://www.hcup-us.ahrq.gov/toolssoftware/ccsr/",ref_file),temp2, mode="wb")
icd_10_code_desc<-readxl::read_excel(temp2,sheet="DX_to_CCSR_Mapping",
                                     col_names = FALSE) %>%
                                     tail(-1) %>%
                                     janitor::row_to_names(1) %>%
  select(`ICD-10-CM Code`, `ICD-10-CM Code Description`) %>%
  rename(
    `ICD-10` = `ICD-10-CM Code`,
    `ICD-10-label` = `ICD-10-CM Code Description` 
  )
  

# Cleaned dataset with ICD-10 codes per PQI and description 
ahrq_pqi_cleaned_dataset<-files %>%
  map(function(x) pdftools::pdf_text(x) %>%
                  .[3:length(.)] %>%
                  str_extract_all('\\s[A-Z]\\d{2,6}') %>%
                  unlist()) %>% 
                  setNames(str_remove_all(files,'TechSpecs/|.*/|.pdf')) %>% 
  map(`length<-`,max(lengths(.))) %>%
                  bind_rows() %>% 
  pivot_longer(cols=starts_with('PQI'),
               names_to="PQI",
               values_to="ICD-10") %>%
  filter(!is.na(`ICD-10`)) %>%
  mutate(`ICD-10` = str_remove_all(`ICD-10`,"\n"),
         PQI_num=str_extract(PQI,"PQI_\\d{2}"),
         PQI_lab=str_remove_all(PQI,"PQI_\\d{2}_")) %>%
  select(`ICD-10`, PQI_num, PQI_lab) %>%
  merge(icd_10_code_desc, by="ICD-10") 
  
unlink(temp)
unlink(temp2)

wb<-createWorkbook()
addWorksheet(wb,sheetName="PQIs")
writeData(wb, "PQIs", ahrq_pqi_cleaned_dataset)
saveWorkbook(wb, paste0("ahrqPQIs_v",pqi_year,"_ICD10v",ref_file_year,".xlsx"), overwrite = TRUE)