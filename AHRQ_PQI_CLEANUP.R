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
  

# Clean dataset with ICD-10 codes per PQI and description 
ahrq_pqi_cleaned_dataset<-str_subset(files, "Composite", negate=TRUE) %>%
  map(function(x) pdftools::pdf_text(x) %>%
                  .[3:length(.)] %>%
                  str_extract_all('\\s[A-Z]\\d{2,6}') %>%
                  unlist()) %>% 
                  setNames(str_remove_all(str_subset(files, "Composite", negate=TRUE),'TechSpecs/|.*/|.pdf')) %>% 
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

PQI_composite<-files %>% str_subset("Composite")%>%
  map(function(x) pdftools::pdf_text(x) %>%
     str_extract_all("PQI\\s#\\d+") %>%
     unlist()) %>%
  setNames(str_remove_all(files %>% str_subset("Composite"),'TechSpecs/|.*/|.pdf')) %>%
  map(function(x) str_replace_all(x," #","_")) %>%
  map(`length<-`,max(lengths(.))) %>%
  bind_rows() %>% pivot_longer(
    cols = starts_with("PQI"),
    names_to="Composite",
    values_to = "PQI_num"
  ) %>%
mutate(
  PQI_Composite = str_extract(Composite,"PQI_\\d{2}"),
  Composite_label = str_remove_all(Composite,"PQI_\\d{2}_")
) %>%
select(PQI_num, PQI_Composite, Composite_label)

ahrq_cleaned_w_composite<-merge(ahrq_pqi_cleaned_dataset, PQI_composite)

  
unlink(temp)
unlink(temp2)

# Saves an excel copy of the file
wb<-createWorkbook()
addWorksheet(wb,sheetName="PQIs")
writeData(wb, "PQIs", ahrq_cleaned_w_composite)
saveWorkbook(wb, paste0("ahrqPQIs_v",pqi_year,"_ICD10v",ref_file_year,".xlsx"), overwrite = TRUE)