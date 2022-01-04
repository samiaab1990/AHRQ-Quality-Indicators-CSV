# Libraries 
library(xml2)
library(rvest)
library(pdftools)
library(tesseract)
library(tidyverse)
library(janitor)
library(readr)

# For PQIs
## Pulls most recent PQI zipfile from when script is run
main_pg<-"https://qualityindicators.ahrq.gov/"
pqi_resources<-"https://qualityindicators.ahrq.gov/modules/pqi_resources.aspx#techspecs"
pqi_resources_html<-read_html(pqi_resources)
tech_spec<-pqi_resources_html %>% html_nodes(".btn-download") %>% html_attr("href") %>%
  str_subset("TechSpec")
pqi_pdf_page<-read_html(paste0(main_pg,tech_spec))

zip_file<-paste0(main_pg,str_remove_all(pqi_pdf_page %>% html_nodes(".btn-download") %>% html_attr("href"),
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
download.file(paste0("https://www.hcup-us.ahrq.gov/toolssoftware/ccsr/",ref_file),temp2, mode="wb", exdir=FALSE)
icd_10_code_desc<-readxl::read_excel(temp2,sheet="DX_to_CCSR_Mapping",
                                     col_names = FALSE) %>%
                                     tail(-1) %>%
                                     janitor::row_to_names(1) %>%
  select(`ICD-10-CM Code`, `ICD-10-CM Code Description`) %>%
  rename(
    `ICD-10` = `ICD-10-CM Code`,
    `ICD-10-label` = `ICD-10-CM Code Description` 
  ) %>%
  distinct(.keep_all=TRUE)
  

# For ICD-10 PCS codes
url3<-'https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs10.jsp'
css10<-read_html(url3)
css10_recent_zip<-css10 %>% html_nodes("a") %>% html_attr("href") %>% str_subset("ccs_pr_icd10pcs") %>%
head(1)

pcs_year<-css10_recent_zip %>% str_extract('\\d{4}')

temp3<-tempfile()
download.file(paste0('https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/',css10_recent_zip), temp3, mode="wb")
icd10_pcs<-read.csv(unz(temp3, str_replace(css10_recent_zip,'.zip','.csv'))) %>%
  rename(`ICD-10-PCS` = "X.ICD.10.PCS.CODE.",
         `ICD-10-PCS-label`= "X.ICD.10.PCS.CODE.DESCRIPTION.",
         `ICD-10-PCS-category`="X.CCS.CATEGORY.DESCRIPTION.") %>%
  select(`ICD-10-PCS`, `ICD-10-PCS-label`, `ICD-10-PCS-category`) %>% 
  mutate(`ICD-10-PCS` = str_remove_all(`ICD-10-PCS`, "'"))


# For code sets 
temp4<-tempfile()
code_set_link<-read_html(paste0(main_pg, pqi_resources_html %>% html_nodes("a") %>% html_attr("href") %>% str_subset("Coding") %>% head(1))) %>%
html_nodes("a") %>% html_attr("href") %>% str_subset(".xlsx")
download.file(paste0(main_pg,str_remove_all(code_set_link,"^[.]{2}+/")), temp4, mode="wb")

diagnosis_code_set_name<-readxl::read_excel(temp4, sheet='Code set changes and content') %>% 
  filter(str_detect(`Code Set Value`,'[A-Z0-9]{3,7}'),
         !!as.symbol(paste0("Mapped Value v",pqi_year))=="1") %>%
  select(`Code Set Name`, `Code Set Value`) %>%
  rename(
    diagnosis_code_set_name = `Code Set Name`,
    `ICD-10` = `Code Set Value`
  )


# Clean dataset with ICD-10 codes per PQI and description 
ahrq_pqi_cleaned_dataset<-str_subset(files, "Composite|Appendix", negate=TRUE) %>%
  map(function(x) pdftools::pdf_text(x) %>%
                  str_extract_all('\\s[A-Z]{1}+\\d{1}+[A-Z0-9]{1,5}') %>%
                  unlist()) %>% 
                  setNames(str_remove_all(str_subset(files, "Composite|Appendix", negate=TRUE),'TechSpecs/|.*/|.pdf')) %>% 
  map(`length<-`,max(lengths(.))) %>%
                  bind_rows() %>% 
  pivot_longer(cols=starts_with('PQI'),
               names_to="PQI",
               values_to="ICD-10") %>%
  filter(!is.na(`ICD-10`)) %>%
  mutate(`ICD-10`= str_remove_all(`ICD-10`,"\n|\\s"),
         PQI_num=str_extract(PQI,"PQI_\\d{2}"),
         PQI_lab=str_remove_all(PQI,"PQI_\\d{2}_")) %>% 
  select(`ICD-10`, PQI_num, PQI_lab) %>%
  left_join(icd_10_code_desc, by="ICD-10") %>%
  left_join(diagnosis_code_set_name, by="ICD-10")
       

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
select(PQI_num, PQI_Composite, Composite_label) %>%
filter(!is.na(PQI_num))

ahrq_cleaned_w_composite<-left_join(ahrq_pqi_cleaned_dataset, PQI_composite)

icd_procedures_exclusions<-str_subset(files, "Composite", negate=TRUE) %>%
  map(function(x) pdftools::pdf_text(x) %>%
        str_extract_all('\\s[A-Z0-9]{7}')) %>%
  unlist() %>% 
  str_subset('[A-Z]{7}', negate=TRUE) %>%
  str_subset('\\d{7}', negate=TRUE) %>%
  str_remove('\n|\\s') %>% 
  str_subset(paste(ahrq_pqi_cleaned_dataset %>% select(`ICD-10`) %>% distinct() %>% pull(), collapse="|"), negate=TRUE) %>%
  tibble() %>%  
  rename(.,`ICD-10-PCS`=.) %>% 
  left_join(icd10_pcs) %>%
  left_join(diagnosis_code_set_name, by=c("ICD-10-PCS"="ICD-10"))

 
c(temp,temp2,temp3,temp4) %>%
  map(unlink)


# creates a csv copy of file
write_csv(ahrq_cleaned_w_composite, paste0("ahrqPQIs_v",pqi_year,"_ICD10v",ref_file_year,".csv"))
write_csv(icd_procedures_exclusions, paste0("ahrqPQIs_v",pqi_year,"ICD10PCSv",pcs_year,".csv"))
