library(readxl)
library(tidyverse)
library(vctrs)
library(XLConnect)
library(xlsx)

read_excel_allsheets <- function(filename, tibble = TRUE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

ATL_CAFR <- read_excel_allsheets("ATL_CAFR_2_18.xlsx")

`2007` <- ATL_CAFR[["2007"]]
`2008` <- ATL_CAFR[["2008"]]
`2009` <- ATL_CAFR[["2009"]]
`2010` <- ATL_CAFR[["2010"]]
`2011` <- ATL_CAFR[["2011"]]
`2012` <- ATL_CAFR[["2012"]]
`2013` <- ATL_CAFR[["2013"]]
`2014` <- ATL_CAFR[["2014"]]
`2015` <- ATL_CAFR[["2015"]]
`2016` <- ATL_CAFR[["2016"]]
`2017` <- ATL_CAFR[["2017"]]
`2018` <- ATL_CAFR[["2018"]]
`2019` <- ATL_CAFR[["2019"]]
`2020` <- ATL_CAFR[["2020"]]


ATL_CAFR_all <-
  rbind(`2007`, 
            `2008`, 
            `2009`, 
            `2010`,
            `2011`, 
            `2012`, 
            `2013`, 
            `2014`,
            `2016`, 
            `2017`, 
            `2018`,
            `2019`, 
            `2020`,
            `2015`)

ATL_CAFR_BS <- ATL_CAFR_all %>%
  rename(FS_type = 1,
         Class = 2,
         Year = 3,
         Account = 4,
         Gov_activities = 6,
         Bus_type_activities = 8,
         Comp_units = 12) %>%
    select(1:4, 6, 8, 12) %>%
    filter(FS_type == "Statement of Net Assets", 
           Account != "ASSETS", 
           Account != "LIABILITIES",
           Account != "NET ASSETS",
           Account != "NET POSITION") %>%
    filter(across(everything(), ~ !grepl('Total', .))) %>%
  pivot_longer(
    c("Gov_activities":"Comp_units"), # same as last pivot_longer
    names_to = "Activity_type", 
    values_to = "Value")

xlcFreeMemory()
options( java.parameters = "-Xmx4g" )

write.xlsx(
  ATL_CAFR_BS,
  file = "ATL CAFR BS Flat.xlsx",
  sheetName = "Sheet1")


ATL_CAFR_IS <- ATL_CAFR_all %>%
  rename(FS_type = 1,
         Class = 2,
         Year = 3,
         Functions_Programs = 4,
         Expenses = 6,
         Charges_for_services = 8,
         Operating_grants_cont_1 = 9,
         Operating_grants_cont = 10,
         Capital_grants_cont_1 = 11,
         Capital_grants_cont = 12,
         Gov_act_1 = 13,
         Gov_act = 14,
         Bus_type_act_1 = 15,
         Bus_type_act = 16,
         Comp_units = 20) %>%
  select(1:4, 6, 8:16, 20) %>%
  filter(FS_type == "Statement of Activities",
         Functions_Programs != "Component Units") %>%
  filter(across(everything(), ~ !grepl('Total', .))) %>%
  pivot_longer(
    c("Expenses":"Comp_units"),
    names_to = "Activity_type", 
    values_to = "Value") %>%
  filter(Value != "Units",
         Functions_Programs != "Component Units",
         Value != "Charges for Services",
         Value != "Capital Grants and Contributions",
         Value != "Activities",
         Value != "Expenses",
         Functions_Programs != "Net Position - beginning of period",
         Functions_Programs != "Net Position - beginning of period as restated (Note 1G",
         Functions_Programs != "Net Position - beginning of period, as restated",
         Functions_Programs != "NET POSITION - END OF PERIOD",
         Functions_Programs != "NET POSITION- END OF PERIOD",
         Functions_Programs != "NET POSITION- END OF PERIOD $",
         Functions_Programs != "Transfers",
         Functions_Programs != "transfers",
         Functions_Programs != "Change in net assets",
         Functions_Programs != "Net assets - beginning of period, as previously stated",
         Functions_Programs != "Net assets - beginning of period, as restated",
         Functions_Programs != "NET ASSETS - END OF PERIOD",
         Functions_Programs != "NET ASSETS - END OF PERIOD",
         Functions_Programs != "Correction of prior year errors",
         Functions_Programs != "Extraordinary Loss (Note III-C",
         Functions_Programs != "Net assets - beginning of period",
         Functions_Programs != "Net Position - beginning of period as adjusted (Note 1G",
         Functions_Programs != "Net Position - beginning of period, as restated (Note I.G.",
         Functions_Programs != "Change in net position",
         Functions_Programs != "(Note I.G.",
         Functions_Programs != "Net Position (Deficit - beginning of period",
         Functions_Programs != "PERIOD",
         Functions_Programs != "NET POSITION (DEFICIT - END OF PERIOD") %>% 
  distinct()

xlcFreeMemory()
options( java.parameters = "-Xmx4g" )

write.xlsx(
  ATL_CAFR_IS,
  file = "ATL CAFR IS Flat.xlsx",
  sheetName = "Sheet1")
