library(readxl)
library(tidyverse)
library(vctrs)
library(XLConnect)
library(xlsx)

DC_pop_a <- read_excel("DC Pop Source.xlsx") %>%
  filter(str_detect(`Rank 5`, 'years|$|White|Male|Female|Some other race|
                    Native Hawaiian|Asian|Black|Indian|') | 
           str_detect(`Rank 4`, 'years|$|White|Male|Female|Some other race|
                    Native Hawaiian|Asian|Black|Indian|')) %>%
  filter(Value != "(X)",
         `Rank 4` != "18 years and over",
         `Rank 4` != "21 years and over",
         `Rank 4` != "62 years and over",
         `Rank 4` != "65 years and over") %>%
  filter(grepl('RACE|SEX AND AGE|INCOME AND BENEFITS', `Rank 3`)) %>%
  mutate(Value = as.double(Value)) %>%
  mutate(Keep = ifelse(`Rank 1` == "Demographic" & `Rank 2` %in% c("Estimate"), "Keep", "No")) %>%
  mutate(Keep1 = ifelse(`Rank 1` == "Economic" & `Rank 2` == "Estimate", "Keep", "No")) %>%
  mutate(Filter = ifelse(Keep == "Keep" | Keep1 == "Keep", "Keep", "No")) %>%
  filter(Filter == "Keep") %>%
  select(1, 3:7, 10, 11) 

DC_pop_race <- DC_pop_a %>%
  filter(`Rank 3` == "RACE",
         `Rank 5` != "Total population") %>%
  mutate(`New 4` = ifelse(`Rank 4` == "Total population", `Rank 5`, `Rank 4`)) %>%
  mutate(`New 5` = ifelse(`Rank 5` %in% c("Two or more races", "Some other race", "One race"), `Rank 6`, `Rank 5`)) %>%
  mutate(`New 6` = ifelse(`Rank 4` == "Total population", `Rank 7`, `Rank 6`)) %>%
  relocate(`New 4`, .before = `Rank 4`) %>%
  relocate(`New 5`, .before = `Rank 4`) %>% 
  relocate(`New 6`, .before = `Rank 4`) %>%
  select(1:5, 10:11) %>%  
  rename(`Rank 2` = `Rank 3`,
         `Rank 3` = `New 4`,
         `Rank 4` = `New 5`,
         `Rank 5` = `New 6`) %>%
  unite(test, `Rank 4`:`Rank 5`, remove = F) %>%
  unite(test1, `Rank 3`:`Rank 3`, remove = F) 

DC_pop_race_1 <- DC_pop_race %>%   
  filter(test != "Asian_NA",
         test != "American Indian and Alaska Native_NA",
         test != "Native Hawaiian and Other Pacific Islander_NA",
         test1 != "Two or more races_NA") %>%
  unite(test2, c(test, test1), remove = F) %>%
  mutate(`New 4.1` = ifelse(test2 == "NA_NA_One race" & Year %in% 2010:2016, "Some other race", `Rank 4`)) %>%
  group_by(`New 4.1`, Year, `Rank 1`, `Rank 2`, `Rank 3`, `Rank 5`) %>%
  summarise(Value = min(Value), test2, test) %>%
  filter(test2 != "NA_NA_Two or more races") %>%
  unite(test3, c(test2, `New 4.1`), remove = F) %>%
  filter(test3 != "NA_NA_One race_NA") %>%
  rename(`Rank 4` = `New 4.1`) %>%
  relocate(`Rank 1`, `Rank 2`, `Rank 3`, `Rank 4`, `Rank 5`, Year, Value, test, test2, test3) 

DC_pop_race_2 <- DC_pop_race_1 %>%
  select(1:7) %>%
  distinct()

diff <- DC_pop_race %>%
  filter(`Rank 4` %in% c("Asian","American Indian and Alaska Native", "Native Hawaiian and Other Pacific Islander")) %>%
  mutate(test2 = ifelse(is.na(`Rank 5`), test, `Rank 4`)) %>%
  group_by(test2, Year) %>%
  summarise(sum = sum(Value), `Rank 1`, `Rank 2`, `Rank 3`, `Rank 4`) %>%
  distinct() %>%
  pivot_wider(
    names_from = "test2", 
    values_from = "sum") %>%
  mutate(`American Indian and Alaska Native - Other` = `American Indian and Alaska Native_NA` - `American Indian and Alaska Native`) %>%
  mutate(`Asian - Other` = `Asian_NA` - `Asian`) %>%
  mutate(`Native Hawaiian and Other Pacific Islander - Other` = `Native Hawaiian and Other Pacific Islander_NA` - `Native Hawaiian and Other Pacific Islander`) %>%
  pivot_longer(
    c(12:14),
    names_to = c("Rank 4.5", "Rank 5"),
    names_sep = " - ",
    values_to = "Value",
    values_transform = list(Value = as.numeric)) %>%
  filter(!is.na(Value)) %>%
  select(1:5, 13, 14) %>%
  relocate(`Rank 1`, `Rank 2`, `Rank 3`, `Rank 4`, `Rank 5`, Year, Value) 

diff_1 <- DC_pop_race %>%
  filter(`Rank 3` == "Two or more races") %>%
  mutate(test2 = ifelse(is.na(`Rank 4`), test, `Rank 3`)) %>%
  distinct() %>%
  group_by(test2, Year) %>%
  summarise(sum = sum(Value), `Rank 1`, `Rank 2`, `Rank 3`, `Rank 5`) %>%
  distinct() %>%
  pivot_wider(
    names_from = "test2", 
    values_from = "sum") %>%
  mutate(`Two or more races - Other` = `NA_NA` - `Two or more races`) %>%
  pivot_longer(
    `Two or more races - Other`,
    names_to = c("Rank 4"),
    values_to = "Value",
    values_transform = list(Value = as.numeric)) %>%
  filter(!is.na(Value)) %>%
  select(1:5, 8, 9) %>%
  relocate(`Rank 1`, `Rank 2`, `Rank 3`, `Rank 4`, `Rank 5`, Year, Value) 

DC_pop_sex_age <- DC_pop_a %>%
  filter(`Rank 3` == "SEX AND AGE") %>%
  mutate(`New 5` = ifelse(is.na(`Rank 5`), `Rank 4`, `Rank 5`)) %>%
  relocate(`New 5`, .before = `Rank 5`) %>%
  select(1, 2, 4, 6:9) %>%
  filter(!`New 5` %in% c("Total population", 
                         "Median age (years)", 
                         "Sex ratio (males per 100 females)",
                         "Under 18 years", 
                         "16 years and over", 
                         "18 years and over",
                         "21 years and over",
                         "62 years and over",
                         "65 years and over")) %>%
  mutate(`Rank 2` = ifelse(`New 5` %in% c("Male", "Female"), "SEX", "AGE")) %>%
  relocate(`Rank 2`, .before = `Rank 3`) %>%
  select(1, 2, 4:8) %>%
  rename(`Rank 3` = `New 5`,
         `Rank 4` = `Rank 6`,
         `Rank 5` = `Rank 7`) 

DC_pop_age <- DC_pop_sex_age %>%
  filter(`Rank 2` == "AGE") 

DC_pop_sex <- DC_pop_sex_age %>%
  filter(`Rank 2` == "SEX") %>%
  group_by(`Rank 3`, `Rank 4`, `Rank 5`, Year) %>%
  summarise(Value = max(Value), `Rank 1`, `Rank 2`) %>%
  distinct() %>%
  relocate(`Rank 1`:`Rank 2`, .before = `Rank 3`)

DC_pop_inc <- DC_pop_a %>%
  filter(str_detect(`Rank 3`, 'INCOME AND BENEFITS')) %>%
  mutate(`Rank 2` = "INCOME AND BENEFITS") %>%
  relocate(`Rank 2`, .before = `Rank 3`) %>%
  select(1, 2, 4:9) %>%
  mutate(`New 3` = ifelse(`Rank 4` %in% c("Total households", "Families"), `Rank 4`, NA)) %>%
  fill(`New 3`) %>%
  mutate(`New 4` = ifelse(`Rank 4` %in% c("Less than $10,000", 
                                          "$10,000 to $14,999", 
                                          "$15,000 to $24,999", 
                                          "$25,000 to $34,999", 
                                          "$35,000 to $49,999", 
                                          "$50,000 to $74,999",
                                          "$75,000 to $99,999", 
                                          "$100,000 to $149,999", 
                                          "$150,000 to $199,999", 
                                          "$200,000 or more"), `Rank 4`, `Rank 5`)) %>%
  relocate(`New 4`, .before = `Rank 4`) %>%
  relocate(`New 3`, .before = `New 4`) %>%
  select(1:4, 8:10) %>%
  filter(`New 4` %in% c("Less than $10,000", 
                        "$10,000 to $14,999", 
                        "$15,000 to $24,999", 
                        "$25,000 to $34,999", 
                        "$35,000 to $49,999", 
                        "$50,000 to $74,999",
                        "$75,000 to $99,999", 
                        "$100,000 to $149,999", 
                        "$150,000 to $199,999", 
                        "$200,000 or more")) %>%
  rename(`Rank 3` = `New 3`,
         `Rank 4` = `New 4`,
         `Rank 5` = `Rank 7`)

unique(DC_pop_inc$Year)

DC_pop_all <-
  rbind(DC_pop_sex, 
        DC_pop_age, 
        DC_pop_race_2, 
        DC_pop_inc,
        diff,
        diff_1) 


DC_pop_all_1 <- as.data.frame(DC_pop_all)


xlcFreeMemory()
options( java.parameters = "-Xmx4g" )

write.xlsx(
  DC_pop_all_1,
  file = "DC Pop Table.xlsx",
  sheetName = "Sheet1")


