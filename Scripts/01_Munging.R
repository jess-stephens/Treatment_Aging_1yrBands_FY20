
# library(tidyverse)
# library(readxl)
# library(here)
# library(ICPIutilities)
# library(glitr)
# library(glamr)
# library(janitor)
# library(dplyr)


## Importing Data


df<-read_excel(path = "C:/Users/jStephens/Documents/GitHub/Treatment_Aging_1yrBands_FY20/Data/HIV2020Estimates_ART by single year_8Sept2020_norow.xlsx", sheet="DataList")


# df <- read_xlsx("Data/HIV2020Estimates_ART by single year_8Sept2020_norow.xlsx",
#                 sheet="DataList") 

## Check Data Import
df %>% glimpse()
glimpse(df)
View(df)


## Clean dataset: Isolate Age from E_Ind and drop unnecessary columns/variables

# Goal: the numbers for each one-year age band of people with HIV and of people on ART
# make a new variable with the the values after "Age" and before ";"
df_full_seperate <- df %>% 
  separate(E_Ind, c("A","B", "C", "D", "E"), sep = "([+;])") %>%#"B" holds the age
  separate(B, c("X", "Y", "Age_num"), sep = "([ ])")%>%
  separate(A, c("X1", "PLVHIV_or_ART", "X2"), sep = "([-])")%>%
  separate("PLVHIV_or_ART", c("PLVHIV_or_ART", "Y1"), sep = "([0])") %>% #drop 0 from PLHIV vs ART
  rename(Country=E_Count, sex=D, sex2=E)  %>%#rename country variable and vars with sex
  mutate(malefemale=ifelse(is.na(sex2), 0, 1)) %>%
  mutate(sex=ifelse(malefemale==1, "MaleFemale", sex)) %>% # not working to change sex here %>%
  mutate(Country=recode(Country, "Cote dIvoire"="Cote d'Ivoire", 
                        "Lao People Democratic Republic"="Laos",
                        "United Republic of Tanzania"= "Tanzania",
                        "Viet Nam"="Vietnam"))%>%
  select(Country, Age_num, sex, Time, Value) %>% #drop X1, X2, Y, X, C,Y1, sex2 Rounded
  view() %>%
  glimpse()

df_full_seperate$Age_num <-
  as.numeric(as.character(unlist(df_full_seperate$Age_num)))
view(df_full_seperate)
glimpse(df_full_seperate)



#############################################
# Restructure data
##############################################

## add variables for Age Categories for ACTUAL Age

# Create categories for PEPFAR age brackets
# <1
# 1-4
# 5-9
# 10-14
# 15-19
# 20-24
# 25-29
# 30-34
# 35-39
# 40-44
# 45-49
# 50+
#   
#   
#   https://stackoverflow.com/questions/12979456/r-code-to-categorize-age-into-group-bins-breaks 


#find max
range(df_full_seperate$Age_num)
#0-80
library(data.table)
PEPFARagebreaks19 <- c(0,1,5,10,15,20,25,30,35,40,45,50,85)
PEPFARagelabels19 <- c("<01","01--04", "05--09","10--14","15-19","20-24","25-29","30-34",
                       "35-39","40-44","45-49","50+")
df_full_PEPFARage2019 <- setDT(df_full_seperate) [ , ACTUAL_Age_cat_SemiFine := cut(Age_num, 
                                                                                    breaks = PEPFARagebreaks19, 
                                                                                    right = FALSE, 
                                                                                    labels = PEPFARagelabels19)]
view(df_full_PEPFARage2019)
glimpse(df_full_PEPFARage2019)
names(df_full_PEPFARage2019)





# Figure out predicted ages 
#ex, 2017 ages if 2016 aged w/o other loss/gains


df_full_PREDICTagetime <- df_full_PEPFARage2019 %>%
  mutate(Age_predict=Age_num+1, Time_predict=Time+1)
view(df_full_PREDICTagetime)
df_full_PREDICTagegroups <- setDT(df_full_PREDICTagetime) [ , PREDICT_Age_cat_SemiFine := cut(Age_predict, 
                                                                                              breaks = PEPFARagebreaks19, 
                                                                                              right = FALSE, 
                                                                                              labels = PEPFARagelabels19)]
view(df_full_PREDICTagegroups)
glimpse(df_full_PREDICTagegroups)
#Rows: 119,280
#Columns: 11
names(df_full_PREDICTagegroups)




 ############################################################################## -->
 # Multiple possible data formats, see archive for 1 & 2 (Rmd files) -->
 # 1) long with disaggregate (<15 no disaggregate) -->
 # 2) long without sex disaggregate -->
 # 3) long with all data (male, female and malefemale) - can be disaggregated later -->
 
  
  
  
  ###########################################################
######    ALL DATA (format 3) ######
###########################################################



## Group by Age Groups and Country and sex
#actual and predicted separately
#merge actual and predicted

#insert the df_long_sexdisagg
#actual
df_long_Grouped_Actual_SemiFine_disagg<- df_full_PREDICTagegroups %>%
  group_by(Country, sex, Time, ACTUAL_Age_cat_SemiFine) %>%
  summarise(Total_Value_Actual=sum(Value))
glimpse(df_long_Grouped_Actual_SemiFine_disagg)
view(df_long_Grouped_Actual_SemiFine_disagg)
#predict
df_long_Grouped_Predict_SemiFine_disagg<- df_full_PREDICTagegroups %>%
  group_by(Country, sex, Time_predict, PREDICT_Age_cat_SemiFine) %>%
  summarise(Total_Value_Predict=sum(Value))
glimpse(df_long_Grouped_Predict_SemiFine_disagg)
view(df_long_Grouped_Predict_SemiFine_disagg)
#rename predict variables to match actual so they can merge
df_long_Grouped_Predict_SemiFine_vars_disagg<- df_long_Grouped_Predict_SemiFine_disagg %>%
  rename(Time=Time_predict, ACTUAL_Age_cat_SemiFine=PREDICT_Age_cat_SemiFine)
glimpse(df_long_Grouped_Predict_SemiFine_vars_disagg)
view(df_long_Grouped_Predict_SemiFine_vars_disagg)
#merge back together
df_long_Grouped_SemiFine_disagg <- df_long_Grouped_Actual_SemiFine_disagg %>% 
  left_join(df_long_Grouped_Predict_SemiFine_vars_disagg, by=c("Time"="Time", "ACTUAL_Age_cat_SemiFine"="ACTUAL_Age_cat_SemiFine","Country"="Country", "sex"="sex"))
view(df_long_Grouped_SemiFine_disagg)
# 
# write_csv(df_long_Grouped_SemiFine_disagg,"C:/Users/jesse/OneDrive/Documents/Work Material/USAID/Treatment/Data/Output/Spectrum_long_grouped_20200914.csv", na="")







## look at difference between actual ages - aging in/out actual loss or growth 
#ex actual 2019 / actual 2018

##calc predicted % difference
#predicted 2019/actual 2018

##Compare actual to predicted - expected impact based on aging


## BK's solution
## Note: Make sure to install `janitor` and `dplyr` version >= 1.0.0
## This version calculates the perc for all years
df_perc_disagg<- df_long_Grouped_SemiFine_disagg %>%
  janitor::clean_names() %>% 
  arrange( actual_age_cat_semi_fine) %>% #making year values sequential 
  group_by(country, actual_age_cat_semi_fine) %>% #dont want to confuse the countries or hiv/art obs - keep lag/lead in these groups
  mutate( total_prev_value_actual = lag(total_value_actual),
          
          perc_diff = ifelse(
            !is.na(total_value_actual) & !is.na(total_prev_value_actual), #ignore the values that are empty (na)
            total_value_actual / total_prev_value_actual*100, 
            NA
          ),
          
          perc_of_lastyr = ifelse(
            !is.na(total_value_predict) & !is.na(lag(total_value_actual)),
            total_value_predict / total_prev_value_actual * 100,
            NA
            #  ),
            #       perc_diff_Compare2= (perc_diff/perc_of_lastyr)*100
          )) %>% 
  ungroup() %>% 
  relocate(total_prev_value_actual, .after = total_value_actual)

View(df_perc_disagg)






#filter ART, time and select relevant variables



spectrum_clean_all_disagg<-df_perc_disagg%>%
  filter(time %in% c('2019')) %>%
  select(country, time, sex, actual_age_cat_semi_fine, perc_of_lastyr)%>%
  rename(year=time, age_group=actual_age_cat_semi_fine)%>%
  mutate_if(is.factor, as.character)%>%
  mutate(sex=ifelse(sex==" Female", "Female", sex)) %>%
  mutate(sex=ifelse(sex==" Male", "Male", sex)) %>%
  glimpse()
view(spectrum_clean_all_disagg)
glimpse(spectrum_clean_all_disagg)
write_csv(spectrum_clean_all_disagg,"C:/Users/jesse/OneDrive/Documents/Work Material/USAID/Treatment/Data/Output/spectrum_clean_all_total_20201123_FY20.csv", na="")





