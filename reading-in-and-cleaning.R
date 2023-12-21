# Setting working directory as the inputs for R folder where all metrics are saved,
# makes it easier to read in the relevant metrics without long filepaths
# can check working directory using 'getwd()'.
## if an error in setwd() occurs, skip this line when running the code and should still be fine 
### note: (could add an if else function here - if wd is as below, ignore the line of code, if not then set the wd as given below)

setwd("./Metrics/inputs for R")

# loading relevant packages
# if not already installed, can do so with: install.packages("tidyverse") etc 

library(tidyverse)
library(unpivotr)
library(tidyxl)
library(readxl)
library(openxlsx) 
library(dplyr)

# ---------- Healthcare acquired infection rates and antimicrobial resistance -----------------------------

#loading in HCAI rates and antimicrobial resistance csv files (all fingertips data)
HCAI_Ecoli <- read_csv("04_HCAI_E-coli.csv")
HCAI_MSSA <- read_csv("05_HCAI_MSSA.csv")
HCAI_Klebsiella <- read_csv("06_HCAI_Klebsiella spp.csv")
HCAI_P.aeruginosa <- read_csv("07_HCAI_P.aeruginosa.csv")
AMR_total_prescribing <- read_csv("10_Antimicrobial resistance total prescribing.csv")
AMR_broad_spectrum <- read_csv("11_Antimicrobial resistance broad spectrum.csv")

#cleaning the data - starting by selecting columns to keep using select() function 

HCAI_Ecoli <- HCAI_Ecoli %>% select(`Time period`, Value, Count)
HCAI_MSSA <- HCAI_MSSA %>% select(`Time period`, Value, Count)
HCAI_Klebsiella <- HCAI_Klebsiella %>% select(`Time period`, Value, Count)
HCAI_P.aeruginosa <- HCAI_P.aeruginosa %>% select(`Time period`, Value, Count)


#filtering so only one value for each quarter for AMR datasets (only keeping NAs in 'Category' and then removing duplicates)
AMR_broad_spectrum <- AMR_broad_spectrum[is.na(AMR_broad_spectrum$Category), ]
AMR_broad_spectrum <- AMR_broad_spectrum %>%
  distinct(`Time period`, .keep_all = TRUE)

AMR_total_prescribing <- AMR_total_prescribing[is.na(AMR_total_prescribing$Category), ]
AMR_total_prescribing <- AMR_total_prescribing %>%
  distinct(`Time period`, .keep_all = TRUE)


#Removing the 'Category' variable as no longer needed
AMR_total_prescribing <- AMR_total_prescribing %>% select(`Time period`, Value, Count)
AMR_broad_spectrum <- AMR_broad_spectrum %>% select(`Time period`, Value, Count)




#---- Other csv files (patient safety incidents, deaths from VTE, hip fractures, emergency readmissions) ---

#loading in the relevant files
PS_incidents <- read_csv("01_patient safety incidents.csv")
VTE_deaths <- read_csv("12_deaths from VTE.csv")
inpatient_hip_fractures <- read_csv("13_inpatient hip fractures.csv")
emergency_readmissions <- read_csv("16_emergency readmissions.csv")





# cleaning patient safety incidents data:

#delete first 3 rows and the end column
PS_incidents <- PS_incidents[-c(1:3),-3]

#renaming the column names to Quarter and England

#first create list of names
PS_incident_colnames <- list("Quarter", "Total incidents")

#then assigning the list as the header names
names(PS_incidents) <- PS_incident_colnames

# need to delete bottom 3 rows (remaining NA values and Total)

# removing any rows with a cell containing NA using na.omit()
PS_incidents <- na.omit(PS_incidents)

#remove final row (as we don't need the grand total)
PS_incidents <- head(PS_incidents, -1) #NOTE - 
#choosing this way rather than deleting 
#a specified row number to try and 
#automate for future versions where extra
#rows are added each time





#cleaning deaths from VTE data
# selecting columns to keep
VTE_deaths <- VTE_deaths %>% select(Year,`Indicator value`)

#reversing the order of rows to go instead from earliest to latest year
VTE_deaths <- VTE_deaths[order(nrow(VTE_deaths):1),]






#cleaning inpatient hip fractures data

# selecting columns to keep
inpatient_hip_fractures <- inpatient_hip_fractures %>% select(`Trust Discharge Year & Month`, 
                                                              `Inpatient fractures % (annual)`)

#removing NA columns
inpatient_hip_fractures <- na.omit(inpatient_hip_fractures)






#cleaning emergency readmissions data

#filter for England and select columns to keep
emergency_readmissions <- emergency_readmissions %>% 
  filter(Level=="England") %>% 
  select(Year, `Indicator value`)

#reverse order of rows to go from earliest to latest year
emergency_readmissions <- emergency_readmissions[order(nrow(emergency_readmissions):1),]



# csv files are now clean and ready for exporting to an excel master spreadsheet





#------- NEXT TO DO EXCEL FILES ----------------




#bed occupancy

bed_occupancy <- read_xlsx("17_bed occupancy.xlsx", sheet = "Timeseries all acute trusts")

#delete first 10 rows
bed_occupancy <- bed_occupancy[-c(1:10),]

#renaming columns as the values in first row of data
#copying the first row to be the names of the headers
names(bed_occupancy) <- bed_occupancy[1,]

#deleting the first row (as this is now the header)
bed_occupancy <- bed_occupancy[-1,]

#deleting all irrelevant columns (now keeping the month, beds occupied, beds available and the rate)
bed_occupancy <- bed_occupancy %>% select(Month, `G&A beds available`, `G&A beds occupied`, `G&A occupancy rate`)

# 2 columns at the end containing NA - delete them using na.omit
bed_occupancy <- na.omit(bed_occupancy)







# avoidable mortality
#READ IN DATA
avoidable_mortality <- read_xlsx("15_avoidable mortality.xlsx", sheet = "Table_1")

#delete first 4 rows and just keep first 3 columns
avoidable_mortality <- avoidable_mortality[-c(1:4), 1:3]

#rename headers to the values in 1st row
names(avoidable_mortality) <- avoidable_mortality[1,]

#deleting the first row (as this is now the header)
avoidable_mortality <- avoidable_mortality[-1,] 


# Explanation:
# dataframe <- dataframe %>% filter(column header [==, !=, >, <,] "cell value")

# removing male and female values, keeping persons

avoidable_mortality <- avoidable_mortality %>% filter(Sex == "Persons")










#stillbirths and neonatal deaths
#read in
stillbirth_neonatal <- read_excel("22_23_stillbirth and neonatal mortality.xlsx", sheet = "1")
#deleting first 8 rows
stillbirth_neonatal <- stillbirth_neonatal[-c(1:8),]

#naming the column headers
names(stillbirth_neonatal) <- stillbirth_neonatal[1,]
#deleting the first row (as this is now the header)
stillbirth_neonatal <- stillbirth_neonatal[-1,] 

#selecting relevant columns
stillbirth_neonatal <- stillbirth_neonatal %>% select(`Year`, 
                                                      `Stillbirth rate per 1,000 births`,
                                                      `Neonatal under 28 days mortality rate`)
#reversing order of rows
stillbirth_neonatal <- stillbirth_neonatal[order(nrow(stillbirth_neonatal):1),]






# trolley waits

# read data in - reading in the range rather than full sheet solves the date format issue
# but means it isn't as automated. something to be aware of. 
#  each update, will have to increase rows from 170, to 171, etc...

trolley_waits <- read_xlsx("18_trolley waits.xlsx", sheet = "Activity", range = "B14:N172")

# selecting relevant columns
trolley_waits <- trolley_waits %>% select(`Period`, `Total Attendances`, 
                                          `Number of patients spending >4 hours from decision to admit to admission`)








# staff survey questions 
#NOTE: current approach is reading in from aggregated table that's been created in excel

staff_survey <- read_xlsx("09_staff survey safety culture questions.xlsx", sheet = "clean table") 

#naming the column headers
names(staff_survey) <- staff_survey[1,]

#deleting the first row (as this is now the header) 
#and deleting any 0 values by filtering one arbitrary column to remove 0
staff_survey <- staff_survey[-1,] %>% filter(`top priority` != 0)







#****NOTE THIS SECTION NEEDS UPDATING TO THE NEW 31D METRIC INSTEAD OF TWO WEEK WAIT*****#
#cancer waiting times
#read in data - note that range will have to increase by one row each month
cancer_waits <- read_xlsx("21_cancer waits.xlsx", sheet = "Monthly Data", range = "AR4:AV169")
#only need month and performance
cancer_waits <- cancer_waits %>% select(Monthly, `Performance (%)`)
colnames(cancer_waits)[1] <- "Month"







#vacancy rates
#read in relevant sheet in vacancy rate spreadsheet

vacancy_rates <- read_xlsx("08_NHS vacancy rates.xlsx", sheet = "Total 2018 onwards")

#delete first 65 rows (we only want the vacancy rate, not vacancy count data)
vacancy_rates <- vacancy_rates[-c(1:66),]

#filtering for grand total, acute total, ambulance total (latter two have been calculated in excel prior to being read into R)
# also filtering for region to keep the dates row
vacancy_rates <- vacancy_rates %>% filter(`NHS Vacancy Statistics` == "Grand Total" | 
                                            `NHS Vacancy Statistics` =="Acute Total" | 
                                            `NHS Vacancy Statistics` =="Ambulance Total" |
                                            `NHS Vacancy Statistics` == "Region")

# transpose to match format of other metrics
vacancy_rates <- t(vacancy_rates)

#removing row names 
rownames(vacancy_rates) <- NULL

#naming the column headers
colnames(vacancy_rates) <- vacancy_rates[1,]
#deleting the first row (as this is now the header), and second row as is just NA values
vacancy_rates <- vacancy_rates[-c(1:2),]



#SOF data
SOF_segment <- read_xlsx("14_SOF segmentation.xlsx", sheet = "Sheet1")



# long waits
#read in data - also specifying a range to read in to make filtering columns easier - 
# lots of columns have the same headings further on in the table. 
# This also solves the dates issue for some reason & reads in the column in date format!

#NOTE: This range will only work until March 2024 - will then have to update
long_waiters <- read_xlsx("20_long waiters.xlsx", range = "B10:M215")

#filtering for just Month and %>52 weeks wait
long_waiters <- long_waiters %>% select(Month, ...12)

#naming the second column as % > 52 weeks
colnames(long_waiters)[2] <- "% > 52 weeks"

#deleting first row
long_waiters <- long_waiters[-1,]

#removing NA values 
long_waiters <- na.omit(long_waiters)



#Never events data
Never_events <- read_xlsx("24_never events.xlsx", sheet = "never events")


#Friends and family test data
FFT_data <- read_xlsx("25_FFT data.xlsx", sheet = "Combined data over time")


#Adult inpatient survey data
Adult_IP_survey <- read_xlsx("26_adult IP survey.xlsx", sheet = "Sheet1")



#CURRENTLY MISSING CODE FOR 02_03_CQC DATA AND 19_AMBULANCE RESPONSE TIMES, ambulance is below the read back into excel, and CQC is yet to be generated.
#They need to be added to the list of datasets below once they are added above this line.



#--------------------------------------------------------------------------------

# READING BACK INTO EXCEL

#creating a list of all dataframes which will then be read into an excel file

list_of_datasets <- list("01 Patient safety incidents" = PS_incidents,
                         "04 HCAI E-coli" = HCAI_Ecoli, 
                         "05 HCAI MSSA" = HCAI_MSSA, 
                         "06 HCAI Klebsiella" = HCAI_Klebsiella,
                         "07 HCAI P.aeruginosa" = HCAI_P.aeruginosa,
                         "08 Vacancy rates" = vacancy_rates,
                         "09 Staff survey" = staff_survey,
                         "10 AMR total" = AMR_total_prescribing,
                         "11 AMR broad spectrum" = AMR_broad_spectrum,
                         
                         "12 VTE deaths" = VTE_deaths,
                         "13 Hip fractures" = inpatient_hip_fractures,
                         "14 SOF segmentation" = SOF_segment,
                         "15 Avoidable mortality" = avoidable_mortality,
                         "16 Emergency readmissions" = emergency_readmissions,
                         "17 Bed occupancy" = bed_occupancy,
                         "18 Trolley waits" = trolley_waits,
                         "20 Long waiters" = long_waiters,
                         "21 Cancer waits" = cancer_waits,
                         "22 23 SB & Neonatal mort" = stillbirth_neonatal,
                         "24 Never events" = Never_events,
                         "25 Friends family test" = FFT_data,
                         "26 Adult IP survey" = Adult_IP_survey) 

#reading in the list of datasets so each metric will have it's own tab

write.xlsx(list_of_datasets, file = "inputs-master.xlsx")


#----------------------------------------------------------------------------------------------


#Metrics below need further cleaning before they can be added to master s/sheet

#ambulance waiting times - NOTE: need to figure out how to format min:sec times properly 

amb_wait_times <- read_xlsx("19_ambulance waiting times.xlsx", sheet = "Response times", range = "B5:U84")

#delete first 8 rows - want monthly data not annual
amb_wait_times<- amb_wait_times[-c(1:8),]

#need to filter for mean min:sec data for cat 1, cat 1T and cat 2
amb_wait_times <- amb_wait_times %>% select(England,
                                            ...2,
                                            `Mean (min:sec)...7`,
                                            `Mean (min:sec)...13`, 
                                            `Mean (hour: min:sec)`)

#merging year and month columns
amb_wait_times <- amb_wait_times %>%
  unite(Year, ...2, England, sep = " ")





#for long waiters, if needed:
#capping number of decimal places for percentages - TO BE CHECKED
"% > 52 weeks" <- sprintf("%.4f", x) 

`% > 52 weeks` <- sprintf("%.4f", long_waiters)

# TO ADD BACK IN: "20 Long waiters" = long_waiters,



#--------------------------------------------------------------------------------------------
