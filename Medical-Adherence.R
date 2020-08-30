#1) Loading packages, reading in data set, and reformatting.----------------------------------------------------------------------------------------------------------

rm(list = ls()) #removes all variables stored previously in Environment (good habit to include)

library(AdhereR) #Run R package for medical adherence analysis
library(tidyverse) #Run tidyverse, if you do not have: install.packages("tidyverse")
library(lubridate) #to modify date-time entries, if you do not have it: install.packages("lubridate")
library(scales) #allows to modify scientific notation for values

#read the data set into RStudio and stored into object
medical_adherence_data <- read_excel("medical.adherence.data.xlsx")

data <- as_tibble(medical_adherence_data) # hypothetical data set for medical adherence.

#We will filter the data set to analyze the medical adherence of patient 11 and patient 89.
patient.11.89 <- data %>%
  filter(PATIENT_ID == "11" | PATIENT_ID == "89")

