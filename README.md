# AdhereR - Medication Adherence
Project #2: Medical Adherence using sample data from AdhereR package: med.events

## Prerequisites
```{r setup}
rm(list = ls()) #removes all variables stored previously in Environment (good habit to include)

library(AdhereR) #Run R package for medical adherence analysis
library(tidyverse) #Run tidyverse, if you do not have: install.packages("tidyverse")
library(lubridate) #to modify date-time entries, if you do not have it: install.packages("lubridate")
library(readxl) #allows to read in excel documents as a data set

#read the data set into RStudio and stored into object
medical_adherence_data <- read_excel("medical.adherence.data.xlsx")

data <- as_tibble(medical_adherence_data) # hypothetical data set for medical adherence as a tibble to use with tidyverse.
```

## Abstract

This analysis deals with exploring medication adherence from the hypothetical data set included in the AdhereR package in RStudio. The exploration of medication adherence begins with ensuring that both medication A (medA) and medication B (medB) were required to be taken by each patient. Once confirmed, we will narrow our focus onto two patients and dive into a general overview of the medication events per patient. We will analyze their adherence which will be represented as a percentage. The data represented in this analysis can be found on <https://cran.r-project.org/web/packages/AdhereR/vignettes/AdhereR-overview.html> as well as definitions to the terminology used within this analysis.

**The data analysis will be broken down by the following sections:**

* Medication A and Medication B required for every patient?

* Medication Adherence of Patient 11 and Patient 89

* Medication Adherence Calculations

* CMA Estimates per Medication Episode

* CMA Estimates - Sliding Window