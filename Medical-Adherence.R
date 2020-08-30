#1) Loading packages, reading in data set, and reformatting.----------------------------------------------------------------------------------------------------------

rm(list = ls()) #removes all variables stored previously in Environment (good habit to include)

library(AdhereR) #Run R package for medical adherence analysis
library(tidyverse) #Run tidyverse, if you do not have: install.packages("tidyverse")
library(lubridate) #to modify date-time entries, if you do not have it: install.packages("lubridate")
library(scales) #allows to modify scientific notation for values
library(readxl) #allows to read in excel documents as a data set

#read the data set into RStudio and stored into object
medical_adherence_data <- read_excel("medical.adherence.data.xlsx")

data <- as_tibble(medical_adherence_data) # hypothetical data set for medical adherence as a tibble to use with tidyverse.

# We can look at the frequency and see the likelihood of a patient to take both medications (MedA and MedB), if the bar chart is of
# equal frequency, we can assume equal use.
ggplot(data = data, mapping = aes(x = CATEGORY)) +
  geom_bar()
# It shows that the medication between medA and medB are relatively equal with medA having slightly more use.

#sum of each medication medA and medB.
medication_count <- data %>%
  group_by(CATEGORY) %>%
  summarise(count = n())
# from this table, we can see that medA had 549 medication events and medB had 531 medication events.

# We can now filter our data set to see if there are any patients who were not taking both medications through out the entire follow up window
medicationA_taken <- data %>%
  filter(CATEGORY == "medA")
unique(medicationA_taken$PATIENT_ID)
  
medicationB_taken <- data %>%
  filter(CATEGORY == "medB")
unique(medicationB_taken$PATIENT_ID)
# As shown above, every patient (1-100) has taken both medication throughout their entire Follow Up Window (start to finish of their prescribed medication)

# Focus on 2 patients and seeing how their FUW (Follow Up Window) are and see if we can understand/deduce any trends.
#We will filter the data set to analyze the medical adherence of patient 11 and patient 89.
patient.11.89 <- data %>%
  filter(PATIENT_ID == "11" | PATIENT_ID == "89")

# We will utilize the AdhereR package and the functions included to analyze patient 11 and patient 89
# Visualize the patient records for both patient 11 and patient 89
# We will use the CMA0 (zero), to have the general overview of the medication events.
cma0 <- CMA0(data = patient.11.89,
             ID.colname = "PATIENT_ID",
             event.date.colname = "DATE",
             event.duration.colname = "DURATION",
             event.daily.dose.colname = "PERDAY",
             medication.class.colname = "CATEGORY",
             followup.window.start = 0,
             observation.window.start = 182, #Observation window starts roughly 6 months after the start of treatment
             observation.window.duration = 365, # Observation window will be a year long from the start of the observation window as set in the above argument
             date.format = "%m/%d/%Y")

plot(cma0,
     print.dose = TRUE, plot.dose = TRUE,
     align.all.patients = TRUE,
     show.legend = FALSE)  
# The green dashed box shows the follow up window
# The yellow-tan filled box between 180-540 is the observation window (of 1 year)
# The red arrow signifies the duration of the medA and the number below it represents the daily dosage of medA.
# The blue arrow signifies the duration of the medB and the number below it represents the daily dosage of medB.
# The X axis represents a 2 year follow up window
# The Y axis represents the two patients (patient 89 and patient 11) and their medication history

# We can see in patient 89 that they consistently finished their medication supply before obtaining their next supply. However, they had an interruption
# of over 100 days between the first and second supply of medA.
# We can see in patient 11 that the patient would acquire new supply of medication before the previous supply was used. As shown in the second and third supply 
# of medA and the first and second supply of medB. We can also see that this patient had a interruption of more than 200 days between the first and second supply
# and an interruption of more than 250 days between the third and fourth supply of medA.


  
  
















