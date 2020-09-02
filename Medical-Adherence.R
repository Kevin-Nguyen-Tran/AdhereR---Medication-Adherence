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
  geom_bar(aes(fill = CATEGORY)) +
  labs(x = "Medication Type", 
       y = "Count",
       title = "Frequency of Medication Type",
       caption = "Source: https://cran.r-project.org/web/packages/AdhereR") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
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


#After analyzing the CMA0, we are under the assumption that the adherence (quality of implementation) is at question
# We will run further CMAs to deduce the quality out of 100%.
# We will use CMA6 
cma6 <- CMA6(data=patient.11.89, # we're estimating CMA6 now!
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             carry.only.for.same.medication=FALSE, # Carry over should always happen irrespective of what medication is supplied
             consider.dosage.change=FALSE,
             followup.window.start=0, observation.window.start=250, 
             observation.window.duration=365,
             date.format="%m/%d/%Y");
plot(cma6,patients.to.plot=c("11"), show.legend=FALSE) #23.9% Adherence
plot(cma6,patients.to.plot=c("89"), show.legend=FALSE) #45.2% Adherence
# Based on the CMA6 taking to account for the first and last medical event, the medical adherence for patient 11 is 23.9%
# and for patient 89 is 45.2%.
# The reason for the low percentage of adherence is due to the large gaps between each medication event.

# Did not consider using CMAs 1-4 due to major limitation of not taking timing of events to account
# CMA5 ignores the last event of the OW as does CMA1
# CMA7 accounts for gap between start of OW and first medication event. However, our OW does not have that issue. It is predicted that both CMAs should result in smilar adherence
# CMA8 Is for involving ongoing treatment. Our plot does not have that issue
# CMA9 is for longitudinal studies which ours is not.
# Therefore, CMA6 is the best choice for our adherence check.

cma7 <- CMA7(data=patient.11.89, # we're estimating CMA7 now!
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             carry.only.for.same.medication=FALSE,
             consider.dosage.change=FALSE,
             followup.window.start=0, observation.window.start=250, 
             observation.window.duration=365,
             date.format="%m/%d/%Y");
plot(cma7, patients.to.plot=c("11"), show.legend=FALSE) #23%
plot(cma7, patients.to.plot=c("89"), show.legend=FALSE) #43.8

# As shown above, since our OW does not have a gap between the start of the OW and first medication event, our adherence % is roughly identical

cma9 <- CMA9(data=patient.11.89, # we're estimating CMA9 now!
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             carry.only.for.same.medication=FALSE,
             consider.dosage.change=FALSE,
             followup.window.start=0, observation.window.start=250, 
             observation.window.duration=365,
             date.format="%m/%d/%Y");
plot(cma9, patients.to.plot=c("11"), show.legend=FALSE) #18.8%
plot(cma9, patients.to.plot=c("89"), show.legend=FALSE) #38.3%

cmaE <- CMA_per_episode(CMA="CMA6", # apply the simple CMA9 to each treatment episode
                        data=patient.11.89,
                        ID.colname="PATIENT_ID",
                        event.date.colname="DATE",
                        event.duration.colname="DURATION",
                        event.daily.dose.colname="PERDAY",
                        medication.class.colname="CATEGORY",
                        carryover.within.obs.window = TRUE,
                        carry.only.for.same.medication = FALSE,
                        consider.dosage.change = FALSE, # conditions on treatment episodes
                        medication.change.means.new.treatment.episode = TRUE,
                        maximum.permissible.gap = 180,
                        maximum.permissible.gap.unit = "days",
                        followup.window.start=0,
                        followup.window.start.unit = "days",
                        followup.window.duration = 365 * 2,
                        followup.window.duration.unit = "days",
                        observation.window.start=0,
                        observation.window.start.unit = "days",
                        observation.window.duration=365*2,
                        observation.window.duration.unit = "days",
                        date.format="%m/%d/%Y",
                        parallel.backend="none",
                        parallel.threads=1)
cmaE$CMA #To get summary results for each patient
plot(cmaE, patients.to.plot=c("11"), show.legend=FALSE)
plot(cmaE, patients.to.plot=c("89"), show.legend=FALSE)
# Since the permissible gap (determining when the next treatment episode is) is 6 months, each gap between each treatment is at least 6 months long or a transition between MedA and medB
# An assumption: minimum of 6 months needs to pass after the end of a medication supply (taken as prescribed) to be reasonably confident that the pt has discontinued tx

cmaW <- CMA_sliding_window(CMA.to.apply="CMA6", # apply the simple CMA9 to each sliding window
                           data=patient.11.89,
                           ID.colname="PATIENT_ID",
                           event.date.colname="DATE",
                           event.duration.colname="DURATION",
                           event.daily.dose.colname="PERDAY",
                           medication.class.colname="CATEGORY",
                           carry.only.for.same.medication=FALSE,
                           consider.dosage.change=FALSE,
                           followup.window.start=0,
                           observation.window.start=0,
                           observation.window.duration=365*2,
                           sliding.window.start=0, # sliding windows definition
                           sliding.window.start.unit="days",
                           sliding.window.duration=120,
                           sliding.window.duration.unit="days",
                           sliding.window.step.duration=30, # Can change the stacking of the CMA estimations with this number!
                           sliding.window.step.unit="days",
                           date.format="%m/%d/%Y",
                           parallel.backend="none",
                           parallel.threads=1)
cmaW$CMA

plot(cmaW, patients.to.plot=c("11"), show.legend=FALSE)
plot(cmaW, patients.to.plot=c("89"), show.legend=FALSE)
# Sliding windows are good to estimate the variation of adherence during a medication episode. 
# As shown in each patient, we set the step duration to 30, which allows for a CMA estimation to be calculated every month for a 4 month duration.
# We can see that the adherence and quality of implementation swings from 27% to 100% throughout the follow up window.
# Both CMA per episode and CMA sliding windows are necessary to understand adherence per patient.


