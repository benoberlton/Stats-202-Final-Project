library(readr)
library(dplyr)


# Data Loading -----------------------------------------------------------------
setwd("C:/Users/benja/Desktop/classes/Stats 202/Final Project/FinalProjectData/Study_data")
Study_df <- list.files(path = 'C:/Users/benja/Desktop/classes/Stats 202/Final Project/FinalProjectData/Study_data') %>% 
  lapply(read_csv) %>% 
  bind_rows

# formating our data
# removes all E data, all failed audits, and any additional observations on a patient after the first
# introduces a VisitWeek, PositiveToTal, NegativeTotal, and GeneralTotal variable
Test_data <- Study_df %>% 
  filter(Study != "E" & LeadStatus == "Passed") %>% 
  mutate(VisitWeek = ceiling(VisitDay/7)) %>% 
  distinct(PatientID, VisitDay, .keep_all = TRUE) %>% 
  rowwise() %>% 
  mutate(Positive_Total = sum(c_across(P1:P7))) %>% 
  mutate(Negative_Total = sum(c_across(N1:N7))) %>% 
  mutate(General_Total = sum(c_across(G1:G16))) %>% 
  select(Study, Country, PatientID, SiteID, RaterID, AssessmentID, 
         TxGroup, VisitDay, VisitWeek, everything()) %>% 
  relocate(LeadStatus, .after = General_Total)
  

# Control Group ----------------------------------------------------------------

# Group containing only Control patients
Control_group = Test_data %>% 
  filter(TxGroup == "Control")

# Group containing zero day for each control patient
Day_Zero_Control_group = Control_group %>% 
  filter(VisitDay == 0)

# Group containing the last day of each control patient
Last_Day_Control_group = Control_group %>% 
  group_by(PatientID) %>% 
  top_n(1, VisitDay)

# Group containing only the last day controls in which there is a passed 0 day. 
Paired_Last_Day_Control_group = Last_Day_Control_group[(Last_Day_Control_group$PatientID %in% Day_Zero_Control_group$PatientID),] 
  
#Group containing the last day >120 (18th week) of each control patient
Filtered_Last_Day_Control_group = Last_Day_Control_group %>% 
  filter(VisitDay >= 120)

# Group containing the last day >120 controls in which there is a passed 0 day. 
Paired_Filtered_Last_Day_Control_group = Paired_Last_Day_Control_group %>% 
  filter(VisitDay >= 120)

# Group containing the zero days in which there is a last day >120.
Paired_Filtered_Day_Zero_Control_group= Day_Zero_Control_group[(Day_Zero_Control_group$PatientID %in% Paired_Filtered_Last_Day_Control_group$PatientID),]

# Treatment Group --------------------------------------------------------------

# group containing only Treatment patients
Treatment_group = Test_data %>% 
  filter(TxGroup== "Treatment")

# Group containing zero day for each treatment patient
Day_Zero_Treatment_group = Treatment_group %>% 
  filter(VisitDay == 0)

# Group containing the last day of each treatment patient
Last_Day_Treatment_group = Treatment_group %>% 
  group_by(PatientID) %>% 
  top_n(1, VisitDay)

# Group containing only the last day treatments in which there is a passed 0 day. 
Paired_Last_Day_Treatment_group = Last_Day_Treatment_group[(Last_Day_Treatment_group$PatientID %in% Day_Zero_Treatment_group$PatientID),] 

#Group containing the last day >120 days (18th week) of each treatment patient
Filtered_Last_Day_Treatment_group = Last_Day_Treatment_group %>% 
  filter(VisitDay >= 120) 

# Group containing the last day >120 treatments in which there is a passed 0 day. 
Paired_Filtered_Last_Day_Treatment_group = Paired_Last_Day_Treatment_group %>% 
  filter(VisitDay >= 120)

# Group containing the zero days in which there is a last day >120.
Paired_Filtered_Day_Zero_Treatment_group= Day_Zero_Treatment_group[(Day_Zero_Treatment_group$PatientID %in% Paired_Filtered_Last_Day_Treatment_group$PatientID),]



# Overall PANSS score test------------------------------------------------------

# independent t-test comparing mean of last day
t.test(Last_Day_Control_group$PANSS_Total, Last_Day_Treatment_group$PANSS_Total, paired = FALSE)
#(t_test, file = "test.txt")

# independent t-test comparing mean of last day >120 days
t.test(Filtered_Last_Day_Control_group$PANSS_Total, Filtered_Last_Day_Treatment_group$PANSS_Total, paired = FALSE)

# paired t-test comparing first and last day in control group
t.test(Day_Zero_Control_group$PANSS_Total, Paired_Last_Day_Control_group$PANSS_Total, paired = TRUE)

# paired t-test comparing first day and last day >120 in control group
t.test(Paired_Filtered_Day_Zero_Control_group$PANSS_Total, Paired_Filtered_Last_Day_Control_group$PANSS_Total, paired = TRUE)

# paired t-test comparing first and last day in treatment group
t.test(Day_Zero_Treatment_group$PANSS_Total, Paired_Last_Day_Treatment_group$PANSS_Total, paired = TRUE)

# paired t-test comparing first and last day >120 in treatment group
t.test(Paired_Filtered_Day_Zero_Treatment_group$PANSS_Total, Paired_Filtered_Last_Day_Treatment_group$PANSS_Total, paired = TRUE)

# Positive Symptom tests--------------------------------------------------------
# independent t-test comparing mean of last day
t.test(Last_Day_Control_group$Positive_Total, Last_Day_Treatment_group$Positive_Total, paired = FALSE)
#(t_test, file = "test.txt")

# independent t-test comparing mean of last day >120 days
t.test(Filtered_Last_Day_Control_group$Positive_Total, Filtered_Last_Day_Treatment_group$Positive_Total, paired = FALSE)

# paired t-test comparing first and last day in control group
t.test(Day_Zero_Control_group$Positive_Total, Paired_Last_Day_Control_group$Positive_Total, paired = TRUE)

# paired t-test comparing first day and last day >120 in control group
t.test(Paired_Filtered_Day_Zero_Control_group$Positive_Total, Paired_Filtered_Last_Day_Control_group$Positive_Total, paired = TRUE)

# paired t-test comparing first and last day in treatment group
t.test(Day_Zero_Treatment_group$Positive_Total, Paired_Last_Day_Treatment_group$Positive_Total, paired = TRUE)

# paired t-test comparing first and last day >120 in treatment group
t.test(Paired_Filtered_Day_Zero_Treatment_group$Positive_Total, Paired_Filtered_Last_Day_Treatment_group$Positive_Total, paired = TRUE)

# Negative Symptom tests--------------------------------------------------------
# independent t-test comparing mean of last day
t.test(Last_Day_Control_group$Negative_Total, Last_Day_Treatment_group$Negative_Total, paired = FALSE)
#(t_test, file = "test.txt")

# independent t-test comparing mean of last day >120 days
t.test(Filtered_Last_Day_Control_group$Negative_Total, Filtered_Last_Day_Treatment_group$Negative_Total, paired = FALSE)

# paired t-test comparing first and last day in control group
t.test(Day_Zero_Control_group$Negative_Total, Paired_Last_Day_Control_group$Negative_Total, paired = TRUE)

# paired t-test comparing first day and last day >120 in control group
t.test(Paired_Filtered_Day_Zero_Control_group$Negative_Total, Paired_Filtered_Last_Day_Control_group$Negative_Total, paired = TRUE)

# paired t-test comparing first and last day in treatment group
t.test(Day_Zero_Treatment_group$Negative_Total, Paired_Last_Day_Treatment_group$Negative_Total, paired = TRUE)

# paired t-test comparing first and last day >120 in treatment group
t.test(Paired_Filtered_Day_Zero_Treatment_group$Negative_Total, Paired_Filtered_Last_Day_Treatment_group$Negative_Total, paired = TRUE)

# General Assessement tests-----------------------------------------------------
# independent t-test comparing mean of last day
t.test(Last_Day_Control_group$General_Total, Last_Day_Treatment_group$General_Total, paired = FALSE)
#(t_test, file = "test.txt")

# independent t-test comparing mean of last day >120 days
t.test(Filtered_Last_Day_Control_group$General_Total, Filtered_Last_Day_Treatment_group$General_Total, paired = FALSE)

# paired t-test comparing first and last day in control group
t.test(Day_Zero_Control_group$General_Total, Paired_Last_Day_Control_group$General_Total, paired = TRUE)

# paired t-test comparing first day and last day >120 in control group
t.test(Paired_Filtered_Day_Zero_Control_group$General_Total, Paired_Filtered_Last_Day_Control_group$General_Total, paired = TRUE)

# paired t-test comparing first and last day in treatment group
t.test(Day_Zero_Treatment_group$General_Total, Paired_Last_Day_Treatment_group$General_Total, paired = TRUE)

# paired t-test comparing first and last day >120 in treatment group
t.test(Paired_Filtered_Day_Zero_Treatment_group$General_Total, Paired_Filtered_Last_Day_Treatment_group$General_Total, paired = TRUE)