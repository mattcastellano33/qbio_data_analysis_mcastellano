# Matt Castellano | macastel@usc.edu

clinic = read.csv("clinic.csv") # read in clinic file

# Writing Portion 

# 1. Define the following: categorical variable, discrete variable, 
# continuous variable. Provide examples of each.

# A categorical variable is a variable that helps to represent a types of data 
# that can be divided into differentiating groups. An example of this would be 
# race or gender for a patient. 
# A discrete variable is a variable that is finite and countable. An example of
# this would be the number of patients who show a certain mutation. 
# A continuous variable is a variable that can take on an unlimited number of
# values between a certain range. An example of this would be patient's age. 

# 2. Which variable have you chosen?
 
colnames(clinic) # get list of column names in clinic data 
clinic$venous_invasion # selecting column 
sum(clinic$venous_invasion == "") # find sum of true's

# 3. How is your variable measured or collected? Is your variable categorical, 
# discrete, or continuous?

# The variable "venous_invasion" is a categorical variable that represents an 
# independent prognostic indicator in colorectal cancer. 

# 4. 
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4682925/ 
# The first article is an overview of extramural venous invasion in CRC. It seeks
# to understand the relationship of EMVI with other prognostic factors and just how
# crucial identifying a EMVI tumor site is in therapy. 
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1769571/ 
# The second article looks into extramural and intramural venous invasion as 
# prognostic indicators in stage 4 CRC studies. 

# 5. 
colnames(clinic)
clinic$stage_event_pathologic_stage 
# The pathologic stage is a categorical measurement to analyze what stage of 
# cancer (amount or spread of cancer in the body) by removing tissue samples
# during surgery or a biopsy. It is then categorized into stages 1-4. 

# 6. 
# 1) Venous invasions are crucial in identifying stage 2 CRC. 
# 2) Venous invasions yield lower rates of survival in patients. 
# 3) Later stages of CRC yield lower rates of survival in patients.

#7. 
# clean up na's
new_clinic_data = clinic[!is.na(clinic$venous_invasion),]
# clean up other variable
plot_new_clinic = new_clinic_data[new_clinic_data$stage_event_pathologic_stage  != ""]
# plot 
boxplot(plot_new_clinic$venous_invasion ~ plot_new_clinic$stage_event_pathologic_stage)

# I learned how important venous invasions are for prognostic indentifications 
# of patients with CRC. Likewise, I learned about how the different stages of 
# CRC can progress and affect other biomarkers and indicators that researchers 
# record. 

# Coding Portion 

# 1. 
install.packages("survival")
install.pockages("survminer")
library(survival)
library(survminer)
# set up a death indicator
plot_new_clinic$days_to_death = ifelse(is.na(plot_new_clinic$days_to_death), plot_new_clinic$days_to_last_follow_up, plot_new_clinic$days_to_death)
plot_new_clinic$death_event = ifelse(plot_new_clinic$vital_status == "Alive", 0, 1)
# survival object 
survival_object = Surv(time = plot_new_clinic$days_to_death, event = plot_new_clinic$death_event)
# fit object 
venous_invasion_fit = survival_fit(survival_object ~ new_clinic_data$venous_invasion data = new_clinic_data)
# 2. 
# survival plot for venous invasion
survival_plot = ggsurvplot(venous_invasion_fit, pval=TRUE)
p = survival_plot$plot
p
# 3. 
# survival plot for pathologic stage
stage_event_pathologic_stage_fit = survival_fit(survival_object ~ new_clinic_data$stage_event_pathologic_stage, data = new_clinic_data)
survival_plot = ggsurvplot(stage_event_pathologic_stage_fit, pval=TRUE)
p = survival_plot$plot
p
