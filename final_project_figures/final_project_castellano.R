# Matt Castellano | macastel@usc.edu 


if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager") {
    BiocManager::install(version = "3.13")
  }
if(!require(TCGAbiolinks)) {
  BiocManager::install("TCGAbiolinks")
}

library(ggplot2)
library(ggpubr)
library(survminer)
library(survival)

clin_query <- GDCquery(project = "TCGA-COAD",
                       data.category = "Clinical",
                       file.type = "xml")

# GDCdownload(clin_query) ONLY RUN ONCE 

clinic <- GDCprepare_clinic(clin_query,clinical.info = "patient")

colnames(clinic)
# new data set with only patients with venous invasion 
venous_clinic <- clinic[clinic$venous_invasion == "YES",]
venous_clinic$venous_invasion 
venous_clinic_test = venous_clinic[venous_clinic$lymphatic_invasion != "YES",]
# 110 patients with venous invasion (ONLY 13 with VI not LI)

# new data set with only patients with lymphatic invasion 
lymph_clinic <- clinic[clinic$lymphatic_invasion == "YES",]
lymph_clinic$lymphatic_invasion
lymph_clinic_test = lymph_clinic[lymph_clinic$venous_invasion != "YES",]
# 189 patients with lymphatic invasion (ONLY 92 WITH LI not VI)

# new data set with only patients with lymphatic invasion and venous invasion
both_clinic <- lymph_clinic[lymph_clinic$venous_invasion == "YES",]
both_clinic 
# 97 patients with lymphatic invasion and venous invasion 

# survival analysis time for patients with both lymphatic invasion and venous invasion 
both_clinic_test = both_clinic 
na_daystodeath <- is.na(both_clinic_test$days_to_death)
both_clinic_test$days_to_death[is.na(both_clinic_test$days_to_death)] <- both_clinic_test$days_to_last_followup[is.na(both_clinic_test$days_to_death)]
both_clinic_test$survival_time <- both_clinic_test$days_to_death - both_clinic_test$days_to_birth
both_clinic_test$death_event <- ifelse(both_clinic_test$vital_status=="Alive", 1, 0)

# survival analysis of males and females with both li and vi
surv_object_both <- Surv(time = both_clinic_test$days_to_death, 
                    event = both_clinic_test$death_event)
gender_fit_both <- surv_fit( surv_object_both ~ both_clinic_test$gender, data = both_clinic_test)
survplot = ggsurvplot(gender_fit_both, 
                      pval=TRUE, 
                      title = "Venous Invasion and Lymphatic Invasion in Males and Females",
                      ggtheme = theme(plot.margin = unit(c(1,1,1,1), "cm")), 
                      legend = "right")
p = survplot$plot + 
  theme_bw() +  # changes the appearance to be a bit prettier
  theme(axis.title = element_text(size=20), # increase font sizes
        axis.text = element_text(size=10),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12))
p

ggsave("/Users/mattcastellano/Desktop/qbio/qbio_data_analysis_mcastellano/final_project_figures/kmplot_both_invasions.png", plot = p, width = 12, height = 9)

# survival analysis of people with just vi 
# cleaning up data set 
na_daystodeath <- is.na(venous_clinic_test$days_to_death)
venous_clinic_test$days_to_death[is.na(venous_clinic_test$days_to_death)] <- venous_clinic_test$days_to_last_followup[is.na(venous_clinic_test$days_to_death)]
venous_clinic_test$survival_time <- venous_clinic_test$days_to_death - venous_clinic_test$days_to_birth
venous_clinic_test$death_event <- ifelse(venous_clinic_test$vital_status=="Alive", 1, 0)

surv_object_v <- Surv(time = venous_clinic_test$days_to_death, 
                         event = venous_clinic_test$death_event)
gender_fit_v <- surv_fit( surv_object_v ~ venous_clinic_test$gender, data = venous_clinic_test)
survplot = ggsurvplot(gender_fit_v, 
                      pval=TRUE,
                      title = "Venous Invasion in Males and Females",
                      ggtheme = theme(plot.margin = unit(c(1,1,1,1), "cm")), 
                      legend = "right")
p = survplot$plot + 
  theme_bw() +  # changes the appearance to be a bit prettier
  theme(axis.title = element_text(size=20), # increase font sizes
        axis.text = element_text(size=10),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12))
p
ggsave("/Users/mattcastellano/Desktop/qbio/qbio_data_analysis_mcastellano/final_project_figures/kmplot_venous_invasion_only.png", plot = p, width = 12, height = 9)


# survival analysis of people with just li 
# cleaning up data set 
na_daystodeath <- is.na(lymph_clinic_test$days_to_death)
lymph_clinic_test$days_to_death[is.na(lymph_clinic_test$days_to_death)] <- lymph_clinic_test$days_to_last_followup[is.na(lymph_clinic_test$days_to_death)]
lymph_clinic_test$survival_time <- lymph_clinic_test$days_to_death - lymph_clinic_test$days_to_birth
lymph_clinic_test$death_event <- ifelse(lymph_clinic_test$vital_status=="Alive", 1, 0)

surv_object_l <- Surv(time = lymph_clinic_test$days_to_death, 
                      event = lymph_clinic_test$death_event)
gender_fit_l <- surv_fit( surv_object_l ~ lymph_clinic_test$gender, data = lymph_clinic_test)
survplot = ggsurvplot(gender_fit_l, 
                      pval=TRUE,
                      title = "Lymphatic Invasion in Males and Females",
                      ggtheme = theme(plot.margin = unit(c(1,1,1,1), "cm")), 
                      legend = "right")
p = survplot$plot + 
  theme_bw() +  # changes the appearance to be a bit prettier
  theme(axis.title = element_text(size=20), # increase font sizes
        axis.text = element_text(size=10),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12))
p
ggsave("/Users/mattcastellano/Desktop/qbio/qbio_data_analysis_mcastellano/final_project_figures/kmplot_lymph_invasion_only.png", plot = p, width = 12, height = 9)

gender_count = clinic[clinic$gender == "MALE",]
# 280 males 
gender_count_lymph = lymph_clinic_test[lymph_clinic_test$gender == "MALE",]
# 48 males 
gender_count_venous = venous_clinic_test[venous_clinic_test$gender == "MALE",]
# 7 males
gender_count_both = both_clinic_test[both_clinic_test$gender == "MALE",]
# 53 males
death_count_lymph = lymph_clinic_test[lymph_clinic_test$vital_status == "DEAD",]

# visualization of mortality of males and females with both vi and li 
plot(x = both_clinic_test$vital_status,
     y = both_clinic_test$gender,
     xlab = "Vital Status",
     ylab = "Gender",
     pch = 20)