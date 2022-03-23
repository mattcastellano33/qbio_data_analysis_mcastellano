# Matt Castellano | macastel@usc.edu

install.packages("BiocManager, TCGABiolinks")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager") {
    BiocManager::install(version = "3.13")
  }
if(!require(TCGAbiolinks)) {
  BiocManager::install("TCGAbiolinks")
}

library(TCGAbiolinks)
clin_query <- GDCquery(project = "TCGA-COAD", data.category = "Clinical",file.type = "xml")

GDCdownload(clin_query)

clinic <- GDCprepare_clinic(clin_query, clinical.info = "patient")

names(clinic)[names(clinic)=="days_to_last_followup"] <- "days_to_last_follow_up"


# exercise 1.1 
str(clinic)
head(clinic)

# exercise 1.2
colnames(clinic)
clinic$primary_lymph_node_presentation_assessment

# exercise 2.1
plot(x = clinic$age_at_initial_pathologic_diagnosis, 
     y = clinic$weight,
     xlab = "Age",
     ylab = "Weight")

# exercise 2.2
unique(clinic$race_list)
par(mar=c(10,1,1,1))
boxplot(age_at_initial_pathologic_diagnosis ~ race_list,
        data = clinic,
        las = 2,
        cex.axis = 0.5,
        xlab = "Race",
        ylab = "Age in Years")

# exercise 2.3
clinic$race_list = as.character(clinic$race_list)
clinic$race_list[clinic$race_list == ""] = "No data"
table(clinic$race_list)

# exercise 2.4
age = clinic$age_at_initial_pathologic_diagnosis
min(age)
max(age)
mean(age)
median(age)
summary(age)

# exercise 2.5
young_patient = sum(clinic$age_at_initial_pathologic_diagnosis < 50)
old_patient = sum(clinic$age_at_initial_pathologic_diagnosis >= 50)
young_patient
old_patient

# exercise 2.6
young_mask = clinic$age_at_initial_pathologic_diagnosis < 50
young_patient_ids = clinic$bcr_patient_barcode[young_mask]
print(length(young_patient_ids) == young_patient)
old_patient_ids = clinic$bcr_patient_barcode[clinic$age_at_initial_pathologic_diagnosis >= 50]

# exercise 2.7
clinic$age_category = ifelse(
  clinic$age_at_initial_pathologic_diagnosis < 50,
  "young",
  "old"
)

# exercise 2.8
clinic[1,1]
clinic[1,]
clinic[2:5,]
clinic[,3]

# exercise 2.9
young_clinic = clinic[clinic$age_category == "young", ]
old_clinic = clinic[clinic$age_category == "old", ]

# exercise 2.10
young_clinic_one_line = clinic[clinic$age_at_initial_pathologic_diagnosis < 50, ]
identical(dim(young_clinic), dim(young_clinic_one_line))

# exercise 3.1 
library(survival)
library(survminer)
clinic$days_to_death = ifelse(is.na(clinic$days_to_death),
                              clinic$days_to_last_follow_up,
                              clinic$days_to_death)

# exercise 3.2
clinic$death_event = ifelse(clinic$vital_status == "Dead", 1, 0)

# exercise 3.3
write.csv(clinic, "~/Desktop/qbio/qbio_data_analysis_castellano/coad_clinical_data.csv",
          
          row.names = F)