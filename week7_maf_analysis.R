# Matt Castellano | macastel@usc.edu 

# exercise 1.1 
getwd()
setwd("/Users/mcastellano/Desktop/qbio_data_analysis_mcastellano/analysis_data/")
BiocManager::install("maftools")
library(maftools)
library(TCGAbiolinks)
library(gplot2)

# exercise 1.2 
clinic <- data.table::fread("/Users/mcastellano/Desktop/qbio/qbio_data_analysis_mcastellano/analysis_data/clinic.csv",
                            data.table = F)
colnames(clinic)[ colnames(clinic) == "bcr_patient_barcode" ] <- "Tumor_Sample_Barcode"

# exercise 1.3 
length(colnames(clinic))
length(colnames(clinic) == "bcr_patient_barcode")
colnames(clinic) == "bcr_patient_barcode"
typeof(colnames(clinic) == "bcr_patient_barcode")

# exercise 1.4 
mutation_query <- GDCquery_Maf(tumor = "COAD", 
                               pipeline = "mutect2",
                               save.csv = TRUE)

maf_object <- read.maf(maf = mutation_query, 
                       clinicalData = clinic, 
                       isTCGA = TRUE)

# exercise 1.5
maf_dataframe = data.table::fread("TCGA.COAD.mutect.03652df4-6090-4f5a-a2ff-ee28a37f9301.DR-10.0.somatic.maf.csv")
clinic = data.table::fread("./coad_clinical_data.csv")
colnames(clinic)[ colnames(clinic) == "bcr_patient_barcode" ] <- "Tumor_Sample_Barcode"
maf_object <- read.maf(maf = mutation_query, 
                       clinicalData = clinic, 
                       isTCGA = TRUE)

# exercise 2.1 
max_object
str(maf_object)
maf_object@data

# exercise 3.1
oncoplot(maf = maf_object,
         top = 10)
ggsave("/Users/mcastellano/Desktop/qbio/qbio_data_analysis_mcastellano/analysis_data/clinic.csv")

# exercise 3.2 
# FLT3 is one of the top 10 most mutated gene in the oncoplot 
# It is responsible for controlling survival, proliferation and differentiation of hematopoietic cells.

# exercise 3.3 
clinic = maf_object@clinical.data
clinic$age_category = ifelse(clinica$age_at_initial_pathologic_diagnosis <= 50, "Young", "Old")
name <- ifelse(clinic$age_category=="Young", TRUE, FALSE)
young_patient_ids <- clinic[name, Tumor_Sample_Barcode]
young_maf = subsetMaf(maf = young_maf,
                      tsb = young_patient_ids)
name2 <- ifelse(clinic$age_category=="Old", TRUE, FALSE)
old_patient_ids <- clinic[name2, Tumor_Sample_Barcode]
old_maf = subsetMaf(maf = old_maf,
                      tsb = old_patient_ids)

# exercise 3.4 
coOncoplot(m1 = young_maf, 
           m2 = old_maf, 
           m1Name = "Young Patients", 
           m2Name = "Old Patients")

ggsave("/Users/mcastellano/Desktop/qbio/qbio_data_analysis_mcastellano/week7_MAF/young_old_oncoplot.png")

# exercise 3.5 
#genes are mutated more in old than in young 

# exercise 3.6 
lollipopPlot(maf_object, gene = "FLT3")
ggsave("/Users/mcastellano/Desktop/qbio/qbio_data_analysis_mcastellano/week7_MAF/lollipop.png")

# exercise 3.7 
lollipopPlot2(m1 = young_maf, 
              m2 = old_maf, 
              m1_name = "Young",
              m2_name = "Old",
              gene = "FLT3")
ggsave("/Users/mcastellano/Desktop/qbio/qbio_data_analysis_mcastellano/week7_MAF/young_old_lollipop.png")

# exercise 3.8 
# nonsense mutation for FLT3

# exercise 3.9 
# b = 7, c = 2, d =  35, e = 37, f = 42 
# A and B are independent

# exercise 3.10
geneA_maf = subsetMaf(maf = maf_object,
                       genes = "TP53")
geneB_maf = subsetMaf(maf = maf_object,
                       genes = "KRAS")

# exercise 3.11
geneA_maf
geneB_maf

# exercise 3.12 
mut_bc_geneA = geneA_maf@data$Tumor_Sample_Barcode
mut_bc_geneB = geneB_maf@data$Tumor_Sample_Barcode
num_mut_geneA = length(mut_bc_geneA)
num_mut_geneB = length(mut_bc_geneB)
mut_bc_geneAB = intersect(mut_bc_geneA, mut_bc_geneB)
num_mut_geneAB = length(mut_bc_geneAB)

# exercise 3.13
num_mut_geneA_only = num_mut_geneA - num_mut_geneAB
num_mut_geneB_only = num_mut_geneB - num_mut_geneAB

# exercise 3.14 
num_mut_neither = 397 - (num_mut_geneA_only + num_mut_geneB_only + num_mut_geneAB)
contig_table = matrix(c(num_mut_geneAB, 
                        num_mut_geneB_only,
                        num_mut_geneA_only,
                        num_mut_neither), 
                      nrow=2)
contig_table

# exercise 3.15
fe_results <- fisher.test(contig_table)
fe_results
