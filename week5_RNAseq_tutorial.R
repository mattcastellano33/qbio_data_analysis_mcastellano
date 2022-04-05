# Matt Castellano | macastel@usc.edu

# exercise 1.1 
# install 
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager") {
    BiocManager::install(version = "3.13")
  }
if(!require(SummarizedExperiment)) {
  BiocManager::install("SummarizedExperiment")
}
# load 
library(BiocManager)
library(TCGAbiolinks)
library(SummarizedExperiment)
# set working directory
setwd("~/Dekstop/qbio/qbio_data_analysis_mcastellano/")

# exercise 2.1 
# RNAseq 
query <- GDCquery(project = "TCGA-COAD",
                  
                  data.category = "Transcriptome Profiling", #RNAseeq transcriptome
                  data.type = "Gene Expression Quantification", #gets the counts 
                  workflow.type = "HTSeq - Counts") #gets raw counts 

# download
sum_exp <- GDCprepare(query) 
str(sum_exp)

# exercise 2.2 
counts = assays(sum_exp)$"HTSeq - Counts"
counts
assays(sum_exp)$"HTSeq - Counts"[1:5, 1:5]
head(rowData(sum_exp))
head(colData(sum_exp))
colData(sum_exp)[1:5, 25:29]

# exercise 2.3 
# dimensions of data set 
dim(colData(sum_exp))
# repeat for row 
dim(rowData(sum_exp))
# repeat for assays(sum_exp)$"HTSeq - Counts"
dim(assays(sum_exp))$"HTSeq - Counts"

# exercise 2.4 
str(colData(sum_exp))
head(colData(sum_exp))
head(rowData(sum_exp))

# exercise 2.5
colnames(colData(sum_exp))
(colData(sum_exp)$age_at_diagnosis)

# exercise 2.6 
colData(sum_exp)$age_at_diagnosis[1:10]

# exercise 2.7 
colData(sum_exp)$age_at_diagnosis = colData(sum_exp)$age_at_diagnosis/365

# exercise 2.8 
colData(sum_exp)$age_category = ifelse(colData(sum_exp)$age_at_diagnosis >= 50, "Old", "Young")
colData(sum_exp)$age_category

# exercise 2.9 
head(rowData(sum_exp))
dim(rowData(sum_exp))

# exercise 2.10 
"TP53" %in% rowData(sum_exp)$external_gene_name
"APC" %in% rowData(sum_exp)$external_gene_name

# exercise 2.11
assays(sum_exp)$"HTSeq - Counts"[20:25, 30:35]


# exercise 2.12
geneA_id_mask = (rowData(sum_exp)$external_gene_name == "TP53")
sum(geneA_id_mask)
ensembl_geneA = rowData(sum_exp)$ensembl_gene_id[geneA_id_mask] 
ensembl_geneA
geneB_id_mask = (rowData(sum_exp)$external_gene_name == "APC")
sum(geneB_id_mask)
ensembl_geneB = rowData(sum_exp)$ensembl_gene_id[geneB_id_mask] 
ensembl_geneB

# exercise 2.13
# row 

# exercise 2.14
min(assays(sum_exp)$"HTSeq - Counts"[ geneA_id_mask, ])
max(assays(sum_exp)$"HTSeq - Counts"[ geneA_id_mask, ])
summary(assays(sum_exp)$"HTSeq - Counts"[ geneB_id_mask, ])

# exercise 2.15
plot(assays(sum_exp)$"HTSeq - Counts"[ geneA_id_mask, ],
     assays(sum_exp)$"HTSeq - Counts"[ geneB_id_mask, ],
     xlab = "TP53", 
     ylab = "APC")

# exercise 2.16
bool_age_na = is.na(colData(sum_exp)$age_category)
num_na = sum(bool_age_na)
num_na

# exercise 2.17
age_cat_no_NAs <- colData(sum_exp)$age_category[!bool_age_na]
sum(is.na(age_cat_no_NAs))


# exercise 2.18
length(age_cat_no_NAs)
dim( colData(sum_exp) )[1] == length(age_cat_no_NAs) + num_na

# exercise 2.19
dim(assays(sum_exp)$"HTSeq - Counts")
dim(age_cat_no_NAs)
# 4 patients 

# exercise 2.20
identical( rownames(colData(sum_exp)), colnames(assays(sum_exp)$"HtSeq - Counts"))
gene_counts = assays(sum_exp)$"HTSeq - Counts"[
  geneA_id_mask,
  !bool_age_na
]

# exercise 2.21
length(age_cat_no_NAs) == length(gene_counts) # true

# exercise 2.22 
boxplot(gene_counts ~ age_cat_no_NAs, 
        xlab = "Age Category", 
        ylab = "Counts for TP53 Gene")

# exercise 3.1
# 1. 
assays(sum_exp)$"HTSeq - Counts"
# 2. 
rowData(sum_exp)
# 3.
colData(sum_exp)

