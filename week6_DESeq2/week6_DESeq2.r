# Matt Castellano | macastel@usc.edu 

# exercise 1.1 
library(BiocManager)
library(TCGAbiolinks)
library(SummarizedExperiment)
BiocManager::install("DESeq2")
library(DESeq2)
query <- GDCquery(project = "TCGA-COAD",
                  data.category = "Transcriptome Profiling",
                  data.type = "Gene Expression Quantification",
                  workflow.type = "HTSeq - Counts")
sum_exp <- GDCprepare(query)

# exercise 1.2 
# copy of clinical data 
patients_data = colData(sum_exp)
# copy of counts data 
counts = assays(sum_exp)$"HTSeq - Counts"
# covert into data frame
counts = data.frame(counts)
# see how many patients have NA age 
sum(is.na(patient_data$age_at_diagnosis)) # 4 patients with NA age 
# remove NA data with boolean indexing 
age_is_na = is.na(patients_data$age_at_diagnosis)
patient_data = patient_data[!age_is_na,]
counts = counts[,!age_is_na]
# convert patient age into years
patient_data$age_at_diagnosis = patient_data$age_at_diagnosis / 365
patient_data$age_category = ifelse(patient_data$age_at_diagnosis >= 50, "Old", "Young")
# turn patient data into a factor 
patient_data$age_category = factor(patient_data$age_category, levels = c("Young", "Old"))

# exercise 1.3
# convert to make readable 
if (all(rownames(counts) == names(rowRanges(sum_exp)))){
  print("Changing row names!")
  rownames(counts) = rowRanges(sum_exp)$external_gene_name
}
# compute sum of rows across the row of counts
counts_row_sums = rowSums(counts)
# identify genes that have fewer than 10 reads
low_gene_counts_mask = (counts_row_sums >= 10)
sum(low_gene_counts_mask == FALSE)
# remove lowly expressed genes from counts 
counts = counts[low_gene_counts_mask,]

# exercise 2.1 
dds = DESeqDataSetFromMatrix(countData = counts, 
                             colData = patient_data, 
                             design = ~age_category)
dds_obj = DESeq(dds)
resultsNames(dds_obj)
results = results(dds_obj, format = "DataFrame", contrast = c("age_category", "young", "old"))
  
# exercise 2.2 
# analysis
my_df = data.frame(x = c('b', 'd', 'c', 'e', 'a'),
                   y = c(2,4,3,5,1))
order_indices = order(my_df$y)
order_indices 
my_df = my_df[order_indices, ]
my_df

# exercise 2.3 
row_order = order(results$padj)
# AGER gene is more expressed in older people 
# Its full name is Advanced Glycosylation End-Product Specific Receptor and it is a promoter/enhancer of GH

# exercise 2.4 
log2FoldChange_threshold = 1
padj_threshold = 0.05
log_mask = ifelse(results$log2FoldChange > log2FoldChange_threshold |
                    results$log2FoldChange < -log2FoldChange_threshold, TRUE, FALSE)
sum(log_mask)
padj_na = is.na(results$padj)
results = results[!padj_na, ]
padj_mask = ifelse(results$padj < padj_threshold, TRUE, FALSE)
sum(padj_mask)

# exercise 2.5 
# plot   
fc_threshold = 2
p_threshold = 0.05
plot(x = log2FoldChange_threshold,
     y = -log10(padj),
     xlab = "young/old", 
     ylab = "p-value",
     pch = 20)
abline(v=c(-log2(fc_threshold), log2(fc_threshold)), h = c(-log1-(p_threshold)), col="green")
# volcano plot 
library(ggplot2)

volcano_plot = ggplot(data = data.frame(results), 
                      aes(x = log2FoldChange, y = -log10(padj))) + 
  geom_point(aes(color = ifelse(log2FoldChange < -1 & padj < 0.05, "lower in young",
                                ifelse(log2FoldChange > 1 & padj < 0.05, "higher in young", "NS"))),
             size = 0.5) + 
  theme_minimal() + # make things pretty +
  theme(legend.title = element_blank()) + 
  # next 2 lines draw lines at the thresholds
  geom_vline(xintercept=c(-log2(fc_threshold), log2(fc_threshold)), color="green") + 
  geom_hline(yintercept=-log10(p_threshold), color="green") + 
  scale_color_discrete(type=c("red", "blue", "black")) +
  labs(x = "log2 Fold Change (Young/Old)",
       y = "-log10 Adjusted p-value")


volcano_plot

# exercise 2.6 
write.csv(x = results,
          file = "/Users/mattcastellano/Desktop/qbio/qbio_data_analysis/week6_DESeq2/results.csv",
          row.names - FALSE)