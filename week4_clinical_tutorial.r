install.packages("BiocManager, TCGABiolinks")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager") {
    BiocManager::install(version = "3.13")
  }
if(!require(TCGAbiolinks)) {
  BiocManager::install("TCGAbiolinks")
}

# You will use variations of this command for every data type
clin_query <- GDCquery(project = "TCGA-COAD",
                       data.category = "Clinical",
                       file.type = "xml")

# ONLY RUN THIS ONCE
GDCdownload(clin_query)

# Load the query into R
clinic <- GDCprepare_clinic(clin_query, 
                            clinical.info = “patient”)


x <- 1:100

y <- cumsum(x)

plot(y)

my_vector = c(1,2,3,4,5)  # equivalently, my_vector = 1:5
my_vector = my_vector + 1

plot(my_vector)

