library(aod)
library(ggplot2)
library (ROCR)
library(caret)
library(readxl)

df <- read_excel("churn_dataset.xlsx", sheet="Case Data")
View(df)

dim(df)
#6347 rows and 13 columns

str(df)
