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

#Formating the column names in the dataset
library(janitor)
df <- clean_names(df)
names(df) <- gsub("0-1", "0_1", names(df))
str(df)

#Removing junk characters from the dataset
df$chi_score_month_0 <- gsub("-", "", df$chi_score_month_0)
df$chi_score_0_1 <- gsub("-", "", df$chi_score_0_1)
df$support_cases_0_1 <- gsub("-", "", df$support_cases_0_1)
df$sp_0_1 <- gsub("-", "", df$sp_0_1)
df$logins_0_1 <- gsub("-", "", df$logins_0_1)
df$blog_articles_0_1 <- gsub("-", "", df$blog_articles_0_1)
df$views_0_1 <- gsub("-", "", df$views_0_1)

#Rename Columns
names(df) <-  c("id","cust_age","churn","chi_0","chi_01","supp_case_0","supp_case_01","sp_0",
                "sp_01","logins_01","blog_art_01","views_01","days_since_last_login_01")

str(df)
View(df)

#Backup of the dataset
df_bkp <- df

