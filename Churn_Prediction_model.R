library(aod)
library(ggplot2)
library (ROCR)
library(caret)
library(readxl)
library(dplyr)

df <- read_excel("churn_dataset.xlsx", sheet="Case Data")
View(df)

dim(df)
#6347 rows and 13 columns

str(df)

#Check for null values
sum(is.na(df))
#No null values in the dataset

#Formating the column names in the dataset
library(janitor)
df <- clean_names(df)
names(df) <- gsub("0-1", "0_1", names(df))
str(df)

#Removing junk characters from column names in the dataset
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

#Variable Transformation
df$churn <- as.factor(df$churn)
df$chi_0 <- as.numeric(df$chi_0)
df$chi_01 <- as.numeric(df$chi_01)
df$supp_case_01 <- as.numeric(df$supp_case_01)
df$sp_01 <- as.numeric(df$sp_01)
df$logins_01 <- as.numeric(df$logins_01)
df$blog_art_01 <- as.numeric(df$blog_art_01)
df$views_01 <- as.numeric(df$views_01)

str(df)
View(df)



df$churn[df$churn=="1"] <- "Yes"
df$churn[df$churn=="0"] <- "No"

df$churn

##############Insights from the data######################
#cust_age represents how long (in months) customer has been with QWC

summary(df$cust_age)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0     5.0    11.0    13.9    20.0    67.0

plot(density(df$cust_age))
#The distribution is highly right skewed with Median of 11 months and Mean 13.9 months

#Case infers that cust_age more than 14 months are less likely to leave.
# cust_age -- >14 months -- less likely to leave
# cust_age -- <6 months -- less likely to leave
# cust_age -- 6-14 months -- highly likely to leave

df %>% group_by(churn) %>% summarise(mean=mean(cust_age), med=median(cust_age), std=sd(cust_age))
#  churn     mean   med   std
#   No       13.8    11   11.3 
#   Yes      15.4    13   8.87

table(df$churn)
#6024 rows with churn Yes and 323 rows with No

summary(df[df$churn=="Yes",]$cust_age)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00   10.00   13.00   15.35   18.50   47.00

plot(density(df[df$churn=='Yes',]$cust_age))

summary(df[df$churn=='No',]$cust_age)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    5.00   11.00   13.82   20.00   67.00

plot(density(df[df$churn=='No',]$cust_age))

anova_age_churn <- aov(cust_age~churn, data=df)
summary(anova_age_churn)
#p-value of 0.0161


#PLAN OF ACTION

#Construct Visualizations to understand relationship of churn with all the variables
#Construct age buckets
#Perform t-tests on all the variables for churn Yes or No values


