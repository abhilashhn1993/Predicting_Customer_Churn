library(aod)
library(ggplot2)
library(tidyverse)
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

#Remove ID column
df$id <- NULL

str(df)
View(df)

#df$churn[df$churn=="1"] <- "Yes"
#df$churn[df$churn=="0"] <- "No"

##############    INSIGHTS FROM DATA   #############################
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

summary(df[df$churn=="1",]$cust_age)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00   10.00   13.00   15.35   18.50   47.00

plot(density(df[df$churn=='1',]$cust_age))

summary(df[df$churn=='0',]$cust_age)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    5.00   11.00   13.82   20.00   67.00

plot(density(df[df$churn=='0',]$cust_age))

anova_age_churn <- aov(cust_age~churn, data=df)
summary(anova_age_churn)
#p-value of 0.0161


####################### PLAN OF ACTION  ############################
#Construct Visualizations to understand relationship of churn with all the variables
#Construct age buckets
#Perform t-tests on all the variables for churn Yes or No values for feature selection
#Divide train and test data (cross validation technique)
#Design a Logistic regression model
#Construct inferences from the co-eff and intercept values
####################################################################


######################  VISUALIZATIONS  ############################
str(df)

cust_age = df$cust_age
churn = df$churn

###Plotting cust_age against churn
#Density plot
ggplot(df, aes(x=cust_age, fill=churn)) + geom_density(alpha=0.4) +
      labs(title="Customer Age distribution by Churn")

#Box Plot
ggplot(df, aes(x=churn, y=cust_age)) + geom_boxplot(fill="cornflowerblue") +
      labs(title="Customer Age distribution by Churn")
#From the box plot, we can infer that the median age for 

#Cust_age distribution by churn
ggplot(df, aes(y=churn, x=cust_age, color=churn))+
      geom_jitter(alpha=0.4)+
      labs(title="Customer age in months vs Churn")+
      theme(legend.position = "none")

#####################   INFERENCES   ###############################




####################################################################

###Plotting chi_0 against churn
plt1 <- df %>% ggplot(aes(x=df$churn, y=df$chi_0, fill=df$churn))+
  geom_boxplot(alpha=.5, width=.3, position="identity")+
  labs(title="Dist of CHI score/churn", x="Churn", y="CHI score")+
  theme(legend.position = "none")

plt2 <- df %>% ggplot(aes(x=df$chi_0, fill=df$churn))+
  geom_density(alpha=.3, position="identity")+
  labs(title="Density of Chi score/Churn", x="CHI score", y="Density of CHI score")+
  theme(legend.position="none")

plot(plt1)
plot(plt2)
grid.arrange(plt1,plt2, ncol=2)

####################################################################

######################  t-test #####################################
#Null Hyp: Population mean of customers who churned IS EQUAL to popln mean of customers who didn't churn
#Alt Hyp: Popln mean of customers who churned is NOT EQUAL to popln mean of customers who didn't churn

df_y <- df[df$churn=="1",]
df_n <- df[df$churn=="0",]

df_y$churn = as.numeric(df_y$churn)
df_n$churn = as.numeric(df_n$churn)

mean(df_y$chi_01)

str(df)
library(tidyverse)
library(broom)
library(pander)
t1 <- tidy(t.test(df_y$cust_age, df_n$cust_age, paired=FALSE))
t2 <- tidy(t.test(df_y$chi_0, df_n$chi_0, paired=FALSE))
t3 <- tidy(t.test(df_y$chi_01, df_n$chi_01, paired=FALSE))
t4 <- tidy(t.test(df_y$supp_case_0, df_n$supp_case_0, paired=FALSE))
t5 <- tidy(t.test(df_y$supp_case_01, df_n$supp_case_01, paired=FALSE))
t6 <- tidy(t.test(df_y$sp_0, df_n$sp_0, paired=FALSE))
t7 <- tidy(t.test(df_y$sp_01, df_n$sp_01, paired=FALSE))
t8 <- tidy(t.test(df_y$logins_01, df_n$logins_01, paired=FALSE))
t9 <- tidy(t.test(df_y$blog_art_01, df_n$blog_art_01, paired=FALSE))
t10 <- tidy(t.test(df_y$views_01, df_n$views_01, paired=FALSE))
t11 <- tidy(t.test(df_y$days_since_last_login_01, df_n$days_since_last_login_01, paired=FALSE))

res=rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11)

t_res=data.frame(rbind("Cust_Age","CHI_Score_0","CHI_Score_01",
                       "Supp_case_0","Supp_case_01","SP_0","SP_01",
                       "Logins_01","Blog_01","Views_01","Days_Since_Last_Login_01"))
res = cbind(t_res,res)
View(res)
res[,c(1,5,6)]

##        Variable Name         Mean_Churned   Mean_Unchurned    p.value  
## --------------------------- -------------- ---------------- -----------
##  Customer Age (in months)       15.35           13.82        0.003057  
##      CHI Score Month 0          63.27           88.61        2.097e-13 
##        CHI Score 0-1            -3.737           5.53        1.571e-08 
##    Support Cases Month 0        0.3715          0.7243       6.281e-08 
##      Support Cases 0-1         0.03715        -0.009296       0.5278   
##         SP Month 0              0.4996          0.8296       4.381e-07 
##           SP 0-1               -0.0167         0.03268        0.5218   
##         Logins 0-1              8.062           16.14        0.0004037 
##      Blog Articles 0-1         -0.1022          0.1711        0.01158  
##          Views 0-1              -95.77          106.6         0.05631  
##  Days Since Last Login 0-1      6.486           1.511        5.215e-05

#From the p-values of the t-tests, following are the statistically significant predictors 
#at 95% level confidence
str(df)

#cust_age 
#chi_0
#chi_01
#supp_case_0
#sp_0
#logins_01
#blog_art_01
#views_01
#days_since_last_login_01
