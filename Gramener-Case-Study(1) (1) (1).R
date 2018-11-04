# Gramener Case Study

getwd()
setwd("C:\\Users\\eravdiv\\Desktop\\Ravi\\PG-Data-Science\\Gramener-Case-Study")
library(ggplot2)
library(stringr)
loan <- read.csv("loan.csv", stringsAsFactors = FALSE)
ncol(loan)
nrow(loan)

##### Data Cleaning Activity ###########
# 1. Check for columns which only have NA values
# 2. Check for columns with only 0 as values
# 3. Check for each remaining column for relevance

# 1. Check for columns which only have NA values
NA_Columns<- data.frame(which(colSums(is.na(loan)) == nrow(loan)))
colnames(NA_Columns)[1] <- "ColumnNumbers"

for (i in nrow(NA_Columns):1) {
  print(paste(i))
  loan <- loan[ ,-NA_Columns$ColumnNumbers[i]]
}
ncol(loan)

# 2. Check for columns with only 0 as values

summary(loan$tax_liens)
loan <- loan[ , -which(colnames(loan) == "tax_liens")]

summary(loan$chargeoff_within_12_mths)
loan <- loan[ , -which(colnames(loan) == "chargeoff_within_12_mths")]

summary(loan$delinq_amnt)
loan <- loan[ , -which(colnames(loan) == "delinq_amnt")]

summary(loan$acc_now_delinq)
loan <- loan[ , -which(colnames(loan) == "acc_now_delinq")]

summary(loan$collections_12_mths_ex_med)
loan <- loan[ , -which(colnames(loan) == "collections_12_mths_ex_med")]

# 3. Check for each remaining column for relevance

#check for duplicate values. As id is the primary key we will check for duplicate values of id
sum(duplicated(loan$id))
sum(duplicated(loan$member_id))
sum(which(loan$id == ""))

# Checking for correlation between funded_amnt_inv and funded_amnt and if the correlation is
# high, we will delete one of the columns as both the columns present in the dataset will lead
# to redundancy
summary(loan$loan_amnt)
summary(loan$funded_amnt)
cor(loan$loan_amnt, loan$funded_amnt)
cor(loan$funded_amnt_inv, loan$funded_amnt)
loan <- loan[ , -which(colnames(loan) == "loan_amnt")]
loan <- loan[ , -which(colnames(loan) == "funded_amnt")]

table(loan$term)
loan$term <-factor(loan$term)

summary(loan$int_rate)
table(loan$int_rate)

# changing the data tyoe of int_rate from character to numeric
loan$int_rate <- gsub("[%]","",loan$int_rate)
loan$int_rate <- as.numeric(loan$int_rate)
table(is.na(loan$int_rate))
summary(loan$int_rate)

str(loan$grade)
loan$grade <- factor(loan$grade)
table(loan$grade)

str(loan$sub_grade)
loan$sub_grade <- factor(loan$sub_grade)
table(loan$sub_grade)

table(duplicated(loan$emp_title))
loan$emp_title <- factor(loan$emp_title)
loan <- loan[ , -which(colnames(loan) == "emp_title")]

table(loan$emp_length)
loan$emp_length <- factor(loan$emp_length)

table(loan$home_ownership)
loan$home_ownership <- factor(loan$home_ownership)

# Need to check if outliers need to be removed
head(sort(loan$annual_inc, decreasing = TRUE))
loan <- loan[-which(loan$annual_inc > quantile(loan$annual_inc, probs = 0.99)), ]
summary(loan$annual_inc)
# removed some rows from the dataset as outliers as including those rows would have resulted 
# in biased mean estimate for the annual income. we did it at 99 percentile

table(loan$verification_status)
loan$verification_status <- factor(loan$verification_status)

table(loan$loan_status)
loan$loan_status <- factor(loan$loan_status)

# the column contains ony single value of "n" in all the rows and hence won't be useful in
# the analysis
table(loan$pymnt_plan)
loan <- loan[ , -which(colnames(loan) == "pymnt_plan")]

# Url column doesn't contain anything relevant that can be used in the analysis
loan$url
loan <- loan[ , -which(colnames(loan) == "url")]

# Desc column doesn't contain anything relevant that can be used in the analysis
#loan$desc
loan <- loan[ , -which(colnames(loan) == "desc")]

table(loan$purpose)
loan$purpose <- factor(loan$purpose)

# title column doesn't contain anything relevant that can be used in the analysis
table(loan$title)
loan <- loan[ , -which(colnames(loan) == "title")]

# removing zip code as it doesn't contain complete info and won't be required in the analysis
table(loan$zip_code)
loan$zip_code <- factor(loan$zip_code)
loan <- loan[ , -which(colnames(loan) == "zip_code")]

table(loan$addr_state)
loan$addr_state <- factor(loan$addr_state)

table(loan$delinq_2yrs)
loan$delinq_2yrs <- factor(loan$delinq_2yrs)

table(loan$earliest_cr_line)

table(loan$inq_last_6mths)
loan$inq_last_6mths <- factor(loan$inq_last_6mths)

# As most of the values in this column are NA's, deleting this column
summary(loan$mths_since_last_delinq)
loan <- loan[ , -which(colnames(loan) == "mths_since_last_delinq")]

# Again as most of the values in this column are NA's, deleting this column
summary(loan$mths_since_last_record)
table(loan$mths_since_last_record)
loan <- loan[ , -which(colnames(loan) == "mths_since_last_record")]

summary(loan$open_acc)
table(loan$open_acc)

summary(loan$pub_rec)
table(loan$pub_rec)
loan$pub_rec <- factor(loan$pub_rec)

summary(loan$revol_bal)

# Revove % sign and make numeric
loan$revol_util <- gsub("[%]","",loan$revol_util)
loan$revol_util <- as.numeric(loan$revol_util)
table(is.na(loan$revol_util))
summary(loan$revol_util)

summary(loan$total_acc)
table(is.na(loan$total_acc))

# As all the rows in the column contain single value "f", it won't be useful in the analysis
summary(loan$initial_list_status)
table(loan$initial_list_status)
loan <- loan[ , -which(colnames(loan) == "initial_list_status")]

# checking for correlation between out_prncp and out_prncp_inv and delete if the correlation
# is very high
summary(loan$out_prncp)
summary(loan$out_prncp_inv)
cor(loan$out_prncp, loan$out_prncp_inv)
loan <- loan[ , -which(colnames(loan) == "out_prncp")]
loan <- loan[ , -which(colnames(loan) == "out_prncp_inv")]

# checking for correlation and deleting the columns as any one of the column can be used
# in the analysis
summary(loan$total_pymnt)
summary(loan$total_pymnt_inv)
cor(loan$total_pymnt, loan$total_pymnt_inv)
loan <- loan[ , -which(colnames(loan) == "total_pymnt_inv")]
cor(loan$total_pymnt, loan$total_rec_prncp)
loan <- loan[ , -which(colnames(loan) == "total_rec_prncp")]

# We are not going to use these columns in our anaysis based on business understanding, hence
# deleting it
summary(loan$total_rec_int)
summary(loan$total_rec_late_fee)
summary(loan$recoveries)
summary(loan$collection_recovery_fee)
loan <- loan[ , -which(colnames(loan) == "total_rec_int")]
loan <- loan[ , -which(colnames(loan) == "total_rec_late_fee")]
loan <- loan[ , -which(colnames(loan) == "recoveries")]
loan <- loan[ , -which(colnames(loan) == "collection_recovery_fee")]


summary(loan$last_pymnt_d)

# next_pymnt_d is not going to give any relevant insight in this scenario, so deleting it
summary(loan$next_pymnt_d)
sum(loan$next_pymnt_d == "")
loan <- loan[ , -which(colnames(loan) == "next_pymnt_d")]

# Another date field which needs to be investigated
summary(loan$last_credit_pull_d)

# since policy code contains single type of value which is "1", deleting it
summary(loan$policy_code)
table(loan$policy_code)
loan <- loan[ , -which(colnames(loan) == "policy_code")]

# since application type contains single type of value which is "Individual" or all the loan
# applicants are individual, deleting it
summary(loan$application_type)
table(loan$application_type)
loan <- loan[ , -which(colnames(loan) == "application_type")]

summary(loan$pub_rec_bankruptcies)
table(loan$pub_rec_bankruptcies)

ncol(loan)


############# Start analysis of columns ###################

# Create new column to indentify individuals who have defaulted on their loan
loan$Has_Defaulted <- factor(ifelse(loan$loan_status == "Charged Off", "Yes", "No"))

# Univariante analysis of loans defaulted vs amount of loan. The trends indicate that loan defaulted and not defaulted
# are following the same trend.
ggplot(loan, aes(x= loan$funded_amnt)) + geom_histogram(binwidth = 1000) + facet_wrap(~loan$Has_Defaulted)

summary(loan$purpose)
ggplot(loan, aes(x = loan$purpose)) + geom_bar()
# Highest number of loan taken for debt_consolidation puprose

#univariate analysis of emp_length
summary(loan$emp_length)
ggplot(loan, aes(x = loan$emp_length)) + geom_bar()
# Highest number of loans is being taken by customers with employment length more than 10 years
# while if we see it closely, we can observe that new generation with employee length between
# 0 and 9 years have taken the highest number of loans. It will be interesting to look at
# which group is defaulting more on loan through bivariate analysis later.

# Univariate analysis for annual income
mean(loan$annual_inc)
ggplot(loan, aes(x = loan$annual_inc)) + geom_histogram()


# Univariate Analysis
ggplot(loan, aes(x = loan$open_acc)) + geom_histogram()
# Lot of customers are there with 10 open credit lines. It will be interesting to see it's
# effect on the default rate. Also we can check how open_acc moves along with dti column

# Bivariate Analysis - between open_acc and dti to check whether they have correlation
ggplot(loan, aes(x = loan$open_acc, y = loan$dti)) + geom_point() + geom_smooth()
# they don't seem to follow high correlation from the chart plotted, so we keep both the
# variables for future analysis

# Bi-variante analysis between open_acc and HAs_Fedaulted does not show any specific trend.
ggplot(loan, aes(x = loan$open_acc, fill = loan$Has_Defaulted)) + geom_histogram()

# Bivariate Analysis - between term and Has_default to check whether term has anything to do
# with default rate
ggplot(loan, aes(x = loan$term, fill = loan$Has_Defaulted)) + geom_bar()
# higher percentage of customers with longer payment term have defaulted.

# Bivariate analysis -between emp_length and Has_default to check whether employee duration with
# job has anything to do with default rate. The default trend seem to follow the same trend
# as the number of customers applying for loan in each category
ggplot(loan, aes(x = loan$emp_length, fill = loan$Has_Defaulted)) + geom_bar(position = "dodge")


# Bivariate analysis between home_ownership and is_default to check if any correlation
# exists between them
ggplot(loan, aes(x = loan$home_ownership, fill = loan$Has_Defaulted)) + geom_bar()
# from the graph we can see that those having mortgage on living on rent have more 
# propensity to default on loan than those owning their own house. This could be because 
# they have too much financial liability through paying mortgage amount or rent and hence
# inability to payback the loan

# Bivariate analysis to check whether verification status has anything to do with defaulting
# of customers
ggplot(loan, aes(x = loan$verification_status, fill = loan$Has_Defaulted)) + geom_bar()
# It's well known fact that non- verified customers are risk but, even though the verification status is verified or source verified, there are still high number
# of applicants who default on their loan

# Bivariate analysis between dti and has_default. There seems to be an increasing trend in the
# default rate even though though the trend for number of customers is first increasing and then
# decreasing
summary(loan$dti)
ggplot(loan, aes(x = loan$dti, fill = loan$Has_Defaulted)) + geom_histogram()

# To analyse it more clearly, dti field is grouped in order for bivariante analysis later.
dti <- function(vector)
{
  if( vector >= 0 & vector <= 10 )
  {
    status = "1.Low"
  } else if ( vector > 10 & vector <= 20)
    (
      status = "2.Medium"
    ) else if ( vector > 20 & vector <= 30)
    {
      status = "3.High"
    }
  return(status)
}

# Bivarinate analysis of dti_category and Has_defaulted. We can observe that higher percentage
# of customers are defaulting in the High category which is "20-30"
loan$dti_category <- sapply(loan$dti, dti)
ggplot(loan, aes(x = loan$dti_category, fill = loan$Has_Defaulted)) + geom_bar()


# Bivariate Analysis for grade and has_defaulted
summary(loan$grade)
ggplot(loan, aes(x = loan$grade, fill = loan$Has_Defaulted)) + geom_bar()
# Higher percentage of customers have defaulted as we move down the grede and A to G.

# Bivariate analysis betweeb revol_util and has defaulted. There is definitely some connection
# between high revolving utilization rate and customers defaulting
summary(loan$revol_util)
ggplot(loan, aes(x = loan$revol_util, fill = loan$Has_Defaulted)) + geom_histogram()

cor(loan$revol_util, loan$revol_bal)

table(is.na(loan$revol_bal))
table(is.na(loan$revol_util))
ggplot(loan, aes(x= loan$revol_util, y = loan$revol_bal, fill = loan$revol_bal)) + geom_point() + geom_smooth()

# Column revolv_util is categorized in order to do bivariante analysis later.
revol <- function(vector)
{
  category <- vector(mode = "character", length = length(vector))
  if( vector >= 0 & vector <= 25 & !is.na(vector))
  {
    category <- "1.Low"
  } else if ( vector > 25 & vector <= 50 & !is.na(vector))
    (
      category <- "2.Medium"
    ) else if ( vector > 50 & vector <= 75 & !is.na(vector))
    {
     category <- "3.High"
    } else if (vector > 75 & vector <= 100 & !is.na(vector))
    {
      category <- "4.Very high"
    } 
  return(category)
}

loan$revol_util_category <- sapply(loan$revol_util, revol)
loan$revol_util_category <- factor(loan$revol_util_category)
summary(loan$revol_util_category)

ggplot(loan, aes(x = loan$revol_util_category, fill = loan$Has_Defaulted)) + geom_bar()
# High and very high revolving utilization rate has higher default rate

#Bivaraite Analysis between purpose and has_default. there doesn't seem to be any definitive
# trend emerging out of this graph
ggplot(loan, aes(x = loan$purpose, fill = loan$Has_Defaulted)) + geom_bar(position = "stack")

# Bivariate analysis between annual income and has_defaulted. Here, again doesn't seem to
# following any visible trend
ggplot(loan, aes(x = loan$annual_inc, fill = loan$Has_Defaulted)) + geom_histogram()

# converting earlist_cr_line to date format to extract the year and to analyze whether there 
# is any correlation between earlist_cr_line and has_defaulted ie whether old customers 
# are defaulting less as compared to the new one
summary(loan$earliest_cr_line)
loan$earliest_cr_line <- paste("01-",loan$earliest_cr_line, sep = "" )
loan$earliest_cr_line <- as.Date(loan$earliest_cr_line, format = "%d-%b-%y")                              
str(loan$earliest_cr_line)
loan$earliest_cr_line_year <- format(loan$earliest_cr_line, "%Y")
loan$earliest_cr_line_year <- as.factor(loan$earliest_cr_line_year)
ggplot(loan, aes(x = loan$earliest_cr_line_year, fill = loan$Has_Defaulted)) + geom_bar()
loan

summary(loan$issue_d)
aggregate(annual_inc ~ Has_Defaulted, loan, mean)

# converting issue_d to date format to extract the year from it and check whether issue yaer
# has any relation with the customer defaulting ie any macro level issue like recession 
# effecting the loan payment by customers
loan$issue_d <- paste("01-",loan$issue_d, sep = "" )
loan$issue_d <- as.Date(loan$issue_d, format = "%d-%b-%y")                              
str(loan$issue_d)
loan$issue_year <- format(loan$issue_d, "%Y")
ggplot(loan, aes(x = loan$issue_year, fill = loan$Has_Defaulted)) + geom_bar()

# plotting interest rate and has_defaulted to check if there is any relation between the
# two
summary(loan$int_rate)
ggplot(loan, aes(x = loan$int_rate, fill = loan$Has_Defaulted)) + geom_histogram()

# Column interest rate is categorized in order to conduct bivariante analysis.
interest_rate <- function(vector)
{
  rate <- vector(mode = "character", length = length(vector))
  if( vector >= 5 & vector <= 10)
  {
    rate <- "1.Low"
  } else if ( vector > 10 & vector <= 15)
    (
      rate <- "2.Medium"
    ) else if ( vector > 15 & vector <= 20)
    {
      rate <- "3.High"
    } else if (vector > 20 & vector <= 25)
    {
      rate <- "4.Very high"
    } 
  return(rate)
}

loan$int_rate_category <- sapply(loan$int_rate, interest_rate)
loan$int_rate_category <- factor(loan$int_rate_category)
summary(loan$int_rate_category)
## We see a trend that applicants with high and very high interest rate takers default more.
ggplot(loan, aes(x = loan$int_rate_category, fill = loan$Has_Defaulted)) + geom_bar()

#
ggplot(loan, aes(x = loan$int_rate_category, fill = loan$term)) + geom_bar() + facet_wrap(~loan$Has_Defaulted)

ggplot(loan, aes(x = loan$int_rate_category, fill = loan$Has_Defaulted)) + geom_bar() + facet_wrap(~loan$home_ownership)
ggplot(loan, aes(x = loan$purpose, fill = loan$Has_Defaulted)) + geom_bar() + facet_wrap(~loan$revol_util_category)

ggplot(loan, aes(x = factor(loan$issue_year) , fill = loan$term)) + geom_bar() + facet_wrap(~loan$Has_Defaulted)

ggplot(loan, aes(x = factor(loan$term) , fill = loan$Has_Defaulted)) + geom_bar(identity = "stack")


ggplot(loan, aes(x = loan$verification_status, fill = loan$issue_year)) + geom_bar() + facet_wrap(~loan$Has_Defaulted)

ggplot(loan, aes(x = loan$revol_util_category, fill = loan$home_ownership)) + geom_bar() + facet_wrap(~loan$Has_Defaulted)

ggplot(loan, aes(x = loan$int_rate_category, fill = loan$Has_Defaulted)) + geom_bar()
ggplot(loan, aes(x = loan$grade, fill = loan$Has_Defaulted)) + geom_bar()
ggplot(loan, aes(x = loan$revol_util_category, fill = loan$Has_Defaulted)) + geom_bar()


# subsetting the data based on verification status to further drill down the reason behing customers
# whose income was verified but still they defaulted
loan_verified <- subset(loan, loan$verification_status == "Verified")
ggplot(loan_verified, aes(x = loan_verified$dti_category, fill = loan_verified$Has_Defaulted)) + geom_bar() 
ggplot(loan_verified, aes(x = loan_verified$term, fill = loan_verified$Has_Defaulted)) + geom_bar() 

# subsetting the data based on verification status to further drill down the reason behing customers
# whose income was source verified but still they defaulted
loan_source_verified <- subset(loan, loan$verification_status == "Source Verified")
ggplot(loan_source_verified, aes(x = loan_source_verified$dti_category, fill = loan_source_verified$Has_Defaulted)) + geom_bar() 
ggplot(loan_source_verified, aes(x = loan_source_verified$term, fill = loan_source_verified$Has_Defaulted)) + geom_bar() 

# subsetting data based on revol_util_category to further analyse the reason behind as to
# why the customers with "low" and "medium" categories are defaulting
loan_revol_util <- subset(loan, loan$revol_util_category == "1.Low" | loan$revol_util_category == "2.Medium")
ggplot(loan_revol_util, aes(x = loan_revol_util$emp_length)) + geom_bar() + facet_wrap(~loan_revol_util$Has_Defaulted) 
ggplot(loan_revol_util, aes(x = loan_revol_util$dti_category, fill = loan_revol_util$Has_Defaulted)) + geom_bar()

#bivariante analysis of address_state with Has defaulted. No specific trend observed except that California has most number of loans
ggplot(loan, aes(x= loan$addr_state, y = loan$Has_Defaulted, fill = loan$Has_Defaulted)) + geom_bar(stat = "Identity")

#bivarinate anlaysis of address_state and verification status does not reveal any specific trend.
ggplot(loan, aes(x = loan$addr_state, y = loan$verification_status, fill = loan$verification_status)) + geom_bar(stat = "Identity")

#bivarinate analysis of inquiry in last 6 months with has defaulted indicates that as the enquires go up the number of defaulters also go up.
ggplot(loan, aes(x= factor(loan$inq_last_6mths), y = loan$Has_Defaulted, fill = loan$Has_Defaulted)) + geom_bar(stat= "Identity")

# bivarinate analysis of public record and has defaulted does not any specific trend.
ggplot(loan, aes(x = factor(loan$pub_rec), y = loan$Has_Defaulted, fill = loan$Has_Defaulted)) + geom_bar(stat= "Identity")       

# Bivarinate analysis of total_acc and has defaulted defaulted does not show any specific trend.
ggplot(loan, aes(x = loan$total_acc, fill = loan$Has_Defaulted)) + geom_histogram()        

# Bivarinate analysis of public record backrupties and has defaulted does not show any trends.
ggplot(loan, aes(x = factor(loan$pub_rec_bankruptcies), y = loan$Has_Defaulted, fill = loan$Has_Defaulted)) + geom_bar(stat= "Identity")       

# Bivarinate analysis of purpose and has defaulted shows that debt has maximum number of loans and defaulters.
ggplot(loan, aes(x = loan$purpose, y = loan$Has_Defaulted, fill = loan$Has_Defaulted)) + geom_bar(stat= "Identity")       

