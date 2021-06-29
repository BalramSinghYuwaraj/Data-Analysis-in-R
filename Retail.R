
#1. Merge the datasets Customers, Product Hierarchy and Transactions as
#Customer_Final. Ensure to keep all customers who have done
#transactions with us and select the join type accordingly.
#a. Use the base merge()
#b. Dplyr merge functions

Customer = read.csv(choose.files())
Transactions = read.csv(choose.files())
Prod_cat_info = read.csv(choose.files())

library(dplyr)

Customer$Gender <- as.character(Customer$Gender)

Customer$DOB <- as.Date(Customer$DOB, format = "%d-%m-%Y")

Customer_Final <- merge(x=Transactions,y=Customer,by.x = "cust_id",by.y = "customer_Id")

Customer_Final <- merge(x=Customer_Final,y= Prod_cat_info,by ="prod_cat_code",all = T)

Customer_Final


#2. Prepare a summary report for the merged data set.
#a. Get the column names and their corresponding data types

summary(Customer_Final, title = "Retail Data")

#b. Top/Bottom 10 observations

library(dplyr)

filter(Customer_Final, row_number(desc(total_amt))<=10)

#c. "Five-number summary" for continuous variables (min, Q1, median,Q3 and max)

summary(Customer_Final$transaction_id.x)

#d. Frequency tables for all the categorical variables

Gender <- table(Customer_Final$Gender)
Bar_Gender <- barplot(Gender,main = "Gender",col="Green")

Tran_date <- table(Customer_Final$tran_date)
Bar_Tran_date <- barplot(Tran_date,main = "Transaction Date")

Store_Type <- table(Customer_Final$Store_type)
Bar_Store_Type <- barplot(Store_Type,main = "Store Type",col="Blue")

Prod_cat <- table(Customer_Final$prod_cat)
Bar_Prod_cat <- barplot(Prod_cat,main = "Product Category",col="orange red")

prod_subcat <- table(Customer_Final$prod_subcat)
Bar_prod_subcat <- barplot(prod_subcat,main = "Product Sub-Category",col="red") 


#3. Generate histograms for all continuous variables and frequency bars for
#categorical variables.

library(tidyr)
library(ggplot2)

hist_prod_cat_code <- hist(Customer_Final$prod_cat_code,main = "Histogram of prod_cat_code",xlab = "prod_cat_code")

hist_transaction_id <- hist(Customer_Final$transaction_id,main = "Histogram of transaction_id",xlab = "transaction_id")

hist_prod_subcat_code <- hist(Customer_Final$prod_subcat_code,main = "Histogram of prod_subcat_code",xlab = "prod_subcat_code" )

hist_Qty <- hist(Customer_Final$Qty,main = "Histogram of Qty",xlab = "Quantity")

hist_Rate <- hist(Customer_Final$Rate,main = "Histogram of Rate",xlab = "Rate")

hist_Tax <- hist(Customer_Final$Tax,main = "Histogram of Tax",xlab = "Tax")

hist_Total_amt <- hist(Customer_Final$total_amt,main = "Histogram of Total Amount",xlab = "Total Amount")

hist_city_code <- hist(Customer_Final$city_code,main = "Histogram of City Code",xlab = "city_code")

hist_prod_sub_cat_code <- hist(Customer_Final$prod_sub_cat_code,main = "Histogram of prod_sub_cat_code",xlab = "prod_sub_cat_code")


#4. Calculate the following information using the merged dataset :
#  a. Time period of the available transaction data

library(lubridate)
Transaction_Date <- parse_date_time(Customer_Final$tran_date, c("mdy", "dmy"))
Transaction_Date <- as.Date(Transaction_Date,format = "%d-%m-Y")
TimePeriod <- print(max(Transaction_Date,na.rm = T)-min(Transaction_Date,na.rm = T))

#  b. Count of transactions where the total amount of transaction was
#negative.

Total_Neg_Num <- (length(which(Customer_Final$total_amt<0)))
print(Total_Neg_Num)

#5. Analyze which product categories are more popular among females vs male
#customers.

No.ofMales <- print(length(Customer_Final$Gender[Customer_Final$Gender=="M"]))

No.ofFemales <- print(length(Customer_Final$Gender[Customer_Final$Gender=="F"]))

Sum <- sum(No.ofMales+No.ofFemales)

Total <- c(Males=No.ofMales,Females=No.ofFemales)

Bar_M_F <- barplot(Total,main = "Numbers of Male-Female",ylab = "Numbers",col="Green")

PercentMales <- print((No.ofMales/Sum)*100)

PercentFemales <- print((No.ofFemales/Sum)*100)

Percentage <- c(Males=PercentMales,Female=PercentFemales)

Bar_M_F_Per <- barplot(Percentage, main = "Percentage Wise Male-Female",ylab = "Percentage",col="red")

#6. Which City code has the maximum customers and what was the
#percentage of customers from that city?

CityWiseCust <- Customer %>% group_by(city_code) %>% summarise(No_Of_Cust=length(city_code))


CityWiseCust$Percentage <-(CityWiseCust$No_Of_Cust/sum(CityWiseCust$No_Of_Cust)*100)

CityWiseCust$Percentage <- round(CityWiseCust$Percentage,digits = 3)

MaxCityCustomer <-subset(CityWiseCust,Percentage=max(Percentage))

MaxCity <- print(MaxCityCustomer[which.max(CityWiseCust$Percentage),])

#7. Which store type sells the maximum products by value and by quantity?

Store_Type1 <- Customer_Final %>% group_by(Store_type) %>% summarise(TOtalAmount= sum(total_amt,na.rm = T))

Store_Type2 <- Customer_Final %>% group_by(Store_type,cust_id) %>% summarise()

Store_Type3 <- Store_Type2 %>% group_by(Store_type) %>% summarise(TotalQuantity = length(Store_type))

Store_Type1 <- merge(Store_Type1,Store_Type3,all = T)

StoreName_byValue <- Store_Type1[which.max(Store_Type1$TOtalAmount),]
print(StoreName_byValue)

StoreName_byQuantity <- Store_Type1[which.max(Store_Type1$TotalQuantity),]
print(StoreName_byQuantity)

#8. What was the total amount earned from the "Electronics" and "Clothing"
#categories from Flagship Stores?

Flagship_Stores <- Customer_Final %>% group_by(Store_type,prod_cat) %>% summarise(TotalRevenue=sum(total_amt,na.rm=T))

Flagship_Stores_Clothing <- Flagship_Stores[Flagship_Stores$Store_type=="Flagship store" & Flagship_Stores$prod_cat=="Clothing",]
print(Flagship_Stores_Clothing)

Flagship_Stores_Electronics <- Flagship_Stores[Flagship_Stores$Store_type=="Flagship store" & Flagship_Stores$prod_cat=="Electronics",]
print(Flagship_Stores_Electronics)


#9. What was the total amount earned from "Male" customers under the
#Electronics" category?

Male_customer <- Customer_Final %>% group_by(Gender,prod_cat) %>% summarise(Total_Revenue = sum(total_amt,na.rm = T))

Male_cust <- Male_customer[Male_customer$Gender=="M"& Male_customer$prod_cat == "Electronics",]

print(Male_cust)

#10. How many customers have more than 10 unique transactions, after
#removing all transactions which have any negative amounts?

library(dplyr)

Transactions$total_amt[Transactions$total_amt < 0] <- NA
Transactions$total_amt

Unique_transactions = unique(Customer_Final$cust_id)
Unique_transactions

length(Unique_transactions)

install.packages("sqldf")

library(sqldf)
 
x = sqldf("SELECT cust_id, 
    SUM(total_amt) AS Total_Amount_of_Transactions, 
    COUNT(cust_id) AS Count_of_Transactions
FROM Customer_Final
WHERE Qty >= 0
GROUP BY cust_id
HAVING COUNT(cust_id) > 10")

x

#11. For all customers aged between 25 - 35, find out:
#  a. What was the total amount spent for "Electronics" and "Books"
#product categories?
Customer_Final$tran_date <- parse_date_time(Customer_Final$tran_date, c("mdy", "dmy"))  #imp

Customer_Final$tran_date <- as.Date(Customer_Final$tran_date,format = "%d-%m-Y")

Customer_Final$Age <- (Customer_Final$tran_date - Customer_Final$DOB)/365.24

Customer_Final$Age <- round(Customer_Final$Age,digits = 2)

#Elder is 18-25

Customer_Final$Age_Group <- ifelse(Customer_Final$Age<=25,"Young",ifelse(Customer_Final$Age<=35,"Elder","Mature"))

Aged <- Customer_Final %>% group_by(Age_Group,prod_cat) %>% summarise(TotalRevenue=sum(total_amt,na.rm = T))

Age_Grp_Elec <- Aged[Aged$Age_Group=="Young" & Aged$prod_cat=="Electronics",]

print(Age_Grp_Elec)

Age_Grp_Book <- Aged[Aged$Age_Group=="Young" & Aged$prod_cat=="Books",]

print(Age_Grp_Book)

#  b. What was the total amount spent by these customers between 1st
#Jan, 2014 to 1st Mar, 2014?

date_revenue <- Customer_Final %>% group_by(tran_date) %>% summarise(TotalRevenue=sum(total_amt, na.rm = T))
Date <- date_revenue[date_revenue$tran_date>="2014-01-01" & date_revenue$tran_date<="2014-03-01",]
TotalRevenueGen <- print(sum(date_revenue$TotalRevenue))

