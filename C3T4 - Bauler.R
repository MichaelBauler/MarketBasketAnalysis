# Course 3 Task 4 - Market Basket Analysis


install.packages("arules")
install.packages("arulesViz")

library(tidyverse)
library(openxlsx)
library(knitr)
library(ggplot2)
library(arules)
library(arulesViz)
#package for visualization of arules
library(dplyr)
library(kableExtra)
library(gplots)
library(Matrix)
library(lazyeval)
#package for analyzing transactional dat

# The read.transactions() function changes the dataset into a sparse matrix. It makes each row represent a transaction and creates columns for each item that a customer might purchase. Electronidex sells 125 items, so the sparse matrix creates 125 columns. It also changes the data to binary. (1=item purchased in that transaction OR 0=no purchase.)
#Using read transactions bc opencsv will convert to columns, we want basket format - separator is commas and we are trying to remove duplicates in each column
data<- read.transactions("ElectronidexTransactions2017.csv", format= "basket", sep=",", rm.duplicates = TRUE )
# WARNING - incomplete final line found on 'ElectronidexTransactions2017.csv'????

#################################################################
# Summary Statistics
summary(data) # 9835 transactions 125 columns (items) 
# most frequently bought items:
# 1. iMac 2519
# 2. HP Laptop 1909
# 3. CYBERPOWER Gamer Desktop 1809
# 4. Apple Earpods 1715
# 5. Apple MacBook Air 1530                   33622 
# Average # of items purchased is 4.383

inspect(data) # You can view the transactions

length(data) # Number of transactions # 9835
# [1] 9835

size(data) # Number of items per transactions

LIST(data) # Lists the transactions by conversion (LIST must be capitalized)

itemLabels(data)# To see the item (Column) labels


####################
## VISUALIZATIONS ##
####################

# View Item Frequency
itemFrequency(data, type = 'absolute')


# Plot top 20 most frequently purchased items
itemFrequencyPlot(data, topN=20, type = 'absolute', col = 1:20)


# Plot top 10 most frequently purchased items
itemFrequencyPlot(data, topN=10, type = 'absolute', col = 1:10)

sample(data) # will create a sample that contains a random set of transactions.


image(sample(data, 125))

#################################################################
## Market Basket Analysis ##

#Number of times a->b happens / total number of transactions
#confidence 
#Number of times a->b / times a is in the data - Can be misleading if X is in a lot of the data

#Lift
#confidence / fraction of transactions containing b
#less than 1 means it's a bad rule

#apiari model
Rules <-apriori(data, parameter=list(supp=0.005, conf=.65, minlen=2))
inspect(Rules)

#Making sure there's no redundancy
is.redundant(Rules) #False there is no redundancy

#################################################################
# Evaluate Model
# Statistical Summary of the rules created
summary(Rules)

#Seeing the rules sorted in decreasing order by measurement in quotations
inspect(sort(Rules, by = "lift"))

#Seeing only the rules that have the item "HP Laptop" in quotations
itemrules <- subset(Rules, items %in% "HP Laptop")
inspect(itemrules)



#################################################################
# Visualizing Rules
plot(Rules, method="matrix")
plot(Rules, method="graph")
plot(Rules, method="paracoord", control=list(reorder=TRUE))













