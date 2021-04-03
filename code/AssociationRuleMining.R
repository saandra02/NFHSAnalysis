library(readr)
library(arules)
library(arulesViz)

data <- read_csv("Dataset Cleaned.csv")

#Discretizing

for(i in 3:16){
  low_label = paste("Low", i)
  medium_label = paste("Medium", i)
  high_label = paste("High", i)
  data[[i]] = discretize(data[[i]], method = "frequency", breaks = 3, labels = c(low_label, medium_label, high_label))
  
}
basket_data = data[,-c(1,2,17)]
#Too many features to test at once- causes a hang when apriori is run so we split features including literacy in each set
basket_data_1= basket_data[,c(1,2,3,4,5)]
basket_data_2 = basket_data[,c(1,6,7,8,9)]
basket_data_3 = basket_data[,c(1,10,11,12,13,14)]
write.csv(basket_data_1,"mb1.csv", quote = FALSE, row.names = FALSE)
write.csv(basket_data_2,"mb2.csv", quote = FALSE, row.names = FALSE)
write.csv(basket_data_3,"mb3.csv", quote = FALSE, row.names = FALSE)

transactions = read.transactions("mb1.csv", format='basket', sep=',')
summary(transactions)
association_rules <- apriori(transactions, parameter = list(supp=0.01, conf=0.8, maxlen=10))
inspect(association_rules)

transactions = read.transactions("mb2.csv", format='basket', sep=',')
summary(transactions)
association_rules <- apriori(transactions, parameter = list(supp=0.01, conf=0.8, maxlen=10))
inspect(association_rules[1:30])

transactions = read.transactions("mb3.csv", format='basket', sep=',')
summary(transactions)
association_rules <- apriori(transactions, parameter = list(supp=0.01, conf=0.8, maxlen=10))
inspect(association_rules[1:30])