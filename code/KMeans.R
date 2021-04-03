library(readr)
library(dplyr)
library(stats)
library(arules)
library(factoextra)
library(NbClust)

data <- read_csv("Dataset Cleaned.csv")

#Discretizing
for(i in 4:16){
  data[[i]] = discretize(data[[i]], method = "frequency", breaks = 3, labels = c("Low", "Medium", "High"))
  
}
set.seed(123)
# Elbow method
fviz_nbclust(data[3], kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(data[3], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
fviz_nbclust(data[3], kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

KMeansResult <- kmeans(data[3], 3, nstart = 20)

data$Lit_Cluster = KMeansResult$cluster

column_names = colnames(data)

for (c in c(1, 2, 3)) {
  cluster_data = data %>%
  filter(Lit_Cluster==c)
  for(i in 4:16){
    cat("In cluster", c, "For the variable", column_names[i], "\n")
    con_table <- table(data[[i]])
    print(prop.table(con_table))
    cat("\n")
  }
  
}
