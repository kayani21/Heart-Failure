library(dplyr)

df_original <- read.csv("C:/Users/kayla/OneDrive/Documents/TSU/Adv_Topics_CS497/Projects/HeartFailure_excerpt.csv", header=TRUE)

if ("Label" %in% colnames(df_original)) {
    colnames(df_original)[colnames(df_original) == "Label"] <- "DEATH_EVENT"
}

df <- df_original %>% select(-Cluster_Hierarchical, -Cluster_KMeans)

expected_columns <- c("Age", "CPK_Levels", "Ejection_Fraction", "Platelets", 
                      "Serum_Creatinine", "Serum_Sodium", "Time", "DEATH_EVENT")

if (length(colnames(df)) == length(expected_columns)) {
    colnames(df) <- expected_columns
} else {
    print("Warning: Column count mismatch. Here are the current column names:")
    print(colnames(df)) 
}

print("Final Column Names in df (after removing clustering labels):")

print(colnames(df))

if ("DEATH_EVENT" %in% colnames(df)) {
    X <- df %>% select(-DEATH_EVENT)  
    X_scaled <- scale(X)  
} else {
    stop("Error: Column 'DEATH_EVENT' not found in the dataset!")
}

library(cluster)
library(dplyr) 
library(ggplot2)

chooseCRANmirror(graphics=FALSE, ind=66) 
# print("Checking if X exists before scaling:")
# print("Checking if X is selected correctly:")

hc <- hclust(dist(X_scaled), method="ward.D2")
print(hc)

df$Cluster_Hierarchical <- cutree(hc, k=3)

print(dim(X)) 

X_scaled <- scale(X)

silhouette_scores <- c()
for (k in 2:10) {
    km <- kmeans(X_scaled, centers=k, nstart=25)
    sil <- silhouette(km$cluster, dist(X_scaled))
    silhouette_scores <- c(silhouette_scores, mean(sil[, 3]))
}

best_k <- which.max(silhouette_scores) + 1
kmeans_result <- kmeans(X_scaled, centers=best_k, nstart=25)
df$Cluster_KMeans <- kmeans_result$cluster 

if ("Cluster_Hierarchical" %in% colnames(df) & "Cluster_KMeans" %in% colnames(df)) {
    X <- df %>% select(-DEATH_EVENT, -Cluster_Hierarchical, -Cluster_KMeans)
} else {
    stop("Error: Clustering columns are missing from df!")
}

install.packages("cluster", dependencies=TRUE)
set.seed(123)
print("Checking if X_scaled exists:")

print(dim(X_scaled)) 

print("Checking if K-Means clustering worked:")

print(table(kmeans_result$cluster))  

library(cluster)

hc <- hclust(dist(X_scaled), method="ward.D2")  
plot(hc, main="Hierarchical Clustering Dendrogram", xlab="", sub="", cex=0.6)

best_hierarchical_clusters <- 3
df$Cluster_Hierarchical <- cutree(hc, k=best_hierarchical_clusters)


df_original <- read.csv("C:/Users/kayla/OneDrive/Documents/TSU/Adv_Topics_CS497/Projects/HeartFailure_excerpt.csv", header=TRUE)

print("Checking column names before confusion matrix:")

print(colnames(df_original))

if ("Label" %in% colnames(df_original)) {
    colnames(df_original)[colnames(df_original) == "Label"] <- "DEATH_EVENT"
}
print("First few rows of df:")

print(head(df)) 

if ("DEATH_EVENT" %in% colnames(df_original)) {

print(paste("Length of df$Cluster_KMeans:", length(df$Cluster_KMeans)))
print(paste("Length of df_original$DEATH_EVENT:", length(df_original$DEATH_EVENT)))

if (length(df$Cluster_KMeans) == length(df_original$DEATH_EVENT)) {
    print("Confusion Matrix for K-Means Clustering:")
    print(table(df$Cluster_KMeans, df_original$DEATH_EVENT))
} else {
    stop("Error: Cluster assignments and DEATH_EVENT do not have the same length!")
}

print("Confusion Matrix for Hierarchical Clustering:")
if (length(df$Cluster_Hierarchical) == length(df_original$DEATH_EVENT)) {
    print(table(df$Cluster_Hierarchical, df_original$DEATH_EVENT))
} else {
    stop("Error: Cluster assignments and DEATH_EVENT do not have the same length!")
 }
}

sil_hc <- silhouette(df$Cluster_Hierarchical, dist(X_scaled))

plot(sil_hc[, 3], main="Silhouette Plot for Hierarchical Clustering",
     xlab="Cluster Index", ylab="Silhouette Width", col="palevioletred1", pch=19)

     sil_kmeans <- silhouette(df$Cluster_KMeans, dist(X_scaled))

plot(sil_kmeans[, 3], main="Silhouette Plot for K-Means Clustering",
     xlab="Cluster Index", ylab="Silhouette Width", col="#40E0D0", pch=19)

     library(ggplot2)
install.packages("ggplot2", dependencies=TRUE)
file_path <- "C:/Users/kayla/OneDrive/Documents/TSU/Adv_Topics_CS497/Projects/HeartFailure_excerpt.csv"
if (file.exists(file_path)) {
    HeartFailure_excerpt <- read.csv(file_path, header=TRUE)
    print("Dataset loaded successfully")
} else {
    stop("Error: File not found! Check the path.")
}

print(colnames(HeartFailure_excerpt))

silhouette_df <- data.frame(k=2:10, Silhouette=silhouette_scores)
ggplot(silhouette_df, aes(x=k, y=Silhouette)) +
  geom_point(color="#FF34B3", size=3) +   
  geom_line(color="#00E5EE") +          
  labs(title="Silhouette Score for K-Means Clustering",
       x="Number of Clusters (k)", 
       y="Silhouette Score") +
  theme_minimal()

  df$Cluster_KMeans <- as.factor(kmeans_result$cluster)

ggplot(df, aes(x=Age, y=CPK_Levels, color=Cluster_KMeans)) +
  
  geom_point(size=3) +

 labs(title="K-Means Clustering Visualization", x="Age", y="CPK_Levels") +

  theme_minimal()
