library(readr)
library(dplyr)
library(ggpubr)
library(factoextra)
library(gplots)
library(RColorBrewer)
set.seed(1)



# Data Loading -----------------------------------------------------------------
setwd("C:/Users/benja/Desktop/classes/Stats 202/Final Project/FinalProjectData/Study_data")
Study_df <- list.files(path = 'C:/Users/benja/Desktop/classes/Stats 202/Final Project/FinalProjectData/Study_data') %>% 
  lapply(read_csv) %>% 
  bind_rows


# Total Groups -----------------------------------------------------------------
# formating our data
# removes all E data, all failed audits, and any additional observations on a patient after the first
# introduces a VisitWeek, PositiveToTal, NegativeTotal, and GeneralTotal variable
Test_data <- Study_df %>% 
  filter(Study != "E" & LeadStatus == "Passed" & VisitDay == 0) %>% 
  mutate(VisitWeek = ceiling(VisitDay/7)) %>%  
  rowwise() %>%
  mutate(Pos = sum(c_across(P1:P7))) %>% 
  mutate(Neg = sum(c_across(N1:N7))) %>% 
  mutate(Gen = sum(c_across(G1:G16))) %>% 
  select(PatientID, Pos, Neg, Gen)
  



# Eight Cluster ------------------------------------------------------------------


cluster_params = Test_data %>% select(!PatientID)
cluster_params = scale(cluster_params)

                
km_clusters = kmeans(cluster_params, 8, nstart = 100)


Test_data$cluster = km_clusters$cluster
Test_data = Test_data %>% 
  arrange(cluster)

fviz_cluster(km_clusters, data = cluster_params)

df = Test_data %>% 
  select(!c(PatientID, cluster))
df = scale(df) 
rownames(df) = paste0("clust_",Test_data$cluster)
cluster_names = (c("clust_1", "clust_2", "clust_3", "clust_4", "clust_5", "clust_6"))
pheatmap(as.matrix(df), cluster_rows = F, cluster_col = F, color = brewer.pal(9, "OrRd"), labels_row = cluster_names)
par(xpd=TRUE)
legend("topright",  legend=c("min", "med", "max"),fill=brewer.pal(3,"OrRd"))




Test_data[c(2:4)] <- lapply(Test_data[c(2:4)], function(x) c(scale(x)))
Test_groups = Test_data %>% group_by(cluster)
Test_groups %>% summarise(pos_med = median(Pos),
                          neg_med = median(Neg),
                          gen_med = median(Gen),
                          pos_avg = mean(Pos),
                          neg_avg = mean(Neg),
                          gen_avg = mean(Gen))
wss <- sapply(1:20,
              function(k){kmeans(cluster_params, k, nstart=100)$tot.withinss})
wss
plot(1:20, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


# Subset Groups -----------------------------------------------------------------
Test_data <- Study_df %>% 
  filter(Study != "E" & LeadStatus == "Passed" & VisitDay == 0) %>% 
  mutate(VisitWeek = ceiling(VisitDay/7)) %>%  
  select(3, 9:38)

set.seed(1)
cluster_params = Test_data %>% select(2:dim(Test_data)[2])
cluster_params = scale(cluster_params)

km_clusters = kmeans(cluster_params, 6, nstart = 100)
Test_data$cluster = km_clusters$cluster
Test_data = Test_data %>% 
  arrange(cluster)

fviz_cluster(km_clusters, data = cluster_params)

df = Test_data %>% 
  select(!c(PatientID, cluster))
df = scale(df) 
rownames(df) = paste0("clust_",Test_data$cluster)
cluster_names = (c("clust_1", "clust_2", "clust_3", "clust_4", "clust_5", "clust_6"))
pheatmap(as.matrix(df), cluster_rows = F, cluster_col = F, color = brewer.pal(9, "OrRd"), labels_row = cluster_names)


