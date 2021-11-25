setwd("F:/actif E&R IGR DITEP/RESOLVED²/SCRIPTS_WD")

# this script is an example for clustering using clut, dist and tSNE

# PMID_MeSH is a dataframe from document term matrix -> output of dtm <- DocumentTermMatrix(docs)
# read https://eight2late.wordpress.com/2015/07/22/a-gentle-introduction-to-cluster-analysis-using-r/

# dataframe with PMID and MeSH: one row per PMID; one column per MeSH
PMID_MeSH <- read.csv2("PMID_MeSH.txt", sep = "\t", header = TRUE, check.names=FALSE )

# simple cluster 
library(ggplot2)
install.packages("ggdendro")
library(ggdendro)

#compute distance between document vectors
d = dist(PMID_MeSH, method = "binary")
hc = hclust(d, method="ward.D2")

ggdendrogram(hc, rotate = FALSE, size = 0.01, labels = FALSE) +
  ggtitle("Trial clustering based on MeSH terms", subtitle = NULL)
ggsave('XXX _ Figure 4 _ trial clustring based on MeSH terms.png', 
       width=6, height=6, unit='in', dpi=300)

# analyze multiple description https://www.r-bloggers.com/clustering-mixed-data-types-in-r/
library(dplyr) # for data cleaning
library(cluster) # for gower similarity and pam
install.packages("Rtsne")
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization

# calculate Gower distances
gower_dist <- daisy(PMID_MeSH,
                    metric = "gower",
                    type = list(logratio = 3))
summary(gower_dist)
gower_mat <- as.matrix(gower_dist)

# compute most similar pair
SIM_TOP <- PMID_MeSH[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

View(SIM_TOP)
tSIM_TOP <- t(SIM_TOP)
View(tSIM_TOP)
# get PMID
SIM_TOP <- SIM_TOP$PMID
# create table 
tbl_Abst_Corpus_f_2606_simtop <- tbl_Abst_Corpus_f_2606 %>% filter(PMID %in% c(SIM_TOP))
write.table(x = tbl_Abst_Corpus_f_2606_simtop , file = "YYY - Table 1 - most similar abstract based on MeSH terms.txt", sep = "\t")
write.table(x = tSIM_TOP , file = "YYY - Table 1b - most similar abstract based on MeSH terms - matrix.txt", sep = "\t")

# compute most dissimilar pair
DISS_TOP <- PMID_MeSH[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
# get PMID
DISS_TOP <- DISS_TOP$PMID
# create table 
tbl_Abst_Corpus_f_2606_diss_top <- tbl_Abst_Corpus_f_2606 %>% filter(PMID %in% c(DISS_TOP))
write.table(x = tbl_Abst_Corpus_f_2606_diss_top , file = "YYY - Table 2 - most dissimilar abstract based on MeSH terms.txt", sep = "\t")

# continue clustering with Gower distances
# select number of clusters
  # Calculate silhouette width for many k using PAM
sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
    sil_width[i] <- pam_fit$silinfo$avg.width
  }

# Plot sihouette width (higher is better)
png(filename = "XXX _ Figure 5 _ Number of clusters based on the silhouette width method.png",
    width = 2000, height = 2000, res = 300, pointsize = 8)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width", main = "Optimal number of clusters based on the silhouette width method")
lines(1:10, sil_width)
dev.off()

#  visualize clusters in a lower dimensional space, 
# based on t-distributed stochastic neighborhood embedding, or t-SNE - https://lvdmaaten.github.io/tsne/ 
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = PMID_MeSH$PMID)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) +
ggtitle("t-distributed stochastic neighborhood embedding
clusters representation", subtitle = NULL)
ggsave('XXX _ Figure 6 _ t-SNE representation of clusters.png', 
       width=6, height=6, unit='in', dpi=300)
