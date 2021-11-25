setwd("F:/actif E&R IGR DITEP/RESOLVED²/SCRIPTS_WD")

# load the 5 bases
# dataframe with PMID; Article_Title; YearPub - one row per PMID
base_f_2606 <- read.csv2("base_f_short_2606.txt", sep = "\t", header = TRUE)
# dataframe with Authors; PMID - one row per author
tbl_Auth_f_2606 <- read.csv2("tbl_Auth_f_2606.txt", sep = "\t", header = TRUE)
# dataframe with MeSH; Type ; PMID
tbl_MeSH_f_2606 <- read.csv2("tbl_MeSH_f_2606.txt", sep = "\t", header = TRUE)
# dataframe with PMID; Journal; Year_publication; Month_publication; Title; Abstract text
tbl_Abst_Corpus_f_2606 <- read.csv2("pubmed_data_2606.txt", sep = "\t", header = TRUE)
# dataframe with PMID and MeSH: one row per PMID; one column per MeSH
PMID_MeSH <- read.csv2("PMID_MeSH.txt", sep = "\t", header = TRUE, check.names=FALSE )


### package TM


tbl_Abst_Corpus_f_2606 <- read.csv2("pubmed_data_2606.txt", sep = "\t", header = TRUE)

docs <- head(tbl_Abst_Corpus_f_2606$Abstract, n = 20)
docs <- tbl_Abst_Corpus_f_2606$Abstract
#docs[[1]]
docs <- gsub("</AbstractText>:","", docs)
docs <- gsub("Clinicaltrials.gov","", docs)
docs <- gsub("\\."," ", docs)
docs <- gsub("\\("," ", docs)
docs <- gsub("\\)"," ", docs)
docs <- gsub("\\["," ", docs)
docs <- gsub("\\]"," ", docs)
docs <- gsub(";"," ", docs)
docs <- gsub(":"," ", docs)
#docs <- gsub("\\d"," ", docs)
docs <- gsub(",", " ", docs)
#  removePunctuation(docs, 
#                             preserve_intra_word_contractions = TRUE, 
 #                            preserve_intra_word_dashes = TRUE)
# docs <- gsub("[\\!\\\\\"\\#\\$\\&\\'\\*\\+\\,\\.\\)\\(:;\\<\\=\\>\\?\\@\\]\\[\\^\\`\\{\\|\\}\\~]","", docs)
library(tm)
corpus <- VCorpus(VectorSource(docs))
corpus <- tm_map(corpus, stripWhitespace)

# to lower case
# corpus <- tm_map(corpus, content_transformer(tolower))
# remove stop words & useless symbols / punctuation
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# word stemming -   
#corpus <- tm_map(corpus, stemDocument)

# visualize word cloud
library(SnowballC)
#install.packages("wordcloud")
library(wordcloud)

png(filename = "XXX _ Figure 9 _ Abstract word cloud unstemmed - 2606 abs.png",
    width = 1000, height = 1000, res = 300, pointsize = 8)
wordcloud(corpus, max.words = 150, random.order = FALSE, random.color = TRUE)
title("Abstract words cloud")
dev.off()

# create a document term matrix with words in columns and doc in row https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
#install.packages("tidytext")
library(tidytext)

dtm <- DocumentTermMatrix(corpus)
# inspect(dtm)
DF <- tidy(dtm)
View(DF)

DF_tabled <- as.data.frame(sort(table(DF$term), decreasing = TRUE))
DF_tabled <- DF_tabled[1:50,]

library(ggplot2)

ABSTRACT_WORD_TOP50 <-ggplot(DF_tabled, aes(x = DF_tabled$Var1, y = Freq)) +# data & aesthetics
  geom_histogram(stat="identity") +
  theme_bw() +
  ggtitle("Distribution of words used (top 50)", subtitle = NULL) +
  labs(x = "Words", y = "Number of mention")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('XXX _ Figure 10 _ Asbtract words used - top 50.png', width=16, height=8, unit='in', dpi=300)

# then analyse
findFreqTerms(dtm, 5)

# find associations wih the term "term"
findAssocs(dtm, "toxicity", 0.8)


# ALLER VOIR : METAMAP - nlm : outils d'annotation de texts basés sur l'umls: objectif trouver les concepts dans chaque abstract
# ALLER VOIR : BIOPORTAL - stanford : idem mais autre type de terminologie.
