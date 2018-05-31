setwd("RESOLVED²")

require(ggplot2)

pubNdrugs = read.table(file = "PUBMED_DATA/pubmedNdrugs.txt", header = TRUE, as.is = TRUE, na.strings = "NA", sep = "\t")


freqPlot <- function(df) {
  theme_set(theme_bw())
  ggplot(df, aes(x=df[,1], y=Freq)) + 
    geom_bar(stat="identity", width=.5, fill="tomato3") + 
    xlab(label = "Drug alias") + 
    ylab(label = "Apparitions in abstracts")
}


flat = pubNdrugs$Drugs[!is.na(pubNdrugs$Drugs)]

drug = as.vector(unlist(sapply(flat, function(X){
      s=strsplit(x = X, split = ";")[[1]]
      return(s[1])
  })))

size_ofsample = length(pubNdrugs$PMID)

table_ofdrugs = table(drug)

top50 = as.data.frame((sort(table_ofdrugs,decreasing = TRUE))[1:50])


top200 = as.data.frame((sort(table_ofdrugs,decreasing = TRUE))[1:200])


zoom = as.data.frame((sort(table_ofdrugs,decreasing = TRUE))[1:100])




freqPlot(top50) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title = "Top 50 most studied drugs")


freqPlot(top200) + 
  theme(axis.text.x = element_blank()) + 
  labs(title = "Distribution of the apparition of the top 200 most studied drugs") + 
  scale_x_discrete(labels=waiver()) +
  xlab(label = "")
                    


freqPlot(zoom) + theme(axis.text.x = element_text(angle=65, vjust=0.6)) + labs(title = "30-70")

besttable = pubNdrugs[0, ]
for(pat in top50$drug) {
  besttable = rbind(besttable,pubNdrugs[grep(pattern = pat, x = pubNdrugs$Drugs, ignore.case = TRUE, fixed = TRUE),])
}


write.table(x = besttable, file = "top50.txt", sep = "\t")
