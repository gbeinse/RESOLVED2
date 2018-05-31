# setwd("")

require(ggplot2)

pubNdrugs = read.table(file = "PUBMED_DATA/pubmedNdrugs.txt", header = TRUE, as.is = TRUE, na.strings = "NA", sep = "\t")


sortPlot <- function(df) {
  theme_set(theme_bw())
  ggplot(df, aes(x=df[,1], y=Freq)) + 
    geom_bar(stat="identity", width=.5, fill="tomato3") + 
    labs(xlab = "colnames(df)[1]")
}


flat = pubNdrugs$Drugs[!is.na(pubNdrugs$Drugs)]

drug = as.vector(unlist(sapply(flat, function(X){
      s=strsplit(x = X, split = ";")[[1]]
      return(s[1])
  })))

size_ofsample = length(pubNdrugs$PMID)

table_ofdrugs = table(drug)

top50 = as.data.frame((sort(table_ofdrugs,decreasing = TRUE))[1:50])
top50$percentage = top50$Freq

top200 = as.data.frame((sort(table_ofdrugs,decreasing = TRUE))[1:200])
top200$percentage = top200$Freq







sortPlot(top50) + theme(axis.text.x = element_text(angle=65, vjust=0.6)) + labs(title = "Top 50")
sortPlot(top200) + labs(title = "Top 200")
