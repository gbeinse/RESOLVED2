# setwd("")

require(ggplot2)

pubNdrugs = read.table(file = "pubmedNdrugs.txt", header = TRUE, as.is = TRUE, na.strings = "NA", sep = "\t")


sortPlot <- function(df) {
  ggplot(df, aes(x=drug, y=percentage)) + 
    geom_bar(stat="identity", width=.5, fill="tomato3") +
    scale_y_continuous(breaks=seq(0, 10, 0.25))
}


flat = pubNdrugs$Drugs[!is.na(pubNdrugs$Drugs)]

length(flat)

drug = as.vector(unlist(sapply(flat, function(X){
      strsplit(x = X, split = ";")
  })))

size_ofsample = length(pubNdrugs$PMID)

table_ofdrugs = table(drug)

top50 = as.data.frame((sort(table_ofdrugs,decreasing = TRUE))[1:50])
top50$percentage = 100*top50$Freq/size_ofsample

top200 = as.data.frame((sort(table_ofdrugs,decreasing = TRUE))[1:200])
top200$percentage = 100*top200$Freq/size_ofsample



theme_set(theme_bw())



sortPlot(top50) + theme(axis.text.x = element_text(angle=65, vjust=0.6)) + labs(title = "Top 50")
sortPlot(top200) + labs(title = "Top 200")
