require(ggplot2)

miss = read.table(file = "PUBMED_DATA/pubmed_data_2606_noDRUG.txt", header = TRUE, as.is = TRUE, na.strings = "NA", sep = "\t")
pubmed = read.table(file = "PUBMED_DATA/pubmed_data_2606.txt", as.is = TRUE)




nodrug = as.data.frame((table(miss$Year_publication)))
colnames(nodrug) = c("Publication_Year", "Freq")


all = as.data.frame((table(pubmed$Year_publication)))
colnames(all) = c("Publication_Year", "Freq")



df1 <- data.frame(Match=c(rep("No_Match",times = length(nodrug$Publication_Year)), rep("All",times = length(all$Publication_Year))),
                  Publication_Year=c(as.numeric(as.character(nodrug$Publication_Year)), as.numeric(as.character(all$Publication_Year))),
                  Freq=c(nodrug$Freq, all$Freq))

df2 <- data.frame(Match=c(rep("No_Match",times = length(nodrug$Publication_Year)), rep("All",times = length(all$Publication_Year))),
                  Publication_Year=c(as.numeric(as.character(nodrug$Publication_Year)), as.numeric(as.character(all$Publication_Year))),
                  Freq=c(nodrug$Freq/length(miss$PMID), all$Freq/length(pubmed$PMID)))

df3 <- data.frame(Match=c(rep("No_Match",times = length(nodrug$Publication_Year))),
                  Publication_Year=c(as.numeric(as.character(nodrug$Publication_Year))),
                  Freq=c(nodrug$Freq))






ggplot(data=df1, aes(x=Publication_Year, y=Freq, fill=Match)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  geom_smooth(method='auto')

ggplot(data=df2, aes(x=Publication_Year, y=Freq, fill=Match)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+ 
  geom_smooth(method='auto')

ggplot(data=df3, aes(x=Publication_Year, y=Freq, fill=Match)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+ 
  geom_smooth(method='auto')
