library(ggplot2)
library(tidyverse)
library(dplyr)

scaffold <- read.table("Trinity_read_size.txt",header = F)

####scaffold####

max(scaffold$V2)
scaffold_hist=hist(scaffold$V2, breaks = 1000) #<-- bigger number - smaller bins
scaffold_hist$breaks
scaffold_hist$counts
cbind(scaffold_hist$breaks,scaffold_hist$counts)
write.table(scaffold_hist$counts, file="FR697_ntedit_k96_1000.txt", sep = "\t")

qplot(scaffold$V2, geom="histogram")

options(scipen=10000)
ggplot(data=scaffold, aes(V2)) + 
  geom_histogram(breaks=seq(0, max(scaffold$V2), by=1000), 
                 col="red", 
                 fill="green") + scale_y_log10() + 
  scale_x_continuous(name = "Assembled Transcripts size (bp)")

####>4000
scaffold_4000<-scaffold %>% filter(scaffold$V2 > 4000)
scaffold_1000<-scaffold %>% filter(scaffold$V2 > 1000)
scaffold_less_4000<-scaffold %>% filter(scaffold$V2 < 4000)

options(scipen=1000)
ggplot(data=scaffold_4000, aes(V2)) + 
  geom_histogram(breaks=seq(4000, max(scaffold_4000$V2), by=500), 
                 col="red", 
                 fill="green") + scale_y_log10() #+ scale_x_log10()

######OLD

scaffold_old <- read.table("old_Trinity_transcript_size.txt",header = F)

options(scipen=10000)
ggplot(data=scaffold_old, aes(V2)) + 
  geom_histogram(breaks=seq(0, max(scaffold_old$V2), by=1000), 
                 col="red", 
                 fill="cyan") + scale_y_log10() + 
  scale_x_continuous(name = "Assembled Transcripts size (bp)")

scaffold_old_4000<-scaffold_old %>% filter(scaffold_old$V2 > 4000)
scaffold_old_1000<-scaffold_old %>% filter(scaffold_old$V2 > 1000)
