library(ggplot2)
library(tidyverse)

#### rna_bloom ####
rnb <- read.table("rnabloom_dist.txt",header = F)

max(rnb$V1)
ggplot(rnb, aes(V1)) + geom_histogram(binwidth = 100,col="red",fill="green") +
  scale_x_continuous(name = "Assembled Transcripts size (bp)"
                     ,breaks = seq(0, max(rnb$V1), by=500)
                     ,limits=c(0, 9500)) +
  scale_y_continuous(name = "Frequency"
                     ,breaks = seq(0, 1000000, by=100000)
                     ,limits=c(0, 1000000))

####contig####
contig <- read.table("FR697-contigs.dist.txt",header = F)

max(contig$V1)
contig_hist=hist(contig$V1, breaks = 10000)
write.table(contig_hist$counts, file="contig_dist.txt", sep = "\t")


# ggplot(contig, aes(V1)) +
#   geom_histogram(binwidth = 100,col="red",fill="green") +
#   scale_x_continuous(name = "Assembled contig size (bp)"
#                      ,breaks = c(seq(0, 10000, by=100),max(contig$V1))
#                      ,limits=c(0, 1000))

ggplot(contig, aes(V1)) +
  geom_histogram(breaks=c(seq(0, 1000, by=100)),
                 col="red",
                 fill="green") +
  scale_x_continuous(name = "Assembled contig size (bp)",
                     breaks = c(seq(0, 1000, by=100)),
                     limits=c(0, 1000),
                     labels=c(seq(0,1000, by=100)))

##
ggplot(data=dfsac, aes(dfsac$visits)) + 
  geom_histogram(breaks=c(seq(0, 200, by=10)), 
                 col="black", 
                 fill="red") +
  labs(x="Visits", y="Count")+
  scale_x_continuous(limits=c(0, 200), 
                     breaks=c(seq(0, 200, by=10)), 
                     labels=c(seq(0,190, by=10), "200+"))

# ,breaks = seq(0, max(contig$V1), by=1000)

tc=table(contig$V1)
tcdt=as.data.frame(tc)

# +
  # scale_y_continuous(name = "Frequency"
  #                    ,breaks = seq(0, 1000000, by=100000)
  #                    ,limits=c(0, 1000000))


####scaffold####
scaffold <- read.table("FR697-scaffold.dist.txt",header = F)

max(scaffold$V1)
scaffold_hist=hist(scaffold$V1, breaks = 1000)
scaffold_hist$breaks
scaffold_hist$counts
write.table(scaffold_hist$counts, file="scaffold_dist_1000.txt", sep = "\t")

####unitig####
unitigs <- read.table("FR697-unitigs.dist.txt",header = F)

max(unitigs$V1)
unitigs_hist=hist(unitigs$V1, breaks = 10000)
unitigs_hist$breaks
unitigs_hist$counts
write.table(unitigs_hist$counts, file="unitigs_dist_1000.txt", sep = "\t")
###########
table(rnb$V1)
w=table(rnb$V1)

tw=as.data.frame(w)

write.table(tw, file="rna_bloom_dist.txt", sep = "\t")

ggplot(data=rnb, aes(rnb$V1)) + 
  geom_histogram(breaks=seq(0, max(rnb$V1), by=100), 
                 col="red", 
                 fill="green")+
  labs(title="Histogram for Age", x="Age", y="Count") + 
  xlim(c(0,)) +
  ylim(c(0,9500))
#######
