library(ape)
library(phytools)
library(ggtree)
library(ggplot2)
library(dplyr)
library(tidyr)

birdphy<-read.nexus(file.choose())
birdphy_consensusls<- ls.consensus(birdphy)
plot(birdphy_consensusls)
edgelabels()
nodelabels()


#load dataset
birddata<-read.csv(file.choose(), stringsAsFactors = FALSE)
birddata<-birddata[1:90,]
birddata<-birddata[,-c(1)]
birddata<-birddata[,-c(7-12)]
names(birddata)
birddata$Birdtree<-gsub(" ", "_", birddata$Birdtree)#make the underscore to match the birdtree output

annot<-cbind(birddata$Species.example,birddata$Sampled)

#compare species names
namediffs<-setdiff(birddata$Birdtree, birdphy_consensusls$tip.label)
namediffs

spnames<-data.frame(cbind(birddata$Species_scientific_name,birdphy_consensusls$tip.label,namediffs))
spnamediffs<-write.csv(spnames, "spnamediffs.csv")


birddata$fullname<-paste(birddata$Common.name, birddata$High_low_example, birddata$Order, sep = " - ")

#without clade labels
ppiped <- ggtree(birdphy_consensusls) %<+% birddata + 
  geom_tiplab(aes(label=Birdtree), offset = 6) + 
  xlim(NA, 400) + 
  geom_tippoint(aes(color = Category, shape = Sampled.or.in.process.of.sampling.head), size = 3)
ppiped
ppiped + theme(legend.position="right") + scale_color_manual(values=c("green","darkolivegreen","lightgreen","black","gray","green","darkgray","darkgreen","cornsilk4","blue"))

#save your work
write.csv(birddata,"birddataMar14b.csv")
write.nexus(birdphy_consensusls, file = "birdphyconsensusMar14.nex")



colnames(birddata) <- birddata[1,] #*****column names are the first row****
birddata <- birddata[-1,]#rem #so far so good

#ggtree
p1 <- ggtree(birdphy_consensusls) + geom_tiplab(offset = 2) + geom_tippoint() + geom_hilight(node=48, fill="steelblue", alpha=.6) + geom_hilight(node=46, fill="blue", alpha=.6) + geom_hilight(node=34, fill="green", alpha=.6) + geom_hilight(node=49, fill="purple", alpha=.6) + xlim(NA, 600)
p1


#heatmap - makes grid at least
p2 <- gheatmap(p1, birddata, offset = 120, width=1.1, font.size=3,
               colnames=TRUE, colnames_angle=90, low = "white", high = "red", color = "black",
               colnames_position = "top", colnames_offset_y = -0.2,
               hjust=0) + scale_fill_manual(values=c("blue", "red"))
    theme(plot.margin = unit(c(2,1,2,1), "cm")) + ylim(NA, 50)
p2

#add dataframe
ppiped <- ggtree(birdphy_consensusls) %<+% birddata + geom_tiplab(aes(label=common.name), offset = 6) + xlim(NA, 250) + geom_cladelabel(node=74, label="Paleognathous", offset = 70, align=T, hjust='center', offset.text = 35) + geom_cladelabel(node=71, label="Neognathous", offset = 70, align=T, hjust='center', offset.text = 35) + geom_tippoint(aes(color = Low_Hz), size = 3)
ppiped
ppiped + theme(legend.position="right")



geom_cladelabel(node=45, label="test label", align=T, angle=270, hjust='center', offset.text=.5) 
