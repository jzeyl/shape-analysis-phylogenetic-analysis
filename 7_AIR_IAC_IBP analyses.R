library(ggpubr)
library(ggplot2)
library(EnvStats)
?ggarrange()
df<-read.csv(file.choose())
names<-as.data.frame(names(df))
df<-df[1:88,]
df<-dplyr::distinct(df, Birdtree, .keep_all = TRUE)


# IBP/IAC analysis --------------------------------------------------------

multiplot(IAC_bar, IAClogreg, IBP_bar, IBPlogreg, cols=2)
distinctdf<-distinct(df, Birdtree, .keep_all = TRUE)

#remove fluid filled observations
fluidfilledremoved<-subset(df, fluid.filled.by.columella.round.window. != "fluid filled")


# remove duplicate species ------------------------------------------------
# remove repeated measurements from different species ----------------------
#library(stringr)
#IACsingles<-str_which(fluidfilledremoved$Birdtree, "Razorbill")#list from summarize functions
#IACsingles
#ff_rem_IACsingles<- fluidfilledremoved[-c(6,9,8,12,5,45),]
#write.csv(ff_rem_IACsingles, "ff_rem_IACsingles.csv")
#sum<-ff_rem_IACsingles %>% group_by(Birdtree) %>% count()

# select IAC/IBP rows -------------------------------------------------------------
dfIAC<- subset(fluidfilledremoved, IAC == "Y" | IAC == "N")#select data for IAC analysis
dfIBP<- subset(fluidfilledremoved, IBP == "Y" | IBP == "N")
repeatIAC<- dfIAC%>%group_by(Species) %>% count()#species with doubles

y_IAC<-fluidfilledremoved[fluidfilledremoved$IAC =="Y"]
fluidfilledremoved$IAC

# bar plot  --------------------------------------------------------------
#IBP
IBP_bar<- ggplot(data = dfIBP, aes(x = Category, fill = IBP)) + 
  geom_bar(stat = "count", position = "fill") + 
  #geom_text(stat = "count") +
  theme_classic() + 
  scale_fill_manual(values = c("red","black")) +#+ coord_flip()
  ggtitle("Presence/Absence of interbullar passage") 
IBP_bar

#IAC
IAC_bar<- ggplot(data = dfIAC, aes(x = Category, fill = IAC)) + 
  geom_bar(position = "fill") + theme_classic() + 
  stat_summary(geom = "text")+
  scale_fill_manual(values = c("red","grey")) +#+ coord_flip()
  ggtitle("Presence/Absence of interaural canal") 
IAC_bar 

ggarrange(IBP_bar,IAC_bar)

noIAC<- subset(ff_rem_IACsingles, IAC == "N")

# IBP analysis ------------------------------------------------------------
dfIBP$IBPnumbY<-gsub("Y", "1", dfIBP$IBP)
dfIBP$IBPnumbN<-as.numeric(gsub("N", "0", dfIBP$IBPnumbY))

# IBP log regression ------------------------------------------------------
g<-glm(IBPnumbN~Air.volume,family=binomial,dfIBP) # run a logistic regression model (in this case, generalized linear model with logit link). see ?glm
summary(g)
IBPlogreg<-ggplot(dfIBP, aes(x=log(Air.volume), y = IBPnumbN)) +
  geom_point() + geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "black") + 
  theme_bw()
IBPlogreg

# IAC analysis ------------------------------------------------------------
dfIAC$IACnumbY<-gsub("Y", "1", dfIAC$IAC)
dfIAC$IACnumbN<-as.numeric(gsub("N", "0", dfIAC$IACnumbY))

# IBP log regression ------------------------------------------------------
g<-glm(IACnumbN~Air.volume,family=binomial,dfIAC) # run a logistic regression model (in this case, generalized linear model with logit link). see ?glm
summary(g)
IAClogreg<-ggplot(dfIAC, aes(x=log(Air.volume), y = IACnumbN)) +
  geom_point() + geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "black") + 
  theme_bw()
IAClogreg


# X^2 association IBP and A_T categories ----------------------------------
tbl_IBP<-table(dfIBP$Category, dfIBP$IBP)
tbl_IBP
chisq.test(tbl_IBP)

tbl_IAC<-table(dfIAC$Category, dfIAC$IAC)
tbl_IAC
chisq.test(tbl_IAC)

