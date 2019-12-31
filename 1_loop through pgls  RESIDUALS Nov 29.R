library(caper)
library(phytools)
library(ape)
library(dplyr)
library(ggpubr)
library(ggplot2)
#library(stringr)
outputpath<-"D:/0Earmeasurements/earmeasurements/R files/Plots"

#OUTLINE#
#LOOP PGLS VARIABLES TO REGRESS AGAINST SKULL WIDTH AND HEAD MASS
########TESTING#####basic PGLS formula and validation
##########function to run PGLS and extract values - slopes and lambda######
######run PGLS and print all summaries to copy to word file#######
#####################RESIDUALS - extract head size-correcteded PGLS residuals####################
#longform for facetgrid plotting
####################loop through plots to make contmap plots of residuals



#APPLICATION TO MY DATA##########
df<-read.csv(file.choose(), stringsAsFactors = FALSE) #, stringsAsFactors = FALSE
df<-df[1:97,]

distinctdf<-distinct(df, Birdtree, .keep_all = TRUE)
distinctdf$Binomial<-gsub(" ", "_", distinctdf$Birdtree)# change data names to match tree
distinctdf$area_ratio<-(distinctdf$Tympanic.membrane.area)/(distinctdf$FP.area.points)
distinctdf$Tympanic.membrane.area<-as.numeric(distinctdf$Tympanic.membrane.area)

str(distinctdf$Tympanic.membrane.area)
#remove fluid-filled for air volume analysis
fluidfilledremoved<-subset(distinctdf, fluid.filled.by.columella.round.window. != "fluid filled")
distinctdf$meanTMangle<-distinctdf$X.


#READ TREE
birdtree <-read.nexus(file.choose())
birdtreels<- ls.consensus(birdtree) # consensus reduces equally parsimonious phylogenies to one
plot(birdtreels)# plot simply

birdCDO<-comparative.data(phy = birdtreels,data = distinctdf, 
                          names.col = Binomial, 
                          vcv = TRUE, na.omit = FALSE, 
                          warn.dropped = TRUE)
birdCDO$dropped


#regressing variables against head size
pgls_todo <- c(#"log(Head.mass..g.)~log(Bodymass_lit)", 
               #"log(Skull.width..mm.)~log(Head.mass..g.^(1/3))",
               
               
               "log(Columella.length.mm)~log(Skull.width..mm.)",
               "log(Columella.length.mm)~log(Head.mass..g.^(1/3))",  
               
               "log(Columella.volume.mm3^(1/3))~log(Skull.width..mm.)",
               "log(Columella.volume.mm3)~log(Head.mass..g.)", 
               
               "log(totalEClength)~log(Skull.width..mm.)",
               "log(totalEClength)~log(Head.mass..g.^(1/3))",
               
               "log(meanTMangle)~log(Skull.width..mm.)",
               "log(meanTMangle)~log(Head.mass..g.)",
               
               "log(Air.volume^(1/3))~log(Skull.width..mm.)",
               "log(Air.volume)~log(Head.mass..g.)",#   
               
               "log(FP.area.points)~log(Skull.width..mm.^(2))",
               "log(FP.area.points)~log(Head.mass..g.^(2/3))",#    
               
               "log(Tympanic.membrane.area^0.5)~log(Skull.width..mm.)",
               "log(Tympanic.membrane.area)~log(Head.mass..g.^(2/3))",# 
               
               "log(RW.area^(0.5))~log(Skull.width..mm.)",
               "log(RW.area)~log(Head.mass..g.^(2/3))", 
               
               "log(Cochlear.aqueduct.area^(0.5))~log(Skull.width..mm.)",
               "log(Cochlear.aqueduct.area^(0.5))~log(Head.mass..g.^(2/3))",
               
               "log(area_ratio)~log(Skull.width..mm.^(2))",
               "log(area_ratio)~log(Head.mass..g.^(2/3))",
               
               "log(Umbo_distancetoTMplane)~log(Skull.width..mm.)",
               "log(Umbo_distancetoTMaplane)~log(Head.mass..g.^(1/3))",
               
               "log(coltip_distancetoTMplane)~log(Skull.width..mm.)",
               "log(coltip_distanctoTMplane)~log(Head.mass..g.^(1/3))",
               
               "log(angle_FP_TM)~log(Skull.width..mm.)",
               "log(angle_FP_TM)~log(Head.mass..g.)",
               
               "log(angle_Col_EC)~log(Skull.width..mm.)",
               "log(angle_Col_EC)~log(Head.mass..g.)",
               
               "log(ECD.length)~log(Skull.width..mm.)",
               "log(ECD.length)~log(Head.mass..g.^(1/3))"
)

#ADD: COLUMELLA VOLUME, COCHLEAR AQUEDUCT AREA,MEAN TM ANGLE, ECD LENGTH

distinctdf$meanTMangle<-distinctdf$meanTM


#other PGLS  
pgls_todo_category <- paste(pgls_todo,"+ Category")
pgls_todo_divingvsother<- paste(pgls_todo,"+ Pursuit.diving")
pgls_todo_odd<-pgls_todo[seq(1,length(pgls_todo),2)]
pgls_todo_odd_category<-pgls_todo_category[seq(1,length(pgls_todo_category),2)]

##################################Scaling WITHIN-EAR measurements #regressing variables against each other  
pglstodo_within_ear<-c(
  "log(Tympanic.membrane.area)~log(FP.area.points)",
  "log(RW.area)~log(FP.area.points)",
  "log(Columella.volume.mm3^(2/3))~log(Columella.length.mm)",
  "log(totalEClength)~log(Columella.length.mm)",
  "log(FP.area.points^(2))~log(Columella.length.mm")

########################################################3


########TESTING#####basic PGLS formula and validation
pglsmodel_test<-pgls(log(Air.volume)~log(Skull.width..mm.), 
                     data = birdCDO, lambda = 'ML')
summary(pglsmodel_test)
residuals(pglsmodel_test)
lmp<-lm(log(Air.volume)~log(Skull.width..mm.), data = distinctdf)
residuals(lmp)

#TO VALIDATE THE LIST OUTPUT, CHECK AT SPECIFIC POINTS IN LIST
test<-pgls(as.formula(pgls_todo_odd[2]), 
           data = birdCDO, 
           lambda = 'ML')
summary(test)




#####################RESIDUALS - extract head size-correcteded PGLS residuals####################
pgls_resid_outputs<-function(i){
  pglsfit<-pgls(as.formula(i), data = birdCDO, lambda = 'ML')
  resids<-residuals(pglsfit)
  #species<- row.names(resids)
  return(resids)
}
#apply the PGLS function to the list of formulas
all_pgls_resids<-lapply(pgls_todo_odd,pgls_resid_outputs)# returns a list of dataframes


pgls_resid_outputs(pgls_todo_odd[5])

#addin the residuals to df
pgls_todo_odd

#attach residuals to distinctdf data frame
distinctdf$V1<-all_pgls_resids[[1]][match(distinctdf$Binomial,row.names(all_pgls_resids[[1]]))]# attach
distinctdf$V2<-all_pgls_resids[[2]][match(distinctdf$Binomial,row.names(all_pgls_resids[[2]]))]
distinctdf$V3<-all_pgls_resids[[3]][match(distinctdf$Binomial,row.names(all_pgls_resids[[3]]))]
distinctdf$V4<-all_pgls_resids[[4]][match(distinctdf$Binomial,row.names(all_pgls_resids[[4]]))]
distinctdf$V5<-all_pgls_resids[[5]][match(distinctdf$Binomial,row.names(all_pgls_resids[[5]]))]
distinctdf$V6<-all_pgls_resids[[6]][match(distinctdf$Binomial,row.names(all_pgls_resids[[6]]))]
distinctdf$V7<-all_pgls_resids[[7]][match(distinctdf$Binomial,row.names(all_pgls_resids[[7]]))]
distinctdf$V8<-all_pgls_resids[[8]][match(distinctdf$Binomial,row.names(all_pgls_resids[[8]]))]
distinctdf$V9<-all_pgls_resids[[9]][match(distinctdf$Binomial,row.names(all_pgls_resids[[9]]))]
distinctdf$V10<-all_pgls_resids[[10]][match(distinctdf$Binomial,row.names(all_pgls_resids[[10]]))]
distinctdf$V11<-all_pgls_resids[[11]][match(distinctdf$Binomial,row.names(all_pgls_resids[[11]]))]
distinctdf$V12<-all_pgls_resids[[12]][match(distinctdf$Binomial,row.names(all_pgls_resids[[12]]))]
distinctdf$V13<-all_pgls_resids[[13]][match(distinctdf$Binomial,row.names(all_pgls_resids[[13]]))]
distinctdf$V14<-all_pgls_resids[[14]][match(distinctdf$Binomial,row.names(all_pgls_resids[[14]]))]
distinctdf$V15<-all_pgls_resids[[15]][match(distinctdf$Binomial,row.names(all_pgls_resids[[15]]))]
#distinctdf$V16<-all_pgls_resids[[16]][match(distinctdf$Binomial,row.names(all_pgls_resids[[16]]))]

#check column numbers of added residuals
names(distinctdf)

#colnames(distinctdf)
#colnames(distinctdf[,61:71])<-(pgls_todo_odd[1:11])
oldnames = colnames(distinctdf[,62:76])
newnames = paste("A", str_replace_all(pgls_todo_odd, "[^[:alnum:]]", "_"))
#rename residual columns
#residformula_lets<-paste("A",as.character(i),str_replace_all(residformula, "[^[:alnum:]]", "_"))

dfwithresids<-distinctdf %>% rename_at(vars(oldnames), ~ newnames)#dplyr function

#write.csv(distincdfcorrres, "withpglsresiduals.csv")
#getwd()
distinctdf<-dfwithresids

str((distincdfcorrres[62:72]))
resids<-distincdfcorrres[62:72]
head(distinctdf)
str(distinctdf$V1)
names(distincdfcorrres)
names(resids)
names(distinctdf)

names(distinctdf[62:76])
oldnames = colnames(distinctdf[,61:76])
pgls_todo_odd
newnames<-c("PGLSresid_HM_BM",
"PGLSresid_CL_SW",
"PGLSresid_CV_SW",
"PGLSresid_EClength_SW",
"PGLSresid_meanTMangle_SW",
"PGLSresid_airvolume_SW",
"PGLSresid_FParea_SW",
"PGLSresid_TMarea_SW",
"PGLSresid_RWarea_SW",
"PGLSresid_CAarea_SW",
"PGLSresid_arearatio_SW",
"PGLSresid_Umbodistance to TM plane",
"PGLSresid_coltip to TMplane",
"PGSLresid_angle_FP_TM",
"PGLSresid_angle_col_EC",
"PGLSresid_ECDlength")
distinctdf<-distinctdf %>% rename_at(vars(oldnames), ~ newnames)#dplyr function
names(distinctdf)
index<-62:76
factindex<-c(12,14)
pdf(paste("J:/","nestedboth3_PGLS residuals ear measures regressed vs skull width",".pdf"), onefile = TRUE)
library(ggpubr)
library(EnvStats)

for (i in index){
  for (j in factindex){
  print(ggplot(data = distinctdf, aes_string(x = names(distinctdf[j]), y = names(distinctdf[i]))) + 
                         geom_boxplot() +
          #stat_compare_means(method = "t.test")+
          stat_n_text()+
          theme_bw() 
                       )
    }
    }
dev.off()



#str(distincdfcorrres)


######################longform for facetgrid plotting
names(distinctdf[62:71])
names(distinctdf)
forlongresids<-distinctdf[,c(1, 12,13,14,62:76)]
forlongresids_sig<-distinctdf[,c(1, 12,13,14,66,71,70,67,65,64,68)]
forlongresids_raw_sig<-distinctdf[,c(1,12,13,14,22,25,41,56,55,50,47,54)]#, 25,,49,52,53,45,46

forlongresids_nonsig<-distinctdf[,c(1,12,13,14,62,63,69)]
forlongresids_raw_all<-distinctdf[,c(1,12,13,14,22,25,41,56,55,50,47,54,49,52,53,45,46,36,37,57)]#, 25,

names(forlongresids_raw)
names(forlongresids)

#subset terrestrial only
forlongresidsTerronly<-subset(forlongresids, Category != "Pursuit diving")
forlongresidsTRUE_Terronly<-subset(forlongresidsTerronly, Category != "Surface foraging")
forlongresidsTRUE_Terronly$Birdtree

library(tidyr)
longresids<-gather(forlongresids, key = "earmeasure", 
                   value = "PGLS_residuals_value", -c(Category, 
                                       Birdtree, Pursuit.diving, Low.Hz))

longresids_ns<-gather(forlongresids_nonsig, key = "earmeasure", 
                   value = "PGLS_residuals_value", -c(Category, 
                                                      Birdtree, Pursuit.diving, Low.Hz))

longresids_raw_all<-gather(forlongresids_raw_all, key = "earmeasure", 
                      value = "value", -c(Category, 
                                              Birdtree, Pursuit.diving, Low.Hz,
                                              Skull.width..mm.))


names(longresids)
head(longresids)

install.packages("EnvStats")
library(EnvStats) #for stats
library(ggplot2)

my_comparisons <- list( c("Pursuit diving", "Terrestrial"), 
                        c("Pursuit diving", "Surface foraging"), 
                        c("Surface foraging", "Terrestrial") )
#longresids$Low_Hz<-gsub("","Other", longresids$Low.Hz)
my_comparisons <- list( c("Low.Hz", "Other"))
str(longresids)

#use comparisons = my_comparisons inside stat_compare_means for multiple

#panel plot residuals by category
p2<- ggplot(data = longresids, aes(x = Category, y = PGLS_residuals_value, factor = Category)) + 
  geom_boxplot(aes(fill = Category)) +
  stat_compare_means(comparisons = my_comparisons)+
  stat_n_text() +
  facet_wrap(~earmeasure) +
  geom_smooth(method = "lm", col = "black") +
  theme_bw() + 
  #ggtitle("Terrestrial only")
  scale_fill_manual(values=c("blue","grey","lightgreen","black","gray","green","darkgray","darkgreen","cornsilk4","blue"))
#scale_color_manual(values=c("blue","grey","lightgreen","black","gray","green","darkgray","darkgreen","cornsilk4","blue"))
p2

longdistinctdf<-

scatter<- ggplot(data = longresids_raw_all, aes(x = log(Skull.width..mm.), y = log(value), factor = Category)) + 
  geom_point(aes(color = Category)) +
  #stat_compare_means(comparisons = my_comparisons)+
  #stat_n_text() +
  facet_wrap(~earmeasure, scales = "free") +
  geom_smooth(method = "lm", col = "black") +
  theme_bw() + 
  #ggtitle("Terrestrial and surface foraging only")
  scale_color_manual(values=c("blue","grey","lightgreen","black","gray","green","darkgray","darkgreen","cornsilk4","blue"))
#scale_color_manual(values=c("blue","grey","lightgreen","black","gray","green","darkgray","darkgreen","cornsilk4","blue"))
scatter

ggarrange(scatter,p2, ncol = 1)


pdf

##########################################

#try swappin out each
names(all_pgls_resids[[1]])
str(all_pgls_resids[[1]])# should be named numeric


####gsub('[0-9]+', '', x) to add removal of special characters to save residformula
library(stringr)
pgls_todo
index<-1:16

####################loop through plots to make contmap plots of residuals
#for (i in 1:length(pgls_todo)){
#  residformula<-pgls_todo[[i]]
#  residformula_lets<-paste("O",as.character(i),str_replace_all(residformula, "[^[:alnum:]]", ""))
#  resids_eachvar<-as.numeric(all_pgls_resids[[i]])
#  phy<-keep.tip(birdtreels, names(speciesnames))
#  obj<-contMap(phy,speciesnames)#basically combines the tree + variable
#  plot(obj, type = "fan")#contmap plot
#  
#  #plot(obj, type = "fan")
#  #par(mfrow=c(2,1))
#  pdf(paste("J:/",residformula_lets,".pdf"), onefile = TRUE)
#  phenogram<-phenogram(phy,speciesnames)
#  plot(phenogram)
#  
#  dev.off()
#}


pdf(paste("J:/","Contmap and phenogramsNov 29",".pdf"), onefile = TRUE)
par(mfrow=c(2,1))
for (i in 1:length(pgls_todo)){
  residformula<-pgls_todo[[i]]
  residformula_lets<-paste("A",as.character(i),str_replace_all(residformula, "[^[:alnum:]]", ""))
  #resids_eachvar<-as.numeric(all_pgls_resids[[i]])
  spnames<-setNames(all_pgls_resids[[i]],row.names(all_pgls_resids[[i]]))
  phy<-keep.tip(birdtreels, names(spnames))
  obj<-contMap(phy,spnames)#basically combines the tree + variable
  title(pgls_todo[[i]])
  #plot(obj, type = "fan")

  #pdf(paste("J:/","A",residformula_lets,".pdf"), onefile = TRUE)
  phenogram(phy,spnames)
  #plot(obj)#contmap plot
  title(pgls_todo[[i]])
  #plot(phy)
    #dev.off()
}
dev.off()

pgls_todo[[1]]
residformula_lets<-paste("O",as.character(i),str_replace_all(residformula, "[^[:alnum:]]", ""))

PGLS_TM_SW<-pgls(log(Tympanic.membrane.area)~log(Skull.width..mm.)+Category, data = birdCDO, lambda = 'ML')
spnames<-setNames(TM_res,row.names(residuals(PGLS_TM_SW)))
phy_naomit<-keep.tip(birdtreels, names(spnames))
obj<-contMap(phy_naomit,TM_res)#basically combines the tree + variable
plot(obj, type = "fan")
phenogram<-phenogram(phy_naomit,residuals(PGLS_TM_SW))


par(mfrow=c(2,1))
pdf(paste("C:/Users/jeffzeyl/Desktop/","a",".pdf"), onefile = TRUE)
plot(obj, type = "fan")
phenogram(phy_naomit,TM_res,fsize=0.6,spread.costs=c(1,0), colors = cols)
dev.off()

str(pglsmodel_test)
getwd()
