library(ggplot2)
library(ggpubr)
library(dplyr)
#library(gridExtra)
#library(plotly)
#grid.arrange(collength, collength, ncol=2)
df<-read.csv(file.choose(), stringsAsFactors = FALSE) #, stringsAsFactors = FALSE
names<-as.data.frame(names(df))
df<-df[1:349,]

NumberofSpecies<-df %>% group_by(Species) %>% count()#ordersSpecies
NumberofSpecies<-as.data.frame(NumberofSpecies)

top<-ggbarplot(NumberofSpecies, x = "Species", y = "n",
          fill = "lightgray", 
          xlab = "Species", 
          ylab = "Number in Freezer",
          sort.val = "desc", # Sort in descending order
          top = 20,          # select top 20 most citated genes
          x.text.angle = 45  # x axis text rotation angle
)
top
ggarrange(top,b, ncol = 1)


dfsubsetwireframe<-df[!is.na(df$RW.area),]# selects ones that 
dfsubsetwireframe$Species

dfsubsetwireframe %>% group_by(Category) %>% count()
dfsubsetwireframe %>% group_by(Low.Hz) %>% count()
Species<-as.data.frame(dfsubsetwireframe$Species)
Speciescateg<-dfsubsetwireframe[,c("Species","Low.Hz","Category")]
write.csv(Speciescateg, file = "speciescateg.csv")
iris   %>%   group_by(Species)   %>%   summarise(.) 

# outline -----------------------------------------------------------------
-summary statistics and data wrangling
-scatterplots as a function of size (skull width)
-lm
-lm with family as a random factor
-pgls vs ols regressions
-
  
#distinct
df<-birddata[1:115,]
ddf<-distinct(df, Birdtree, .keep_all = TRUE) 

  # summarise levels----------------------------------------------------
NumberofOrders<-df %>% group_by(Order) %>% summarise()#orders
NumberofSpecies<-df %>% group_by(Species) %>% count()#ordersSpecies
NumberofFamilies<-ddf %>% group_by(Family) %>% count()#unniqe families listed
count_numberofFamilies<-NumberofFamilies %>% group_by(Order) %>% count()#families per order
NumberByaquatic_category<-t(ddf %>% group_by(Category) %>% count())#orders and families
NumberByaquatic_categorysp<-df %>% group_by(Category,Species) #%>% count())#orders and families
NumberoflowHzcomp<- ddf %>% group_by(Low.Hz) %>% count()#orders and families
df %>% group_by(Category, Species) %>% count()***Count duplicates of species***
  
  NumberIAC<-df %>% group_by(Species,IAC) %>% count()#ordersSpecies  
NumberIBP<-df %>% group_by(Species,IAC) %>% count()#ordersSpecies  

test<- df%>% group_by(Species) %>% summarise(IBP)
#mean()

# select columns ----------------------------------------------------------
select(iris, Sepal.Width, Petal.Length, Species)

dfna<-na.omit(df)
write.csv(df,"aug11backuprestored.csv")

# create log-transformed df of continuous measurements --------------------
continuousvars<-select(df,Bodymass_lit, Headmassnumbersonly, Skull.width..mm., Air.volume)
loggeddata <- log(continuousvars)
names(loggeddata) <- paste0(names(continuousvars), "_log")
dfaddlogged<-cbind(df,loggeddata)

# remove fluid-filled observations ----------------------------------------
fluidfilledremoved<-subset(dfaddlogged, fluid.filled != "fluid filled")
write.csv(fluidfilledremoved, "aug22data.csv")

# sort data according to varialbe -----------------------------------------
arrange(lowHzcomp,desc(Low.Hz))#flip arrangement

# cross table -------------------------------------------------------------
#table(birdear$IAC)
#table(birdear$IBP)
bar<- data.frame(table(df$Category,df$IAC))
table(birdear$Species,birdear$IAC)

table(birdear$Category,birdear$IBP)
table(birdear$Species,birdear$IBP)



