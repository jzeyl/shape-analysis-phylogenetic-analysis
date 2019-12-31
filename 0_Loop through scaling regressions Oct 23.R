library(ggplot2)
library(dplyr)

# loop through dependent variables
response_list <- c("mpg~ hp", "disp~ hp", "drat~ hp")

lm_todo <- c("log(Head.mass..g.) ~log(Bodymass_lit)", 
             "log(Skull.width..mm.) ~log(Head.mass..g.^(1/3))",
             "log(Columella.length.mm)~log(Skull.width..mm.)",
             "log(Columella.length.mm)~log(Head.mass..g.^(1/3))",
             "log(Columella.volume.mm3^(1/3))~log(Skull.width..mm.)",
             "log(Columella.volume.mm3)~log(Head.mass..g.)",
             "log(Air.volume^(1/3))~log(Skull.width..mm.)+Category",
             "log(Air.volume)~log(Head.mass..g.)+Category",
             "log(FP.area.points)~log(Skull.width..mm.^(2))",
             "log(FP.area.points)~log(Head.mass..g.^(2/3))",
             "log(Tympanic.membrane.area^0.5)~log(Skull.width..mm.)",
             "log(Tympanic.membrane.area)~log(Head.mass..g.^(2/3))",
             "log(RW.area^(1/2))~log(Skull.width..mm.)",
             "log(RW.area)~log(Head.mass..g.^(2/3))")

#to add: totalEClength~SW, 
#totalEClength~headmass
#meanangle~SW
#meanangle~headmass

#function to extract the coefficients of a linear regression
lmfunction<-function(i){
  lmfit <- coef(lm(as.formula(i), data = df))
  lmfit[2]# can switch out and repeat to get intercept vs the others
}

#apply the lm function, extracting the coeffiecient
all_lms_int<-lapply(lm_todo,lmfunction)
all_lms_int

#apply the lm function, extracting the coeffiecient#
all_lms_slope<-lapply(lm_todo,lmfunction)
all_lms_slope

#convert back to a dataframe
all_lms_intdf <- do.call(rbind.data.frame, all_lms_int)
all_lms_slopedf <- do.call(rbind.data.frame, all_lms_slope)

lms_export<-cbind(all_lms_intdf,all_lms_slopedf)
lms_export$variable<-lm_todo

#unname(all_lms_df[,1])

write.csv(lms_export, file = "lms export sept 25.csv")

for (y in lm_todo) {
  lmfit <- summary(lm(as.formula(y), data = df))
  print(y)
  print(lmfit$r.squared)
  print(lmfit$coefficients)
  print("")
}