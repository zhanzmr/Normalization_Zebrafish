## Loading Librarys
library(Hotelling)
library(ggplot2)
library(qcc)
library(gplots)
#source("myImagePlot.R")   ## plot image
library(readxl)  ## read the light intensity data
library(ggthemes)


## lighton stimulus
lt.on = read.table("lt.on.txt", header = T)

light_intensity <- read_excel("light_intensity.xlsx")
## creat lightintensity and attach it to lt.on data frame
lt.on$lightintensity = rep(0,dim(lt.on)[1])
for (jj in 1:96){
  lt.on$lightintensity[which(lt.on$lights=='On'&lt.on$location== levels(lt.on$location)[jj]) ] = light_intensity$`intensity (W/m^2)`[jj]
}


###### Plot the original data #########
##Light On Data
sub.on.3 = lt.on[ lt.on$genotype == "TL" & lt.on$stage == 3 , c(7,10)]
sub.on.6 = lt.on[ lt.on$genotype == "TL" & lt.on$stage == 6 , c(7,10)]
sub.on.9 = lt.on[ lt.on$genotype == "TL" & lt.on$stage == 9 , c(7,10)]

ndt = data.frame(stage_3dpf = rep(0,60),
                 SE1 = rep(0,60),
                 stage_6dpf = rep(0,60),
                 SE2 = rep(0,60), stage_9dpf = rep(0,60),
                 SE3 = rep(0, 60), time = c(-29:30))

for(t in -29:30){
  ndt[t+30,1] = mean(sub.on.3[ sub.on.3$time == t , 2])
  ndt[t+30,2] = sqrt( var(sub.on.3[ sub.on.3$time == t, 2])/length( sub.on.3[ sub.on.3$time == t, 2] ) )
  
  ndt[t+30,3] = mean(sub.on.6[ sub.on.6$time == t , 2])
  ndt[t+30,4] = sqrt( var(sub.on.6[ sub.on.6$time == t, 2])/length( sub.on.6[ sub.on.6$time == t, 2] ) )
  
  ndt[t+30,5] = mean(sub.on.9[ sub.on.9$time == t , 2])
  ndt[t+30,6] = sqrt( var(sub.on.9[ sub.on.9$time == t, 2])/length( sub.on.9[ sub.on.9$time == t, 2] ) )
}

rib.off1 <- aes(ymax = stage_3dpf + SE1, ymin = stage_3dpf - SE1)
rib.off2 <- aes(ymax = stage_6dpf + SE2, ymin = stage_6dpf - SE2)
rib.off3 <- aes(ymax = stage_9dpf + SE3, ymin = stage_9dpf - SE3)
gra = ggplot(data = ndt, aes(time)) +
  geom_line(aes(y = stage_3dpf, colour = "3dpf")) +
  geom_ribbon(rib.off1, fill="red", alpha = I(1/10)) +
  geom_line(aes(y = stage_6dpf, colour = "6dpf")) +
  geom_ribbon(rib.off2, fill="green",  alpha = I(1/10)) +
  geom_line(aes(y = stage_9dpf, colour = "9dpf")) +
  geom_ribbon(rib.off3, fill="blue",  alpha = I(1/10)) +
  scale_color_discrete(name="Stage") +
  scale_x_continuous(name="Time(s)") +
  scale_y_continuous(name="Activity", limits=c(0.00,0.15)) +
  theme_set(theme_grey(base_size = 15)) +
  ## change input here  ==========------------------------------>
  labs(title = "Light-On VMR of strain TL (Original)")+
  theme(legend.position=c(.5,0.5))
gra_Original_on = gra + theme_bw()+  theme(legend.position=c(.11,0.8)) +theme(axis.title=element_text(size=14), plot.title = element_text(size=13))


mu = 0.06   # offset value

##### Light intensity for all ###########################################################
############Light On Data (Light Intensity)#########
#light-intensity normalized data
subdata.stage3 = subset(lt.on, genotype == "TL"& stage == 3)
subdata.stage6 = subset(lt.on, genotype == "TL"& stage == 6)
subdata.stage9 = subset(lt.on, genotype == "TL"& stage == 9)

#subdata.stage3.lightOn = subset(lt.on, genotype == "AB"& stage == 3&lights=='On')
#subdata.stage6.lightOn = subset(lt.on, genotype == "AB"& stage == 6&lights=='On')
#subdata.stage9.lightOn = subset(lt.on, genotype == "AB"& stage == 9&lights=='On')

####### Stage ==3
# #linear Regression Model 
light.lm3 = lm(burdur~lightintensity,data= subdata.stage3)
#Take the residuals of this model (first stage linear regression)
#add light off part (red part)
blue_burdur_normalized = light.lm3$residuals
subdata.stage3$burdur_normalized = light.lm3$residuals
subdata.stage3$burdur_normalized = subdata.stage3$burdur_normalized + mu

######Stage==6
# #linear Regression Model 
light.lm6 = lm(burdur~lightintensity,data= subdata.stage6)
#Take the residuals of this model (first stage linear regression)
#add light off part (red part)
blue_burdur_normalized = light.lm6$residuals
subdata.stage6$burdur_normalized = light.lm6$residuals
subdata.stage6$burdur_normalized = subdata.stage6$burdur_normalized + mu

####### Stage == 9
# #linear Regression Model 
light.lm9 = lm(burdur~lightintensity,data= subdata.stage9)
#Take the residuals of this model (first stage linear regression)
#add light off part (red part)
blue_burdur_normalized = light.lm9$residuals
subdata.stage9$burdur_normalized = light.lm9$residuals
subdata.stage9$burdur_normalized = subdata.stage9$burdur_normalized + mu

##### ggplot
sub.light.3 = subdata.stage3[c(7,14)]
sub.light.6 = subdata.stage6[c(7,14)]
sub.light.9 = subdata.stage9[c(7,14)]

ndt = data.frame(stage_3dpf = rep(0,60),
                 SE1 = rep(0,60),
                 stage_6dpf = rep(0,60),
                 SE2 = rep(0,60), stage_9dpf = rep(0,60),
                 SE3 = rep(0, 60), time = c(-29:30))

for(t in -29:30){
  ndt[t+30,1] = mean(sub.light.3[ sub.light.3$time == t , 2])
  ndt[t+30,2] = sqrt( var(sub.light.3[ sub.light.3$time == t, 2])/length( sub.light.3[ sub.light.3$time == t, 2] ) )
  
  ndt[t+30,3] = mean(sub.light.6[ sub.light.6$time == t , 2])
  ndt[t+30,4] = sqrt( var(sub.light.6[ sub.light.6$time == t, 2])/length( sub.light.6[ sub.light.6$time == t, 2] ) )
  
  ndt[t+30,5] = mean(sub.light.9[ sub.light.9$time == t , 2])
  ndt[t+30,6] = sqrt( var(sub.light.9[ sub.light.9$time == t, 2])/length( sub.light.9[ sub.light.9$time == t, 2] ) )
}

rib.off1 <- aes(ymax = stage_3dpf + SE1, ymin = stage_3dpf - SE1)
rib.off2 <- aes(ymax = stage_6dpf + SE2, ymin = stage_6dpf - SE2)
rib.off3 <- aes(ymax = stage_9dpf + SE3, ymin = stage_9dpf - SE3)
gra = ggplot(data = ndt, aes(time)) +
  geom_line(aes(y = stage_3dpf, colour = "3dpf")) +
  geom_ribbon(rib.off1, fill="red", alpha = I(1/10)) +
  geom_line(aes(y = stage_6dpf, colour = "6dpf")) +
  geom_ribbon(rib.off2, fill="green",  alpha = I(1/10)) +
  geom_line(aes(y = stage_9dpf, colour = "9dpf")) +
  geom_ribbon(rib.off3, fill="blue",  alpha = I(1/10)) +
  scale_color_discrete(name="Stage") +
  scale_x_continuous(name="Time(s)") +
  scale_y_continuous(name="Activity", limits=c(0.00,0.15)) +
  theme_set(theme_grey(base_size = 18)) +
  ## change input here  ==========------------------------------>
  labs(title = "Light-On VMR of strain TL (Light Intensity)")+
  theme(legend.position=c(.5,0.5))
gra_ALL_light_on = gra + theme_bw()+  theme(legend.position=c(.11,0.8)) +theme(axis.title=element_text(size=14), plot.title = element_text(size=13))


#Batch Effect#####
## Stage==3
#Linear Regression to remove batch effect.
batch.lm3 =  lm(burdur~factor(replicate),data = subdata.stage3)
subdata.stage3$residuals =  batch.lm3$residuals
subdata.stage3$residuals = subdata.stage3$residuals + mu

## Stage==6
#Linear Regression to remove batch effect.
batch.lm6 =  lm(burdur~factor(replicate),data = subdata.stage6)
subdata.stage6$residuals =  batch.lm6$residuals
subdata.stage6$residuals = subdata.stage6$residuals + mu

## Stage ==9
#Linear Regression to remove batch effect.
batch.lm9 =  lm(burdur~factor(replicate),data = subdata.stage9)
subdata.stage9$residuals =  batch.lm9$residuals
subdata.stage9$residuals = subdata.stage9$residuals + mu

##### ggplot
sub.batch.3 = subdata.stage3[c(7,15)]
sub.batch.6 = subdata.stage6[c(7,15)]
sub.batch.9 = subdata.stage9[c(7,15)]

ndt = data.frame(stage_3dpf = rep(0,60),
                 SE1 = rep(0,60),
                 stage_6dpf = rep(0,60),
                 SE2 = rep(0,60), stage_9dpf = rep(0,60),
                 SE3 = rep(0, 60), time = c(-29:30))

for(t in -29:30){
  ndt[t+30,1] = mean(sub.batch.3[ sub.batch.3$time == t , 2])
  ndt[t+30,2] = sqrt( var(sub.batch.3[ sub.batch.3$time == t, 2])/length( sub.batch.3[ sub.batch.3$time == t, 2] ) )
  
  ndt[t+30,3] = mean(sub.batch.6[ sub.batch.6$time == t , 2])
  ndt[t+30,4] = sqrt( var(sub.batch.6[ sub.batch.6$time == t, 2])/length( sub.batch.6[ sub.batch.6$time == t, 2] ) )
  
  ndt[t+30,5] = mean(sub.batch.9[ sub.batch.9$time == t , 2])
  ndt[t+30,6] = sqrt( var(sub.batch.9[ sub.batch.9$time == t, 2])/length( sub.batch.9[ sub.batch.9$time == t, 2] ) )
}

rib.off1 <- aes(ymax = stage_3dpf + SE1, ymin = stage_3dpf - SE1)
rib.off2 <- aes(ymax = stage_6dpf + SE2, ymin = stage_6dpf - SE2)
rib.off3 <- aes(ymax = stage_9dpf + SE3, ymin = stage_9dpf - SE3)
gra = ggplot(data = ndt, aes(time)) +
  geom_line(aes(y = stage_3dpf, colour = "3dpf")) +
  geom_ribbon(rib.off1, fill="red", alpha = I(1/10)) +
  geom_line(aes(y = stage_6dpf, colour = "6dpf")) +
  geom_ribbon(rib.off2, fill="green",  alpha = I(1/10)) +
  geom_line(aes(y = stage_9dpf, colour = "9dpf")) +
  geom_ribbon(rib.off3, fill="blue",  alpha = I(1/10)) +
  scale_color_discrete(name="Stage") +
  scale_x_continuous(name="Time") +
  scale_y_continuous(name="Activity", limits=c(0.00,0.15)) +
  theme_set(theme_grey(base_size = 15)) +
  ## change input here  ==========------------------------------>
  labs(title = "Light-On VMR of strain TL (Batch Effect)")+
  theme(legend.position=c(.5,0.5))
gra_batch_on = gra + theme_bw()+  theme(legend.position=c(.11,0.8)) +theme(axis.title=element_text(size=14), plot.title = element_text(size=13))


## Baseline Normalized Data ###

##Stage == 3
# Grand mean of Beta.
beta = 0.01024
# Light off subset.
subdata.stage3.lightoff = subset(subdata.stage3,lights=='Off')
# Pull the light off part together with estimate beta calculation
est_beta_stage3 = mean(subdata.stage3.lightoff$burdur) - beta
# Calculate estimate Burdur using the estimate beta
subdata.stage3$est_burdur = subdata.stage3$burdur - est_beta_stage3
subdata.stage3$est_burdur = subdata.stage3$est_burdur + mu

##Stage == 6
# Light off subset.
subdata.stage6.lightoff = subset(subdata.stage6,lights=='Off')
# Pull the light off part together with estimate beta calculation
est_beta_stage6 = mean(subdata.stage6.lightoff$burdur) - beta
# Calculate estimate Burdur using the estimate beta
subdata.stage6$est_burdur = subdata.stage6$burdur - est_beta_stage6
subdata.stage6$est_burdur = subdata.stage6$est_burdur + mu

##Stage == 9
# Light off subset.
subdata.stage9.lightoff = subset(subdata.stage9,lights=='Off')
# Pull the light off part together with estimate beta calculation
est_beta_stage9 = mean(subdata.stage9.lightoff$burdur) - beta
# Calculate estimate Burdur using the estimate beta
subdata.stage9$est_burdur = subdata.stage9$burdur - est_beta_stage9
subdata.stage9$est_burdur = subdata.stage9$est_burdur + mu

## ggPlot
sub.baseline.3 = subdata.stage3[c(7,16)]
sub.baseline.6 = subdata.stage6[c(7,16)]
sub.baseline.9 = subdata.stage9[c(7,16)]

ndt = data.frame(stage_3dpf = rep(0,60),
                 SE1 = rep(0,60),
                 stage_6dpf = rep(0,60),
                 SE2 = rep(0,60), stage_9dpf = rep(0,60),
                 SE3 = rep(0, 60), time = c(-29:30))

for(t in -29:30){
  ndt[t+30,1] = mean(sub.baseline.3[ sub.baseline.3$time == t , 2])
  ndt[t+30,2] = sqrt( var(sub.baseline.3[ sub.baseline.3$time == t, 2])/length( sub.baseline.3[ sub.baseline.3$time == t, 2] ) )
  
  ndt[t+30,3] = mean(sub.baseline.6[ sub.baseline.6$time == t , 2])
  ndt[t+30,4] = sqrt( var(sub.baseline.6[ sub.baseline.6$time == t, 2])/length( sub.baseline.6[ sub.baseline.6$time == t, 2] ) )
  
  ndt[t+30,5] = mean(sub.baseline.9[ sub.baseline.9$time == t , 2])
  ndt[t+30,6] = sqrt( var(sub.baseline.9[ sub.baseline.9$time == t, 2])/length( sub.baseline.9[ sub.baseline.9$time == t, 2] ) )
}

rib.off1 <- aes(ymax = stage_3dpf + SE1, ymin = stage_3dpf - SE1)
rib.off2 <- aes(ymax = stage_6dpf + SE2, ymin = stage_6dpf - SE2)
rib.off3 <- aes(ymax = stage_9dpf + SE3, ymin = stage_9dpf - SE3)
gra = ggplot(data = ndt, aes(time)) +
  geom_line(aes(y = stage_3dpf, colour = "3dpf")) +
  geom_ribbon(rib.off1, fill="red", alpha = I(1/10)) +
  geom_line(aes(y = stage_6dpf, colour = "6dpf")) +
  geom_ribbon(rib.off2, fill="green",  alpha = I(1/10)) +
  geom_line(aes(y = stage_9dpf, colour = "9dpf")) +
  geom_ribbon(rib.off3, fill="blue",  alpha = I(1/10)) +
  scale_color_discrete(name="Stage") +
  scale_x_continuous(name="Time(s)") +
  scale_y_continuous(name="Activity", limits=c(0.00,0.15)) +
  theme_set(theme_grey(base_size = 15)) +
  ## change input here  ==========------------------------------>
  labs(title = "Light-On VMR of strain TL (Baseline)")+
  theme(legend.position=c(.5,0.5))
gra_baseline_on = gra + theme_bw()+  theme(legend.position=c(.11,0.8)) +theme(axis.title=element_text(size=14), plot.title = element_text(size=13))


### integrated-normalized data ##
## Stage==3
#Linear Regression to remove batch effect.
batch.lm3 =  lm(burdur_normalized~factor(replicate),data = subdata.stage3)
subdata.stage3$int_residuals =  batch.lm3$residuals


## Stage==6
#Linear Regression to remove batch effect.
batch.lm6 =  lm(burdur_normalized~factor(replicate),data = subdata.stage6)
subdata.stage6$int_residuals =  batch.lm6$residuals


## Stage ==9
#Linear Regression to remove batch effect.
batch.lm9 =  lm(burdur_normalized~factor(replicate),data = subdata.stage9)
subdata.stage9$int_residuals =  batch.lm9$residuals

# Grand mean of Beta.
beta = 0.01024
# Light off subset.
subdata.stage3.lightoff = subset(subdata.stage3,lights=='Off')
# Pull the light off part together with estimate beta calculation
est_beta_stage3 = mean(subdata.stage3.lightoff$int_residuals) - beta
# Calculate estimate Burdur using the estimate beta
subdata.stage3$ingr_est_burdur = subdata.stage3$int_residuals - est_beta_stage3
subdata.stage3$ingr_est_burdur = subdata.stage3$ingr_est_burdur + mu

##Stage == 6
# Light off subset.
subdata.stage6.lightoff = subset(subdata.stage6,lights=='Off')
# Pull the light off part together with estimate beta calculation
est_beta_stage6 = mean(subdata.stage6.lightoff$int_residuals) - beta
# Calculate estimate Burdur using the estimate beta
subdata.stage6$ingr_est_burdur = subdata.stage6$int_residuals - est_beta_stage6
subdata.stage6$ingr_est_burdur = subdata.stage6$ingr_est_burdur + mu

##Stage == 9
# Light off subset.
subdata.stage9.lightoff = subset(subdata.stage9,lights=='Off')
# Pull the light off part together with estimate beta calculation
est_beta_stage9 = mean(subdata.stage9.lightoff$int_residuals) - beta
# Calculate estimate Burdur using the estimate beta
subdata.stage9$ingr_est_burdur = subdata.stage9$int_residuals - est_beta_stage9
subdata.stage9$ingr_est_burdur = subdata.stage9$ingr_est_burdur + mu

## ggPlot
sub.integ.3 = subdata.stage3[c(7,18)]
sub.integ.6 = subdata.stage6[c(7,18)]
sub.integ.9 = subdata.stage9[c(7,18)]

ndt = data.frame(stage_3dpf = rep(0,60),
                 SE1 = rep(0,60),
                 stage_6dpf = rep(0,60),
                 SE2 = rep(0,60), stage_9dpf = rep(0,60),
                 SE3 = rep(0, 60), time = c(-29:30))

for(t in -29:30){
  ndt[t+30,1] = mean(sub.integ.3[ sub.integ.3$time == t , 2])
  ndt[t+30,2] = sqrt( var(sub.integ.3[ sub.integ.3$time == t, 2])/length( sub.integ.3[ sub.integ.3$time == t, 2] ) )
  
  ndt[t+30,3] = mean(sub.integ.6[ sub.integ.6$time == t , 2])
  ndt[t+30,4] = sqrt( var(sub.integ.6[ sub.integ.6$time == t, 2])/length( sub.integ.6[ sub.integ.6$time == t, 2] ) )
  
  ndt[t+30,5] = mean(sub.integ.9[ sub.integ.9$time == t , 2])
  ndt[t+30,6] = sqrt( var(sub.integ.9[ sub.integ.9$time == t, 2])/length( sub.integ.9[ sub.integ.9$time == t, 2] ) )
}

rib.off1 <- aes(ymax = stage_3dpf + SE1, ymin = stage_3dpf - SE1)
rib.off2 <- aes(ymax = stage_6dpf + SE2, ymin = stage_6dpf - SE2)
rib.off3 <- aes(ymax = stage_9dpf + SE3, ymin = stage_9dpf - SE3)
gra = ggplot(data = ndt, aes(time)) +
  geom_line(aes(y = stage_3dpf, colour = "3dpf")) +
  geom_ribbon(rib.off1, fill="red", alpha = I(1/10)) +
  geom_line(aes(y = stage_6dpf, colour = "6dpf")) +
  geom_ribbon(rib.off2, fill="green",  alpha = I(1/10)) +
  geom_line(aes(y = stage_9dpf, colour = "9dpf")) +
  geom_ribbon(rib.off3, fill="blue",  alpha = I(1/10)) +
  scale_color_discrete(name="Stage") +
  scale_x_continuous(name="Time(s)") +
  scale_y_continuous(name="Activity", limits=c(0.00,0.15)) +
  theme_set(theme_grey(base_size = 15)) +
  ## change input here  ==========------------------------------>
  labs(title = "Light-On VMR of strain TL (Integrated)")+
  theme(legend.position=c(.5,0.5))
gra_integ_on = gra + theme_bw()+  theme(legend.position=c(.11,0.8)) +theme(axis.title=element_text(size=14), plot.title = element_text(size=13))

#done
#Hotelling T-test
####################################################
########################### for hotelling's t-square
func_wide = function(data){
  dt.wide = matrix(0,nrow= min(summary(factor(data[,1]))),ncol=60)
  for(i in -29:30){
    temp = data[ data[,1] == i, ]
    dt.wide[,i+30] = temp[1:min(summary(factor(data[,1]))),2]
  }
  return(dt.wide)
}
#####################################################
# after normalization
####################################################
#Before light change.     
# the 18th column contains the integrated noramlized data, and the 10th colum conatins the orgianl data
dt.off1 = subdata.stage3[c(7,18)]
# subdata.stage3 = subset(lt.on, genotype == "TL"& stage == 3)
dt.off2 = subdata.stage6[c(7,18)]
dt.off3 = subdata.stage9[c(7,18)]

# reshape data
dt.n1 = func_wide(dt.off1)
dt.n2 = func_wide(dt.off2)
dt.n3 = func_wide(dt.off3)

mytest1 = hotelling.test(dt.n1[,1:30], dt.n2[,1:30], shrinkage = T, perm=T, B=500)
mytest1
mytest2 = hotelling.test(dt.n1[,31:60], dt.n2[,31:60], shrinkage = T,perm=T, B=500)
mytest2
#mytest = hotelling.test(dt.n1, dt.n2)
#mytest
##
mytest3 = hotelling.test(dt.n2[,1:30], dt.n3[,1:30], shrinkage = T, perm=T, B=500)
mytest3
mytest4 = hotelling.test(dt.n2[,31:60], dt.n3[,31:60], shrinkage = T, perm=T, B=500)
mytest4
#mytest = hotelling.test(dt.n2, dt.n3)
#mytest
##
mytest5 = hotelling.test(dt.n1[,1:30], dt.n3[,1:30], shrinkage = T, perm=T, B=500)
mytest5
mytest6 = hotelling.test(dt.n1[,31:60], dt.n3[,31:60], shrinkage = T, perm=T, B=500)
mytest6
#mytest = hotelling.test(dt.n1, dt.n3)
#mytest
p.adjust(c(mytest1$pval, mytest3$pval, mytest5$pval), method = "BH")
p.adjust(c(mytest2$pval, mytest4$pval, mytest6$pval), method = "BH")

####ãelect times
before.time = 28:30
after.time = 31:33


mytest1.select = hotelling.test(dt.n1[,before.time], dt.n2[,before.time], shrinkage = T)#, perm=T, B=500)
mytest1.select
mytest2.select = hotelling.test(dt.n1[,after.time], dt.n2[,after.time], shrinkage = T)#,perm=T, B=500)
mytest2.select
#mytest = hotelling.test(dt.n1, dt.n2)
#mytest
##
mytest3.select = hotelling.test(dt.n2[,before.time], dt.n3[,before.time], shrinkage = T)#, perm=T, B=500)
mytest3.select
mytest4.select = hotelling.test(dt.n2[,after.time], dt.n3[,after.time], shrinkage = T)#, perm=T, B=500)
mytest4.select
#mytest = hotelling.test(dt.n2, dt.n3)
#mytest
##
mytest5.select = hotelling.test(dt.n1[,before.time], dt.n3[,before.time], shrinkage = T)#, perm=T, B=500)
mytest5.select
mytest6.select = hotelling.test(dt.n1[,after.time], dt.n3[,after.time], shrinkage = T)#, perm=T, B=500)
mytest6.select
#mytest = hotelling.test(dt.n1, dt.n3)
#mytest
p.adjust(c(mytest1.select$pval, mytest3.select$pval, mytest5.select$pval), method = "BH")
p.adjust(c(mytest2.select$pval, mytest4.select$pval, mytest6.select$pval), method = "BH")




#####################################################
# Before normalization
####################################################
# the 18th column contains the integrated noramlized data, and the 10th colum conatins the orgianl data
dt.off1.raw = subdata.stage3[c(7,10)]
# subdata.stage3 = subset(lt.on, genotype == "TL"& stage == 3)
dt.off2.raw  = subdata.stage6[c(7,10)]
dt.off3.raw  = subdata.stage9[c(7,10)]

# reshape data
dt.n1.raw  = func_wide(dt.off1.raw )
dt.n2.raw  = func_wide(dt.off2.raw )
dt.n3.raw  = func_wide(dt.off3.raw )


mytest1.raw  = hotelling.test(dt.n1.raw [,1:30], dt.n2.raw [,1:30], shrinkage = T, perm=T, B=500)
mytest1.raw 
mytest2.raw  = hotelling.test(dt.n1.raw [,31:60], dt.n2.raw [,31:60], shrinkage = T,perm=T, B=500)
mytest2.raw 
#mytest = hotelling.test(dt.n1, dt.n2)
#mytest
##
mytest3.raw  = hotelling.test(dt.n2.raw [,1:30], dt.n3.raw [,1:30], shrinkage = T, perm=T, B=500)
mytest3.raw 
mytest4.raw  = hotelling.test(dt.n2.raw [,31:60], dt.n3.raw[,31:60], shrinkage = T, perm=T, B=500)
mytest4.raw 
#mytest = hotelling.test(dt.n2, dt.n3)
#mytest
##
mytest5.raw  = hotelling.test(dt.n1.raw [,1:30], dt.n3.raw [,1:30], shrinkage = T, perm=T, B=500)
mytest5.raw 
mytest6.raw  = hotelling.test(dt.n1.raw [,31:60], dt.n3.raw [,31:60], shrinkage = T, perm=T, B=500)
mytest6.raw 
#mytest = hotelling.test(dt.n1, dt.n3)
#mytest
p.adjust(c(mytest1.raw $pval, mytest3.raw $pval, mytest5.raw $pval), method = "BH")
p.adjust(c(mytest2.raw $pval, mytest4.raw $pval, mytest6.raw $pval), method = "BH")

####lect times
# t=32
before.time = 28:30
after.time = 31:33


mytest1.select.raw  = hotelling.test(dt.n1.raw [,before.time], dt.n2.raw [,before.time], shrinkage = T)#, perm=T, B=500)
mytest1.select.raw 
mytest2.select.raw  = hotelling.test(dt.n1.raw [,after.time]+mu, dt.n2.raw [,after.time]+mu, shrinkage = T)#, perm=T, B=500)
mytest2.select.raw 
#mytest = hotelling.test(dt.n1, dt.n2)
#mytest
##
mytest3.select.raw  = hotelling.test(dt.n2.raw [,before.time], dt.n3.raw [,before.time], shrinkage = T)#, perm=T, B=500)
mytest3.select.raw 
mytest4.select.raw  = hotelling.test(dt.n2.raw [,after.time]+mu, dt.n3.raw [,after.time]+mu, shrinkage = T)#, perm=T, B=500)
mytest4.select.raw 
#mytest = hotelling.test(dt.n2, dt.n3)
#mytest
##
mytest5.select.raw  = hotelling.test(dt.n1.raw [,before.time], dt.n3.raw [,before.time], shrinkage = T)#, perm=T, B=500)
mytest5.select.raw 
mytest6.select.raw  = hotelling.test(dt.n1.raw [,after.time]+mu, dt.n3.raw [,after.time]+mu, shrinkage = T)#, perm=T, B=500)
mytest6.select.raw 
#mytest = hotelling.test(dt.n1, dt.n3)
#mytest
p.adjust(c(mytest1.select.raw $pval, mytest3.select.raw $pval, mytest5.select.raw $pval), method = "BH")
p.adjust(c(mytest2.select.raw $pval, mytest4.select.raw $pval, mytest6.select.raw $pval), method = "BH")
after.time





