rm(list = ls())
library(apsimx)
library(tidyverse)
library(BayesianTools)
library(rstantools)

############
# 晚稻
setwd('D:/APSIM_workplace/OPTIM-rice')
obsWh <- read.csv('D:/APSIM_workplace/OPTIM-rice/guangfeng_晚稻.csv',fileEncoding = "GBK")
obsWh <- obsWh[c(1:10),]
class(obsWh)


rice <- apsim('late.apsim') 
rice_out_1 <- rice[(nrow(rice):1) %% 2 == 0,]
rice_out_2 <- rice[(nrow(rice):1) %% 2 != 0,]
rice_out <- cbind(rice_out_1,rice_out_2)
rice_out $yield <- NULL
rice_out <- rice_out[c(10,1:9),]



ObsSim <- cbind(obsWh,rice_out)
ObsSim $ yield_obs <- ObsSim $yield
ObsSim $yield <- NULL
ObsSim $ yield_sim <- ObsSim $yield
ObsSim $Date <- NULL
ObsSim $Date <- as.Date(ObsSim$Date)
ObsSim $Date1 <- as.numeric(ObsSim $Date - as.Date(paste0(year(ObsSim $Date), "-01-01")))
ObsSim $har_vest<-as.Date(ObsSim$har_date,format = "%m/%d/%Y")
ObsSim $har_vest1 <- as.numeric(ObsSim $har_vest - as.Date(paste0(year(ObsSim $har_vest), "-01-01")))
ObsSim $har_vest <- NULL

RMSEFun=function(simvalue,obsvalue) {sqrt(mean((simvalue-obsvalue)^2))}
RRMSEFun=function(simvalue,obsvalue) {sqrt(mean((simvalue-obsvalue)^2))/mean(obsvalue)}
EFFun=function(simvalue,obsvalue){
  1-sum((simvalue-obsvalue)^2)/sum((obsvalue-mean(obsvalue))^2)
}
DvalueFun=function(simvalue,obsvalue){1-sum((simvalue-obsvalue)^2)/sum((abs(simvalue-mean(obsvalue))+abs(obsvalue-mean(simvalue)))^2)}
MBEFun=function(simvalue,obsvalue){mean(abs(simvalue-obsvalue))}
R2Fun=function(simvalue,obsvalue){summary(lm(simvalue~obsvalue))$r.squared}
R2Funadj=function(simvalue,obsvalue){summary(lm(simvalue~obsvalue))$adj.r.squared}






####多个参数调参——针对物候期######
ObjectFun1 <- function(pars){
  DVRJ <- as.character(pars[1])
  edit_apsim_xml("Oryza.xml", 
                 src.dir = 'D:/APSIM_workplace/OPTIM-rice',
                 wrt.dir = './New',
                 parm.path = ".//Model/RICECHUZHOU_Late/DVRJ",
                 value = DVRJ)
  
  DVRI <- as.character(pars[2])
  edit_apsim_xml("Oryza-edited.xml", 
                 src.dir = './New',
                 wrt.dir = './New',
                 overwrite = TRUE,
                 parm.path = ".//Model/RICECHUZHOU_Late/DVRI",
                 value = DVRI)

  
  
  rice <- apsim('late2.apsim')
  rice_out_1 <- rice[(nrow(rice):1) %% 2 == 0,]
  rice_out_2 <- rice[(nrow(rice):1) %% 2 != 0,]
  rice_out <- cbind(rice_out_1,rice_out_2)
  rice_out $yield <- NULL
  rice_out <- rice_out[c(10,1:9),]
  
  
  
  ObsSim <- cbind(obsWh,rice_out)
  ObsSim $ yield_obs <- ObsSim $yield
  ObsSim $yield <- NULL
  ObsSim $ yield_sim <- ObsSim $yield
  ObsSim $Date <- NULL
  ObsSim $Date <- as.Date(ObsSim$Date)
  ObsSim $Date1 <- as.numeric(ObsSim $Date - as.Date(paste0(year(ObsSim $Date), "-01-01")))
  ObsSim $har_vest<-as.Date(ObsSim$har_date,format = "%m/%d/%Y")
  ObsSim $har_vest1 <- as.numeric(ObsSim $har_vest - as.Date(paste0(year(ObsSim $har_vest), "-01-01")))
  ObsSim $har_vest <- NULL
  
  ModelRes=RRMSEFun(ObsSim$Date1,ObsSim$har_vest1)
  return(ModelRes)
}


Parsopt1 <- optim(c(0.000761,0.000475), ObjectFun1, method="Nelder-Mead")

Parsopt1$par
ObjectFun1(Parsopt1$par)



rice <- apsim('late2.apsim') 
rice_out_1 <- rice[(nrow(rice):1) %% 2 == 0,]
rice_out_2 <- rice[(nrow(rice):1) %% 2 != 0,]
rice_out <- cbind(rice_out_1,rice_out_2)
rice_out $yield <- NULL
rice_out <- rice_out[c(10,1:9),]



ObsSim <- cbind(obsWh,rice_out)
ObsSim $ yield_obs <- ObsSim $yield
ObsSim $yield <- NULL
ObsSim $ yield_sim <- ObsSim $yield
ObsSim $Date <- NULL
ObsSim $Date <- as.Date(ObsSim$Date)
ObsSim $Date1 <- as.numeric(ObsSim $Date - as.Date(paste0(year(ObsSim $Date), "-01-01")))
ObsSim $har_vest<-as.Date(ObsSim$har_date,format = "%m/%d/%Y")
ObsSim $har_vest1 <- as.numeric(ObsSim $har_vest - as.Date(paste0(year(ObsSim $har_vest), "-01-01")))

RRMSEFun(ObsSim$Date1,ObsSim$har_vest1)
RRMSEFun(ObsSim$yield_sim,ObsSim$yield_obs)
R2Fun(ObsSim$yield_sim,ObsSim$yield_obs)
R2Funadj(ObsSim$yield_sim,ObsSim$yield_obs)

SSR <- sum((ObsSim$Date1 - mean(ObsSim$har_vest1))^2)
SST <- sum((ObsSim$har_vest1 - mean(ObsSim$har_vest1))^2)

# 计算R²
SSR / SST













####多个参数调参-针对产量######
ObjectFun2 <- function(pars){
  DVRP <- as.character(pars[1])
  edit_apsim_xml("Oryza.xml", 
                 src.dir = 'D:/APSIM_workplace/OPTIM-rice',
                 wrt.dir = './New',
                 parm.path = ".//Model/RICECHUZHOU/DVRP",
                 value = DVRP)
  
  DVRR <- as.character(pars[2])
  edit_apsim_xml("Oryza-edited.xml", 
                 src.dir = './New',
                 wrt.dir = './New',
                 overwrite = TRUE,
                 parm.path = ".//Model/RICECHUZHOU/DVRR",
                 value = DVRR)
  
  rice <- apsim('late2.apsim')
  rice_out_1 <- rice[(nrow(rice):1) %% 2 == 0,]
  rice_out_2 <- rice[(nrow(rice):1) %% 2 != 0,]
  rice_out <- cbind(rice_out_1,rice_out_2)
  rice_out $yield <- NULL
  rice_out <- rice_out[c(10,1:9),]
  
  
  
  ObsSim <- cbind(obsWh,rice_out)
  ObsSim $ yield_obs <- ObsSim $yield
  ObsSim $yield <- NULL
  ObsSim $ yield_sim <- ObsSim $yield
  ObsSim $Date <- NULL
  ObsSim $Date <- as.Date(ObsSim$Date)
  ObsSim $Date1 <- as.numeric(ObsSim $Date - as.Date(paste0(year(ObsSim $Date), "-01-01")))
  ObsSim $har_vest<-as.Date(ObsSim$har_date,format = "%m/%d/%Y")
  ObsSim $har_vest1 <- as.numeric(ObsSim $har_vest - as.Date(paste0(year(ObsSim $har_vest), "-01-01")))
  ObsSim $har_vest <- NULL
  ModelRes <- RRMSEFun(ObsSim$Date1,ObsSim$har_vest1)
  return(ModelRes)
}

Parsopt2 <- optim(c(0.000644,0.001582), ObjectFun2, method="Nelder-Mead")


Parsopt2$par
ObjectFun2(Parsopt2$par)


















####多个参数调参-针对产量物候######
ObjectFun3 <- function(pars){
  DVRJ <- as.character(pars[1])
  edit_apsim_xml("Oryza.xml", 
                 src.dir = 'D:/APSIM_workplace/OPTIM-rice',
                 wrt.dir = './New',
                 parm.path = ".//Model/RICECHUZHOU/DVRJ",
                 value = DVRJ)
  
  DVRI <- as.character(pars[2])
  edit_apsim_xml("Oryza-edited.xml", 
                 src.dir = './New',
                 wrt.dir = './New',
                 overwrite = TRUE,
                 parm.path = ".//Model/RICECHUZHOU/DVRI",
                 value = DVRI)
  
  DVRR <- as.character(pars[3])
  edit_apsim_xml("Oryza-edited.xml", 
                 src.dir = './New',
                 wrt.dir = './New',
                 overwrite = TRUE,
                 parm.path = ".//Model/RICECHUZHOU/DVRR",
                 value = DVRR)
  
  DVRP <- as.character(pars[4])
  edit_apsim_xml("Oryza-edited.xml", 
                 src.dir = './New',
                 wrt.dir = './New',
                 overwrite = TRUE,
                 parm.path = ".//Model/RICECHUZHOU/DVRP",
                 value = DVRP)
  
  rice <- apsim('late2.apsim')
  rice_out_1 <- rice[(nrow(rice):1) %% 2 == 0,]
  rice_out_2 <- rice[(nrow(rice):1) %% 2 != 0,]
  rice_out <- cbind(rice_out_1,rice_out_2)
  rice_out $yield <- NULL
  rice_out <- rice_out[c(10,1:9),]
  
  
  
  ObsSim <- cbind(obsWh,rice_out)
  ObsSim $ yield_obs <- ObsSim $yield
  ObsSim $yield <- NULL
  ObsSim $ yield_sim <- ObsSim $yield
  ObsSim $Date <- NULL
  ObsSim $Date <- as.Date(ObsSim$Date)
  ObsSim $Date1 <- as.numeric(ObsSim $Date - as.Date(paste0(year(ObsSim $Date), "-01-01")))
  ObsSim $har_vest<-as.Date(ObsSim$har_date,format = "%m/%d/%Y")
  ObsSim $har_vest1 <- as.numeric(ObsSim $har_vest - as.Date(paste0(year(ObsSim $har_vest), "-01-01")))
  ObsSim $har_vest <- NULL
  ModelRes1=RRMSEFun(ObsSim$Date1,ObsSim$har_vest1)


  return(ModelRes1)
}



rice <- apsim('late2.apsim')
rice_out_1 <- rice[(nrow(rice):1) %% 2 == 0,]
rice_out_2 <- rice[(nrow(rice):1) %% 2 != 0,]
rice_out <- cbind(rice_out_1,rice_out_2)
rice_out $yield <- NULL
rice_out <- rice_out[c(10,1:9),]
ObsSim <- cbind(obsWh,rice_out)
ObsSim $ yield_obs <- ObsSim $yield
ObsSim $yield <- NULL
ObsSim $ yield_sim <- ObsSim $yield
ObsSim $Date <- NULL
ObsSim $Date <- as.Date(ObsSim$Date)
ObsSim $Date1 <- as.numeric(ObsSim $Date - as.Date(paste0(year(ObsSim $Date), "-01-01")))
ObsSim $har_vest<-as.Date(ObsSim$har_date,format = "%m/%d/%Y")
ObsSim $har_vest1 <- as.numeric(ObsSim $har_vest - as.Date(paste0(year(ObsSim $har_vest), "-01-01")))








Parsopt3 <- optim(c(0.0016922566,0.0019488924,0.0017890167,0.0009679051), ObjectFun3, method="Nelder-Mead")

Parsopt3$par



RFGRIND <- expand.grid(n.trees = (1:10)*50,
                       mtry= ((1:20)))#参数寻优





ObjectFun3(Parsopt3$par)
RRMSEFun(ObsSim$Date1,ObsSim$har_vest1)
RRMSEFun(ObsSim$yield_sim,ObsSim$yield_obs)








#########################################









