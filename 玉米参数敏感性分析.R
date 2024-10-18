rm(list = ls())
library(apsimx)
library(tidyverse)
library(BayesianTools)
library(rstantools)

############
# Classic
setwd('D:/APSIM_workplace/咸鱼客户/教学文件夹')
maize <- apsim('maize2.apsim', simplify = FALSE)

SEN_endjuv<- expand.grid("tt_emerg_to_endjuv" = seq(100,300,5),
                       "tt_endjuv_to_init" = 0,
                       "photoperiod_crit1" = 12.5,
                       "photoperiod_crit2" = 24,
                       "photoperiod_slope" = 23,
                       "tt_flag_to_flower" = 10,
                       "tt_flower_to_start_grain" = 170,
                       "tt_flower_to_maturity" = 750,
                       "tt_maturity_to_ripe" = 1,
                       "potKernelWt" = 260,
                       "rue" = 1.6)

for (i in 1:nrow(SEN_endjuv)) {
  tt_emerg_to_endjuv <- as.character(SEN_endjuv$tt_emerg_to_endjuv[i])
  edit_apsim_xml("Maize-edited.xml", 
                 src.dir = 'D:/APSIM_workplace/咸鱼客户/教学文件夹/New',
                 wrt.dir = './New',
                 overwrite = TRUE,
                 parm.path = ".//Model/My_cul/tt_emerg_to_endjuv",
                 value = tt_emerg_to_endjuv)
  maize <- apsim('maize2.apsim', simplify = FALSE)
  SEN_endjuv$yield[i] <- max(maize$Yield)
}
max(SEN_endjuv$yield)
min(SEN_endjuv$yield)
## yield
ggplot() + 
  geom_line(data = SEN_endjuv, aes(x = tt_emerg_to_endjuv, y = yield)) + 
  ggtitle("Biomass (kg/hm2)")





SEN_flower_to_maturity<- expand.grid("tt_emerg_to_endjuv" = 290,
                         "tt_endjuv_to_init" = 0,
                         "photoperiod_crit1" = 12.5,
                         "photoperiod_crit2" = 24,
                         "photoperiod_slope" = 23,
                         "tt_flag_to_flower" = 10,
                         "tt_flower_to_start_grain" = 170,
                         "tt_flower_to_maturity" = seq(200,1000,5),
                         "tt_maturity_to_ripe" = 1,
                         "potKernelWt" = 260,
                         "rue" = 1.6)

for (i in 1:nrow(SEN_flower_to_maturity)) {
  tt_flower_to_maturity <- as.character(SEN_flower_to_maturity$tt_flower_to_maturity[i])
  edit_apsim_xml("Maize-edited.xml", 
                 src.dir = 'D:/APSIM_workplace/咸鱼客户/教学文件夹/New',
                 wrt.dir = './New',
                 overwrite = TRUE,
                 parm.path = ".//Model/My_cul/tt_flower_to_maturity",
                 value = tt_flower_to_maturity)
  maize <- apsim('maize2.apsim', simplify = FALSE)
  SEN_flower_to_maturity$Yield[i] <- max(maize$Yield)
}
max(SEN_flower_to_maturity$Yield)
min(SEN_flower_to_maturity$Yield)
## yield
ggplot() + 
  geom_line(data = SEN_flower_to_maturity, aes(x = tt_flower_to_maturity, y = Yield)) + 
  ggtitle("Biomass (kg/hm2)")





SEN_flower_to_start_grain<- expand.grid("tt_emerg_to_endjuv" = 290,
                                     "tt_endjuv_to_init" = 0,
                                     "photoperiod_crit1" = 12.5,
                                     "photoperiod_crit2" = 24,
                                     "photoperiod_slope" = 23,
                                     "tt_flag_to_flower" = 10,
                                     "tt_flower_to_start_grain" = seq(0,300,5),
                                     "tt_flower_to_maturity" = 750,
                                     "tt_maturity_to_ripe" = 1,
                                     "potKernelWt" = 260,
                                     "rue" = 1.6)

for (i in 1:nrow(SEN_flower_to_start_grain)) {
  tt_flower_to_start_grain <- as.character(SEN_flower_to_start_grain$tt_flower_to_start_grain[i])
  edit_apsim_xml("Maize-edited.xml", 
                 src.dir = 'D:/APSIM_workplace/咸鱼客户/教学文件夹/New',
                 wrt.dir = './New',
                 overwrite = TRUE,
                 parm.path = ".//Model/My_cul/tt_flower_to_start_grain",
                 value = tt_flower_to_start_grain)
  maize <- apsim('maize2.apsim', simplify = FALSE)
  SEN_flower_to_start_grain$Yield[i] <- max(maize$Yield)
}
max(SEN_flower_to_start_grain$Yield)
min(SEN_flower_to_start_grain$Yield)
## yield
ggplot() + 
  geom_line(data = SEN_flower_to_start_grain, aes(x = tt_flower_to_start_grain, y = Yield)) + 
  ggtitle("Biomass (kg/hm2)")











#######手动调参#######

##调参生育期
tt_emerg_to_endjuv <- '330'
edit_apsim_xml("Maize.xml", 
               src.dir = 'D:/APSIM工作文件夹/教学文件夹',
               wrt.dir = './New',
               parm.path = ".//Model/My_cul/tt_emerg_to_endjuv",
               value = tt_emerg_to_endjuv)

maize2 <- apsim('maize2.apsim')
#Phe
ggplot() + 
  geom_point(data = obsWh, aes(x = Date, y = Stage)) +
  geom_line(data = maize2, aes(x = Date, y = Stage)) + 
  ggtitle("Phenology")

## LAI
ggplot() + 
  geom_point(data = obsWh, aes(x = Date, y = lai)) +
  geom_line(data = maize2, aes(x = Date, y = LAI)) + 
  ggtitle("LAI")

## Biomass
ggplot() + 
  geom_point(data = obsWh, aes(x = Date, y = biomass)) +
  geom_line(data = maize2, aes(x = Date, y = Biomass)) + 
  ggtitle("Biomass (g/m2)")




#######单参数调参#######
ObjectFun1 <- function(par){
  tt_emerg_to_endjuv <- as.character(par)
  edit_apsim_xml("Maize.xml", 
                 src.dir = 'D:/APSIM工作文件夹/教学文件夹',
                 wrt.dir = './New',
                 parm.path = ".//Model/My_cul/tt_emerg_to_endjuv",
                 value = tt_emerg_to_endjuv)
  
  maize <- apsim('maize2.apsim')
  
  ObsSim <- maize%>%filter(Date%in%obsWh$Date)%>%
    select(Date,Stage,Biomass,LAI)%>%left_join(obsWh,.,by='Date')%>%
    filter(LAI>0)

  
  ModelRes <- RRMSEFun(ObsSim$Stage.y,ObsSim$Stage.x)
  return(ModelRes)
}
Parsopt <- optim(270, ObjectFun1, method = "Nelder-Mead")

print(Parsopt$par)

ObjectFun1(Parsopt$par)
edit_apsim_xml("Maize.xml", 
               src.dir = 'D:/APSIM工作文件夹/教学文件夹',
               wrt.dir = './New',
               parm.path = ".//Model/My_cul/tt_emerg_to_endjuv",
               value = Parsopt$par%>%as.character())

maize3 <- apsim('maize2.apsim')

#Phe
ggplot() + 
  geom_point(data = obsWh, aes(x = Date, y = Stage)) +
  geom_line(data = maize3, aes(x = Date, y = Stage)) + 
  ggtitle("Phenology")

## LAI
ggplot() + 
  geom_point(data = obsWh, aes(x = Date, y = lai)) +
  geom_line(data = maize3, aes(x = Date, y = LAI)) + 
  ggtitle("LAI")

## Biomass
ggplot() + 
  geom_point(data = obsWh, aes(x = Date, y = biomass)) +
  geom_line(data = maize3, aes(x = Date, y = Biomass)) + 
  ggtitle("Biomass (g/m2)")









#######多参数调参#######
ObjectFun2 <- function(pars){
  tt_emerg_to_endjuv <- as.character(pars[1])
  edit_apsim_xml("Maize.xml", 
                 src.dir = 'D:/APSIM工作文件夹/教学文件夹',
                 wrt.dir = './New',
                 parm.path = ".//Model/My_cul/tt_emerg_to_endjuv",
                 value = tt_emerg_to_endjuv)
  
  tt_flower_to_start_grain <- as.character(pars[2])
  edit_apsim_xml("Maize-edited.xml", 
                 src.dir = 'D:/APSIM工作文件夹/教学文件夹/New',
                 wrt.dir = './New',
                 overwrite = TRUE,
                 parm.path = ".//Model/My_cul/tt_flower_to_start_grain",
                 value = tt_flower_to_start_grain)
  
  
  maize <- apsim('maize2.apsim')
  
  ObsSim <- maize%>%filter(Date%in%obsWh$Date)%>%
    select(Date,Stage,Biomass,LAI)%>%left_join(obsWh,.,by='Date')%>%
    filter(LAI>0)
  
  
  ModelRes <- RRMSEFun(ObsSim$Stage.y,ObsSim$Stage.x)
  return(ModelRes)
}


Parsopt <- optim(c(270,110), ObjectFun2, method = "Nelder-Mead")
print(Parsopt$par)

ObjectFun2(Parsopt$par)


tt_emerg_to_endjuv <- as.character(Parsopt$par[1])
edit_apsim_xml("Maize.xml", 
               src.dir = 'D:/APSIM工作文件夹/教学文件夹',
               wrt.dir = './New',
               parm.path = ".//Model/My_cul/tt_emerg_to_endjuv",
               value = tt_emerg_to_endjuv)

tt_flower_to_start_grain <- as.character(Parsopt$par[2])
edit_apsim_xml("Maize-edited.xml", 
               src.dir = 'D:/APSIM工作文件夹/教学文件夹/New',
               wrt.dir = './New',
               overwrite = TRUE,
               parm.path = ".//Model/My_cul/tt_flower_to_start_grain",
               value = tt_flower_to_start_grain)
maize4 <- apsim('maize2.apsim')

#Phe
ggplot() + 
  geom_point(data = obsWh, aes(x = Date, y = Stage)) +
  geom_line(data = maize4, aes(x = Date, y = Stage)) + 
  ggtitle("Phenology")

## LAI
ggplot() + 
  geom_point(data = obsWh, aes(x = Date, y = lai)) +
  geom_line(data = maize4, aes(x = Date, y = LAI)) + 
  ggtitle("LAI")

## Biomass
ggplot() + 
  geom_point(data = obsWh, aes(x = Date, y = biomass)) +
  geom_line(data = maize4, aes(x = Date, y = Biomass)) + 
  ggtitle("Biomass (g/m2)")




















