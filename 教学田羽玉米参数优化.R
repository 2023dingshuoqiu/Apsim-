rm(list = ls())
library(apsimx)
library(tidyverse)
library(BayesianTools)
library(rstantools)

############
# Classic
setwd('D:/APSIM_workplace/咸鱼客户/教学文件夹')
maize <- apsim('shifeichuli.apsim', simplify = FALSE)

maize0 <- maize$N0.out
maize150 <- maize$N150.out
maize225 <- maize$N225.out
maize300 <- maize$N300.out
maize375 <- maize$N375.out

obsWh0 <- read.csv('D:/APSIM工作文件夹/教学文件夹/ObsW-maize0.csv')%>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"))

obsWh150 <- read.csv('D:/APSIM工作文件夹/教学文件夹/ObsW-maize150.csv')%>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"))

obsWh225 <- read.csv('D:/APSIM工作文件夹/教学文件夹/ObsW-maize225.csv')%>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"))

obsWh300 <- read.csv('D:/APSIM工作文件夹/教学文件夹/ObsW-maize300.csv')%>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"))

obsWh375 <- read.csv('D:/APSIM工作文件夹/教学文件夹/ObsW-maize375.csv')%>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"))

ObsSim0 <- maize0%>%filter(Date%in%obsWh0$Date)%>%
  select(Date,Stage,Biomass)%>%left_join(obsWh0,.,by='Date')

ObsSim150 <- maize150%>%filter(Date%in%obsWh150$Date)%>%
  select(Date,Stage,Biomass)%>%left_join(obsWh150,.,by='Date')

ObsSim225 <- maize225%>%filter(Date%in%obsWh225$Date)%>%
  select(Date,Stage,Biomass)%>%left_join(obsWh225,.,by='Date')

ObsSim300 <- maize300%>%filter(Date%in%obsWh300$Date)%>%
  select(Date,Stage,Biomass)%>%left_join(obsWh300,.,by='Date')

ObsSim375 <- maize375%>%filter(Date%in%obsWh375$Date)%>%
  select(Date,Stage,Biomass)%>%left_join(obsWh375,.,by='Date')


RMSEFun <- function(simvalue,obsvalue)
  {sqrt(mean((simvalue-obsvalue)^2))}
RRMSEFun <- function(simvalue,obsvalue) 
  {sqrt(mean((simvalue-obsvalue)^2))/mean(obsvalue)}
EFFun <- function(simvalue,obsvalue){
  1-sum((simvalue-obsvalue)^2)/sum((obsvalue-mean(obsvalue))^2)
}
DvalueFun <- function(simvalue,obsvalue)
  {1-sum((simvalue-obsvalue)^2)/sum((abs(simvalue-mean(obsvalue))+abs(obsvalue-mean(simvalue)))^2)}
MBEFun <- function(simvalue,obsvalue)
  {mean(abs(simvalue-obsvalue))}
R2Fun <- function(simvalue,obsvalue)
  {summary(lm(simvalue~1+obsvalue))$r.squared}
R2Funadj <- function(simvalue,obsvalue)
  {summary(lm(simvalue~1+obsvalue))$adj.r.squared}





#######手动调参#######

##调参生育期
tt_emerg_to_endjuv <- '330'
edit_apsim_xml("Maize.xml", 
               src.dir = 'D:/APSIM工作文件夹/教学文件夹',
               wrt.dir = './New',
               parm.path = ".//Model/My_cul/tt_emerg_to_endjuv",
               value = tt_emerg_to_endjuv)




maize2 <- apsim('shifeichuli2.apsim', simplify = FALSE)
maize2_0 <- maize2$`N0-2.out`
maize2_150 <- maize2$`N150-2.out`
maize2_225 <- maize2$`N225-2.out`
maize2_300 <- maize2$`N300-2.out`
maize2_375 <- maize2$`N375-2.out`


#Phe
ggplot() + 
  geom_point(data = obsWh0, aes(x = Date, y = Stage)) +
  geom_line(data = maize2_0, aes(x = Date, y = Stage)) + 
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
                 parm.path = ".//Model/Zhongdan2/tt_emerg_to_endjuv",
                 value = tt_emerg_to_endjuv)
  
  maize <- apsim('shifeichuli2.apsim', simplify = FALSE)%>%.$`N0-2.out`
  ObsSim <- maize%>%filter(Date%in%obsWh0$Date)%>%
    select(Date,Stage,Biomass,LAI)%>%left_join(obsWh0,.,by='Date')%>%
    filter(LAI>0)

  
  ModelRes <- RRMSEFun(ObsSim$Stage.y,ObsSim$Stage.x)
  return(ModelRes)
}
Parsopt <- optim(270, ObjectFun1, method = "Nelder-Mead")

print(Parsopt$par)

ObjectFun1(Parsopt$par)
tt_emerg_to_endjuv <- as.character(Parsopt$par)
edit_apsim_xml("Maize.xml", 
               src.dir = 'D:/APSIM工作文件夹/教学文件夹',
               wrt.dir = './New',
               parm.path = ".//Model/Zhongdan2/tt_emerg_to_endjuv",
               value = tt_emerg_to_endjuv)

maize2 <- apsim('shifeichuli2.apsim', simplify = FALSE)
maize2_0 <- maize2$`N0-2.out`
maize2_150 <- maize2$`N150-2.out`
maize2_225 <- maize2$`N225-2.out`
maize2_300 <- maize2$`N300-2.out`
maize2_375 <- maize2$`N375-2.out`


#Phe
ggplot() + 
  geom_point(data = obsWh0, aes(x = Date, y = Stage)) +
  geom_line(data = maize2_0, aes(x = Date, y = Stage)) + 
  ggtitle("Phenology")

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












#########################################
# MCMC
##1 Likelihood
likelihood <- function(pars){

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
  
  singlelikelihoods <- dnorm(ObsSim$biomass, 
                            mean = ObsSim$Biomass, 
                            sd = pars[3], log = T)
  sumll <- sum(singlelikelihoods)
  
  return(sumll)
}

##2 更新参数
UpdatePar = function(pars=c(300,200,100)){
  return(rnorm(3,mean = pars, sd= c(10,10,10)))
}

##3 run MCMC
run_metropolis_MCMC = function(startvalue=c(300,200,100), iterations=1000){
  chain = array(dim = c(iterations+1,3))
  chain[1,] = startvalue
  for (i in 1:iterations){
    #1更新参数
    NewPars = UpdatePar(chain[i,]) 
    
    #2求后验概率或似然函数
    NewPost=likelihood(NewPars)
    OldPost=likelihood(chain[i,])
    
    ComparePost = exp(NewPost - OldPost) 
    #4如果比较值大于runif(1)则接受
    if (ComparePost>runif(1)){
      chain[i+1,] = NewPars
    }else{
      chain[i+1,] = chain[i,]
    }
    print(paste0('-----',i,'--------'))
  }
  return(chain)
}

startvalue = c(300,200,100)
iterations= 50
chain = run_metropolis_MCMC(startvalue, iterations)



tt_emerg_to_endjuv <- as.character(chain[iterations+1,1])
edit_apsim_xml("Maize.xml", 
               src.dir = 'D:/APSIM工作文件夹/教学文件夹',
               wrt.dir = './New',
               parm.path = ".//Model/My_cul/tt_emerg_to_endjuv",
               value = tt_emerg_to_endjuv)

tt_flower_to_start_grain <- as.character(chain[iterations+1,2])
edit_apsim_xml("Maize-edited.xml", 
               src.dir = 'D:/APSIM工作文件夹/教学文件夹/New',
               wrt.dir = './New',
               overwrite = TRUE,
               parm.path = ".//Model/My_cul/tt_flower_to_start_grain",
               value = tt_flower_to_start_grain)
maize5 <- apsim('maize2.apsim')

#Phe
ggplot() + 
  geom_point(data = obsWh, aes(x = Date, y = Stage)) +
  geom_line(data = maize5, aes(x = Date, y = Stage)) + 
  ggtitle("Phenology")

## LAI
ggplot() + 
  geom_point(data = obsWh, aes(x = Date, y = lai)) +
  geom_line(data = maize5, aes(x = Date, y = LAI)) + 
  ggtitle("LAI")

## Biomass
ggplot() + 
  geom_point(data = obsWh, aes(x = Date, y = biomass)) +
  geom_line(data = maize5, aes(x = Date, y = Biomass)) + 
  ggtitle("Biomass (g/m2)")
















burnIn=10
par(mfrow = c(2,3))
hist(chain[-(1:burnIn),1],nclass=30,  main="Posterior of par1", xlab="True value = red line" )
abline(v = mean(chain[-(1:burnIn),1]))
abline(v = par[1], col="red" )
hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of par2", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),2]))
abline(v = par[2], col="red" )
hist(chain[-(1:burnIn),3],nclass=30, main="Posterior of par3", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),3]) )
abline(v = par[3], col="red" )
plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of par1")
abline(h = par[1], col="red" )
plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of par2" )
abline(h = par[2], col="red" )
plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of par3")
abline(h = par[3], col="red" )

par1=mean(chain[-(1:burnIn),1])
par2=mean(chain[-(1:burnIn),2])

edit_apsim_xml("Wheat.xml", 
               src.dir = 'E:/APSIM/OPTIM',
               wrt.dir = './New',
               parm.path = ".//Model/xifeng24/tt_end_of_juvenile",
               value = par1%>%as.character())
rue <- paste(c(0,0,rep(par2,6),0.00,0.00,0),collapse = "  ")
edit_apsim_xml("Wheat-edited.xml", 
               src.dir = './New',
               wrt.dir = './New',
               parm.path = ".//Model/plant/y_rue",
               edit.tag ='',
               value = rue)
wheat=apsim('Wheat2.apsim')
wheat=apsim('Wheat.apsim')
ObsSim=wheat%>%filter(Date%in%obsWh$Date)%>%
  select(Date,Stage,biomass,lai)%>%left_join(obsWh,.,by='Date')

## Biomass
ggplot() + 
  geom_point(data = obsWh, aes(x = Date, y = biomass)) +
  geom_line(data = wheat, aes(x = Date, y = biomass)) + 
  ggtitle("Biomass (g/m2)")



############################################
#APSIM NG
library(apsimx)
library(tidyverse)
extd.dir <- system.file("extdata", package = "apsimx")
setwd('E:/APSIM/OPTIM')
obsWheat=read.csv('E:/APSIM/OPTIM/ObsWNG.csv')%>%mutate(Date=as.Date(Date))
head(obsWheat)
str(obsWheat)


sim0 <- apsimx("Wheat2.apsimx", src.dir = extd.dir)  
ggplot(sim0, aes(Date, Wheat.AboveGround.Wt)) + 
  geom_line(size=1) +
  ggtitle("Phenology Stages")


sim0.s <- sim0%>%filter(Date > as.Date("2016-09-30") & Date < as.Date("2017-07-01"))

#Phe
ggplot() + 
  geom_point(data = obsWheat, aes(x = Date, y = Wheat.Phenology.Stage)) +
  geom_line(data = sim0.s, aes(x = Date, y = Wheat.Phenology.Stage)) + 
  ggtitle("Phenology")


## LAI
ggplot() + 
  geom_point(data = obsWheat, aes(x = Date, y = Wheat.Leaf.LAI)) +
  geom_line(data = sim0.s, aes(x = Date, y = Wheat.Leaf.LAI)) + 
  ggtitle("LAI")

## Biomass
ggplot() + 
  geom_point(data = obsWheat, aes(x = Date, y = Wheat.AboveGround.Wt)) +
  geom_line(data = sim0.s, aes(x = Date, y = Wheat.AboveGround.Wt)) + 
  ggtitle("Biomass (g/m2)")


## Finding RUE
inspect_apsimx_replacement("Wheat-opt-ex.apsimx", 
                           src.dir = extd.dir,
                           node = "Wheat", 
                           node.child = "Leaf",
                           node.subchild = "Photosynthesis",
                           node.subsubchild = "RUE", 
                           parm = "FixedValue",
                           verbose = FALSE)

## Finding BasePhyllochron
inspect_apsimx_replacement("Wheat-opt-ex.apsimx", 
                           src.dir = extd.dir,
                           node = "Wheat", 
                           node.child = "Cultivars",
                           node.subchild = "USA",
                           node.subsubchild = "Yecora", 
                           verbose = FALSE)

pp1 <- "Wheat.Leaf.Photosynthesis.RUE.FixedValue"
pp2 <- "Wheat.Cultivars.USA.Yecora.BasePhyllochron"


###
wop <- optim_apsimx("Wheat-opt-ex.apsimx", 
                    src.dir = extd.dir, 
                    parm.paths = c(pp1, pp2),
                    data = obsWheat, 
                    weights = "mean",
                    replacement = c(TRUE, TRUE),
                    initial.values = c(1.2, 120),
                    hessian = TRUE)


sim.opt <- apsimx("Wheat-opt-ex.apsimx", src.dir = extd.dir, value = "report")  
sim.opt.s <- sim.opt%>%filter(Date > as.Date("2016-09-30") & Date < as.Date("2017-07-01"))

## phenology
ggplot() + 
  geom_point(data = obsWheat, aes(x = Date, y = Wheat.Phenology.Stage)) +
  geom_line(data = sim.opt.s, aes(x = Date, y = Wheat.Phenology.Stage)) + 
  ggtitle("Phenology")


## LAI
ggplot() + 
  geom_point(data = obsWheat, aes(x = Date, y = Wheat.Leaf.LAI)) +
  geom_line(data = sim.opt.s, aes(x = Date, y = Wheat.Leaf.LAI)) + 
  ggtitle("LAI")


## Biomass
ggplot() + 
  geom_point(data = obsWheat, aes(x = Date, y = Wheat.AboveGround.Wt)) +
  geom_line(data = sim.opt.s, aes(x = Date, y = Wheat.AboveGround.Wt)) + 
  ggtitle("Biomass (g/m2)")




RFGRIND <- expand.grid(n.trees = (1:10)*50,
                       mtry= ((1:20)))




