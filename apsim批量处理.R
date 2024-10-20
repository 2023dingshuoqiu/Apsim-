library(tidyverse)
library(lattice)
library(stringr)
select=dplyr::select
filter=dplyr::filter
summarise=dplyr::summarise
setwd("F:/OneDrive - zju.edu.cn/RAPSIM/Course7")
GenericSimFile_Ori <- readLines("Wheat.apsim",warn=F)

AllMet=list.files(pattern = ".met$",full.names = T)
Scenarios=read.csv("ScenariosManage.csv")%>%sample_n(10)
head(Scenarios)

SowingDate=as.Date('1/10/2012',format='%d/%m/%Y')
SowingDate=c(SowingDate-days(0:15),SowingDate+days(0:15))
Scenarios$SowingDate=sample(SowingDate,nrow(Scenarios),replace = T)%>%format(.,'%d-%b')%>%tolower()
Start_date='01/08/2012'
End_date="31/12/2014"

Scenarios$N=sample(seq(0,400,by=10),nrow(Scenarios),replace = T)
Scenarios$Dressing=sample(c(0.3,0.5,0.7),nrow(Scenarios),replace = T)
Scenarios$Irrigation=sample(seq(0,100,by=10),nrow(Scenarios),replace = T)
Scenarios$Start_date=Start_date
Scenarios$End_date=End_date

ReplaceF=function(i,GenericSimFile_Ori){
  Scenarios_sub=Scenarios[i,]
  
  ##Met
  Met=Scenarios_sub$Met%>%paste0(getwd(),'/',.,'.met')
  Old_location=grep(pattern="metfile", x=GenericSimFile_Ori)[1]+1
  print(GenericSimFile_Ori[Old_location])
  # GenericSimFile_Ori[Old_location]%>%str_extract_all(string = .,pattern = "[\\\\A-Za-z0-9.\\s-:]+.met")
  # Met=gsub(pattern="/","\\",Met)
  OldToNew_value=GenericSimFile_Ori[Old_location]%>% 
    str_replace_all(string = ., pattern = "[\\\\A-Za-z0-9.\\s-:]+.met",replacement = Met) 
  GenericSimFile_Ori[Old_location]=OldToNew_value
  print(OldToNew_value)
  
  
  ##Management
  GenericSimFile_Ori=ChangOneSoil('start_date',Scenarios_sub$Start_date)
  GenericSimFile_Ori=ChangOneSoil('end_date',Scenarios_sub$End_date)
  GenericSimFile_Ori=ChangOneSoil('Enter sowing window START date',Scenarios_sub$SowingDate)
  GenericSimFile_Ori=ChangOneSoil('Enter sowing window END date',Scenarios_sub$SowingDate)
  GenericSimFile_Ori=ChangOneSoil('mount of starter fertiliser at sowing',Scenarios_sub$N*Scenarios_sub$Dressing)
  GenericSimFile_Ori=ChangOneSoil('Amount of fertiliser to apply',Scenarios_sub$N*(1-Scenarios_sub$Dressing))
  GenericSimFile_Ori=ChangOneSoil('Amount of irrigation to apply',Scenarios_sub$Irrigation)
  
  
  ##Out
  Outname=Scenarios_sub$gridcell
  Old_location=grep(pattern="outputfile", x=GenericSimFile_Ori)[1]+1
  print(GenericSimFile_Ori[Old_location])
  Outnameori=GenericSimFile_Ori[Old_location]%>%str_extract_all(string = .,pattern = ">[^&].+<") %>%
    str_replace_all(string = ., pattern = ">|<",replacement = "")%>%strsplit(.,'.out')%>%map_chr(1)
  
  GenericSimFile_Ori=gsub(Outnameori,Outname,GenericSimFile_Ori)
  print(GenericSimFile_Ori[Old_location])
  writeLines(GenericSimFile_Ori,Outname%>%paste0('./NewAPSIM/',.,'.apsim'))
}


lapply(1:nrow(Scenarios),ReplaceF,GenericSimFile_Ori)


RunApsim=function(Simsfile,models_exe){
  cmd <- paste0('"', models_exe, '" "', Simsfile)
  r <- system(cmd, intern = FALSE,ignore.stdout=F,show.output.on.console=T) 
}


models_exe="C:/Program Files (x86)/APSIM710-r4158/Model/ApsimModel.exe"
Sims=list.files("./NewAPSIM/",pattern = ".apsim$",full.names = T)

# RunApsim(Sims,models_exe)

tictoc::tic()
lapply(Sims[1:10], RunApsim,models_exe)
tictoc::toc()









rm(list=ls())
library(tidyverse)
library(lattice)
library(stringr)
select=dplyr::select
filter=dplyr::filter
summarise=dplyr::summarise
setwd("D:/APSIM工作文件夹/小麦玉米轮作")
GenericSimFile_Ori <- readLines("小麦玉米轮作最终.apsim",warn=F)

class(GenericSimFile_Ori)



Mangement <- read.csv("D:/APSIM工作文件夹/OPTIM/mangement.csv")  
met <- Mangement$gridcell
Scenarios_sub <- read.csv("D:/APSIM工作文件夹/OPTIM/ScenariosManage.csv")
#####需要调用的土壤相关的函数######
ChangSoilLayers=function(Oldlabel,Newlabel){
  #对列进行筛选
  colselect <- Scenarios_sub[,colnames(Scenarios_sub)%>%grep(.,pattern = Newlabel[i],value = T)]
  #转化为两位小数的数值形变量
  Newvalue <- paste0(sprintf("%0.2f",colselect))
  #确定位置
  One=grep(pattern=Oldlabel, x=GenericSimFile_Ori)
  One=seq(One[1]+1,One[2]-1,1)
  print(GenericSimFile_Ori[One])
  #进行替换
  GenericSimFile_Ori[One]=GenericSimFile_Ori[One]%>%
    str_replace_all(string = .,pattern = "[0-9.]+",replacement = Newvalue)
  print(GenericSimFile_Ori[One])
  return(GenericSimFile_Ori)
}
ChangMultiSoil=function(Oldlabel,Newlabel){
  colselect=Scenarios_sub[,colnames(Scenarios_sub)%>%grep(.,pattern = Newlabel,value = T)]%>%as.numeric()
  Newvalue=paste0(sprintf("%0.2f",colselect))
  
  AllOne=grep(pattern=Oldlabel, x=GenericSimFile_Ori)
  startLL=1
  if(Oldlabel=="LL"){startLL=2}
  AllLL=length(AllOne)/2
  for(j in startLL:AllLL){
    One=seq(AllOne[2*j-1]+1,AllOne[2*j]-1,1)
    # print(GenericSimFile_Ori[One])
    GenericSimFile_Ori[One]=GenericSimFile_Ori[One]%>%
      str_replace_all(string = .,pattern = "[0-9.]+",replacement = Newvalue)
    # print(GenericSimFile_Ori[One])
  }
  return(GenericSimFile_Ori)
}
ChangOneSoil=function(Oldlabel,Newlabel){
  
  Newvalue=Newlabel%>%as.character()
  Old_location=grep(pattern=Oldlabel, x=GenericSimFile_Ori)
  
  NeedReplace=GenericSimFile_Ori[Old_location]%>%str_extract_all(string = .,pattern = ">[^&].+<") %>%
    str_replace_all(string = ., pattern = ">|<",replacement = "")
  OldToNew_value=gsub(NeedReplace,Newvalue,GenericSimFile_Ori[Old_location])
  
  GenericSimFile_Ori[Old_location]=OldToNew_value
  return(GenericSimFile_Ori)
}




##Met
Old_location <- grep(pattern="metfile", x=GenericSimFile_Ori)[1]+1
# GenericSimFile_Ori[Old_location]%>%str_extract_all(string = .,pattern = "[\\\\A-Za-z0-9.\\s-:]+.met")
str_view(GenericSimFile_Ori[Old_location],pattern = "[\\\\A-Za-z0-9.\\s-:]+.met")

# Met=gsub(pattern="/","\\",Met)  

OldToNew_value <- GenericSimFile_Ori[Old_location]%>% 
  str_replace_all(string = ., pattern = "\\d+\\.met",replacement = as.character(met[1])) 
GenericSimFile_Ori[Old_location] <- OldToNew_value



##Changing Soil for Layers
ChangSoilLayers=function(Oldlabel,Newlabel){
  #对列进行筛选
  colselect <- Scenarios_sub[,colnames(Scenarios_sub)%>%grep(.,pattern = Newlabel,value = T)]%>%as.numeric()
 #转化为两位小数的数值形变量
   Newvalue <- paste0(sprintf("%0.2f",colselect))
  #确定位置
  One=grep(pattern=Oldlabel, x=GenericSimFile_Ori)
  One=seq(One[1]+1,One[2]-1,1)
  print(GenericSimFile_Ori[One])
  #进行替换
  GenericSimFile_Ori[One]=GenericSimFile_Ori[One]%>%
    str_replace_all(string = .,pattern = "[0-9.]+",replacement = Newvalue)
  print(GenericSimFile_Ori[One])
  return(GenericSimFile_Ori)
}

Oldlabel=c("BD","AirDry",'LL15',"DUL","SAT","OC",'PH')
Newlabel=c("BD","AD",'LL',"DUL","SAT","SOC",'PH')
i <- 1
for (i in 1:length(Oldlabel)) {
  print(Oldlabel[i])
  GenericSimFile_Ori=ChangSoilLayers(Oldlabel[i],Newlabel[i])
}

##Changing mutilply 
ChangMultiSoil=function(Oldlabel,Newlabel){
  colselect=Scenarios_sub[,colnames(Scenarios_sub)%>%grep(.,pattern = Newlabel,value = T)]%>%as.numeric()
  Newvalue=paste0(sprintf("%0.2f",colselect))
  
  AllOne=grep(pattern=Oldlabel, x=GenericSimFile_Ori)
  startLL=1
  if(Oldlabel=="LL"){startLL=2}
  AllLL=length(AllOne)/2
  for(j in startLL:AllLL){
    One=seq(AllOne[2*j-1]+1,AllOne[2*j]-1,1)
    # print(GenericSimFile_Ori[One])
    GenericSimFile_Ori[One]=GenericSimFile_Ori[One]%>%
      str_replace_all(string = .,pattern = "[0-9.]+",replacement = Newvalue)
    # print(GenericSimFile_Ori[One])
  }
  return(GenericSimFile_Ori)
}

GenericSimFile_Ori=ChangMultiSoil(Oldlabel="LL",Newlabel="LL")
GenericSimFile_Ori=ChangMultiSoil(Oldlabel="Thickness",Newlabel="Depth")

##Changing one 
ChangOneSoil=function(Oldlabel="CN2Bare",Newlabel=Scenarios_sub$CN2b){
  
  Newvalue=Newlabel%>%as.character()
  Old_location=grep(pattern=Oldlabel, x=GenericSimFile_Ori)
  
  NeedReplace=GenericSimFile_Ori[Old_location]%>%str_extract_all(string = .,pattern = ">[^&].+<") %>%
    str_replace_all(string = ., pattern = ">|<",replacement = "")
  OldToNew_value=gsub(NeedReplace,Newvalue,GenericSimFile_Ori[Old_location])
  
  GenericSimFile_Ori[Old_location]=OldToNew_value
  return(GenericSimFile_Ori)
}

Oldlabel=c("DiffusConst","DiffusSlope",'SummerDate',"WinterDate","Salb","CN2Bare")
Newlabel=c(Scenarios_sub$diffus_const,Scenarios_sub$diffus_slope,"1-Jun","1-Jan",
           Scenarios_sub$Albedo,Scenarios_sub$CN2b)

for (i in 1:length(Oldlabel)) {
  print(Oldlabel[i])
  GenericSimFile_Ori=ChangOneSoil(Oldlabel[i],Newlabel[i])
}


##Management
GenericSimFile_Ori=ChangOneSoil('start_date',Scenarios_sub$Start_date)
GenericSimFile_Ori=ChangOneSoil('end_date',Scenarios_sub$End_date)
GenericSimFile_Ori=ChangOneSoil('Enter sowing window START date',Scenarios_sub$SowingDate)
GenericSimFile_Ori=ChangOneSoil('Enter sowing window END date',Scenarios_sub$SowingDate)
GenericSimFile_Ori=ChangOneSoil('mount of starter fertiliser at sowing',Scenarios_sub$N*Scenarios_sub$Dressing)
GenericSimFile_Ori=ChangOneSoil('Amount of fertiliser to apply',Scenarios_sub$N*(1-Scenarios_sub$Dressing))
GenericSimFile_Ori=ChangOneSoil('Amount of irrigation to apply',Scenarios_sub$Irrigation)






##Out
Outname <- met[1]%>%map_chr(-1)%>%strsplit(.,'.met')
Old_location <- grep(pattern="outputfile", x=GenericSimFile_Ori)[1]+1
str_view(GenericSimFile_Ori[Old_location],pattern = ">[^&].+<")

Outnameori=GenericSimFile_Ori[Old_location]%>%
  str_extract_all(string = .,pattern = ">[^&].+<") %>%
  str_replace_all(string = ., pattern = ">|<",replacement = "")%>%
  strsplit(.,'.out')%>%map_chr(1)

GenericSimFile_Ori=gsub(Outnameori,Outname,GenericSimFile_Ori)
# GenericSimFile_Ori[Old_location+1]
writeLines(GenericSimFile_Ori,Outname%>%paste0(.,'.apsim'))












###################################################
##调用APSIM.exe
RunApsim=function(Simsfile,models_exe){
  cmd <- paste0('"', models_exe, '" "', simsfile)
  r <- system(cmd, intern = FALSE,ignore.stdout=F,show.output.on.console=T) 
}

models_exe="C:/APSIM710/Model/ApsimModel.exe"

Sims=list.files("./",pattern = ".apsim$",full.names = T)

# RunApsim(Sims[1],models_exe)

lapply(Sims, RunApsim,models_exe)




































