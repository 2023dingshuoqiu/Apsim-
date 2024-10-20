library(tidyverse)
library(lattice)
library(stringr)
select=dplyr::select
filter=dplyr::filter
summarise=dplyr::summarise
setwd("F:/OneDrive - zju.edu.cn/RAPSIM/Course5_6")

Nafunc=function(a,b){
  if(is.na(b)){b=a}
  return(b)
}



GR=readRDS("./SoilSource/GRAV.rds") #石头
GR$GRAV_8=mapply(Nafunc,GR$GRAV_7,GR$GRAV_8)


SA=readRDS("./SoilSource/SA.rds") # 沙
SA$SA_8=mapply(Nafunc,SA$SA_7,SA$SA_8)


SI=readRDS("./SoilSource/SI.rds") #silt 粉
SI$SI_8=mapply(Nafunc,SI$SI_7,SI$SI_8)

SOC=readRDS("./SoilSource/SOC.rds") #有机碳
SOC$SOC_8=mapply(Nafunc,SOC$SOC_7,SOC$SOC_8)

BD=readRDS("./SoilSource/BD.rds")
BD$BD_8=mapply(Nafunc,BD$BD_7,BD$BD_8)

PH=readRDS("./SoilSource/PH.rds")
PH$PH_8=mapply(Nafunc,PH$PH_7,PH$PH_8)



head(SA)
head(SI)
head(SOC)
SA=SA%>%gather(SA,value,-longitude,-latitude,-gridcell)
SI=SI%>%gather(SI,value,-longitude,-latitude,-gridcell)
BD=BD%>%gather(BD,value,-longitude,-latitude,-gridcell)
GR=GR%>%gather(GR,value,-longitude,-latitude,-gridcell)
SC=SA%>%bind_cols(SI[,5])%>%dplyr::rename(value2=...6)%>%
  dplyr::mutate(SCvalue=100-value-value2)%>%select(-value,-value2)%>%
  mutate(SA=gsub("SA","SC",SA))%>%
  dplyr::rename(value=SCvalue,SC=SA)

SOC=SOC%>%gather(SOC,value,-longitude,-latitude,-gridcell)



#土壤类型
source("1_main soil function.R")
STexture=SC[,c("gridcell","longitude","latitude",'SC')]
STexture$Texture=mapply(Soiltexture,SA$value,SC$value,SI$value)
# STexture$TextureSimpe=mapply(SoiltextureSimple,SC$value)

STexture=STexture%>%dplyr::rename(.,ST=SC)%>%mutate(ST=gsub("SC","ST",ST))
STextureOut=STexture%>%spread(key=ST,value=Texture)
saveRDS(STextureOut,"./SoilSource/STexture.rds")

##Albedo
STextureAlbedo=STexture
STextureAlbedo$Albedo=mapply(Albedo_texture,STextureAlbedo$Texture)
STextureAlbedo=STextureAlbedo%>%filter(ST=="ST_1")%>%select(-ST,-Texture)
saveRDS(STextureAlbedo,"./SoilSource/SAlbedo.rds")

##CN2b
STextureCN2b=STexture
STextureCN2b$CN2b=mapply(RunoffcurveTexture,STextureCN2b$Texture)
STextureCN2b=STextureCN2b%>%filter(ST=="ST_1")%>%select(-ST,-Texture)
saveRDS(STextureCN2b,"./SoilSource/SCN2b.rds")


##diffus
diffusF=function(soiltext,index){
  if(soiltext%in%c("S","SL","SC")){
    diffus_const=250;diffus_slope=22
  }else if(soiltext %in% c("SIC","L", "LS", "CL","SCL","LS","SIL","SICL")) {
    diffus_const=88;diffus_slope=35
      }else if (soiltext %in% c("C")){
        diffus_const=40;diffus_slope=16
      }

  if(index==1){return(diffus_const)}
  if(index==2){return(diffus_slope)}
}

STextureDiffus=STexture
STextureDiffus$diffus_const=mapply(diffusF,STextureDiffus$Texture,1)
STextureDiffus$diffus_slope=mapply(diffusF,STextureDiffus$Texture,2)
head(STextureDiffus)
STextureDiffus=STextureDiffus%>%filter(ST=="ST_1")%>%select(-ST,-Texture)
saveRDS(STextureDiffus,"./SoilSource/Diff.rds")

##SWCON
SWCONF=function(soiltext,index){
  if(soiltext%in%c("S","SL","SC")){
    SWCON=0.7
  }else if(soiltext %in% c("SIC","L", "LS", "CL","SCL","LS","SIL","SICL")) {
    SWCON=0.5
  }else if (soiltext %in% c("C")){
    SWCON=0.3
  }
  return(SWCON)
}
STextureSWCON=STexture
STextureSWCON$value=mapply(SWCONF,STextureSWCON$Texture)

head(STextureSWCON)
STextureSWCON=STextureSWCON%>%select(-Texture)%>%
  dplyr::rename(SWCON=ST)%>%mutate(SWCON=gsub("ST","SWCON",SWCON))%>%spread(key=SWCON,value=value)

saveRDS(STextureSWCON,"./SoilSource/SWCON.rds")


#SAT
SAT=BD
SAT$Texture=STexture$Texture
SAT=SAT%>%mutate(
  Diffvalue=case_when(
    Texture%in%c("S","SL","SC") ~ 0.07,
    Texture%in%c("C") ~ 0.03,
    TRUE ~0.05))%>%mutate(SATvalue=round(1-value/2.65-Diffvalue,2))%>%
  select(-Texture,-value,-Diffvalue)%>%
  dplyr::rename(SAT=BD)%>%mutate(SAT=gsub("BD","SAT",SAT))
SATsave=SAT%>%spread(key=SAT,value=SATvalue)
saveRDS(SATsave,"./SoilSource/SAT.rds")


##Dul LL
DUL=SOC[,c("gridcell","longitude","latitude","SOC")]
DUL$SAT=SAT$SATvalue
Sclay=SC$value
SLOC=SOC$value
Sand=SA$value



DLLcal=function(Sclay,SLOC){
  A1 = 0.026; D1 = 0.005; E1 = 0.0158
  DLL <- round(A1 + D1 * Sclay + E1 * SLOC*1.724, 2)
  return(DLL)
}

DUlcal=function(Sand,Sclay,SLOC){
  A2 = 0.2576; B2 = -0.002; D2 = 0.0036; E2 = 0.0299
  DUL <- round(A2 + B2 * Sand + D2 * Sclay + E2 * SLOC*1.724, 2)
  return(DUL)
}

DUL$DUL=mapply(DUlcal,Sand,Sclay,SLOC)
head(DUL)
DUL$LL=mapply(DLLcal,Sclay,SLOC)



DULOut=DUL%>%select(-LL,-SAT)%>%dplyr::rename(DULix=SOC)%>%mutate(DULix=gsub("SOC","DUL",DULix))%>%
  spread(key = DULix,value = DUL)
saveRDS(DULOut,"./SoilSource/DUL.rds")


DLLout=DUL%>%select(-DUL,-SAT)%>%dplyr::rename(DLLix=SOC)%>%mutate(DLLix=gsub("SOC","LL",DLLix))%>%
  spread(key = DLLix,value = LL)
DLLout%>%head()
saveRDS(DLLout,"./SoilSource/LL.rds")

AirD=DLLout
colnames(AirD)[4:11]=paste0("AD_",1:8)
AirD$AD_1=AirD$AD_1*0.5
AirD$AD_2=AirD$AD_2*0.8
AirD%>%head()
saveRDS(AirD,"./SoilSource/AirD.rds")

############土壤数据合并
####################################
##combine soil
SoilList=c("Diff","SCN2b","SAlbedo","SAT","DUL","LL","AirD","BD","SOC","PH","SWCON")

soil1=readRDS(paste0("./SoilSource/",SoilList[1],".rds"));nrow(soil1)

for (i in 2:length(SoilList)){
  soilnew=readRDS(paste0("./SoilSource/",SoilList[i],".rds"))%>%select(-longitude,-latitude)
  soil1=merge(soil1,soilnew,by=c("gridcell"))
}

nrow(soil1)
head(soil1)

#保存所有的土壤信息
write.csv(soil1,"ScenariosSoil2.csv",row.names = F)








