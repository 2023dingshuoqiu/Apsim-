rm(list = ls())
library(readxl)
Weather_Station <- read_excel("C:/Users/JATCN/Desktop/国家级地面气象观测站站点基本信息全表（最终）.xlsx")
a <- data.frame()
for (i in 1:nrow(Weather_Station)) {
  if(TRUE){
    a <- rbind(a,Weather_Station[i,])
  }
}

setwd("F:/中国历史气象数据_弓开元/APSIM气象数据用")
#读取气象数据文件，并且计算太阳辐射值
folder_path <- "F:/中国历史气象数据_弓开元/弓开元气象修正（有表头）"

# 获取文件夹中的文件名列表
file_names <- list.files(folder_path)
c <- c()
# 使用for循环遍历文件名列表
for (file_name in file_names) {
  # 打印每个文件名
  c <- c(c,file_name)
}
c
c <- substr(c, 1, nchar(c)-4)
c <- as.matrix(c,ncol = 1)
c


for (i in 1:length(c)) {
  
  #############读取站点气象数据并预处理#############
  File_path_i <- paste("F:/中国历史气象数据_弓开元/弓开元气象修正（有表头）/",c[i,1],'.csv',sep = "")
    Weather <- read.csv(File_path_i, header = T)
    colnames(Weather) <- c("number","year","month","day","TemAver","TemMax","TemMin","Humidity","Rainfall",
                           "WindSpeed","SunHour")
    
    latitude <- a[which(as.character(a$si_id)==c[i,1]),9,drop=T]
    longtitude <- a[which(as.character(a$si_id)==c[i,1]),10,drop=T]
    
    
    Weather $ ordinal_dates <- format(as.Date(paste(Weather $ year, Weather $ month, Weather $ day, sep = "-")), format = "%j")
    Weather $ ordinal_dates <- as.numeric(Weather $ ordinal_dates)
    
    Weather $ Diurnal_inclination <- 0.409*sin(2*pi/365*Weather $ ordinal_dates-1.39)
    
    Weather $ Sunshine_hours_Angle <-acos(-tan(latitude/180*pi)*tan(Weather $ Diurnal_inclination))
    
    Weather $ N <- 24*Weather $ Sunshine_hours_Angle/pi
    Weather $ dr <- 1+0.033*cos(2*pi/365*Weather $ ordinal_dates)
    
    
    Weather $ Ra <- 37.6*Weather $ dr*(Weather $ Sunshine_hours_Angle*sin(latitude/180*pi)*sin(Weather $ Diurnal_inclination)+
                                         cos(latitude/180*pi)*cos(Weather $ Diurnal_inclination)*sin(Weather $ Sunshine_hours_Angle))
    Weather $ SRAD <- (0.25+0.5*Weather $ SunHour/Weather $ N)*Weather $ Ra 
    
    #确定目标年份
    start_year <- 1960
    end_year <- 2020
    Weatherfinal <- Weather[Weather$year >= start_year & Weather$year <= end_year, ]
    rownames(Weatherfinal) <- c(1:nrow(Weatherfinal))
    Weatherfinal$vp <- 1
    Weatherfinal <- Weatherfinal[,-1]
    Weatherfinal $ ordinal_dates <- format(as.Date(paste(Weatherfinal $ year, Weatherfinal $ month, Weatherfinal $ day, sep = "-")), format = "%j")
    Weatherfinal $ "@DATE" <- paste(substring(as.character(Weatherfinal$year),3,4), Weatherfinal $ ordinal_dates, sep = "")
    
    
    Weatheroutput <- Weatherfinal[,c(1,12,18,5,6,8,7,19,11)]
    colnames(Weatheroutput) <-c("year","day","SRAD","TMAX","TMIN","RAIN","Hum","vp","code") 
    
    
      
      TargetWeather <- Weatheroutput

      TargetWeather[which(is.na(TargetWeather$SRAD)),3] <- 
        (TargetWeather[which(is.na(TargetWeather$SRAD))+1,3]+
           TargetWeather[which(is.na(TargetWeather$SRAD))-1,3])/2 #NA值处理
      
      TargetWeather[which(is.na(TargetWeather$TMAX)),4] <- 
        (TargetWeather[which(is.na(TargetWeather$TMAX))+1,4]+
           TargetWeather[which(is.na(TargetWeather$TMAX))-1,4])/2 #NA值处理
      
      TargetWeather$TMAX[which(TargetWeather$TMAX>=1000)]<-
        (TargetWeather[which(TargetWeather$TMAX>=1000)+1,4]+
           TargetWeather[which(TargetWeather$TMAX>=1000)-1,4])/2 #异常值处理
     
       TargetWeather[which(is.na(TargetWeather$TMIN)),5] <- 
        (TargetWeather[which(is.na(TargetWeather$TMIN))+1,5]+
           TargetWeather[which(is.na(TargetWeather$TMIN))-1,5])/2 #NA值处理
       
       TargetWeather$TMIN[which(TargetWeather$TMIN>=1000)]<-
         (TargetWeather[which(TargetWeather$TMIN>=1000)+1,5]+
            TargetWeather[which(TargetWeather$TMIN>=1000)-1,5])/2 #异常值处理
       
       
      TargetWeather[which(is.na(TargetWeather$RAIN)),6] <- 
        (TargetWeather[which(is.na(TargetWeather$RAIN))+1,6]+
           TargetWeather[which(is.na(TargetWeather$RAIN))-1,6])/2 #NA值处理
     
       TargetWeather$RAIN[which(TargetWeather$RAIN>=1000)]<-
        (TargetWeather[which(TargetWeather$RAIN>=1000)+1,6]+
           TargetWeather[which(TargetWeather$RAIN>=1000)-1,6])/2 #异常值处理
      
      TargetWeather[which(is.na(TargetWeather$Hun)),6] <- 
        (TargetWeather[which(is.na(TargetWeather$RAIN))+1,6]+
           TargetWeather[which(is.na(TargetWeather$RAIN))-1,6])/2 #NA值处理
      
      TargetWeather$Hun[which(TargetWeather$Hun>=1000)]<-
        (TargetWeather[which(TargetWeather$Hun>=1000)+1,6]+
           TargetWeather[which(TargetWeather$Hun>=1000)-1,6])/2 #异常值处理
      
      TargetWeather$SRAD <- round(TargetWeather$SRAD,1)
      
      
      
     
      
      #4. read Template File，计算amp,avp
      W <- TargetWeather
      W$month <- Weatherfinal $ month
      W$meanTem <- (W$TMAX+W$TMIN)/2
      yearmonth <- tapply(W$meanTem,list(W$year,W$month),mean)
      tt <- as.list(data.frame(t(yearmonth)))
      maxmin <- sapply(tt, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
      amp <- sum(maxmin)/dim(yearmonth)[1]
      tav <- mean(yearmonth,na.rm = TRUE)
      
      TemplateFilePath <- "C:/Users/JATCN/Desktop/57123.MET"
      TemplateFile <- readLines(TemplateFilePath, n=-1);
      TemplateFileAllLines <- TemplateFile[1:10]; # read Template File 
      
      
      
       outname <- paste0(a[i,2],".met")
      
      gaocheng <- as.character(round(a[i,7], digits = 1))  
      weidu <- as.character(round(a[i,9], digits = 3))
      jingdu <- as.character(round(a[i,10], digits = 3))
     
      
      
      
      sink(outname,append = F)
      cat(TemplateFileAllLines[1], "\n")
      cat(paste0(substr(TemplateFileAllLines[2],1,18),as.character(a[i,2])), "\n")
      cat(TemplateFileAllLines[3], "\n")
      cat(paste0(substr(TemplateFileAllLines[4],1,14),gaocheng),
          substr(TemplateFileAllLines[4],21,24),"\n")
      cat(paste0(substr(TemplateFileAllLines[5],1,11),weidu), "\n")
      cat(paste0(substr(TemplateFileAllLines[6],1,12),jingdu),"\n")
      cat(paste0(substr(TemplateFileAllLines[7],1,6),as.character(round(tav,3)),
                 substr(TemplateFileAllLines[7],16,77)), "\n")
      cat(paste0(substr(TemplateFileAllLines[8],1,6),as.character(round(amp,3)),
                 substr(TemplateFileAllLines[8],15,77)), "\n")
      cat(TemplateFileAllLines[9], "\n")
      cat(TemplateFileAllLines[10], "\n")
      cat(cbind(sprintf("%4d %4s %5.2f %6.2f %6.2f %5.2f %4.1f %4.1f %5d",
                        TargetWeather$year,TargetWeather$day,TargetWeather$SRAD,
                        TargetWeather$TMAX,TargetWeather$TMIN,TargetWeather$RAIN,
                        TargetWeather$Hum,TargetWeather$vp,TargetWeather$code)),
          sep="\n")
      sink()
}
     
       



        
    