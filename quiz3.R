##NOTES:
#1. Remember, when downloading jpg file from web,
#use mode="wb" as param to downnload.file
#2. Actually it may be a good idea to use mode="wb"
#whenever you need to read a file in
#3. Note use of col_types=cols_only(...)
 #in q3
##ISSUSES: 
# 1. The Codebook for question 1 downloads OK using
# download.file, but once downloaded, it cannot be 
# opened. However, using a point and click download
# it CAN be opened

{ 
  if(!require(reshape2)){
    install.packages("reshape2")
    library(reshape2)
  }
  if(!require(dplyr)){
    install.packages("dyplr")
    library(dplyr)
  }
  if(!require(magrittr)){
    install.packages("magrittr")
    library(magrittr)
  }
  if(!require(jpeg)){
    install.packages("jpeg")
    library(jpeg)
  }
}

{ 
  if(!file.exists("ACS2006CodeBook.pdf")){
    codeBkURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf"
    download.file(codeBkURL,destfile="ACS2006CodeBook.pdf") 
  }
  if(!file.exists("housingData.csv")){
    housingURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
    download.file(housingURL,destfile="housingData.csv")
  }
  
  if(!file.exists("jeffLeekPic.jpg")){
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg",destfile="jeffLeekPic.jpg",mode="wb")
  }
  
  if(!exists("housing",envir=.GlobalEnv)){
    assign("housing",read_csv("housingData.csv"),pos=.GlobalEnv)
  }
  if(!file.exists("GDPdata.csv")){
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv",destfile="GDPdata.csv",mode="wb")
  }
  if(!file.exists("Education_data.csv")){
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv",
                  destfile="Education_data.csv",mode="wb")
  }
  
}

q1<-function(){
  agricultureLogical<-housing$ACR==3&housing$AGS==6
  return(which(agricultureLogical)%>%head(3))
}

q2<-function(){
  if(!exists("leekPic")){
    assign("leekPic",readJPEG("jeffLeekPic.jpg",native=T),pos=.GlobalEnv)
  }
  print(str(leekPic))
}

q3<-function(){
  education<-read_csv("Education_data.csv",col_types=cols_only(CountryCode=col_character(),"Income Group"=col_character()))
  assign("education",as.data.frame(education),pos=.GlobalEnv)
  namesOfCols<-c("CountryCode","GDPRank","X3","CountryName","GDP",paste0("X",6:9))
  gdp<-read_csv("GDPdata.csv",skip=5,n_max=231,col_names=namesOfCols,col_types=cols_only(CountryCode=col_character(),GDPRank=col_integer(),CountryName=col_character(),GDP=col_character()))
  gdp<-gdp[complete.cases(gdp),]
  assign("gdp",as.data.frame(gdp),pos=.GlobalEnv)
  
  merged<-inner_join(education,gdp,by=c("CountryCode"="CountryCode"))
  assign("merged",as.data.frame(merged),pos=.GlobalEnv)
  matches<-merged%>%nrow
  country13thLowestGDP<-merged%>%arrange(desc(GDPRank))%>%head(13)%>%tail(1)%>%select(CountryName)
  val<-data.frame(country13thLowestGDP)[1,1]
  return(cat("Matches:",matches,"    country:",val,"\n",sep=""))  
}

q4<-function(){
  merged$`Income Group`<-factor(merged$`Income Group`)
  assign("merged",merged,pos=.GlobalEnv)
  aveRanks<-merged%>%group_by(`Income Group`)%>%summarise(aveRanks=mean(GDPRank))
  print(aveRanks)
  #print(sapply(merged,class)) for debugging
  #print(merged$`Income Group`%>%summary) for debugging
  
}