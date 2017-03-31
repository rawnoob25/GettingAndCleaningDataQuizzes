{
  if(!require(dplyr)){
    install.packages("dyplr")
    library(dplyr)
  }
  if(!require(magrittr)){
    install.packages("magrittr")
    library(magrittr)
  } 
  if(!require(readr)){
    install.packages("readr")
    library(readr)
  }
  if(!require(quantmod)){
    install.packages("quantmod")
  }
}

q1<-function(){
  codeBkURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf"
  if(!file.exists("dataDict.pdf")){
    download.file(codeBkURL,destfile="dataDict.pdf",mode="wb")
  }
  fileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
  if(!exists("housingDF")){
    assign("housingDF",read_csv(fileURL),pos=.GlobalEnv)
  }
  forQ1<-strsplit(names(housingDF),split="wgtp")
  return(forQ1[123])
}

q2<-function(){
  gdpURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
  if(!file.exists("gdp.csv")){
    download.file(gdpURL,destfile="gdp.csv",mode="wb")
  }
  if(!exists("gdp")){
    namesOfCols<-c("countryCodes","V2","V3","countryNames","GDPs",paste0("V",6:9))
    df<-read_csv("gdp.csv",skip=5,n_max=231,col_names=namesOfCols,col_types=cols_only(countryCodes=col_character(),countryNames=col_character(),GDPs=col_character()))
    gdp<-df
  }
  gdpRev<-gdp%>%filter(!is.na(gdp$GDPs),gdp$GDPs!="..")
  gdpRev<-gdpRev[1:190,]%>%as.data.frame
  #pstopping at 190 picks out only country data, excluding regional data
  gdpRev$GDPs<-gsub(",","",gdpRev$GDPs)
  gdpSecondRev<-transform(gdpRev,GDPs=as.numeric(GDPs))
  return(gdpSecondRev$GDPs%>%mean)
}

# In the data set from Question 2 what is a regular expression that would allow you to count the number of countries whose name begins with "United"? 
#Assume that the variable with the country names in it is named countryNames. How many countries begin with United? 

q3<-function(){
  grep("^United",gdp$countryNames,value=T)%>%print
}

q4<-function(){
  eduURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
  edu<-read_csv(eduURL)
  names(edu)
  #you'll see Special Notes among them...a little probing will show you that this
  #is the column that houses the end of fiscal year data
  juneFscalYrEnd<-grep("^.+end: June",edu$`Special Notes`,value=T)
  print(juneFscalYrEnd)
  return(juneFscalYrEnd%>%length)
}


# You can use the quantmod (http://www.quantmod.com/) package to get historical stock 
# prices for publicly traded companies on the NASDAQ and NYSE. Use the following code 
# to download data on Amazon's stock price and get the times the data was sampled.

#library(quantmod)
#amzn = getSymbols("AMZN",auto.assign=FALSE)
#sampleTimes = index(amzn)
q5<-function(){
  library(quantmod)
  amzn = getSymbols("AMZN",auto.assign=FALSE)
  sampleTimes = index(amzn)
  sampleTimesConv<-format(sampleTimes,"%a %b %Y")
  numValsIn2012<-grep("2012$",sampleTimesConv)%>%length
  numValsOnMondaysIn2012<-grep("^Mon.*2012",sampleTimesConv)%>%length
  return(cat("Total in 2012:",numValsIn2012," Mondays in 2012:",numValsOnMondaysIn2012,"\n",sep=""))
}