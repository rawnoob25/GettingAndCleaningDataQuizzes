###This is an incomplete version of Wk2's quiz for getting and cleaning
##data. Question 1 is incomplete. Question 1's placeholder function 
#(q1UsingFromJson())   uses a FromJSON() command
#to retrieve the data from "https://api.github.com/users/jtleek/repos"
#circumventing the oauth2.0 process
#
#q1Incomplete() contains a partial version of this process;
#

{
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  if(!require(magrittr)){
    install.packages("magrittr")
    library(magrittr)
  }
  
}

q1Incomplete<-function(){
  oauth_endpoints("github")
  con<-file("secret.txt","r")
  a<-readLines(con,1)
  b<-readLines(con,1)
  close(con)
  client_ID<-sub(".+:","",a)
  client_secret<-sub(".+:","",b)
  myapp <- oauth_app("MyApplication",key =client_ID,secret =client_secret)
  github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
  gtoken <- config(token = github_token)
  req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
  stop_for_status(req)
  content(req)
}

q1UsingFromJson<-function(){
  reposJsonData<-fromJSON("https://api.github.com/users/jtleek/repos")
  cat("unique values of name fields (verify that datsharing among them):","\n",sep="")
  print(reposJsonData$name%>%unique)
  cat("fields for each observation (to verify that created_at is there)","\n",sep="")
  print(reposJsonData%>%names%>%sort)
  return(invisible(reposJsonData[reposJsonData$name=="datasharing","created_at"]))
}

q2<-function(){
  #browser()
  if(!exists("acs")){
    if(!require(readr)){
      install.packages("readr")
      library(readr)
    }
    fURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
    df<-read_csv(fURL)
    assign("acs",df,pos=.GlobalEnv) 
  }

  if(!require(sqldf)){
    install.packages("sqldf")
    library(sqldf)
  }
  out<-sqldf("select pwgtp1 from acs where AGEP<50",drv='SQLite')
  return(str(out))
}

q3<-function(){
  browser()
  if(!exists("acs")){
    if(!require(readr)){
      install.packages("readr")
      library(readr)
    }
    fURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
    df<-read_csv(fURL)
    assign("acs",df,pos=.GlobalEnv) 
  }
  
  if(!require(sqldf)){
    install.packages("sqldf")
    library(sqldf)
  }
  out<-sqldf("select distinct AGEP from acs",drv='SQLite')
  return (str(out))
}

q4<-function(){
  con<-url("http://biostat.jhsph.edu/~jleek/contact.html")
  htmlCode<-readLines(con,n=101)
  close(con)
  
  lineNums<-c(10,20,30,100)
  nChars<-integer(4)
  nChars[1]<-nchar(htmlCode[10])
  nChars[2]<-nchar(htmlCode[20])
  nChars[3]<-nchar(htmlCode[30])
  nChars[4]<-nchar(htmlCode[100])
  return(data.frame(lineNums,nChars))
}

q5<-function(){
  if(!file.exists("messyFortan.for")){
    fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
    download.file(fileUrl,destfile = "messyFortan.for")
  }
  widths<-c(10,9,4,9,4,9,4,9,4)
  df<-read.fwf(file="messyFortan.for",widths = widths,skip=4) #data starts on 5th line
  cat("first 5 observations (to verify that file was read in correctly","\n")
  names(df)<-paste0("Var",1:9)
  print(df%>%head(5))
  return (sum(df$Var4))      
}

