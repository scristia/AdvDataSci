library(rvest)
library(jsonlite)
library(RCurl)
scop_apiKey<-"&apiKey=8b92081e8f0c54c8c14d17308ee8c8ea"
search_apiweb<-"http://api.elsevier.com/content/search/scopus?"

affil<-"Johns%20Hopkins"
auid<-"35069047000"
query=paste0("query=affil(", affil, ")%20and%20au-id(",
             auid, ")",scop_apiKey)
#http://dev.elsevier.com/elsdoc/search
url.site<-paste0(search_apiweb,query)
testsub<-url(url.site)
newtest<-fromJSON(testsub)
head(newtest)

urltest <- "http://www.scopus.com/results/results.url?numberOfFields=0&src=s&clickedLink=&edit=&editSaveSearch=&origin=searchbasic&authorTab=&affiliationTab=&advancedTab=&scint=1&menu=search&tablin=&searchterm1=Meta-models+and+non+point+&field1=TITLE_ABS_KEY&dateType=Publication_Date_Type&yearFrom=Before+1960&yearTo=Present&loadDate=7&documenttype=All&subjects=LFSC&subjects=HLSC&subjects=PHSC&subjects=SOSC&src=s&st1=Meta-models+and+non+point+&st2=&sot=b&sdt=b&sl=&s=TITLE-ABS-KEY%28Meta-models+and+non+point+%29&sid=5A0C2AE5F7594591436D5A72E9879EE3.FZg2ODcJC9ArCe8WOZPvA%3A40&searchId=5A0C2AE5F7594591436D5A72E9879EE3.FZg2ODcJC9ArCe8WOZPvA%3A40&txGid=5A0C2AE5F7594591436D5A72E9879EE3.FZg2ODcJC9ArCe8WOZPvA%3A4&sort=plf-f&originationType=b&rr="
