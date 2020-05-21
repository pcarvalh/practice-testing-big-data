## import data we will work with
library(plyr)
library(data.table)

addModuleInfo = function(dataSet){
  keyInfo_attempt = read.delim("~/Dropbox/Work/Projects/OLI Sequencing/Psych MOOC/100 students/mapping content weeks/OLI_start_attempt_week_mapping.txt", header=FALSE, stringsAsFactors=FALSE)
  names(keyInfo_attempt) = c("info","module")
  
  keyInfo_session = read.delim("~/Dropbox/Work/Projects/OLI Sequencing/Psych MOOC/100 students/mapping content weeks/OLI_start_session_week_mapping.txt", header=FALSE, stringsAsFactors=FALSE)
  names(keyInfo_session) = c("info","module")
  
  keyInfo_activities = join(keyInfo_attempt,keyInfo_session)
  
  keyInfo_page = read.delim("~/Dropbox/Work/Projects/OLI Sequencing/Psych MOOC/100 students/mapping content weeks/OLI_view_page_week_mapping.txt", header=FALSE, stringsAsFactors=FALSE)
  names(keyInfo_page) = c("info","module")
  
  keyInfo = rbind(keyInfo_page,keyInfo_activities)
  
  d = join(dataSet,keyInfo,by="info",type="left",match="first")
  
  return(d)  
}