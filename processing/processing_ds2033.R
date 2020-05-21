## clean things up.
rm(list=ls())

##load functions that will be needed
source(file = "functions/count_time_act_read.R")

##load libraries
library(plyr)
library(lattice)
library(reshape2)
library(data.table)
library(sciplot)
library(readr)
library(tidyr)

##read test data
evalInfo_import <- read_csv("SummaryQuizzes_ds2033.csv",col_types = cols(`Problem Start Time` = col_datetime(format = "%Y-%m-%d %H:%M:%S"))) ##file available in datashop: https://pslcdatashop.web.cmu.edu/Files?datasetId=2033


##change names of some columns
names(evalInfo_import)[c(1,2,3)] <- c("ds_anon_user_id","oldName","Time")

##remove eval points other than the first
evalInfo_import <- evalInfo_import[evalInfo_import$rep==1,]

##identify pre and exam
evalInfo_import$typeEvalPoint <- ifelse(grepl("pt", evalInfo_import$oldName)==TRUE,"pretest",ifelse(grepl("quiz", evalInfo_import$oldName)==TRUE,"quiz","exam"))

##change names to have one pretest and one exam per unit
for(i in 1:nrow(evalInfo_import)){
  if(!evalInfo_import$oldName[i]=="ccm_final_exam"){
    evalInfo_import$quizName[i] <- paste(strsplit(x = evalInfo_import$oldName[i],split = "_")[[1]][2],evalInfo_import$typeEvalPoint[i],sep="_")
  }else{
    evalInfo_import$quizName[i] <- "exam"
  }
}
rm(i)

##merge multiple parts of same quiz
evalInfo <- ddply(.data = evalInfo_import,.variables = .(ds_anon_user_id,quizName,typeEvalPoint),.fun = summarize,Time=min(Time),quizGrade=mean(quizGrade),nquestions=sum(nquestions))

##remove learning strategies unit
evalInfo <- evalInfo[-which(evalInfo$quizName=="u01_quiz"),]


##select students who have more than 80% of the quizzes and have pretest and exam data
summaryEvalInfo = ddply(.data = evalInfo,.variables = .(ds_anon_user_id ),.fun = summarize,evalPoints=length(quizName),.progress = "text")

##get number of unique evalpoints
evalPoints <- unique(evalInfo$quizName)

studentsToInclude = summaryEvalInfo$ds_anon_user_id[(summaryEvalInfo$evalPoints>length(evalPoints)*0.80)]

studentsToInclude = evalInfo$ds_anon_user_id[evalInfo$quizName=="exam"]

##import log_data
log_act <- read_delim("log_act.txt", "\t", escape_double = FALSE, col_types = cols(server_receipt_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"),session_date = col_skip(), session_server_receipt_date = col_skip(),session_timezone = col_skip(), transaction_time = col_datetime(format = "%Y-%m-%d %H:%M:%S")),trim_ws = TRUE) ##file available in datashop: https://pslcdatashop.web.cmu.edu/Export?datasetId=2033


##import names in info
pages <- read_csv("/ds_2033/countscourse_c_cm2.1_.csv", col_types = cols(X1 = col_skip())) ##file available in datashop: https://pslcdatashop.web.cmu.edu/Files?datasetId=2033


activities <- read_csv("/ds_2033/activityListcourse_c_cm2.1_.csv", col_types = cols(X1 = col_skip(), purpose = col_skip())) ##file available in datashop: https://pslcdatashop.web.cmu.edu/Files?datasetId=2033

info_ids <- c(pages$name,activities$idref)
#rm(pages,activities)

##add student id
log_sess <- read_delim("log_sess.txt","\t", escape_double = FALSE, trim_ws = TRUE) ##file available in datashop: https://pslcdatashop.web.cmu.edu/Files?datasetId=2033

#names(log_sess) <- c("sess_ref","ds_anon_user_id")
names(log_sess)[c(2,3)] <- c("sess_ref","actual_user_id")

anon_mappings <- read_csv("act_log/anon_mappings_031518.csv") ##file available in datashop: https://pslcdatashop.web.cmu.edu/Files?datasetId=2033

names(anon_mappings)[2] <- "ds_anon_user_id"
log_sess <- join(log_sess,anon_mappings)

View(log_sess)

log_act <- join(log_act,log_sess[,c(2,13)])
rm(log_sess)

#remove students without ds_anon_user_id
log_act <- log_act[!is.na(log_act$ds_anon_user_id),]

##remove students who didn't complete enough evalPoints
log_act <- log_act[log_act$ds_anon_user_id%in%studentsToInclude,]

##add number of words etc to the pages

names(pages)[1] <- "info" 
pages$page <-  pages$info
activities_with_numbers <- join(activities,pages[,-1])
names(activities_with_numbers)[1] <- "info"
pages <- pages[,c(1,5,2,3,4)]

availableCounts <- rbind(pages,activities_with_numbers)
availableCounts$textlevel = ifelse(availableCounts$wordcount>mean(availableCounts$wordcount),"High","Low")

log_act <- join(log_act,availableCounts)

##order by student and time
log_act <- log_act[order(log_act$ds_anon_user_id,log_act$server_receipt_time),]

##prepare for exporting counts
##### add timeDiff calculation ---------------
log_act$diffTime = append(0,as.numeric(diff(log_act$server_receipt_time),unit="mins"))
log_act$prevDiffTime = c(tail(log_act$diffTime,-1),NA)
log_act$nextDiffTime = c(NA, head(log_act$diffTime, -1))

### add session and action of next row and previous row ----
log_act$nextSess = c(tail(log_act$sess_ref,-1),NA)
log_act$prevSess = c(NA, head(log_act$sess_ref, -1))

log_act$nextAction = c(tail(log_act$action,-1),NA)
log_act$prevAction = c(NA, head(log_act$action, -1))

### add time of next row and previous row ----
log_act$nextTime = c(tail(log_act$server_receipt_time,-1),NA)
log_act$prevTime = c(as.POSIXct(NA,format="%Y-%m-%d %H:%M:%S"), head(log_act$server_receipt_time, -1))

##remove lines of log_act that do not belong to c@cm
log_act <- log_act[log_act$info%in%info_ids,]
log_act <- log_act[!log_act$ds_anon_user_id=="#N/A",]

#### add module info -------
#number quizzes
QuizNum <- as.numeric(as.factor(unique(evalInfo$quizName[evalInfo$typeEvalPoint=="quiz"])))
PTNum <- as.numeric(as.factor(unique(evalInfo$quizName[evalInfo$typeEvalPoint=="pretest"])))

QuizNum <- paste("quiz",QuizNum,sep = "")
PTNum <- paste("pt",PTNum,sep = "")

quizTable <- data.frame(unique(evalInfo$quizName[evalInfo$typeEvalPoint=="quiz"]),QuizNum)
ptTable <- data.frame(unique(evalInfo$quizName[evalInfo$typeEvalPoint=="pretest"]),PTNum)
names(quizTable) <- c("quizName","quizNum")
names(ptTable) <- c("quizName","quizNum")

quizTable <- rbind(quizTable,ptTable)

#quizTable$evalPoints <- as.character(quizTable$evalPoints)
#names(quizTable) <- c("quizName","quizNum")
evalInfo <- join(evalInfo,quizTable)
#evalInfo$quizNum <- ifelse(is.na(evalInfo$quizNum),"exam",evalInfo$quizNum)

quizDatesTable <- spread(evalInfo[,c(1,4,7)], quizNum, Time)

names(quizDatesTable)[10] <- "exam"

log_act <- join(log_act,quizDatesTable)
log_act$module <- ifelse(log_act$server_receipt_time>log_act$pt1&log_act$server_receipt_time<log_act$quiz1,1,ifelse(log_act$server_receipt_time>log_act$pt2&log_act$server_receipt_time<log_act$quiz2,2,ifelse(log_act$server_receipt_time>log_act$pt3&log_act$server_receipt_time<log_act$quiz3,3,ifelse(log_act$server_receipt_time>log_act$pt4&log_act$server_receipt_time<log_act$quiz4,4,NA))))

#remove stuff that is not activities/pages
log_act$module <- ifelse( grepl("quiz", log_act$info)==TRUE,NA,log_act$module)
log_act$module <- ifelse( grepl("exam", log_act$info)==TRUE,NA,log_act$module)
log_act$module <- ifelse( grepl("pt", log_act$info)==TRUE,NA,log_act$module)

log_act$include = ifelse(!is.na(log_act$module) & log_act$server_receipt_time<log_act$exam,"include","exclude")


##order by student and time
log_act <- log_act[order(log_act$ds_anon_user_id,log_act$server_receipt_time),]

##get counts overall
SumData = ddply(.data = log_act[log_act$include=="include",], .variables = .(ds_anon_user_id,module),.fun=summarize,
                NRead = view_page_count(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,page),
                NRead_unique = view_page_count_unique(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,page),
                NRead_trimmed = view_page_count_trimmed(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,page),
                NRead_trimmed_unique = view_page_count_trimmed_unique(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,page),
                NAction = action_count(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,Time2,nextTime,prevTime),
                NAction_unique = action_count_unique(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,Time2,nextTime,prevTime,info),
                TimeRead = view_page_time(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction),
                TimeReadTrimmed = view_page_time_trimmed(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction),
                TimeAction=action_time(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction),
                TimeActionToPage = action_to_page_time(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction),
                TimePageToAction=page_to_action_time(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction),
                CountActionToPage = action_to_page_count(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction),
                CountPageToAction = page_to_action_count(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction),
                wordcount=sum(wordcount[info==page]),
                npages=length(unique(info[info==page])),
                nactivities=sum(nactivities[info==page]),
                nfigures=sum(figures[info==page]),
                .progress = "text")

SumData$reading_rate = ifelse(!SumData$TimeReadTrimmed==0,SumData$TimeReadTrimmed/SumData$NRead_trimmed,0)

SumData$percentActivities = ifelse(!SumData$NAction==0,SumData$NAction/SumData$nactivities,0)
SumData$percentPages = ifelse(!SumData$NRead==0,SumData$NRead/SumData$npages,0)

OverallSumData <- ddply(.data = log_act[log_act$include=="include",],.variables = .(ds_anon_user_id),.fun = summarize,
                        NRead_trimmed_overall = view_page_count_trimmed(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,page),
                        NRead_trimmed_unique_overall = view_page_count_trimmed_unique(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,page),
                        NAction_overall = action_count(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,Time2,nextTime,prevTime),
                        NAction_unique_overall = action_count_unique(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,Time2,nextTime,prevTime,info),
                        TimeReadTrimmed_overall = view_page_time_trimmed(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction),
                        TimeAction_overall=action_time(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction),
                        wordcount_overall=sum(wordcount[info==page]),
                        npages_overall=length(unique(info[info==page])),
                        nactivities_overall=sum(nactivities[info==page]),
                        nfigures_overall=sum(figures[info==page]),
                        .progress = "text"
)
SumData <- join(SumData,OverallSumData)


##get counts by available stuff
SumDataByNumbers = ddply(.data = log_act[log_act$include=="include",], .variables = .(ds_anon_user_id,module,wordcount,nactivities,figures),.fun=summarize,
                         NRead = view_page_count(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,page),
                         NAction = action_count(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,Time2,nextTime,prevTime),
                         NAction_unique = action_count_unique(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,Time2,nextTime,prevTime,info),
                         NRead_corrected = view_page_count_trimmed(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,page),
                         NRead_corrected_unique = view_page_count_trimmed_unique(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,page),
                         TimeRead = view_page_time(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction),
                         TimeReadTrimmed = view_page_time_trimmed(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction),
                         TimeAction=action_time(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction),
                         TimeActionToPage = action_to_page_time(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction),
                         TimePageToAction=page_to_action_time(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction),
                         CountActionToPage = action_to_page_count(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction),
                         CountPageToAction = page_to_action_count(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction),
                         .progress = "text")

## get RI
##find the first and last event (page or activity) for each module.
d = log_act[!is.na(log_act$module),] #we only care for real modules, so no LS (0) or system stuff (100,300) and events need to happen before the quiz.
d = d[order(d$ds_anon_user_id,d$server_receipt_time),] #always reorder by timestamp and student to maintain ordered sequence! 
#d = d[-which(is.na(d$Time2)),] #remove any empty cells in the db

d$numRow = rep(1:max(length(d$info))) #number all db in the file for later reference.
setDT(d)[, first := seq_len(.N), by=rleid(ds_anon_user_id,module)] #number events per module and per student
setDT(d)[, runlength := max(.N), by=rleid(ds_anon_user_id,module)] #find the number of the last event per module and student
dd = d[d$first==1,] #keep only the first step of each event.
dd$LastRowOfRun = as.numeric(dd$numRow) + as.numeric(dd$runlength) - 1  #add what the last row of the run is for each event.
lines = dd$LastRowOfRun #find the number for the last event for each student and module
endTime = d[which(d$numRow %in% lines),'server_receipt_time'] #get the time of that last event from d
dd$endTime = endTime #add that time to dd (which right now only has the very first event)

dd$retention_interval = ifelse(dd$module==1,difftime(dd$quiz1,dd$endTime,units="mins"),ifelse(dd$module==2,difftime(dd$quiz2,dd$endTime,units="mins"),ifelse(dd$module==3,difftime(dd$quiz3,dd$endTime,units="mins"),ifelse(dd$module==4,difftime(dd$quiz4,dd$endTime,units="mins"),NA))))
  
RI_info <- dd[,c(15,39,46)]
rm(d,dd,endTime,lines)

SumData = join(SumData,RI_info)
SumDataByNumbers = join(SumDataByNumbers,RI_info)
rm(RI_info)

## add performance in activities
ds <- read_delim("ds2033/ds2033_tx_All_Data_3802_2017_0319_041503 copy.txt", "\t", escape_double = FALSE, col_types = cols(Time = col_datetime(format = "%Y-%m-%d %H:%M:%S")), trim_ws = TRUE) ##file available in datashop: https://pslcdatashop.web.cmu.edu/Files?datasetId=2033

#remove students not in the other analyses
ds <-  ds[ds$`Anon Student Id`%in%unique(SumData$ds_anon_user_id),]

#remove eval points stuff
activities <- read_csv("activityListcourse_c_cm2.1_.csv", col_types = cols(X1 = col_skip(), purpose = col_skip()))
ds <- ds[ds$`Problem Name`%in%c(activities$idref),] ##file available in datashop: https://pslcdatashop.web.cmu.edu/Files?datasetId=2033

rm(activities)

#add module info
names(ds)[c(4,17)] = c("ds_anon_user_id","info")
ds <- join(ds,quizDatesTable)

ds$module <- ifelse(ds$Time>ds$pt1&ds$Time<ds$quiz1,1,ifelse(ds$Time>ds$pt2&ds$Time<ds$quiz2,2,ifelse(ds$Time>ds$pt3&ds$Time<ds$quiz3,3,ifelse(ds$Time>ds$pt4&ds$Time<ds$quiz4,4,NA))))

#define whether row should be included or excluded
ds$include = ifelse(!is.na(ds$module) & ds$Time<ds$exam,"include","exclude")

#re-code correctness score
ds$correct = ifelse(ds$Outcome=="CORRECT",1,0)

#summarize
SumDataCorrect = ddply(.data = ds[ds$include=="include",],.variables = .(ds_anon_user_id,module),.fun = summarize,correctness_alltries = mean(correct,na.rm=T),correctness_firsttry = mean(correct[`Attempt At Step`==1],na.rm=T),meanAttempts=mean(`Attempt At Step`,na.rm=T),.progress = "text")

#add to SumData
SumData <- join(SumData,SumDataCorrect)
rm(SumDataCorrect,ds)

## add pretest, exam and quiz grades
#add module to evalInfo
pretestGrades = evalInfo[evalInfo$typeEvalPoint=="pretest",]
pretestGrades$module = ifelse(pretestGrades$quizNum=="pt1",1,ifelse(pretestGrades$quizNum=="pt2",2,ifelse(pretestGrades$quizNum=="pt3",3,ifelse(pretestGrades$quizNum=="pt4",4,NA))))
names(pretestGrades)[5] = "pretestGrade"

quizGrades = evalInfo[evalInfo$typeEvalPoint=="quiz",]
quizGrades$module = ifelse(quizGrades$quizNum=="quiz1",1,ifelse(quizGrades$quizNum=="quiz2",2,ifelse(quizGrades$quizNum=="quiz3",3,ifelse(quizGrades$quizNum=="quiz4",4,NA))))

examGrades = evalInfo[evalInfo$typeEvalPoint=="exam",]
names(examGrades)[5] = "examGrade"

grades = join(pretestGrades[,c(1,5,8)],quizGrades[,c(1,5,8)])
grades = join(grades,examGrades[,c(1,5)])

SumData = join(SumData,grades)
SumDataByNumbers = join(SumDataByNumbers,grades)

## add the dataset info
SumData$dataset <- "ds2033"
SumData$domain <- "c@cm"

SumDataByNumbers$dataset <- "ds2033"
SumDataByNumbers$domain <- "c@cm"

## save data
write.csv(x = SumData,file = "SumData_ds2033.csv",row.names = F)
write.csv(x = SumDataByNumbers,file = "SumDataByNumbers_ds2033.csv",row.names = F)
