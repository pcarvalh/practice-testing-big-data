## clean things up.
rm(list=ls())

##load functions that will be needed
source(file = "functions/addModuleInfo.R")
source(file = "functions/count_time_act_read.R")

##load libraries
library(plyr)
library(lattice)
library(reshape2)
library(data.table)
library(sciplot)
library(readr)

##read test data
evalInfo = read.csv(file = "SummaryQuizzes_ds863.csv") ##file available in datashop: https://pslcdatashop.web.cmu.edu/Files?datasetId=863
evalInfo$submission_time = as.POSIXct(evalInfo$submission_time, format="%Y-%m-%d %H:%M:%S",dz="America/New_York",usetz=TRUE)
evalInfo$submission_time_pretest = as.POSIXct(evalInfo$submission_time_pretest, format="%Y-%m-%d %H:%M:%S",dz="America/New_York",usetz=TRUE)
evalInfo$submission_time_exam = as.POSIXct(evalInfo$submission_time_exam, format="%Y-%m-%d %H:%M:%S",dz="America/New_York",usetz=TRUE)

##select students who have more than 80% of the quizzes and have pretest and exam data
summaryEvalInfo = ddply(.data = evalInfo,.variables = .(ds_anon_user_id),.fun = summarize,evalPoints=length(module),pretest=length(unique(grade_pretest[!is.na(grade_pretest)])),exam=length(unique(grade_exam[!is.na(grade_exam)])),.progress = "text")

studentsToInclude = summaryEvalInfo$ds_anon_user_id[summaryEvalInfo$evalPoints>7&summaryEvalInfo$pretest==1&summaryEvalInfo$exam==1]

studentsToInclude = evalInfo$ds_anon_user_id[!is.na(evalInfo$submission_time_exam)]

##import log_data
log_act <- read_delim("/ds863/log_act.txt", "\t", escape_double = FALSE, col_types = cols(server_receipt_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"),session_date = col_skip(), session_server_receipt_date = col_skip(),session_timezone = col_skip(), transaction_time = col_datetime(format = "%Y-%m-%d %H:%M:%S")),trim_ws = TRUE) ##file available in datashop: https://pslcdatashop.web.cmu.edu/Export?datasetId=863

View(log_act)

##add number of words etc to the pages
pages <- read_csv("/ds_863/countscourse_psychMOOC_.csv", col_types = cols(X1 = col_skip())) ##file available in datashop: https://pslcdatashop.web.cmu.edu/Files?datasetId=863

activities <- read_csv("/ds_863/activityListcourse_psychMOOC_.csv", col_types = cols(X1 = col_skip(), purpose = col_skip())) ##file available in datashop: https://pslcdatashop.web.cmu.edu/Files?datasetId=863

info_ids <- c(pages$name,activities$idref)

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

#### add module info -------
log_act = addModuleInfo(log_act)
log_act$module = ifelse(is.na(log_act$module),300,log_act$module)

log_act = join(log_act,evalInfo[,c(1,2,4,5,7)])                        

log_act = log_act[log_act$ds_anon_user_id%in%studentsToInclude,]

log_act$include = ifelse(log_act$server_receipt_time > log_act$submission_time_pretest & log_act$server_receipt_time < log_act$submission_time_exam,"include","exclude")

log_act$beforeQuiz = ifelse(log_act$server_receipt_time<log_act$submission_time,"beforequiz","afterquiz")

##order by student and time
log_act <- log_act[order(log_act$ds_anon_user_id,log_act$server_receipt_time),]

##get counts overall
SumData = ddply(.data = log_act[log_act$include=="include"&log_act$beforeQuiz=="beforequiz",], .variables = .(ds_anon_user_id,module),.fun=summarize,
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

OverallSumData <- ddply(.data = log_act[log_act$include=="include"&log_act$beforeQuiz=="beforequiz",],.variables = .(ds_anon_user_id),.fun = summarize,
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
SumDataByNumbers = ddply(.data = log_act[log_act$include=="include"&log_act$beforeQuiz=="beforequiz",], .variables = .(ds_anon_user_id,module,wordcount,nactivities,figures),.fun=summarize,
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
d = log_act[!(log_act$module%in%c(0,100,300)) & log_act$server_receipt_time<log_act$submission_time & log_act$server_receipt_time<log_act$submission_time_exam,] #we only care for real modules, so no LS (0) or system stuff (100,300) and events need to happen before the quiz.
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

dd$retention_interval = difftime(dd$submission_time,dd$endTime,units="mins")

RI_info <- dd[,c(2,29,40)]
rm(d,dd,endTime,lines)

SumData = join(SumData,RI_info)
SumDataByNumbers = join(SumDataByNumbers,RI_info)
rm(RI_info)

## add performance in activities
ds <- read_delim("/ds863/ds863_tx_All_Data_2287_2015_0813_191150.txt", "\t", escape_double = FALSE, col_types = cols(Time = col_datetime(format = "%Y-%m-%d %H:%M:%S")), trim_ws = TRUE) ##file available in datashop: https://pslcdatashop.web.cmu.edu/Files?datasetId=863

#remove students not in the other analyses
ds <-  ds[ds$`Anon Student Id`%in%unique(SumData$ds_anon_user_id),]

#add module info
names(ds)[c(4,18)] = c("ds_anon_user_id","info")
ds = addModuleInfo(ds)
ds$module = ifelse(is.na(ds$module),300,ds$module)

#add pretest and exam dates
ds <- join(ds,evalInfo[,c(1,2,4,5,7)])

#define whether row should be included or excluded
ds$include = ifelse(ds$module!=100&ds$Time<ds$submission_time&ds$Time>ds$submission_time_pretest,"include","exclude")

#re-code correctness score
ds$correct = ifelse(ds$Outcome=="CORRECT",1,0)

#summarize
SumDataCorrect = ddply(.data = ds[ds$include=="include",],.variables = .(ds_anon_user_id,module),.fun = summarize,correctness_alltries = mean(correct,na.rm=T),correctness_firsttry = mean(correct[`Attempt At Step`==1],na.rm=T),meanAttempts=mean(`Attempt At Step`,na.rm=T),.progress = "text")

#add to SumData
SumData <- join(SumData,SumDataCorrect)
rm(SumDataCorrect,ds)

## add pretest, exam and quiz grades
SumData = join(SumData,evalInfo[,c(1,4,6,3,8)])
names(SumData)[c(37,38,39)] <- c("pretestGrade","quizGrade","examGrade")

## add the dataset info
SumData$dataset <- "ds863"
SumData$domain <- "psych"

SumDataByNumbers$dataset <- "ds863"
SumDataByNumbers$domain <- "psych"

## save data
write.csv(x = SumData,file = "SumData_ds863.csv",row.names = F)
write.csv(x = SumDataByNumbers,file = "SumDataByNumbers_ds863.csv",row.names = F)
