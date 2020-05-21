view_page_time = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction){
    
  times = nextDiffTime[sess_ref==nextSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")&!nextAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")]
  page_view_time = sum(times)
  return(page_view_time)
}

view_page_count = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,page){
  
  page = page[sess_ref==nextSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")&!nextAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")] 
  
  page_view_count = length(page)
  return(page_view_count)
}

view_page_count_unique = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,page){
  
  page = page[sess_ref==nextSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")&!nextAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")] 
  
  page_view_count = length(unique(page))
  return(page_view_count)
}

view_page_time_noactivities = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,nactivities){
  
  times = nextDiffTime[sess_ref==nextSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")&!nextAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")&nactivities==0]
  page_view_time = sum(times)
  return(page_view_time)
}

view_page_count_noactivities = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,nactivities){
  
  times = nextDiffTime[sess_ref==nextSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")&!nextAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")&nactivities==0] 
  
  page_view_count = length(times)
  return(page_view_count)
}

view_page_time_activities = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,nactivities){
  
  times = nextDiffTime[sess_ref==nextSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")&!nextAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")&!nactivities==0]
  page_view_time = sum(times)
  return(page_view_time)
}

view_page_count_activities = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,nactivities){
  
  times = nextDiffTime[sess_ref==nextSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")&!nextAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")&!nactivities==0] 
  
  page_view_count = length(times)
  return(page_view_count)
}


view_page_time_trimmed = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction){
  
  times = nextDiffTime[sess_ref==nextSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")&!nextAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")]
  
  meanTimes = mean(times)
  low10 = quantile(times,c(0.10,0.90),na.rm = TRUE)[1]
  high10 = quantile(times,c(0.10,0.90),na.rm = TRUE)[2]
  
  times = times[times>low10]
  times[times>high10] = meanTimes
  page_view_time_trimmed = sum(times)
  
  return(page_view_time_trimmed)
}
  

view_page_count_trimmed = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,page){
  
  times = nextDiffTime[sess_ref==nextSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")&!nextAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")]
  pages = page[sess_ref==nextSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")&!nextAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")] 
  
  meanTimes = mean(times)
  low10 = quantile(times,c(0.10,0.90),na.rm = TRUE)[1]
  high10 = quantile(times,c(0.10,0.90),na.rm = TRUE)[2]
  
  #times = times[times>low10]
  #times[times>high10] = meanTimes
  pages = pages[times>low10]
  page_view_count_trimmed = length(pages)
  
  return(page_view_count_trimmed)
}

view_page_count_trimmed_unique = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,page){
  
  times = nextDiffTime[sess_ref==nextSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")&!nextAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")]
  pages = page[sess_ref==nextSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")&!nextAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")] 
  
  meanTimes = mean(times)
  low10 = quantile(times,c(0.10,0.90),na.rm = TRUE)[1]
  high10 = quantile(times,c(0.10,0.90),na.rm = TRUE)[2]
  
  #times = times[times>low10]
  #times[times>high10] = meanTimes
  pages = pages[times>low10]
  page_view_count_trimmed_unique = length(unique(pages))
  
  return(page_view_count_trimmed_unique)
}


action_time = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction){
 
  times = diffTime[sess_ref==prevSess&action%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT") & prevAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")]
  action_time = sum(times)
  
  return(action_time)
}

action_count = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,time,nextTime,prevTime){
  
  times = diffTime[sess_ref==prevSess&action%in%c("START_ATTEMPT","START_SESSION")&!action==prevAction & diffTime!=0 & prevAction != "VIEW_PAGE"]
  
  #times = diffTime[sess_ref==prevSess & action%in%c("START_ATTEMPT","START_SESSION") & prevAction!="VIEW_PAGE"]
  
  #times = diffTime[action%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT","SAVE_ATTEMPT","SUBMIT_ATTEMPT")]
  action_count = length(times)
  
  return(action_count)
}

action_time_hightext = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,textlevel){
  
  times = diffTime[sess_ref==prevSess&action%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT") & prevAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")&textlevel=="High"]
  action_time = sum(times)
  
  return(action_time)
}

action_count_hightext = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,time,nextTime,prevTime,textlevel){
  
  times = diffTime[sess_ref==prevSess&action%in%c("START_ATTEMPT","START_SESSION")&!action==prevAction & diffTime!=0 & prevAction != "VIEW_PAGE" & textlevel=="High"]
  
  #times = diffTime[sess_ref==prevSess & action%in%c("START_ATTEMPT","START_SESSION") & prevAction!="VIEW_PAGE"]
  
  #times = diffTime[action%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT","SAVE_ATTEMPT","SUBMIT_ATTEMPT")]
  action_count = length(times)
  
  return(action_count)
}

action_time_lowtext = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,textlevel){
  
  times = diffTime[sess_ref==prevSess&action%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT") & prevAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT") & textlevel=="Low"]
  action_time = sum(times)
  
  return(action_time)
}

action_count_lowtext = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,time,nextTime,prevTime,textlevel){
  
  times = diffTime[sess_ref==prevSess&action%in%c("START_ATTEMPT","START_SESSION")&!action==prevAction & diffTime!=0 & prevAction != "VIEW_PAGE" & textlevel=="Low"]
  
  #times = diffTime[sess_ref==prevSess & action%in%c("START_ATTEMPT","START_SESSION") & prevAction!="VIEW_PAGE"]
  
  #times = diffTime[action%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT","SAVE_ATTEMPT","SUBMIT_ATTEMPT")]
  action_count = length(times)
  
  return(action_count)
}

action_count_unique = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,time,nextTime,prevTime,info){
  
  info = info[sess_ref==prevSess&action%in%c("START_ATTEMPT","START_SESSION")&!action==prevAction & diffTime!=0 & prevAction != "VIEW_PAGE"]
  
  #times = diffTime[sess_ref==prevSess & action%in%c("START_ATTEMPT","START_SESSION") & prevAction!="VIEW_PAGE"]
  
  #times = diffTime[action%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT","SAVE_ATTEMPT","SUBMIT_ATTEMPT")]
  action_count_unique = length(unique(info))
  
  return(action_count_unique)
}

page_to_action_time = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction){
 
  times = diffTime[sess_ref==prevSess&action%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT") & prevAction %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")]
  page_to_action_time = sum(times)
  
  return(page_to_action_time)
}

page_to_action_count = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction){
  
  times = diffTime[sess_ref==prevSess&action%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT") & prevAction %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")]
  page_to_action_count = length(times)

return(page_to_action_count)
}

action_to_page_time = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction){
  
  times = diffTime[sess_ref==prevSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE") & prevAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")]
  action_to_page_time = sum(times)
  
  return(action_to_page_time)
}

action_to_page_count = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction){
  
  times = diffTime[sess_ref==prevSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE") & prevAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")]
  action_to_page_count = length(times)
  
  return(action_to_page_count)
}
 
