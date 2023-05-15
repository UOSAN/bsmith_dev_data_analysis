library(tidyverse)

clean_sst_data <-function(sst_all_data_raw){
  sst_all_data<-sst_all_data_raw
  sst_all_data$reaction_time_clean<-sst_all_data$reaction_time
  sst_all_data$reaction_time_clean[sst_all_data$reaction_time==0]<-NA
  sst_all_data$reaction_time_clean[sst_all_data$reaction_time_clean<0.15]<-NA
  return(sst_all_data)
}

get_more_sst_trial_stats <- function(sst_all_data){
  
  # OOSTERLAAN et al. 1998:
  # as follows : FIRSTrst, reactiontimes on go trials are rank ordered on a time axis. Second, we pick the nth reaction time, where n is defined by the product ofthe  number  of  reaction  times  in  the  distribution  and  theprobability of responding given a stop signal (or 1 minus theprobability  of  inhibition).  For  example,  if  there  were  100reaction  times  in  the  distribution  and  the  probability  ofresponding given a stop signal was .3, thenth reaction timewould be the 30th in the rank-ordered distribution. Thenthreaction time  is  an estimate  of  the time  at  which  the  stopprocess runs to completion, relative to the onset of the primarytask stimulus.
  #Third, we subtract stop signal delay from then nth reaction  time  and  estimate  SSRT.  For  example,  if  the nth reaction  time  was  545 msec  and  the  stop  signal  delay  was 200 msec, SSRT would be 345 msec.
  #For  example,  if  the nth reaction  time  was  545 msec  and  the  stop  signal  delay  was200 msec, SSRT would be 345 msec. SSRT is calculated foreach stop signal delay and then averaged.
  
  #should probably also exclude subjects who have proportions correct outside a central range
  #because the experiment 'titrates' them to have a constant response of 50%; outside of that range means they aren't engaged in the task.
  sst_all_data <- sst_all_data %>% group_by(subid,waveid, runid) %>%   
    mutate(
      stop_prop_correct=sum(condition=="CorrectStop")/sum(condition %in% c("CorrectStop","FailedStop")),
      nth_reaction_time = quantile(reaction_time_clean[condition=="CorrectGo" & !is.na(reaction_time_clean)],1-stop_prop_correct,na.rm=TRUE)[[1]],
      #ladder_mean = mean(LadderX_SSD_ms[LadderX_SSD_ms>0],na.rm=TRUE),
      ssrt_0=case_when(
        condition %in% c("CorrectStop","FailedStop") ~ nth_reaction_time-SSD_recorded,
        TRUE ~ as.numeric(NA)
      )
    ) %>% ungroup()
  return(sst_all_data)
}

get_more_sst_stats <- function(sst_all_data){
  sst_all_data <- get_more_sst_trial_stats(sst_all_data)
  
  mean_ssrts <- sst_all_data %>% group_by(subid,waveid, runid) %>% summarize(mean_ssrt_0 = mean(ssrt_0[condition=="CorrectStop"],na.rm=TRUE))   
  
  sst_all_data <- merge(sst_all_data,mean_ssrts,by = c("subid","waveid","runid"))
  return(sst_all_data)
  
  
}

get_sst_post_pre <- function(sst_all_data){
  sst_all_data<- 
    sst_all_data %>% 
    mutate(has_response=!is.na(reaction_time_clean)) %>%
    mutate(follows_response_trial=lag(has_response,2),
           precedes_response_trial=lead(has_response,2)
    ) %>%
    mutate(leading_rt = lead(reaction_time_clean,2),lagging_rt=lag(reaction_time_clean,2)) %>%
    mutate(post_pre_rt_change=leading_rt-lagging_rt)
  return(sst_all_data)
}

calculate_rpe<-function(sst_all_data){
  
  
  sst_all_data <- sst_all_data %>% 
    mutate(
      response_initiation_time = case_when(
        reaction_time_clean!=0 ~ (reaction_time_clean-response_latency_10),
        TRUE ~ as.numeric(NA)
      )
    ) %>%
    mutate(
      RPE_SSD_based = case_when(
        has_SSD ~ SSD_recorded - last_tone_delay,
        TRUE ~ as.numeric(NA)
      ),
      
      RPE_SSD_RT_diff = case_when(
        has_SSD ~ SSD_recorded - response_initiation_time,
        TRUE ~ as.numeric(NA)
      )
    )
  return(sst_all_data)
}

calculate_response_latency<-function(sst_all_data){
  response_latency_means <- sst_all_data %>% group_by(subid, waveid,runid) %>% filter(condition!="Cue" & reaction_time!=0) %>%
    summarise(rt_quant_5 = quantile(reaction_time,0.05,na.rm=TRUE),
              rt_quant_10 = quantile(reaction_time,0.1,na.rm=TRUE),
              rt_quant_20 = quantile(reaction_time,0.2,na.rm=TRUE),
              rt_quant_50 = quantile(reaction_time,0.5,na.rm=TRUE),
              rt_quant_90 = quantile(reaction_time,0.9,na.rm=TRUE),
              rt_sample = n()
    ) %>% 
    mutate(quant_50_minus_10 = (rt_quant_50 - rt_quant_10),
           response_latency_10=rt_quant_10
    )
  
  
  sst_all_data <- sst_all_data %>% merge(response_latency_means %>% select(subid,waveid,runid,response_latency_10),all.x = TRUE)
  return(sst_all_data)
}

get_expected_tone_time <- function(sst_all_data){
  #one item after minus one item before
  sst_all_data <- get_sst_post_pre(sst_all_data)
  
  #now let's compare the next response time with the current response time--probably a better measure of learning
  #this relies on the code in get_sst_post_pre
  sst_all_data<- 
    sst_all_data %>%
    mutate(post_current_rt_change=leading_rt-reaction_time_clean)
  
  sst_all_data<- sst_all_data %>%
    mutate(has_SSD=SSD_recorded>0,IsTask=condition!="Cue") %>%
    mutate(has_SSD_lagged=lag(has_SSD,1,default=0)) %>%
    mutate(grp=cumsum(has_SSD_lagged)) %>%
    group_by(subid, waveid, runid, grp) %>%
    mutate(trials_since_last_SSD=cumsum(IsTask)) %>%
    ungroup()# %>% select(subid, waveid, runid, SSD_recorded,condition,grp, has_SSD_lagged) %>% View
  
  sst_all_data$last_tone_delay<-NA
  sst_all_data <-sst_all_data %>% group_by(subid,waveid,runid) %>%
    mutate(
      last_tone_delay=case_when(
        lag(SSD_recorded,1)>0 ~lag(SSD_recorded,1),
        TRUE ~ as.numeric(NA)
      )
    ) %>% #group_by(subid,waveid,runid) %>%
    tidyr::fill(last_tone_delay,.direction="down") %>%
    mutate(
      last_correct_go=case_when(
        condition=="CorrectGo" ~ reaction_time_clean,
        TRUE ~ as.numeric(NA)
      )
    ) %>%
    tidyr::fill(last_correct_go,.direction="down") %>%
    mutate(last_correct_go=lag(last_correct_go,1,default=NA)) %>%
    mutate(
      last_reaction_time=case_when(
        !is.na(reaction_time_clean) ~ reaction_time_clean,
        TRUE ~ as.numeric(NA)
      )
    ) %>%
    tidyr::fill(last_reaction_time,.direction="down") %>%
    mutate(last_reaction_time=lag(last_reaction_time,1,default=NA)) %>%
    mutate(
      next_reaction_time=case_when(
        !is.na(reaction_time_clean) ~ reaction_time_clean,
        TRUE ~ as.numeric(NA)
      )
    ) %>%
    tidyr::fill(next_reaction_time,.direction="up") %>%
    mutate(next_reaction_time=lead(next_reaction_time,1,default=NA))%>%
    mutate(next_last_rt_change=next_reaction_time-last_reaction_time)
  # let's create a measure that uses RPE change if the very last trial had an SSD; otherwise uses rt change
    #this is difficult because we actually need a DIFFERENCE between at least two tone delays
    #we can't compare tone delay directly to response time without knowing response latency, which I don't think we are estimating very well, considering the success of measures based on it
    
    # mutate(case_based_change =
    #          case_when(
    #            trials_since_last_SSD==1 ~ 
    #          )
    #          next_reaction_time-last_reaction_time)
    # 
  
  #now we want to removedtr
  
  sst_all_data$trials_since_last_SSD[sst_all_data$grp==0]<-NA

  sst_all_data$grp<-NULL
  
  #difference in tone delays with this one compared to the last one
  sst_all_data$cur_minus_prev_tone_delay <-sst_all_data$SSD_recorded - sst_all_data$last_tone_delay
  #then mark NA where either of those was zero, i.e., not a real tone
  sst_all_data$cur_minus_prev_tone_delay[(sst_all_data$SSD_recorded==0) | (sst_all_data$last_tone_delay==0)]<-NA
  
  #now let's identify the number of trials to-date
  #should be simple. one column for Go trials; one column for Stop trials; then do a cumsum on each, then calculate sum and proportions of that sum
  
  sst_all_data$is_go_trial<- sst_all_data$go_no_go_condition_label=="Go"
  sst_all_data$is_stop_trial<- sst_all_data$go_no_go_condition_label=="Stop"
  sst_all_data <- sst_all_data %>% 
    #do within group
    group_by(subid, waveid, runid) %>%
    #we need to lag them by one, though, because we don't want to count the current trial
    #alternatively we can subtract the value
    mutate(trial_count_go = cumsum(is_go_trial)-is_go_trial,
           trial_count_stop = cumsum(is_stop_trial) - is_stop_trial
           ) %>% 
    mutate(P_stop_trial = trial_count_stop/(trial_count_go+trial_count_stop)) %>% 
    mutate(trial_count_go_post = cumsum(is_go_trial),
           trial_count_stop_post = cumsum(is_stop_trial)
    ) %>% 
    mutate(P_stop_trial_post = trial_count_stop_post/(trial_count_go_post+trial_count_stop_post)) %>% 
    mutate(P_stop_trial_change = P_stop_trial_post-P_stop_trial)
    
    
  
  #remove the cols, we no longer need them
  sst_all_data$is_go_trial<-NULL
  sst_all_data$is_stop_trial<-NULL
  
  
  return(sst_all_data)
}

label_arrows_and_responses <- function(sst_all_data){
  
  #let's write something to work out whether subjects pressed the right button
  #we'll need to start by figuring out what is the meaningful "left" and "rigth" button for a subject"
  #we can do that just by assuming they get more right than they get wrong, and mapping the most commonly pressed button to left and right
  subject_arrow_responses <- sst_all_data%>% group_by(subid,waveid,runid, arrow_presented) %>% filter(subject_response!=0) %>% 
    summarize(most_common_response_for_arrow=
                as.integer(names(table(subject_response))[which.max(table(subject_response))]),
              proportion_selected=max(table(subject_response))/length(subject_response)
    )#notably, proportion_selected excludes non-responses, so we can't use it as a measure of success
  
  #now let's map the subject responses to 'left' and 'right'.
  #% The fourth column is 0=left, 1=right arrow; 2 is null
  arrow_presentation_key <- data.frame(arrow_presented=c(0,1),arrow_presented_label=c("left","right"))
  subj_arrow_resp_key<-arrow_presentation_key
  colnames(subj_arrow_resp_key)<-c("arrow_presented","subj_response_label")
  subject_arrow_responses<-merge(subject_arrow_responses,subj_arrow_resp_key)
  #we have more work to do yet. Now need to match subjects to these responses to work out when they pressed left/right and when they didn't.
  sst_all_data<-merge(sst_all_data,subject_arrow_responses %>% select(-proportion_selected,-arrow_presented),
                      by.x=c("subid","waveid","runid","subject_response"),
                      by.y=c("subid","waveid","runid","most_common_response_for_arrow"),
                      all.x=TRUE,all.y=FALSE,sort=FALSE
  )
  
  sst_all_data<-merge(sst_all_data,arrow_presentation_key,all.x=TRUE,all.y=FALSE,sort=FALSE)
  sst_all_data<-sst_all_data%>% mutate(
    subj_response_label=case_when(
      !is.na(subj_response_label)~subj_response_label,#leave as is if it has a value
      #if not, label as "other" if the subject had a non-zero response
      (is.na(subj_response_label) & (subject_response!=0) )~ "other",#leave as is if it has a value
      #otherwise leave as NA, which indicates a non-response
      (is.na(subj_response_label) & (subject_response==0) )~ "non-response",
      TRUE~as.character(NA)
    )
  )
  return(sst_all_data)
}