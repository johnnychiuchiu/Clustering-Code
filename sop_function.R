library(jsonlite)
library(plyr)
library(dplyr)
library(tidyjson)
library(ggplot2)
library(scales)
library(reshape)
library(reshape2)
library(lazyeval)
library(fasttime)
library(data.table)
library(rgeolocate)


set_column_type3<- function(df){
  df$actual_time = fastPOSIXct(df$actual_time)-28800
  #df$actual_date = as.Date(df$actual_date) #character type of actual_data can be compare as well
  return(df)
}

my_data_manipulate<- function(df){
  # replace blank with NA
  df[df==""] <- NA
  
  # remove duplicated rows in the df
  df<- df[!duplicated(df), ]
  
  # deal with the time column
  df$actual_time<-as.POSIXct(as.numeric(as.character(df$Time)),origin="1970-01-01",tz="Asia/Taipei")
  df$actual_date<-as.Date(df$actual_time,'Asia/Taipei')
  #  df$hour<-as.numeric(format(df$actual_time, "%H"))
  #  df$weekday <- weekdays(df$actual_date)
  #  df$weekday<-factor(df$weekday, 
  #                     levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
  
  # generate country and region column
  # can only get the country of the ips I have; however, it's the best I can get for the time being.
  #https://cran.r-project.org/web/packages/rgeolocate/rgeolocate.pdf
  # data_ip<-df %>% group_by(IP) %>% dplyr::summarise(count=n()) %>% select(c(1))
  # file <- system.file("extdata","GeoLite2-City.mmdb", package = "rgeolocate")
  # ip_list <- vector("list", nrow(data_ip))
  # for(i in 1:dim(data_ip)[1]){
  #   ip_list[[i]] <- maxmind(as.character(data_ip$IP[i]), file,
  #                           fields = c('country_name','region_name','city_name'))
  # }
  # ip_df <- plyr::rbind.fill(ip_list)
  # data_ip<-cbind(data_ip,ip_df)
  # df <- merge(df, data_ip, by.x=('IP'))
  
  # 產生 clientId 欄位
  df$clientId <- df$gid
  df$clientId <- ifelse(is.na(df$clientId), df$fid, df$clientId)
  df$clientId <- ifelse(is.na(df$clientId), df$cid, df$clientId)
  
  
  return(df)
}

my_session6<- function(df){
  start<-Sys.time()
  df <- as.data.table(df)
  df<- df[order(df$clientId, df$actual_time),]
  df[, session := cumsum(difftime(actual_time, shift(actual_time, fill = min(actual_time)), units = "mins") > 30) +1L, by = clientId]
  
  end<-Sys.time()
  print(paste0("Started: ", start))
  print(paste0("Ended: ", end))
  print(difftime(end, start, Asia/Taipei,units = "mins"))
  
  df<-as.data.frame(df)
  return(df)
}


####################################

truncate_url<- function(df, colname_vector){
  # truncate the utm part of the targeted colnames, such as currentURL, URL_Current & URL_Referrer
  for(x in colname_vector){
    df[,x]<-as.character(df[,x])
    for(i in 1:length(df[,x])){
      if(grepl("utm_medium",df[,x][i])){
        df[,x][i]<-strsplit(df[,x][i],'\\?')[[1]][1]
      }
    }
  }
  return(df)
}

remove_id<- function(df, id_vector){
  df<- df %>% filter(!(clientId %in% id_vector))
  return(df)
}

my_session<- function(df){
  start<-Sys.time()
  clientid_count=data.frame(id=unique(df$clientId),session_count=1)

  clientid_df<-list()
  mydf_clientid<-data.frame()

  for(i in 1:length(clientid_count$id)){
    print(i)
    mydf_clientid <-df %>% filter(clientId==clientid_count$id[i]) %>% arrange(clientId,actual_time)
    clientid_df[[i]]<-mydf_clientid
    #print(i)
  }
  rm(mydf_clientid)

  #for each clientid_df
  for(i in 1:length(clientid_df)){
    paste('session, id:',i)
    #if the clientid count's id match clientid_df's id, then we can generate the session column for that clientid
    for(k in 1:dim(clientid_df[[i]])[1]){
      #The first if statement
      if(k==1){
        clientid_df[[i]]$session[k]<-clientid_count$session_count[i]
        clientid_count$session_count[i]<-clientid_count$session_count[i]+1
      }else{
        #The second if statement
        if(as.numeric(difftime(clientid_df[[i]]$actual_time[k],
                               clientid_df[[i]]$actual_time[k-1], Asia/Taipei,units = "mins"))>30){
          clientid_df[[i]]$session[k]<-clientid_count$session_count[i]
          clientid_count$session_count[i]<-clientid_count$session_count[i]+1
        }else{
          clientid_df[[i]]$session[k]<-clientid_count$session_count[i]-1
        }
      }
    }
  }
  df<-rbind_all(clientid_df)

  end<-Sys.time()
  print(paste0("Started: ", start))
  print(paste0("Ended: ", end))
  difftime(end, start, Asia/Taipei,units = "mins")

  return(df)
}

my_session2<- function(df){
  start<-Sys.time()
  clientid_count=data.frame(id=unique(df$clientId),session_count=1)

  df$session<- NA
  #for each clientid_df
  for(i in 1:length(unique(df$clientId))){
    print(paste('session, id:',i))
    #if the clientid count's id match clientid_df's id, then we can generate the session column for that clientid
    for(k in 1:dim(df[df$clientId==unique(df$clientId)[i],])[1]){
      #The first if statement
      if(k==1){
        df[df$clientId==unique(df$clientId)[i],][['session']][k] <- clientid_count$session_count[i]
        clientid_count$session_count[i] = clientid_count$session_count[i]+1
      }else{
        #The second if statement
        if(as.numeric(difftime(df[df$clientId==unique(df$clientId)[i],][['actual_time']][k],
                               df[df$clientId==unique(df$clientId)[i],][['actual_time']][k-1], Asia/Taipei,units = "mins"))>30){
          df[df$clientId==unique(df$clientId)[i],][['session']][k] = clientid_count$session_count[i]
          clientid_count$session_count[i] = clientid_count$session_count[i]+1
        }else{
          df[df$clientId==unique(df$clientId)[i],][['session']][k] = clientid_count$session_count[i]-1
        }
      }
    }
  }



  end<-Sys.time()
  print(paste0("Started: ", start))
  print(paste0("Ended: ", end))
  difftime(end, start, Asia/Taipei,units = "mins")

  return(df)
}

my_session3<- function(df){
  steps<- df[order(df$clientId, df$actual_time),]
  for (i in 1:(dim(steps)[1]-1)){
    print(paste('for 1, id:',i))
    if (steps$clientId[i]!=steps$clientId[i+1]){
      steps$sec.[i]<- 0	}
    else{steps$sec.[i]<- abs(steps$actual_time[i]-steps$actual_time[i+1])}
  }

  steps$session<- 1
  for (i in 1:(dim(steps)[1]-1)){
    print(paste('for 2, id:',i))
    if (steps$clientId[i]==steps$clientId[i+1]){
      if (steps$sec.[i]>1800){
        steps$session[i+1]<- steps$session[i]+1
      }
      else{steps$session[i+1]<- steps$session[i]}
    }
  }

  for (i in 1:dim(steps)[1]){
    print(paste('for 3, id:',i))
    if (steps$sec.[i]>1800){
      steps$sec.session[i]<- 0
    }
    else{steps$sec.session[i]<-steps$sec.[i]}
  }

}

my_session4<- function(df){
  start<-Sys.time()

  steps<- df[order(df$clientId, df$actual_time),]
  steps$session<- 1

  for (i in 2:(dim(steps)[1])){
    print(paste('row: ',i))
    if (steps$clientId[i]==steps$clientId[i-1]){
      if(as.numeric(difftime(steps$actual_time[i],
                            steps$actual_time[i-1], Asia/Taipei,units = "mins"))>30){
        steps$session[i]<- steps$session[i-1]+1
      }
      else{steps$session[i]<- steps$session[i-1]}
    }
  }

  end<-Sys.time()
  print(paste0("Started: ", start))
  print(paste0("Ended: ", end))
  difftime(end, start, Asia/Taipei,units = "mins")

  return(steps)
}

my_session5<- function(df){
  start<-Sys.time()

  steps<- df[order(df$clientId, df$actual_time),]
  if(!('session' %in% colnames(steps))){
    steps$session<- 1
  }

  for (i in 2:(dim(steps)[1])){
    #print(paste('row: ',i))
    if(is.na(steps$session[i])){
      if (steps$clientId[i]==steps$clientId[i-1]){
        if(as.numeric(difftime(steps$actual_time[i],
                               steps$actual_time[i-1], Asia/Taipei,units = "mins"))>30){
          steps$session[i]<- steps$session[i-1]+1
        }else{steps$session[i]<- steps$session[i-1]}
      }else{steps$session[i]<- 1}
    }
  }

  end<-Sys.time()
  print(paste0("Started: ", start))
  print(paste0("Ended: ", end))
  difftime(end, start, Asia/Taipei,units = "mins")

  return(steps)
}

set_column_type<- function(df){
  df$timeZone = as.numeric(df$timeZone)
  df$Time = as.integer(df$Time)
  df$actual_time = as.POSIXct(df$actual_time,tz="Asia/Taipei")
  df$actual_date = as.Date(df$actual_date)
  df$hour = as.numeric(df$hour)
  df$weekday = as.factor(df$weekday)
#  df$session = as.numeric(df$session)
  df$weekday<-factor(df$weekday, levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
  return(df)
}

set_column_type2<- function(df){
  df$actual_time = fastPOSIXct(df$actual_time)
  df$actual_date = as.Date(df$actual_date)
  df$weekday = as.factor(df$weekday)
#  df$session = as.numeric(df$session)
  df$weekday<-factor(df$weekday, levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
  return(df)
}

my_newold<- function(df){
  # generate the 新舊訪客
  data_newold<-df %>% group_by(actual_date,clientId) %>% dplyr::summarise(count =n(),minsession=min(session))
  data_newold$newold<-ifelse(data_newold$minsession!=1,'old','new')
  data_newold<- data_newold %>% select(actual_date, clientId, newold)
  #data_newold$actual_date<- as.character(data_newold$actual_date)

  #df$actual_date<- as.character(df$actual_date)
  df<- merge(df, data_newold, by =c('actual_date','clientId'), all.x = TRUE)

  return(df)

}

my_count_revenue<- function(df){
  # - 總營收
  data_6_1_id<- df %>% filter(Event == 'Complete_Shopping') %>% select(clientId)
  data_6_1_id<- data_6_1_id$clientId
  data_6_2<- df %>% filter(Event %in% c('Complete_Shopping','Checkout3_Payment','Cart')) %>%
    filter(clientId %in% data_6_1_id) %>% arrange(desc(clientId), actual_time)

  data_6_3<- data.frame()
  for(i in 1:dim(data_6_2)[1]){
    if(data_6_2$Event[i]=='Complete_Shopping'){
      for(j in 1:i){
        if(data_6_2$clientId[i-j]!=data_6_2$clientId[i]){
          break
        }else{
          if(data_6_2$Event[i-j]=='Complete_Shopping'){
            break
          }else{
            if(data_6_2$Event[i-j]=='Checkout3_Payment'){
              temp_time<-data_6_2 %>% filter(actual_time %in% data_6_2$actual_time[i-j])
              data_6_3<-rbind(data_6_3,temp_time)
              break
            }else{
              if(data_6_2$Event[i-j]=='Cart'){
                temp_time<-data_6_2 %>% filter(actual_time %in% data_6_2$actual_time[i-j])
                data_6_3<-rbind(data_6_3,temp_time)
                break
              }
            }
          }
        }
      }
    }
  }

  data_6_3$Price <-sapply(strsplit(data_6_3$Price, split='M', fixed=TRUE), function(x) (x[2]))
  data_6_3$Price <-as.numeric(data_6_3$Price)
  return(data_6_3)
}

my_data_summary<- function(df){
  
  # - 人數
  id_count<-length(unique(df$clientId))
  days<- as.numeric(max(df$actual_date)-min(df$actual_date))
  
  # - 平均每人瀏覽頁數
  data_1<- df %>% group_by(clientId) %>% dplyr::summarise(page_count=n())
  page_per_id<-mean(data_1$page_count)
  
  # - 總工作階段
  data_2<-df %>% group_by(clientId) %>% dplyr::summarize(clieitid_session=max(session))
  session_total<-sum(data_2$clieitid_session)
  
  # - 平
  session_per_id<-mean(data_2$clieitid_session)
  
  # - 平均停留時間
  data_3_1<- df %>% group_by(clientId, session) %>% 
    dplyr::summarize(session_duration=as.numeric(difftime(max(actual_time), min(actual_time), Asia/Taipei,units = "secs")))
  
  data_3<- data_3_1 %>%  group_by(clientId) %>% 
    dplyr::summarize(total_time=sum(session_duration)) %>% 
    mutate(total_mins=total_time/60,total_hours=total_time/60/60)
  mins_per_id<-mean(data_3$total_mins)
  
  # - 平均跳出率
  data_7<- df %>% group_by(clientId, session) %>% 
    dplyr::summarize(count=n())
  data_7$bounce<-ifelse(data_7$count>1,0,1)
  bounce_rate<-percent(sum(data_7$bounce)/sum(data_2$clieitid_session))
  # - 總營收
#   data_6<- my_count_revenue(df)
#   total_revenue<-sum(data_6$Price)
  # - 總購買次數
#   purchased_count<- df %>% filter(Event=='Complete_Shopping') %>% group_by(clientId) %>% 
#     dplyr::summarise(count= n())
#   purchased_count<- sum(purchased_count$count)
#   purchased_count_df<- data_6 %>% group_by(clientId) %>% dplyr::summarise(count=n_distinct(actual_time))
#   purchased_count<- sum(purchased_count_df$count) 
  
  # - 單次工作階段頁數
  data_8<- df %>% group_by(clientId, session) %>% dplyr::summarise(page_count=n())
  page_per_session<-mean(data_8$page_count)
  
  
  final_list<-list('人數'=id_count, '平均每日人'=id_count/days,
                   '平均每人瀏覽頁數'=page_per_id, '總工作階段'=session_total,'平均每人工作階段'=session_per_id,
                   '平均每人停留時間(分鐘)'=mins_per_id,'單次工作階段頁數'=page_per_session,'平均跳出'=bounce_rate
#                    ,
#                    '總營收'=total_revenue, '平均每日營收'=total_revenue/days,'客單價'=average_revenue, 
#                    '總購買次數'=purchased_count,'總購買人次'=purchased_id_count,
#                    ''=return_purchase_id_count,'回購天數表'=return_days_table
                    )
  return(final_list)
}

my_user_time_plot<- function(df, time_dimension, fill_dimension, type, facet_or_not){
  if(facet_or_not=='No'){
    if(type=='bar'){
      if(missing(fill_dimension)){
        grp_cols <- c(time_dimension,'clientId') # Columns you want to group by
        dots <- lapply(grp_cols, as.symbol)# Convert character vector to list of symbols
        time_plot<-df %>% group_by_(.dots=dots) %>% dplyr::summarise(count = n())# Perform frequency counts
        
        switch(time_dimension,
               "weekday"={
                 week_plot<-ggplot(data=time_plot, aes_string(time_dimension)) + geom_bar()+theme_bw()+
                   theme(axis.text.x = element_text(angle=45))+
                   ggtitle("造訪人-")+theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   theme(axis.title.x = element_text(size = rel(1.8), angle = 00),axis.text=element_text(size=15))+
                   theme(axis.title.y = element_text(size = rel(1.8), angle = 90),axis.text=element_text(size=15))
                 return(week_plot)
               },
               "actual_date"={
                 day_plot<-ggplot(data=time_plot, aes_string(time_dimension)) + geom_bar()+theme_bw()+
                   theme(axis.text.x = element_text(angle=45))+scale_x_date(date_minor_breaks = "1 day")+
                   ggtitle("造訪人次-日")+theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   theme(axis.title.x = element_text(size = rel(1.8), angle = 00),axis.text=element_text(size=15))+
                   theme(axis.title.y = element_text(size = rel(1.8), angle = 90),axis.text=element_text(size=15))
                 return(day_plot)
               },
               "hour"={
                 hour_plot<-ggplot(data=time_plot, aes_string(time_dimension)) + geom_bar()+theme_bw()+
                   theme(axis.text.x = element_text(angle=45))+
                   ggtitle("造訪人次-小")+theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   theme(axis.title.x = element_text(size = rel(1.8), angle = 00),axis.text=element_text(size=15))+
                   theme(axis.title.y = element_text(size = rel(1.8), angle = 90),axis.text=element_text(size=15))
                 return(hour_plot)
               })
      }else{
        grp_cols <- c(time_dimension,'clientId',fill_dimension) # Columns you want to group by
        dots <- lapply(grp_cols, as.symbol)# Convert character vector to list of symbols
        time_plot<-df %>% group_by_(.dots=dots) %>% dplyr::summarise(count = n())# Perform frequency counts
        
        time_plot[[fill_dimension]]<-as.factor(time_plot[[fill_dimension]])
        if(fill_dimension=='newold'){
          time_plot$newold <- relevel(time_plot$newold, "old")
        }
        
        switch(time_dimension,
               "weekday"={
                 week_plot<-ggplot(data=time_plot, aes_string(time_dimension)) + geom_bar(aes_string(fill=fill_dimension))+theme_bw()+
                   theme(axis.text.x = element_text(angle=45))+
                   ggtitle("造訪人次-週")+theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   theme(axis.title.x = element_text(size = rel(1.8), angle = 00),axis.text=element_text(size=15))+
                   theme(axis.title.y = element_text(size = rel(1.8), angle = 90),axis.text=element_text(size=15))
                 return(week_plot)
               },
               "actual_date"={
                 day_plot<-ggplot(data=time_plot, aes_string(time_dimension)) + geom_bar(aes_string(fill=fill_dimension))+theme_bw()+
                   theme(axis.text.x = element_text(angle=45))+scale_x_date(date_minor_breaks = "1 day")+
                   ggtitle("造-")+theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   theme(axis.title.x = element_text(size = rel(1.8), angle = 00),axis.text=element_text(size=15))+
                   theme(axis.title.y = element_text(size = rel(1.8), angle = 90),axis.text=element_text(size=15))
                 return(day_plot)
               },
               "hour"={
                 hour_plot<-ggplot(data=time_plot, aes_string(time_dimension)) + geom_bar(aes_string(fill=fill_dimension))+theme_bw()+
                   theme(axis.text.x = element_text(angle=45))+
                   ggtitle("造訪人次-小時")+theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   theme(axis.title.x = element_text(size = rel(1.8), angle = 00),axis.text=element_text(size=15))+
                   theme(axis.title.y = element_text(size = rel(1.8), angle = 90),axis.text=element_text(size=15))
                 return(hour_plot)
               })
      }
    }else if(type=='line'){
      if(missing(fill_dimension)){
        grp_cols <- c(time_dimension) # Columns you want to group by
        dots <- lapply(grp_cols, as.symbol)# Convert character vector to list of symbols
        time_plot<-df %>% group_by_(.dots=dots) %>% dplyr::summarise(count = n_distinct(clientId))# Perform frequency counts
        
        switch(time_dimension,
               "weekday"={
                 week_plot<-ggplot(time_plot,aes_string(x=time_dimension, y='count',group = 1)) + 
                   geom_line(size=1.5, alpha=1/10, linetype='dashed')+ 
                   geom_smooth(span = 0.85,se = FALSE)+ 
                   ggtitle('-週')+
                   theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   xlab(time_dimension) + geom_text(aes(label=count, vjust=-0.5))
                 return(week_plot)
               },
               "actual_date"={
                 time_filter_sort<-my_filter_sort_time(time_plot,time_dimension,p=0.75)
                 time_group<- my_group_time(time_filter_sort,time_dimension)
                 
                 #####count default date
                 default_date<-seq(min(time_plot$actual_date),max(time_plot$actual_date),by = 7)
                 #####count data date
                 a<-c()
                 for(i in 1:dim(time_group)[1]){
                   a<-c(a,as.character(time_group[i,c(2,3)]$start+1))
                   a<-c(a,as.character(time_group[i,c(2,3)]$end-1))
                 }
                 a<-as.Date(a)
                 combined_date<-unique(sort(c(a,default_date)))
                 
                 
                 day_plot<-ggplot(time_plot,aes_string(x=time_dimension, y='count')) + 
                   geom_line(size=1.5, alpha=1/10, linetype='dashed')+ 
                   geom_smooth(span = 0.5,se = FALSE)+ 
                   ggtitle('造訪人次-日')+
                   theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   theme(axis.text.x = element_text(angle=-20))+
                   scale_x_date(breaks = combined_date,date_labels = "%b %d")+
                   xlab(time_dimension) + geom_text(aes(label=count, vjust=-0.5))+
                   geom_rect(data=time_group, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=-Inf,
                                                                     ymax=Inf, group=group), color="transparent", fill="orange", alpha=0.3)
                 
                   return(day_plot)
               },
               "hour"={
                 time_filter_sort<-my_filter_sort_time(time_plot,time_dimension,p=0.75)
                 time_group<- my_group_time(time_filter_sort,time_dimension)
                 
                 a<- time_filter_sort[[time_dimension]]
                 default_hour<- c(0,5,10,15,20)
                 combined_time<-unique(sort(c(a,default_hour)))
                 
                 hour_plot<-ggplot(time_plot,aes_string(x=time_dimension, y='count')) + 
                   geom_line(size=1.5, alpha=1/10, linetype='dashed')+ 
                   geom_smooth(span = 0.5,se = FALSE)+ 
                   ggtitle('-')+
                   theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   xlab(time_dimension) + geom_text(aes(label=count, vjust=-0.5))+
                   scale_x_continuous(breaks = unique(sort(combined_time)))+
                   geom_rect(data=time_group, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=-Inf,
                                                                     ymax=Inf, group=group), color="transparent", fill="orange", alpha=0.3)
                   return(hour_plot)
               })
      }else{
        grp_cols <- c(time_dimension,fill_dimension) # Columns you want to group by
        dots <- lapply(grp_cols, as.symbol)# Convert character vector to list of symbols
        time_plot<-df %>% group_by_(.dots=dots) %>% dplyr::summarise(count = n_distinct(clientId))# Perform frequency counts
        
        time_plot[[fill_dimension]]<-as.factor(time_plot[[fill_dimension]])
        if(fill_dimension=='newold'){
          time_plot$newold <- relevel(time_plot$newold, "old")
        }
        

        
        switch(time_dimension,
               "weekday"={
                 week_plot<-ggplot(time_plot,aes_string(x=time_dimension, y='count',group=fill_dimension , colour=fill_dimension)) + 
                   geom_line(size=1.5, alpha=1/10, linetype='dashed')+ 
                   geom_smooth(span = 1,se = FALSE)+ 
                   theme_bw()+
                   #ggtitle(paste("造訪人-:",fill_dimension))+
                   #theme_gray(base_family="STHeiti")+
                   #theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   xlab(time_dimension) + geom_text(aes(label=count, vjust=-0.5),size=6)+
                   theme(axis.text=element_text(size=20),
                         axis.title.x=element_text(size=30),
                         axis.title.y=element_text(size=30),
                         legend.text=element_text(size=18),
                         legend.title=element_text(size=25))
                   
                 
                 return(week_plot)
               },
               "actual_date"={
                 time_filter_sort<-my_filter_sort_time(time_plot,time_dimension,p=0.75)
                 time_group<- my_group_time(time_filter_sort,time_dimension)
                 
                 #####count default date
                 default_date<-seq(min(time_plot$actual_date),max(time_plot$actual_date),by = 7)
                 #####count data date
                 a<-c()
                 for(i in 1:dim(time_group)[1]){
                   a<-c(a,as.character(time_group[i,c(2,3)]$start+1))
                   a<-c(a,as.character(time_group[i,c(2,3)]$end-1))
                 }
                 a<-as.Date(a)
                 combined_date<-unique(sort(c(a,default_date)))
                 

                 day_plot<-ggplot(time_plot,aes_string(x=time_dimension, y='count',group=fill_dimension , colour=fill_dimension)) + 
                   geom_line(size=1.5, alpha=1/10, linetype='dashed')+ 
                   geom_smooth(span = 0.5,se = FALSE)+ 
                   ggtitle(paste("造訪人次-日:",fill_dimension))+
                   theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   theme(axis.text.x = element_text(angle=-20))+
                   xlab(time_dimension) + geom_text(aes(label=count, vjust=-0.5))+
                   scale_x_date(breaks = combined_date,date_labels = "%b %d")+
                   geom_rect(data=time_group, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=-Inf,
                                                                     ymax=Inf, group=group), color="transparent", fill="orange", alpha=0.3)
                 
                 return(day_plot)
               },
               "hour"={
                 time_filter_sort<-my_filter_sort_time(time_plot,time_dimension,p=0.75)
                 time_group<- my_group_time(time_filter_sort,time_dimension)
                 
                 a<- time_filter_sort[[time_dimension]]
                 default_hour<- c(0,5,10,15,20)
                 combined_time<-unique(sort(c(a,default_hour)))
                 
#                  hour_plot<-ggplot(time_plot,aes_string(x=time_dimension, y='count',group=fill_dimension , colour=fill_dimension)) + 
#                    geom_line(size=1.5, alpha=1/10, linetype='dashed')+ 
#                    geom_smooth(span = 0.5,se = FALSE)+ 
#                    ggtitle(paste("造訪人-:",fill_dimension))+
#                    theme_gray(base_family="STHeiti")+
#                    theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
#                    xlab(time_dimension) + geom_text(aes(label=count, vjust=-0.5))+
#                    scale_x_continuous(breaks = unique(sort(combined_time)))+
#                    geom_rect(data=time_group, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=-Inf,
#                                                                      ymax=Inf, group=group), color="transparent", fill="orange", alpha=0.3)
#                  
                 hour_plot<-ggplot(time_plot,aes_string(x=time_dimension, y='count',group=fill_dimension , colour=fill_dimension)) + 
                   geom_line(size=1.5, alpha=1/10, linetype='dashed')+ 
                   geom_smooth(span = 0.5,se = FALSE)+ 
                   theme_bw()+
                   #ggtitle(paste("造訪人次-小時:",fill_dimension))+
                   #theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   xlab(time_dimension) + geom_text(aes(label=count, vjust=-0.5),size=5)+
                   #scale_x_continuous(breaks = unique(sort(combined_time)))+
                   #geom_rect(data=time_group, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=-Inf,
                   #                                                  ymax=Inf, group=group), color="transparent", fill="orange", alpha=0.3)
                   theme(axis.text=element_text(size=20),
                         axis.title.x=element_text(size=30),
                         axis.title.y=element_text(size=30),
                         legend.text=element_text(size=18),
                         legend.title=element_text(size=25))
                 
                 
                 return(hour_plot)
               })
      }
    }
  }else if(facet_or_not=='Yes'){
    if(type=='bar'){
      if(missing(fill_dimension)){
        print('facet_or_not is not functioned when fill_dimension is empty')
        grp_cols <- c(time_dimension,'clientId') # Columns you want to group by
        dots <- lapply(grp_cols, as.symbol)# Convert character vector to list of symbols
        time_plot<-df %>% group_by_(.dots=dots) %>% dplyr::summarise(count = n())# Perform frequency counts
        
        switch(time_dimension,
               "weekday"={
                 week_plot<-ggplot(data=time_plot, aes_string(time_dimension)) + geom_bar()+theme_bw()+
                   theme(axis.text.x = element_text(angle=45))+
                   ggtitle("造訪人次-週")+theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   theme(axis.title.x = element_text(size = rel(1.8), angle = 00),axis.text=element_text(size=15))+
                   theme(axis.title.y = element_text(size = rel(1.8), angle = 90),axis.text=element_text(size=15))
                 return(week_plot)
               },
               "actual_date"={
                 day_plot<-ggplot(data=time_plot, aes_string(time_dimension)) + geom_bar()+theme_bw()+
                   theme(axis.text.x = element_text(angle=45))+scale_x_date(date_minor_breaks = "1 day")+
                   ggtitle("造訪人次-日")+theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   theme(axis.title.x = element_text(size = rel(1.8), angle = 00),axis.text=element_text(size=15))+
                   theme(axis.title.y = element_text(size = rel(1.8), angle = 90),axis.text=element_text(size=15))
                 return(day_plot)
               },
               "hour"={
                 hour_plot<-ggplot(data=time_plot, aes_string(time_dimension)) + geom_bar()+theme_bw()+
                   theme(axis.text.x = element_text(angle=45))+
                   ggtitle("造訪人次-小時")+theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   theme(axis.title.x = element_text(size = rel(1.8), angle = 00),axis.text=element_text(size=15))+
                   theme(axis.title.y = element_text(size = rel(1.8), angle = 90),axis.text=element_text(size=15))
                 return(hour_plot)
               })
      }else{
        grp_cols <- c(time_dimension,fill_dimension) # Columns you want to group by
        dots <- lapply(grp_cols, as.symbol)# Convert character vector to list of symbols
        time_plot<-df %>% group_by_(.dots=dots) %>% dplyr::summarise(count = n_distinct(clientId))# Perform frequency counts
        
        time_plot[[fill_dimension]]<-as.factor(time_plot[[fill_dimension]])
        if(fill_dimension=='newold'){
          time_plot$newold <- relevel(time_plot$newold, "old")
        }
        
        switch(time_dimension,
               "weekday"={
                 week_plot<-ggplot(data=time_plot, aes_string(time_dimension,y='count', fill=fill_dimension)) + 
                   geom_bar(stat="identity")+
                   geom_smooth(span = 0.8,se = FALSE)+ 
                   facet_wrap(as.formula(paste("~", fill_dimension)), scales = "free")+ 
                   ggtitle("造訪-")+
                   theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   theme(axis.title.x = element_text(size = rel(1.8), angle = 00),axis.text.x=element_text(size=15,angle=15))+
                   theme(axis.title.y = element_text(size = rel(1.8), angle = 90),axis.text=element_text(size=15))

                 return(week_plot)
               },
               "actual_date"={
               day_plot<-ggplot(data=time_plot, aes_string(time_dimension,y='count', fill=fill_dimension)) + 
                   geom_bar(stat="identity")+
                   geom_smooth(span = 0.8,se = FALSE)+ 
                   facet_wrap(as.formula(paste("~", fill_dimension)), scales = "free")+ 
                   scale_x_date(date_minor_breaks = "1 day")+
                   ggtitle("造訪人次-日")+theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   theme(axis.title.x = element_text(size = rel(1.8), angle = 00),axis.text.x=element_text(size=15,angle=15))+
                   theme(axis.title.y = element_text(size = rel(1.8), angle = 90),axis.text=element_text(size=15))
                 
                 return(day_plot)
               },
               "hour"={
                 hour_plot<-ggplot(data=time_plot, aes_string(time_dimension,y='count', fill=fill_dimension)) + 
                   geom_bar(stat="identity")+
                   geom_smooth(span = 0.8,se = FALSE)+ 
                   facet_wrap(as.formula(paste("~", fill_dimension)), scales = "free")+ 
                   ggtitle("造訪人次-小時")+theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   theme(axis.title.x = element_text(size = rel(1.8), angle = 00),axis.text=element_text(size=15))+
                   theme(axis.title.y = element_text(size = rel(1.8), angle = 90),axis.text=element_text(size=15))
                 
                 return(hour_plot)
               })
      }
    }else if(type=='line'){
      if(missing(fill_dimension)){
        print('facet_or_not is not functioned when fill_dimension is empty')
        grp_cols <- c(time_dimension) # Columns you want to group by
        dots <- lapply(grp_cols, as.symbol)# Convert character vector to list of symbols
        time_plot<-df %>% group_by_(.dots=dots) %>% dplyr::summarise(count = n_distinct(clientId))# Perform frequency counts
        
        switch(time_dimension,
               "weekday"={
                 week_plot<-ggplot(time_plot,aes_string(x=time_dimension, y='count',group = 1)) + 
                   geom_line(size=1.5, alpha=1/2)+ 
                   ggtitle('造訪人次-週')+
                   theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   xlab(time_dimension) + geom_text(aes(label=count, vjust=-0.5))
                 return(week_plot)
               },
               "actual_date"={
                 day_plot<-ggplot(time_plot,aes_string(x=time_dimension, y='count',group = 1)) + 
                   geom_line(size=1.5, alpha=1/2)+ ggtitle('造訪人次-日')+
                   theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   xlab(time_dimension) + geom_text(aes(label=count, vjust=-0.5))
                 return(day_plot)
               },
               "hour"={
                 hour_plot<-ggplot(time_plot,aes_string(x=time_dimension, y='count',group = 1)) + 
                   geom_line(size=1.5, alpha=1/2)+ ggtitle('造訪人次-小時')+
                   theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   xlab(time_dimension) + geom_text(aes(label=count, vjust=-0.5))
                 return(hour_plot)
               })
      }else{
        grp_cols <- c(time_dimension,fill_dimension) # Columns you want to group by
        dots <- lapply(grp_cols, as.symbol)# Convert character vector to list of symbols
        time_plot<-df %>% group_by_(.dots=dots) %>% dplyr::summarise(count = n_distinct(clientId))# Perform frequency counts
        
        time_plot[[fill_dimension]]<-as.factor(time_plot[[fill_dimension]])
        if(fill_dimension=='newold'){
          time_plot$newold <- relevel(time_plot$newold, "old")
        }
        
        switch(time_dimension,
               "weekday"={
#                  week_plot<-ggplot(time_plot,aes_string(x=time_dimension, y='count',group=fill_dimension , colour=fill_dimension)) + 
#                    facet_wrap(as.formula(paste("~", fill_dimension)), scales = "free")+ 
#                    geom_smooth(span = 1,se = FALSE)+ 
#                    geom_line(size=1.5, alpha=1/10, linetype='dashed')+ 
#                    geom_point()+
#                    ggtitle(paste("造訪人次-週:",fill_dimension))+
#                    theme_gray(base_family="STHeiti")+
#                    theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
#                    xlab(time_dimension) + geom_text(aes(label=count, vjust=-0.5))+
#                    theme(axis.text=element_text(size=20),
#                        axis.title.x=element_text(size=30),
#                        axis.title.y=element_text(size=30),
#                        legend.text=element_text(size=18),
#                        legend.title=element_text(size=25)) 
                 
                 week_plot<-ggplot(time_plot,aes_string(x=time_dimension, y='count',group=fill_dimension , colour=fill_dimension)) + 
                   facet_wrap(as.formula(paste("~", fill_dimension)), scales = "free")+ 
                   
                   geom_smooth(span = 1,se = FALSE)+ 
                   geom_line(size=1.5, alpha=1/10, linetype='dashed')+ 
                   geom_point()+
                   #ggtitle(paste("造訪人次-週:",fill_dimension))+
                   #theme_gray(base_family="STHeiti")+
                   theme_bw(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   xlab(time_dimension) + geom_text(aes(label=count, vjust=-0.5),size=5)+
                   theme(axis.text=element_text(size=12),
                         axis.title.x=element_text(size=30),
                         axis.title.y=element_text(size=30),
                         legend.text=element_text(size=18),
                         legend.title=element_text(size=25),
                         strip.text.x = element_text(size = 15))     
                 return(week_plot)
               },
               "actual_date"={
                 day_plot<-ggplot(time_plot,aes_string(x=time_dimension, y='count',group=fill_dimension , colour=fill_dimension)) +
                   facet_wrap(as.formula(paste("~", fill_dimension)), scales = "free")+ 
                   geom_smooth(span = 0.8,se = FALSE)+ 
                   geom_line(size=1.5, alpha=1/10, linetype='dashed')+ 
                   geom_point()+
                   ggtitle(paste("造訪人次-週:",fill_dimension))+
                   theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   xlab(time_dimension) + geom_text(aes(label=count, vjust=-0.5))
                 return(day_plot)
               },
               "hour"={
                 hour_plot<-ggplot(time_plot,aes_string(x=time_dimension, y='count',group=fill_dimension , colour=fill_dimension)) + 
                   facet_wrap(as.formula(paste("~", fill_dimension)), scales = "free")+ 
                   geom_smooth(span = 0.8,se = FALSE)+ 
                   geom_line(size=1.5, alpha=1/10, linetype='dashed')+ 
                   geom_point()+
                   ggtitle(paste("造訪人次-小時:",fill_dimension))+
                   theme_gray(base_family="STHeiti")+
                   theme(plot.title = element_text(color="#666666", face="bold", size=40, hjust=0))+
                   xlab(time_dimension) + geom_text(aes(label=count, vjust=-0.5))
                 return(hour_plot)
               })
      }
    }
  }
  
  
}

my_filter_sort_time<- function(time_plot, time_dimension, p=0.75){
  grp_cols <- c(time_dimension) # Columns you want to group by
  dots <- lapply(grp_cols, as.symbol)# Convert character vector to list of symbols
  
  temp_time<- time_plot %>% group_by_(.dots=dots) %>% dplyr::summarise(sum_count=sum(count)) %>% 
    arrange(desc(sum_count)) %>% filter(sum_count > quantile(sum_count,probs=p, names=FALSE)) %>% 
    arrange_(.dots = dots)
}

my_group_time<- function(my_filter_sort_time_df,time_dimension){
  my_filter_sort_time_df$group=1
  group_count=1
  for(i in 2:dim(my_filter_sort_time_df)[1]){
    if((my_filter_sort_time_df[[time_dimension]][i]-1) != my_filter_sort_time_df[[time_dimension]][i-1]){
      group_count=group_count+1
      my_filter_sort_time_df$group[i]=group_count
    }else{
      my_filter_sort_time_df$group[i]=group_count
    }
  }
  
  temp_time2<- my_filter_sort_time_df %>% group_by(group) %>% 
    dplyr::summarise_(start=interp(~min(v)-1, v=as.name(time_dimension)),
                      end  =interp(~max(v)+1, v=as.name(time_dimension)))
  return(temp_time2)
}

my_time_revenue_plot<- function(df, time_dimension, fill_dimension){
  gg_y<-'Revenue'
  revenue_data<-my_count_revenue(df)
  
  if(missing(fill_dimension)){
    grp_cols <- c(time_dimension) # Columns you want to group by
    dots <- lapply(grp_cols, as.symbol)# Convert character vector to list of symbols
    revenue_plot<-revenue_data %>% group_by_(.dots=dots) %>% dplyr::summarise(Revenue=sum(Price))# Perform frequency counts
    
    switch(time_dimension,
           "actual_date"={
             revenue_plot<-ggplot(data=revenue_plot, aes_string(x=time_dimension, y=gg_y)) +
               geom_bar(stat="identity")+ ggtitle("Date vs Revenue")+
               theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=40, hjust=0))+
               theme(axis.title.x = element_text(size = rel(1.8), angle = 00))+
               theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
           },
           "weekday"={
             revenue_plot<-ggplot(data=revenue_plot, aes_string(x=time_dimension, y=gg_y)) +
               geom_bar(stat="identity")+ ggtitle("Weekday vs Revenue")+
               theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=40, hjust=0))+
               theme(axis.title.x = element_text(size = rel(1.8), angle = 00))+
               theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
           },
           "hour"={
             revenue_plot<-ggplot(data=revenue_plot, aes_string(x=time_dimension, y=gg_y)) +
               geom_bar(stat="identity")+ ggtitle("Hour vs Revenue")+
               theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=40, hjust=0))+
               theme(axis.title.x = element_text(size = rel(1.8), angle = 00))+
               theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
           })
  }else{
    grp_cols <- c(time_dimension,fill_dimension) # Columns you want to group by
    dots <- lapply(grp_cols, as.symbol)# Convert character vector to list of symbols
    revenue_plot<-revenue_data %>% group_by_(.dots=dots) %>% dplyr::summarise(Revenue=sum(Price))# Perform frequency counts
    
    revenue_plot[[fill_dimension]]<-as.factor(revenue_plot[[fill_dimension]])
    if(fill_dimension=='newold'){
      revenue_plot$newold <- relevel(revenue_plot$newold, "old")
    }
    
    switch(time_dimension,
           "actual_date"={
             revenue_plot<-ggplot(data=revenue_plot, aes_string(x=time_dimension, y=gg_y)) +
               geom_bar(aes_string(fill=fill_dimension),stat="identity")+ ggtitle(paste("Date vs Revenue by",fill_dimension))+
               theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=40, hjust=0))+
               theme(axis.title.x = element_text(size = rel(1.8), angle = 00))+
               theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
             
           },
           "weekday"={
             revenue_plot<-ggplot(data=revenue_plot, aes_string(x=time_dimension, y=gg_y)) +
               geom_bar(aes_string(fill=fill_dimension),stat="identity")+ ggtitle(paste("Weekday vs Revenue by",fill_dimension))+
               theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=40, hjust=0))+
               theme(axis.title.x = element_text(size = rel(1.8), angle = 00))+
               theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
           },
           "hour"={
             revenue_plot<-ggplot(data=revenue_plot, aes_string(x=time_dimension, y=gg_y)) +
               geom_bar(aes_string(fill=fill_dimension),stat="identity")+ ggtitle(paste("Hour vs Revenue by",fill_dimension))+
               theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=40, hjust=0))+
               theme(axis.title.x = element_text(size = rel(1.8), angle = 00))+
               theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
           })
  }
  return(revenue_plot)
}

my_replace_name<- function(df ,name_df, from_language, column_name){
  df[[column_name]]<-trimws(df[[column_name]])
  df[[column_name]]<-stri_replace_all_charclass(df[[column_name]], "\\p{WHITE_SPACE}", " ")
  df[[column_name]]<-gsub('amp;','',df[[column_name]])
  
  if(from_language=='chinese'){
    to_language='english'
  }else if(from_language=='english'){
    to_language='chinese'
  }else{
    print('the language does not exist in the data')
  }
  
  #############################################取代文字
  for(i in 1:dim(df)[1]){
    for(j in 1:dim(name_df)[1]){
      if(  
        if(is.na(name_df[[from_language]][j]==df[[column_name]][i])){
          FALSE
        }else{
          name_df[[from_language]][j]==df[[column_name]][i]
        } 
      ){
        df[[column_name]][i]<-name_df[[to_language]][j]    
      }else{} 
    }
  }
  
  return(df)
}

my_top_product<- function(my_count_revenue_df){
  hot_df<- my_count_revenue_df %>% group_by(Title) %>% 
    dplyr::summarise(count=n(),Revenue=sum(Price)) %>% arrange(desc(count))
  return(hot_df)
}

my_funnel<- function(df,event_array,facet_dimension){
  if(missing(facet_dimension)){
    funnel_df<- df %>% filter(Event %in% event_array) %>% 
      group_by(Event) %>% dplyr::summarise(count=n()) %>% arrange(desc(count))
    
    funnel_df$percentage<-percent(1)
    for(i in 2:dim(funnel_df)[1]){
      funnel_df$percentage[i]<-percent(funnel_df$count[i]/funnel_df$count[1])
    }
    
    funnel_plot<- ggplot(funnel_df,aes(reorder(Event, -count), count))+ 
      geom_bar(stat="identity")+ theme_bw(base_family="STHeiti")+ 
      theme(axis.text.x=element_text(angle=0),axis.text=element_text(size=20),
            axis.title=element_text(size=14,face="bold"))+
      xlab("Event")+
      #ggtitle("Event Count")+
      #theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=40, hjust=0))+
      theme(axis.title.x = element_text(size = rel(1.8), angle = 00))+
      theme(axis.title.y = element_text(size = rel(1.8), angle = 90))+
      geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25, size=10)
    return(funnel_plot)
  }else{
    grp_cols <- c('Event',facet_dimension) # Columns you want to group by
    dots <- lapply(grp_cols, as.symbol)# Convert character vector to list of symbols
    funnel_df<- df %>% filter(Event %in% event_array) %>% 
      group_by_(.dots=dots) %>% dplyr::summarise(count=n()) %>% arrange(desc(count))
    funnel_df$percentage<-percent(1)
    
    facet_df<-list()
    temp_df<-data.frame()
    
    for(i in 1:length(unique(funnel_df[[facet_dimension]]))){
      l <- paste(facet_dimension, "==","'", funnel_df[[facet_dimension]][i],"'",sep='')
      temp_df <-funnel_df %>% filter_(l) %>% arrange(desc(count))
      facet_df[[i]]<-temp_df
    }
    rm(temp_df)
    
    for(k in 1:length(facet_df)){
      for(i in 2:dim(facet_df[[k]])[1]){
        facet_df[[k]]$percentage[i]<-percent(facet_df[[k]]$count[i]/facet_df[[k]]$count[1])
      }
    }
    funnel_df<-rbind_all(facet_df)
    
    funnel_plot<-ggplot(funnel_df,aes(reorder(Event, -count), count,fill=get(facet_dimension)))+ 
      geom_bar(stat="identity")+ theme_bw(base_family="STHeiti")+ 
      facet_wrap(as.formula(paste("~", facet_dimension)),ncol=2)+ 
      theme(axis.text.x=element_text(angle=-20),axis.text=element_text(size=10),
            axis.title=element_text(size=14,face="bold"))+ xlab("Event")+
#      ggtitle(paste("Event Count by",facet_dimension))+
#      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=40, hjust=0))+
#       theme(axis.title.x = element_text(size = rel(1.8), angle = 00))+
#       theme(axis.title.y = element_text(size = rel(1.8), angle = 90))+
      geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25, size=4)+ 
      guides(fill=guide_legend(title=paste(facet_dimension)))+
      theme(axis.text=element_text(size=12),
            axis.title.x=element_text(size=30),
            axis.title.y=element_text(size=30),
            legend.text=element_text(size=18),
            legend.title=element_text(size=25),
            strip.text.x = element_text(size = 15))    
    return(funnel_plot)
  }
  
}

my_timeperiod<- function(df){
  ##############generate the "days" and 'weeks' column
  clientid_count=data.frame(id=unique(df$clientId),count=1)
  clientid_df<-list()
  mydf_clientid<-data.frame()
  
  for(i in 1:length(clientid_count$id)){
    print(paste('generate, clientid:',i))
    mydf_clientid <-df %>% filter(clientId==clientid_count$id[i]) %>% arrange(clientId,actual_time)
    clientid_df[[i]]<-mydf_clientid
  }
  rm(mydf_clientid)
  
  ################### Generate the "days" column
  #for each clientid_df
  for(i in 1:length(clientid_df)){
    print(paste('days, clientid:',i))
    #if the clientid count's id match clientid_df's id, then we can generate the "days" column for that clientid
    for(k in 1:dim(clientid_df[[i]])[1]){
      #The first if statement
      if(k==1){
        clientid_df[[i]]$days[k]<-1
      }else{
        #The second if statement
        if(round(difftime(clientid_df[[i]]$actual_date[k], 
                          clientid_df[[i]]$actual_date[k-1], Asia/Taipei,units = 'days'))==0){
          clientid_df[[i]]$days[k]<-clientid_df[[i]]$days[k-1]
        }else{
          clientid_df[[i]]$days[k]<-round(difftime(clientid_df[[i]]$actual_date[k],clientid_df[[i]]$actual_date[k-1]
                                                         ,Asia/Taipei,units = 'days'))+clientid_df[[i]]$days[k-1] 
        }    
      }
    }
  }
  ################### Generate the "weeks" column
  #for each clientid_df
  for(i in 1:length(clientid_df)){
    print(paste('weeks, clientid:',i))
    #if the clientid count's id match clientid_df's id, then we can generate the "weeks" column for that clientid
    for(k in 1:dim(clientid_df[[i]])[1]){
      #The first if statement
      if(k==1){
        clientid_df[[i]]$weeks[k]<-1
      }else{
        #The second if statement
        if(round(difftime(clientid_df[[i]]$actual_date[k], 
                          clientid_df[[i]]$actual_date[k-1], Asia/Taipei,units = 'weeks'))==0){
          clientid_df[[i]]$weeks[k]<-clientid_df[[i]]$weeks[k-1]
        }else{
          clientid_df[[i]]$weeks[k]<-round(difftime(clientid_df[[i]]$actual_date[k],clientid_df[[i]]$actual_date[k-1]
                                                   ,Asia/Taipei,units = 'weeks'))+clientid_df[[i]]$weeks[k-1] 
        }    
      }
    }
  }
  df<-rbind_all(clientid_df)
  
  return(df)
}

my_cohort_df<- function(df, group_dimension, time_period, event_array, count_unique){
  #group_dimension: actual_date, newold, User_Device,  Ad_traffic, User_member...etc
  #time_period: days, weeks
  #event: complete_shopping
  #count_unique: if==1, the number is the unique clientId number. if==0, the number is the total number.
  #               this parameter is only usable when event_array is not null.  
  if(count_unique==1){
    if(missing(group_dimension)){
      if(missing(event_array)){
        grp_cols <- c('clientId', time_period)
        dots <- lapply(grp_cols, as.symbol)
        df_timecount_group<-df %>% group_by_(.dots=dots) %>% dplyr::summarise(count=n())
        
        myFormula <- as.formula(paste("~",time_period))
        df_timecount_cast<- reshape::cast(df_timecount_group, myFormula, fun.aggregate=length)
      }else{
        grp_cols <- c('clientId', time_period)
        dots <- lapply(grp_cols, as.symbol)
        df_timecount_group<-df %>% filter(Event %in% event_array) %>% 
          group_by_(.dots=dots) %>% dplyr::summarise(count=n())
        
        myFormula <- as.formula(paste("~",time_period))
        df_timecount_cast<- reshape::cast(df_timecount_group, myFormula, fun.aggregate=length)
      }
    }else{
      if(missing(event_array)){
        grp_cols <- c('clientId', group_dimension, time_period)
        dots <- lapply(grp_cols, as.symbol)
        df_timecount_group<-df %>% group_by_(.dots=dots) %>% dplyr::summarise(count=n())
        
        myFormula <- as.formula(paste(group_dimension,"~",time_period))
        df_timecount_cast<- reshape::cast(df_timecount_group, myFormula, fun.aggregate=length)
      }else{
        grp_cols <- c('clientId', group_dimension, time_period)
        dots <- lapply(grp_cols, as.symbol)
        df_timecount_group<-df %>% filter(Event %in% event_array) %>% 
          group_by_(.dots=dots) %>% dplyr::summarise(count=n())
        
        myFormula <- as.formula(paste(group_dimension,"~",time_period))
        df_timecount_cast<- reshape::cast(df_timecount_group, myFormula, fun.aggregate=length)
      }
    }
  }else if(count_unique==0){
    if(missing(group_dimension)){
      if(missing(event_array)){
#         grp_cols <- c('clientId', time_period)
#         dots <- lapply(grp_cols, as.symbol)
#         df_timecount_group<-df %>% group_by_(.dots=dots) %>% dplyr::summarise(count=n())
#         
#         myFormula <- as.formula(paste("~",time_period))
#         df_timecount_cast<- reshape::cast(df_timecount_group, myFormula, fun.aggregate=length)
        
        print('missing event_array with count_unique==0 does not make sense')
      }else{
        grp_cols <- c(time_period)
        dots <- lapply(grp_cols, as.symbol)
        df_timecount_group<-df %>% filter(Event %in% event_array) %>% 
          group_by_(.dots=dots) %>% dplyr::summarise(count=n())
        
        df_timecount_group<-as.data.frame(t(df_timecount_group))
        colnames(df_timecount_group)<-df_timecount_group[1,]
        df_timecount_group<-df_timecount_group[-1,]
        df_timecount_cast<-cbind(value = '(all)', df_timecount_group)
        
      }
    }else{
      if(missing(event_array)){
#         grp_cols <- c('clientId', group_dimension, time_period)
#         dots <- lapply(grp_cols, as.symbol)
#         df_timecount_group<-df %>% group_by_(.dots=dots) %>% dplyr::summarise(count=n())
#         
#         myFormula <- as.formula(paste(group_dimension,"~",time_period))
#         df_timecount_cast<- reshape::cast(df_timecount_group, myFormula, fun.aggregate=length)
        
        print('missing event_array with count_unique==0 does not make sense')
      }else{
        grp_cols <- c(group_dimension, time_period)
        dots <- lapply(grp_cols, as.symbol)
        df_timecount_group<-df %>% filter(Event %in% event_array) %>% 
          group_by_(.dots=dots) %>% dplyr::summarise(count=n())
        
        myFormula <- as.formula(paste(group_dimension,"~",time_period))
        df_timecount_cast<- reshape::cast(df_timecount_group, myFormula, fun.aggregate=)
      }
    }
  }
    
  
  
  return(df_timecount_cast)
}

my_cohort_percent_df<- function(my_cohort_df){

  for(i in 1:dim(my_cohort_df)[1]){
    for(k in 3:dim(my_cohort_df)[2]){
      my_cohort_df[,k][i]<-percent(as.integer(my_cohort_df[,k][i])/as.integer(my_cohort_df[,2][i]))
    }
  }
  
  for(i in 1:dim(my_cohort_df)[1]){
    my_cohort_df[,1][i] <- paste(my_cohort_df[,1][i],"(",my_cohort_df[,2][i], ")") 
    my_cohort_df[,2][i] <- percent(as.integer(my_cohort_df[,2][i])/as.integer(my_cohort_df[,2][i]))
  }
  return(my_cohort_df)
}

my_cohort_plot<- function(my_cohort_percent_df, group_dimension, time_period, max_number){
  if(missing(max_number)){
    cohort_plot<-t(as.matrix(my_cohort_percent_df))
    cohort_plot<-as.data.frame(as.matrix(cohort_plot))
    cohort_plot$time<-rownames(cohort_plot)
    
    cohort_plot <- reshape2::melt(cohort_plot, id="time")  # convert to long format
    cohort_plot$value<-as.numeric(sapply(strsplit(as.character(cohort_plot$value), split='%', fixed=TRUE), function(x) (x[[1]])))
    cohort_plot$time<-as.integer(cohort_plot$time)
    
    cohort_plot<-ggplot(cohort_plot,aes(x=time, y=value,group=variable , colour=variable)) + 
      geom_line(size=2, alpha=1/2)+ 
#      ggtitle(paste("Retention:",group_dimension, "by", time_period))+
      theme_bw(base_family="STHeiti")+
#      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=40, hjust=0))+
      xlab(time_period)
      
  }else{
    cohort_plot<-t(as.matrix(my_cohort_percent_df))
    cohort_plot<-as.data.frame(as.matrix(cohort_plot))
    cohort_plot$time<-rownames(cohort_plot)
    
    cohort_plot <- reshape2::melt(cohort_plot, id="time")  # convert to long format
    cohort_plot$value<-as.numeric(sapply(strsplit(as.character(cohort_plot$value), split='%', fixed=TRUE), function(x) (x[[1]])))
    cohort_plot$time<-as.integer(cohort_plot$time)
    
    cohort_plot<- cohort_plot %>% dplyr::filter(time %in% c(seq(1,max_number)))
    
    cohort_plot<-ggplot(cohort_plot,aes(x=time, y=value,group=variable , colour=variable)) + 
      geom_line(size=2, alpha=1/2)+ 
#      ggtitle(paste("Retention:",group_dimension, "by", time_period))+
      theme_bw(base_family="STHeiti")+
#      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=40, hjust=0))+
      theme(axis.text=element_text(size=12),
            axis.title.x=element_text(size=25),
            axis.title.y=element_text(size=25),
            legend.text=element_text(size=18),
            legend.title=element_text(size=20))+
      xlab(time_period) + geom_text(aes(label=value, vjust=-0.5), size=10)
      
  }

    
  return(cohort_plot)
}


