library(jsonlite)
library(plyr)
library(dplyr)
library(cluster)
library(fasttime)
library(scales)
library(reshape)
library(data.table)
# library(rgeolocate)
library(tidyr)

set_column_type<- function(df){
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

my_session<- function(df){
  # 計算產生 session 欄位
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

my_reshape <- function(dataframe, filter_argument, reshape_column, multiple_factor){
  if(missing(filter_argument)){
    grp_cols <- c('clientId',reshape_column,'session') # Columns you want to group by
    dots <- lapply(grp_cols, as.symbol)# Convert character vector to list of symbols
    temp_df<- dataframe %>% group_by_(.dots=dots) %>% dplyr::summarise(count=n())
    myFormula <- as.formula(paste("clientId ~ ",reshape_column))
    temp_df<- reshape::cast(temp_df, myFormula, fun.aggregate=function(x) multiple_factor*length(x))
    temp_df$'NA'<-NULL
    temp_df<-temp_df %>% mutate(sum = rowSums(.[2:dim(temp_df)[2]])) %>% filter(sum!=0)
    temp_df$sum<-NULL
  }else{
    grp_cols <- c('clientId',reshape_column,'session') # Columns you want to group by
    dots <- lapply(grp_cols, as.symbol)# Convert character vector to list of symbols
    temp_df<-dataframe %>% filter_(paste(filter_argument))
    temp_df<- temp_df %>% group_by_(.dots=dots) %>% dplyr::summarise(count=n())
    myFormula <- as.formula(paste("clientId ~ ",reshape_column))
    temp_df<- reshape::cast(temp_df, myFormula, fun.aggregate=function(x) multiple_factor*length(x))
    temp_df$'NA'<-NULL
    temp_df<-temp_df %>% mutate(sum = rowSums(.[2:dim(temp_df)[2]])) %>% filter(sum!=0)
    temp_df$sum<-NULL
  }
  
  
  return(temp_df)
  
}

cluster_mapping<- function(cluster_df, car_type_df){
  temp_df<- gather(cluster_df,car,count,-clientId)
  temp_df$car_category<-NA
  for(i in 1: dim(car_type_df)[1]){
    temp_df[temp_df[['car']] %in% as.character(car_type_df[['model']][i]), "car_category"] <- as.character(car_type_df[['type']][i])
  }
  temp_df2<-rbind.fill(temp_df %>% separate(car_category, c("car_category_final", "car_category2"), sep = ",", remove=FALSE),
                       temp_df %>% filter(grepl(",",car_category)) %>% separate(car_category, c("car_category2", "car_category_final"), sep = ",", remove=FALSE)) %>%
    select(clientId, car_category_final, count)
  reshape_column='car_category_final'
  myFormula <- as.formula(paste("clientId ~ ",reshape_column))
  temp_df3<- reshape::cast(temp_df2, myFormula, fun.aggregate=function(x) sum(x))
  return(temp_df3)
}

my_normalize<-function(df){
  #input={df: a data frame}
  #output={temp_df: a normalized data frame, scaled to 0~1}
  
  # normalize the data
  temp_df<- t(apply(df[,-1], 1, function(x) scale(x)))
  temp_df<-as.data.frame(temp_df)
  
  # scale the data to 0-1
  temp_df<- t(apply(temp_df, 1, function(x) (x-min(x))/(max(x)-min(x))))
  temp_df<-as.data.frame(temp_df)
  return(temp_df)
}

my_cluster_df<- function(df, df.scaled, km.object){
  #input={df: original reshaped dataframe
  #       df.scaled: a scaled dataframe using my_normalize
  #       km.object: a cluster object}
  #output={temp_df: a summarized viewable dataframe}
  
  temp_df<-df.scaled
  temp_df$clientId<-df$clientId
  temp_df$cluster<-km.object$cluster
  temp_df$clientId<-NULL
  colnames(temp_df)[1:dim(df)[2]-1]<-colnames(df)[2:dim(df)[2]]
  temp_df<-temp_df %>% dplyr::group_by(cluster) %>% dplyr::summarise_each(funs(mean))
  return(temp_df)
}


my_cluster_filter<- function(my_cluster_df, threshold){
  #input
  #   my_cluster_df: a data frame output by my_cluster_df
  #   threshold: the threshold to filter out the column
  #output
  #   temp_df: a filtered data frame 
  name<-c()
  for(i in 2:dim(my_cluster_df)[2]){
    if(max(my_cluster_df[,i])>threshold){
      name<-c(name,colnames(my_cluster_df)[i])
    }
  }
  temp_df<- my_cluster_df[, names(my_cluster_df) %in% name]
  return(temp_df)
}  


my_cluster_merge_back<- function(my_reshape, km.object){
  # 把分群的結果並回本來的 dataframe 裡頭，傳回一個只有 clientId, cluster 的 dataframe
  my_reshape$cluster<-km.object$cluster
  temp_cluster.id<-my_reshape %>% select(clientId,cluster)
  return(temp_cluster.id)
}

cluster_name_table<- function(my_cluster_filter, threshold){
  # 取出 cluster 的名字
  
  temp_list<- list()
  name<-c()
  for(row in 1:dim(my_cluster_filter)[1]){
    for(column in 1:dim(my_cluster_filter)[2]){
      if(my_cluster_filter[row, column]>threshold){
        name<-c(name,colnames(my_cluster_filter)[column])
      }
    }
    temp_list[[row]]<-name
    name<-c()
  }
  return(temp_list)
}

my_cluster_name<- function(final_cluster,cluster_name){
  # 把本來 final_cluster cluster的值，取代為cluster_name_table裡頭的文字，並且加上頭跟尾（對「xxx」感興趣的族群）
  
  for(i in 1:length(unique(final_cluster$cluster))){
    if(!is.null(cluster_name[[i]])){
      final_cluster$cluster[final_cluster$cluster == i]<- as.character(paste('對「',paste(cluster_name[[i]],collapse=' & '),'」感興趣的族群',sep=''))
    }
  }
  return(final_cluster)
}
