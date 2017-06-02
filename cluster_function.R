library(ggplot2)
library(ggfortify)
library(jsonlite)
library(plyr)
library(dplyr)
library(tidyjson)
library(factoextra)
library(cluster)
library(tidyr)



my_reshape2 <- function(dataframe, filter_argument, reshape_column, multiple_factor){
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
  print('A')
  temp_df<- gather(cluster_df,car,count,-clientId)
  print('B')
  temp_df$car_category<-NA
  print('C')
  for(i in 1: dim(car_type_df)[1]){
    print(i)
    temp_df[temp_df[['car']] %in% as.character(car_type_df[['model']][i]), "car_category"] <- as.character(car_type_df[['type']][i])
  }
  print('D')  
  temp_df2<-rbind.fill(temp_df %>% separate(car_category, c("car_category_final", "car_category2"), sep = ",", remove=FALSE),
                       temp_df %>% filter(grepl(",",car_category)) %>% separate(car_category, c("car_category2", "car_category_final"), sep = ",", remove=FALSE)) %>%
    select(clientId, car_category_final, count)
  print('E')  
  reshape_column='car_category_final'
  myFormula <- as.formula(paste("clientId ~ ",reshape_column))
  print('F')  
  temp_df3<- reshape::cast(temp_df2, myFormula, fun.aggregate=function(x) sum(x))
  
  return(temp_df3)
  
}

my_normalize<-function(df){
  # normalize the data
  temp_df<- t(apply(df[,-1], 1, function(x) scale(x)))
  temp_df<-as.data.frame(temp_df)
  
  # scale the data to 0-1
  temp_df<- t(apply(temp_df, 1, function(x) (x-min(x))/(max(x)-min(x))))
  temp_df<-as.data.frame(temp_df)
  return(temp_df)
}
#input={df: a data frame}
#output={temp_df: a normalized data frame, scaled to 0~1}

my_cluster_df<- function(df, df.scaled, km.object){
  temp_df<-df.scaled
  temp_df$clientId<-df$clientId
  temp_df$cluster<-km.object$cluster
  temp_df$clientId<-NULL
  colnames(temp_df)[1:dim(df)[2]-1]<-colnames(df)[2:dim(df)[2]]
  temp_df<-temp_df %>% dplyr::group_by(cluster) %>% dplyr::summarise_each(funs(mean))
  return(temp_df)
}
#input={df: original reshaped dataframe
#       df.scaled: a scaled dataframe using my_normalize
#       km.object: a cluster object}
#output={temp_df: a summarized viewable dataframe}

my_cluster_filter<- function(my_cluster_df, threshold){
  name<-c()
  for(i in 2:dim(my_cluster_df)[2]){
    if(max(my_cluster_df[,i])>threshold){
      name<-c(name,colnames(my_cluster_df)[i])
    }
  }
  temp_df<- my_cluster_df[, names(my_cluster_df) %in% name]
  return(temp_df)
}  
#input
#   my_cluster_df: a data frame output by my_cluster_df
#   threshold: the threshold to filter out the column
#output
#   temp_df: a filtered data frame 

my_cluster_merge_back2<- function(my_reshape, km.object){
  my_reshape$cluster<-km.object$cluster
  temp_cluster.id<-my_reshape %>% select(clientId,cluster)
  return(temp_cluster.id)
}

cluster_name_table<- function(my_cluster_filter, threshold){
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
  for(i in 1:length(unique(final_cluster$cluster))){
    if(!is.null(cluster_name[[i]])){
      final_cluster$cluster[final_cluster$cluster == i]<- as.character(paste('對「',paste(cluster_name[[i]],collapse=' & '),'」感興趣的族群',sep=''))
    }
  }
  #   final_cluster$date<-Sys.Date()
  #   final_cluster$timestamp<-as.numeric(Sys.time())
  #   final_cluster$date<-as.character(final_cluster$date)
  
  return(final_cluster)
}



####################################

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

my_data_clean<- function(df){
  #replace blank with NA
  df[df==""] <- NA

  #deal with the time column
  df$actual_time<-as.POSIXct(as.numeric(as.character(df$Time)),origin="1970-01-01",tz="Asia/Taipei")
  df$actual_date<-as.Date(df$actual_time,'Asia/Taipei')

  return(df)
}

my_reshape_lclass<- function(df,cart_factor=5){
  library(reshape)
  temp_view<- df %>% dplyr::filter(Event=='ViewContent') %>%
    dplyr::select(clientId,Product_Category0:Product_Category3)
  myFormula <- as.formula(paste("clientId ~ Product_Category1"))
  temp_view<- reshape::cast(temp_view, myFormula, fun.aggregate=length)
  temp_view$'NA'<-NULL
  temp_view<-temp_view %>% mutate(sum = rowSums(.[2:dim(temp_view)[2]])) %>% filter(sum!=0)
  temp_view$sum<-NULL

  temp_cart<- df %>% dplyr::filter(Event=='AddToCart') %>%
    dplyr::select(clientId,Product_Category0:Product_Category3)
  myFormula <- as.formula(paste("clientId ~ Product_Category1"))
  temp_cart<- reshape::cast(temp_cart, myFormula, fun.aggregate=function(x) cart_factor*length(x))
  temp_cart$'NA'<-NULL
  temp_cart<-temp_cart %>% mutate(sum = rowSums(.[2:dim(temp_cart)[2]])) %>% filter(sum!=0)
  temp_cart$sum<-NULL

  temp_df<-plyr::rbind.fill(temp_view,temp_cart)
  temp_df[is.na(temp_df)] <- 0

  temp_all<-temp_df %>% dplyr::group_by(clientId) %>% dplyr::summarise_each(funs(sum))


  return(temp_all)
}
#input={df: a data frame}
#output={temp_df: a reshaped data frame}

my_reshape_mclass<- function(df,cart_factor=5){
  library(reshape)
  temp_view<- df %>% dplyr::filter(Event=='ViewContent') %>%
    dplyr::select(clientId,Product_Category0:Product_Category3)
  myFormula <- as.formula(paste("clientId ~ Product_Category2"))
  temp_view<- reshape::cast(temp_view, myFormula, fun.aggregate=length)
  temp_view$'NA'<-NULL
  temp_view<-temp_view %>% mutate(sum = rowSums(.[2:dim(temp_view)[2]])) %>% filter(sum!=0)
  temp_view$sum<-NULL

  temp_cart<- df %>% dplyr::filter(Event=='AddToCart') %>%
    dplyr::select(clientId,Product_Category0:Product_Category3)
  myFormula <- as.formula(paste("clientId ~ Product_Category2"))
  temp_cart<- reshape::cast(temp_cart, myFormula, fun.aggregate=function(x) cart_factor*length(x))
  temp_cart$'NA'<-NULL
  temp_cart<-temp_cart %>% mutate(sum = rowSums(.[2:dim(temp_cart)[2]])) %>% filter(sum!=0)
  temp_cart$sum<-NULL

  temp_df<-plyr::rbind.fill(temp_view,temp_cart)
  temp_df[is.na(temp_df)] <- 0

  temp_all<-temp_df %>% dplyr::group_by(clientId) %>% dplyr::summarise_each(funs(sum))

  return(temp_all)
}
#input={df: a data frame}
#output={temp_df: a reshaped data frame}

my_cluster_merge_back<- function(original_df, my_reshape, km.object, threshold_short_days=7, threshold_days=30){
  my_reshape$cluster<-km.object$cluster
  temp_cluster.id<-my_reshape %>% select(clientId,cluster)
  temp_df<-merge(original_df,temp_cluster.id,by.x='clientId')

  myFormula <- as.formula(paste("clientId ~ Event"))
  id_jump<- reshape::cast(original_df, myFormula, fun.aggregate=length)
  id_jump<- id_jump %>% mutate(sum=rowSums(id_jump[,-1])) %>% filter(sum==1)
  id_jump<- id_jump$clientId
  id_jump_days<- original_df %>% filter(clientId %in% id_jump) %>% dplyr::group_by(clientId)  %>%
    dplyr::summarize(max_date=max(actual_date)) %>%
    dplyr::filter((as.numeric(Sys.Date()-max_date))>threshold_short_days)

  id_days<- temp_df %>% dplyr::group_by(clientId) %>% dplyr::summarize(max_date=max(actual_date)) %>%
    dplyr::filter((as.numeric(Sys.Date()-max_date))<threshold_days)

  temp_df<- temp_df %>% filter(clientId %in% id_days$clientId) %>%
    filter(!(clientId %in% id_jump_days$clientId)) %>%
    group_by(clientId,cluster) %>%
    dplyr::summarise(count=n()) %>%
    select(clientId,cluster)

  return(temp_df)
}

append_l_class_name<- function(df,cluster_name){
  product_df<- df %>% filter(Event=='ViewContent')
  for(i in 1:length(cluster_name)){
    if(!is.null(cluster_name[[i]])){
      product_df_filtered<- product_df %>% filter(Product_Category2==cluster_name[[i]])
      names(cluster_name)[i]<-product_df_filtered$Product_Category1[1]
    }
  }
  return(cluster_name)
}

my_m_cluster_name<- function(final_cluster,cluster_name){
  for(i in 1:length(unique(final_cluster$cluster))){
    if(!is.null(cluster_name[[i]])){
      final_cluster$cluster[final_cluster$cluster == i]<- as.character(paste('Audience > Interest > ',names(cluster_name)[i],' > ',paste(cluster_name[[i]],collapse=' & '),sep=''))
    }else{
      final_cluster<- final_cluster %>% filter(cluster!=i)
    }
  }
#   final_cluster$date<-Sys.Date()
#   final_cluster$timestamp<-as.numeric(Sys.time())
#   final_cluster$date<-as.character(final_cluster$date)

  return(final_cluster)
}

my_rbind_maped<- function(df1,df2,domain_df){
  final_df<-rbind(df1,df2)

  #final_df$date<-Sys.Date()
  final_df$domain<-names(table(domain_df$domain))[which.max(table(domain_df$domain))]
  final_df$timestamp<-Sys.time()
  final_df<- final_df %>%
    dplyr::group_by(clientId, domain, timestamp) %>%
    dplyr::summarise(cluster = list(as.character(unique(cluster))))

  return(final_df)
}






