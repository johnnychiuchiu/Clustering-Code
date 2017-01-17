#http://new.nissan.com.tw/nissan
print(paste('cluster.R start:',Sys.time()))
allstart<-Sys.time()

#setwd('/home/ubuntu/johnny/cluster_calculate/')
setwd('/Users/JohnnyChiu/Desktop/檔案總管/2016專案/R/0817_nissan/final_2')
eval(parse("sop_function.R", encoding="UTF-8"))
eval(parse("cluster_function.R", encoding="UTF-8"))
Sys.setlocale("LC_ALL", "UTF-8")



mydata<-fread('final_nissan.csv',colClasses=c(rep("character",24)))
mydata[,c('V1'):=NULL]
mydata<-set_column_type(mydata)


myjson<-jsonlite::fromJSON('_data/nissan_com_tw.json', flatten = TRUE)
myjson<-my_data_manipulate(myjson)
myjson<-as.data.table(myjson)
myjson$clientId <- ifelse(!is.na(myjson$clientId), myjson$clientId, myjson$gid)
myjson$clientId <- ifelse(!is.na(myjson$clientId), myjson$clientId, myjson$fid)
myjson[,c('fid','gid'):=NULL]
myjson<-remove_id(myjson, c('1705117656.1467707925','1957040293.1471416113'))

mydata2 <- rbind.fill(mydata,myjson)
# mydata2 <- as.data.table(mydata2)
# setkey(mydata2,NULL)
# mydata3 <- unique(mydata2)

#mydata3_11<- mydata3[actual_date>="2016-11-01" & actual_date<="2016-11-30"]
#write.csv(mydata3_11,file='final_nissan_nov.csv')

write.csv(mydata2, file='final_nissan.csv')

mydata2<-my_session6(mydata2)
print('mydata finished')

################################################################################依照汽車型號做分群car
#######################################產生clustering 用的data frame
##############################1 * Event: page car、、page car停留時間
cluster_df_1<-my_reshape2(mydata2, filter_argument = "Event=='Page_Car'", reshape_column = 'car', multiple_factor=1)

##############################3 * 下載型錄
cluster_df_3<-my_reshape2(mydata2, filter_argument = "Event=='Click_Car_DownLoad'", reshape_column = 'car', multiple_factor=3)

##############################3 * 觀看詳細規格Page_Car_Spec”, type
mydata3 <- mydata2 %>% filter(Event=='Page_Car_Spec') 
mydata3$car[is.na(mydata3$car)] <- mydata3$type[is.na(mydata3$car)]
cluster_df_4<-my_reshape2(mydata3, filter_argument = "Event=='Page_Car_Spec'", reshape_column = 'car', multiple_factor=3)

##############################5 * 購車試算, car
cluster_df_5<-my_reshape2(mydata2, filter_argument = "type=='購車試算_試算結果'", reshape_column = 'car', multiple_factor=5)

##############################5 * Click_Car_Build_2_Choose, car
cluster_df_6<-my_reshape2(mydata2, filter_argument = "Event=='Click_Car_Build_2_Choose'", reshape_column = 'car', multiple_factor=5)

##############################8 * 選擇試乘 Page_TestDrive_PC_Chosen, car
cluster_df_7<-my_reshape2(mydata2, filter_argument = "Event=='Page_TestDrive_PC_Chosen'", reshape_column = 'car', multiple_factor=8)

##############################10 * 預約試乘
cluster_df_8<-my_reshape2(mydata2, filter_argument = "Event=='TestDrive_Submit_Click'", reshape_column = 'car', multiple_factor=10)

##############################合併
cluster_df<-plyr::rbind.fill(cluster_df_1,cluster_df_3,cluster_df_4,cluster_df_5,cluster_df_6,cluster_df_7,cluster_df_8)
cluster_df[is.na(cluster_df)] <- 0
cluster_df<-cluster_df %>% dplyr::group_by(clientId) %>% dplyr::summarise_each(funs(sum))

#######################################做一些欄位篩選，有的欄位不應該出現，如：ALL NEWティアナ
cluster_df <- cluster_df %>% dplyr::select(`clientId`, `370Z`, `ALL NEW LIVINA`, `ALL NEW TEANA`, `BIG TIIDA`, `BIG TIIDA Turbo`, `JUKE`, `MURANO`, `NEW MARCH`, `SENTRA aero`, `SUPER SENTRA`, `TIIDA`,`X-TRAIL`,`GT-R`)

print('cluster_df finished')
################################################################################ clustering
####################################### 依type mapping
car_type_df<- data.frame(model=c('TIIDA','NEW MARCH','ALL NEW LIVINA','BIG TIIDA','SUPER SENTRA','SENTRA aero','ALL NEW TEANA','BIG 							 	 TIIDA Turbo','X-TRAIL','JUKE','MURANO','370Z','GT-R'),                             
						type=c('轎車','轎車','轎車,休旅車','轎車','轎車','轎車','轎車','轎車','休旅車','休旅車,進口車','休旅車,進口車',
                                 '跑車,進口車','跑車,進口車'))
type_cluster_df<- cluster_mapping(cluster_df, car_type_df)

####################################### scale
type_cluster_df.scaled<-my_normalize(type_cluster_df)

####################################### remove na rows and rows become na in type_cluster_df
if(any(is.na(type_cluster_df.scaled$V1))){
  type_cluster_df<-type_cluster_df[-which(is.na(type_cluster_df.scaled$V1)),]
  type_cluster_df.scaled<-na.omit(type_cluster_df.scaled)
}

####################################### run cluster algorithm
kmeans.km_2<- clara(type_cluster_df.scaled, 5,samples=100,sampsize=500, pamLike=TRUE)  
km.type_cluster_df.scaled.cluster<-my_cluster_df(type_cluster_df, type_cluster_df.scaled, kmeans.km_2)

############### filtering the columns, which the max of it are larger than 0.7
km.type_cluster_df.scaled.cluster.filter<-my_cluster_filter(km.type_cluster_df.scaled.cluster, 0.7)

############### merge back the cluster and filter out the clientid that does show up for more than 90 days.
############### For the user with only one event, we only keep them for 7 days
km.final_cluster_2<-my_cluster_merge_back2(my_reshape=type_cluster_df, km.object=kmeans.km_2)
cluster_name<-cluster_name_table(km.type_cluster_df.scaled.cluster.filter,0.7)
km.final_cluster_2<-my_cluster_name(km.final_cluster_2,cluster_name)

print('km.final_cluster_2 finished')

x<-km.final_cluster_2
x$methodName<-'product intent clustering'
colnames(x)[which(names(x) == "cluster")] <- "groupName"
colnames(x)[which(names(x) == "clientId")] <- "clientIds"
x<- x %>%
  dplyr::group_by(methodName,groupName) %>% 
  dplyr::summarise(clientIds = list(as.character(unique(clientIds))))  

print('final x finished')

################################################################################依照對網站感興趣的程度做clustering
my_session_time<-mydata %>% dplyr::group_by(clientId, session) %>% 
  dplyr::summarize(session_duration=as.numeric(difftime(max(actual_time), min(actual_time), Asia/Taipei,units = "secs"))) %>% 
  dplyr::group_by(clientId) %>% 
  dplyr::summarize(average_session_min=mean(session_duration)/60) %>%
  dplyr::filter(average_session_min!=0)
my_session_time$Interest <- cut(my_session_time$average_session_min, 
                                breaks = c(0,as.numeric(quantile(my_session_time$average_session_min,0.5)),
                                           as.numeric(quantile(my_session_time$average_session_min,0.75)),Inf), 
                                labels = c("輕度興趣族群","中度興趣族群","高度興趣族群"), 
                                right = FALSE) 
my_session_time<- my_session_time %>% dplyr::select(clientId,Interest)
y<-my_session_time
y$methodName<-'website interest clustering'
colnames(y)[which(names(y) == "Interest")] <- "groupName"
colnames(y)[which(names(y) == "clientId")] <- "clientIds"
y<- y %>%
  dplyr::group_by(methodName,groupName) %>% 
  dplyr::summarise(clientIds = list(as.character(unique(clientIds))))  

print('final y finished')




################################################################################依照汽車type
#######################################clustering
  cluster_df.scaled<-my_normalize(cluster_df)
  
  #######################################remove na rows and rows become na in type_cluster_df
  if(any(is.na(cluster_df.scaled$V1))){
    cluster_df<-cluster_df[-which(is.na(cluster_df.scaled$V1)),]
    cluster_df.scaled<-na.omit(cluster_df.scaled)
  }
  
  kmeans.km_3<- clara(cluster_df.scaled, 13,samples=100,sampsize=500, pamLike=TRUE)  
  km.cluster_df.scaled.cluster<-my_cluster_df(cluster_df, cluster_df.scaled, kmeans.km_3)
  
  ############### filtering the columns, which the max of it are larger than 0.7
  km.cluster_df.scaled.cluster.filter<-my_cluster_filter(km.cluster_df.scaled.cluster, 0.7)
  
  ############### merge back the cluster and filter out the clientid that does show up for more than 90 days.
  ############### For the user with only one event, we only keep them for 7 days
  #km.final_cluster_2<-my_cluster_merge_back(original_df=mydata, my_reshape=type_cluster_df, km.object=kmeans.km_2, 1000, 1000)
  km.final_cluster_3<-my_cluster_merge_back2(my_reshape=cluster_df, km.object=kmeans.km_3)
  cluster_name<-cluster_name_table(km.cluster_df.scaled.cluster.filter,0.7)
  km.final_cluster_3<-my_cluster_name(km.final_cluster_3,cluster_name)

z<-km.final_cluster_3

# clientId_List<-mydata %>% filter(actual_date>='2016-08-24')
# clientId_List<-clientId_List$clientId
# x<- x %>% filter(clientId %in% clientId_List)
z$methodName<-'product intent clustering'
colnames(z)[which(names(z) == "cluster")] <- "groupName"
colnames(z)[which(names(z) == "clientId")] <- "clientIds"
z<- z %>%
  dplyr::group_by(methodName,groupName) %>% 
  dplyr::summarise(clientIds = list(as.character(unique(clientIds))))  



print('final z finished')









######output
#final_output<-rbind(x,y)
#final_cluster_json<- jsonlite::toJSON(final_output, pretty=TRUE) # a better solution
#write(final_cluster_json, file=paste('./_cluster_data/',Sys.Date(),'_cluster.json',sep=''))
for(i in 1:dim(x)[1]){
  final_x<- jsonlite::toJSON(x[i,], pretty=TRUE) # a better solution
  write(final_x, file=paste('./_cluster_data/',Sys.Date(),'_cluster_CarInterest_',i,'.json',sep=''))
}

for(i in 1:dim(y)[1]){
  final_y<- jsonlite::toJSON(y[i,], pretty=TRUE) # a better solution
  write(final_y, file=paste('./_cluster_data/',Sys.Date(),'_cluster_WebIntent_',i,'.json',sep=''))
}

for(i in 1:dim(z)[1]){
  final_z<- jsonlite::toJSON(z[i,], pretty=TRUE) # a better solution
  write(final_z, file=paste('./_cluster_data/',Sys.Date(),'_cluster_CarInterest_Specific_',i,'.json',sep=''))
}

allend<-Sys.time()
print(paste('cluster.R end:',Sys.time()))
difftime(allend, allstart, Asia/Taipei,units = "mins")







###########20170106 手動計算一些數字
#把cluster merge回mydata2
# temp_df<-cluster_df.scaled
# temp_df$clientId<-cluster_df$clientId
# temp_df$cluster<-kmeans.km_3$cluster
# temp_df<- temp_df %>% select(clientId, cluster)
# temp_df<-my_cluster_name(temp_df,cluster_name)
# temp_df2<- merge(mydata2, temp_df, by.x='clientId')

march_clientId<- km.final_cluster_3 %>% filter(cluster=='對「NEW MARCH」感興趣的族群')
march_clientId<- march_clientId$clientId

march_data<- mydata2 %>% filter(clientId %in% march_clientId)

march_data<-set_column_type(march_data)

# 網頁總瀏覽量
dim(march_data)[1]
# 不重複人數
length(unique(march_data$clientId))
# 平均停留時間
data_3_1<- march_data %>% group_by(clientId, session) %>% 
  dplyr::summarize(session_duration=as.numeric(difftime(max(actual_time), min(actual_time), Asia/Taipei,units = "secs")))

data_3<- data_3_1 %>%  group_by(clientId) %>% 
  dplyr::summarize(avg_time=mean(session_duration)) %>% 
  mutate(total_mins=avg_time/60,total_hours=avg_time/60/60)
mins_per_id<-mean(data_3$total_mins)

# 跳出率
data_7<- march_data %>% group_by(clientId, session) %>% 
  dplyr::summarize(count=n())
data_7$bounce<-ifelse(data_7$count>1,0,1)
data_2<-march_data %>% group_by(clientId) %>% dplyr::summarize(clieitid_session=max(session))
bounce_rate<-percent(sum(data_7$bounce)/sum(data_2$clieitid_session))

# 新增訪客
data_8_1<- march_data %>% group_by(clientId) %>% 
  summarise(max_session=max(session)) %>% filter(max_session==1)
new_client<-length(unique(data_8_1$clientId))
new_client_rate<-new_client/length(unique(march_data$clientId))
  
# 回訪訪客
1-new_client_rate
# 進站次數分布圖
data_14_1<- march_data %>% group_by(clientId) %>% 
  summarise(max_session=max(session))
table(data_14_1$max_session)

# 停留時間分布圖
data_12_1<- march_data %>% group_by(clientId, session) %>% 
  dplyr::summarize(session_duration=as.numeric(difftime(max(actual_time), min(actual_time), Asia/Taipei,units = "secs")))

data_12<- data_12_1 %>%  group_by(clientId) %>% 
  dplyr::summarize(total_time=sum(session_duration)) %>% 
  mutate(total_mins=total_time/60,total_hours=total_time/60/60)

data_12$time_interval <- cut(data_12$total_time, 
                                                 breaks = c(0,30,60,90,120,180,240,300,Inf), 
                                                 labels = c("<30", "30~60", "61~90", "91~120", "2m~3m", "3m~4m", "4m~5m", ">5m"), 
                                                 right = FALSE)

table(data_12$time_interval)

# 最後接觸時間圖
data_13<- march_data %>% dplyr::group_by(clientId) %>% dplyr::summarize(max_date=max(actual_date)) %>% 
  mutate(last_contact=as.numeric(Sys.Date()-max_date))
  #dplyr::filter((as.numeric(Sys.Date()-max_date))<threshold_days)

data_13$day_interval <- cut(data_13$last_contact, 
                             breaks = c(0,3,7,14,30,Inf), 
                             labels = c("3d", "7d", "14d", "<30d", ">30d"), 
                             right = FALSE)

table(data_13$day_interval)



referrer_generator<- function(df){
  df$referrer<-df$UTM_Source
  df$referrer<- ifelse(!is.na(df$referrer),df$referrer,
                       ifelse(df$URL_Current==df$URL_Referrer,'Direct / Referral',
                              ifelse(grepl('com.google.android.googlequicksearchboxnull',df$URL_Referrer),'Google Organic / Referral',
                                     ifelse(grepl('google.com',df$URL_Referrer),'Google Organic / Referral',
                                            ifelse(grepl('tpc.googlesyndication.com',df$URL_Referrer),'Google Syndication / Referral',
                                                   ifelse(grepl('facebook.com',df$URL_Referrer),'Facebook / Referral',
                                                          ifelse(grepl('yahoo.com',df$URL_Referrer),'Yahoo Organic / Referral',
                                                                 ifelse(grepl('bing.com',df$URL_Referrer),'Bing Organic / Referral',
                                                                        ifelse(grepl('nissan',df$URL_Referrer),'Nissan / Referral',
                                                                               ifelse(grepl('.5230.com',df$URL_Referrer),'Mitsubishi / Referral',
                                                                                      ifelse(grepl('mitsubishi',df$URL_Referrer),'Mitsubishi / Referral',
                                                                                             ifelse(grepl('outlander',df$URL_Referrer),'Mitsubishi / Referral',
                                                                                                    ifelse(grepl('china-motor',df$URL_Referrer),'ChinaMotor / Referral',
                                                                                                           ifelse(grepl('yulon-motor',df$URL_Referrer),'YulonMotor / Referral',
                                                                                                                  ifelse(is.na(df$URL_Referrer),'others / Referral','others / Referral')))))))))))))))
  df$referrer[is.na(df$referrer)] <- 'others / Referral'
  return(df)
}
#mydata2$actual_time = as.POSIXct(mydata2$actual_time,tz="Asia/Taipei")
mydata2$actual_time = fastPOSIXct(mydata2$actual_time)
df2 <- mydata2 %>% group_by(clientId, session) %>% slice(which.min(actual_time)) 
df3<- referrer_generator(df2)
mydata3<- merge(mydata2,df3, by=c("clientId","session"), all.x=TRUE, all.y=FALSE)
rm(df2,df3)

mydata3<- mydata3 %>% select(1:24,referrer)
colnames(mydata3) <- c("clientId", "session", "IP", "screenResolution", "viewportSize",
                       "language", "Event", "car", "type", "URL_Current", "URL_Referrer",
                       "Device", "UTM_Source", "UTM_Medium" , "UTM_Term", "UTM_Content",
                       "UTM_Compaign", "timeZone", "userAgent", "Time", "actual_time",
                       "actual_date", "hour", "weekday", "referrer")

march_data_2<- mydata3 %>% filter(clientId %in% march_clientId)
march_data_2<-set_column_type(march_data_2)
# 媒體來源

# 瀏覽數
# 不重複人
# 平均瀏覽頁數
data9<-march_data_2 %>% group_by(referrer) %>% 
  summarise(page_count=n(),unique_id=n_distinct(clientId)) %>% 
  mutate(avg_page=page_count/unique_id)

# 平均停留時間
data_10_1<- march_data_2 %>% group_by(referrer, clientId, session) %>% 
  dplyr::summarize(session_duration=as.numeric(difftime(max(actual_time), min(actual_time), Asia/Taipei,units = "secs")))

data_10_2<- data_10_1 %>%  group_by(referrer, clientId) %>% 
  dplyr::summarize(mean_session_time=mean(session_duration)) %>% 
  mutate(total_mins=mean_session_time/60,total_hours=mean_session_time/60/60)

data_10<- data_10_2 %>%  group_by(referrer) %>% 
  dplyr::summarize(mean_session_referrer=mean(mean_session_time)) %>% 
  mutate(total_mins=mean_session_referrer/60,total_hours=mean_session_referrer/60/60)



# 預約試乘數
data_11_1 <- march_data_2 %>% filter(Event == 'TestDrive_Submit_Click') %>% group_by(referrer, Time) %>% summarise(count=n())
data_11 <- data_11_1 %>% group_by(referrer) %>% summarise(count=n())

final_referrer<- merge(data9, data_10, by="referrer", all.x = TRUE)
final_referrer<- merge(final_referrer, data_11, by="referrer", all.x = TRUE)
final_referrer<- final_referrer %>% select(referrer, page_count, unique_id, avg_page, total_mins, count)
final_referrer[is.na(final_referrer)] <- 0
final_referrer<- final_referrer %>% arrange(desc(page_count))

write.csv(final_referrer, file="final_referrer.csv")

