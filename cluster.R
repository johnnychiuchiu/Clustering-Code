# #http://new.nissan.com.tw/nissan
print(paste('cluster.R start:',Sys.time()))
allstart<-Sys.time()

setwd('/Users/JohnnyChiu/Desktop/MyFiles/2017_projects/coding/0217_nissan_interest_generator')
eval(parse("cluster_function.R", encoding="UTF-8"))
Sys.setlocale("LC_ALL", "UTF-8")

################################################################################
##################################### read data

##### 讀取csv檔案中最後2,000,000筆資料
mydata<-fread('tail -2000000 final_nissan.csv',
              colClasses=c(rep("character",21)),
              col.names=c('cid','fid','gid','screenResolution','language',
                          'Event','car','type','URL_Current','URL_Referrer',
                          'UTM_Source','UTM_Medium','UTM_Term','UTM_Content',
                          'UTM_Compaign','userAgent','IP','Time','actual_time',
                          'actual_date','clientId'))
mydata<-set_column_type(mydata)

##### 把當天新的資料讀進來
myjson<-jsonlite::fromJSON(paste('_data/nissan_flat_',format(Sys.Date(),'%y%m%d'),'/nissan_com_tw.json',sep=''), flatten = TRUE)
myjson<-my_data_manipulate(myjson)
# myjson<-as.data.table(myjson)

##### 把當天的資料併回本來的 csv 裡頭
# write.table(myjson, file='final_nissan.csv',row.names = FALSE, col.names = FALSE, append= TRUE, sep=',')

##### 把資料合併，並且產生 session 欄位
mydata2 <- rbind.fill(mydata,myjson)
mydata3 <- my_session(mydata2)

print('mydata finished')




################################################################################
##################################### 產生clustering 用的data frame：根據不同的事件，給予不同的權重

##### 1 * Event: page car、、page car停留時間
cluster_df_1<-my_reshape(mydata3, filter_argument = "Event=='Page_Car'", reshape_column = 'car', multiple_factor=1)

##### 3 * 下載型錄
cluster_df_3<-my_reshape(mydata3, filter_argument = "Event=='Click_Car_DownLoad'", reshape_column = 'car', multiple_factor=3)

##### 3 * 觀看詳細規格Page_Car_Spec”, type
mydata3_temp <- mydata3 %>% filter(Event=='Page_Car_Spec') 
mydata3_temp$car[is.na(mydata3_temp$car)] <- mydata3_temp$type[is.na(mydata3_temp$car)]
cluster_df_4<-my_reshape(mydata3_temp, filter_argument = "Event=='Page_Car_Spec'", reshape_column = 'car', multiple_factor=3)

##### 5 * 購車試算, car
cluster_df_5<-my_reshape(mydata3, filter_argument = "type=='購車試算_試算結果'", reshape_column = 'car', multiple_factor=5)

##### 5 * Click_Car_Build_2_Choose, car
cluster_df_6<-my_reshape(mydata3, filter_argument = "Event=='Click_Car_Build_2_Choose'", reshape_column = 'car', multiple_factor=5)

##### 8 * 選 Page_TestDrive_PC_Chosen, car
cluster_df_7<-my_reshape(mydata3, filter_argument = "Event=='Page_TestDrive_PC_Chosen'", reshape_column = 'car', multiple_factor=8)

##### 10 * 預約試乘
cluster_df_8<-my_reshape(mydata3, filter_argument = "Event=='TestDrive_Submit_Click'", reshape_column = 'car', multiple_factor=10)

##### 合併
cluster_df<-plyr::rbind.fill(cluster_df_1,cluster_df_3,cluster_df_4,cluster_df_5,cluster_df_6,cluster_df_7,cluster_df_8)
cluster_df[is.na(cluster_df)] <- 0
cluster_df<-cluster_df %>% dplyr::group_by(clientId) %>% dplyr::summarise_each(funs(sum))

##### 做一些欄位篩選，有的欄位不應該出現，如：ALL NEWティアナ
cluster_df <- cluster_df %>% dplyr::select(`clientId`, `370Z`, `ALL NEW LIVINA`, `ALL NEW TEANA`, `BIG TIIDA`, `BIG TIIDA Turbo`, `JUKE`, `MURANO`, `NEW MARCH`, `SENTRA aero`, `SUPER SENTRA`, `TIIDA`,`X-TRAIL`,`GT-R`)

print('cluster_df finished')


################################################################################
##################################### 依照汽車類型做分群

##### 把汽車和汽車類型 mapping 起來
car_type_df<- data.frame(model=c('TIIDA','NEW MARCH','ALL NEW LIVINA','BIG TIIDA','SUPER SENTRA','SENTRA aero','ALL NEW TEANA','BIG TIIDA Turbo','X-TRAIL','JUKE','MURANO','370Z','GT-R'),                             
						type=c('轎車','轎車','轎車,休旅車','轎車','轎車','轎車','','轎車','休旅車','休旅車,進口車','休旅車,進口車',
                                 '跑車,進口車','跑車,進口車'))
type_cluster_df<- cluster_mapping(cluster_df, car_type_df)

##### normalize value for clustering
type_cluster_df.scaled<-my_normalize(type_cluster_df)

##### remove na rows and rows become na in type_cluster_df
if(any(is.na(type_cluster_df.scaled$V1))){
  type_cluster_df<-type_cluster_df[-which(is.na(type_cluster_df.scaled$V1)),]
  type_cluster_df.scaled<-na.omit(type_cluster_df.scaled)
}

##### run cluster algorithm
kmeans.km_2<- clara(type_cluster_df.scaled, 5,samples=100,sampsize=500, pamLike=TRUE)  
km.type_cluster_df.scaled.cluster<-my_cluster_df(type_cluster_df, type_cluster_df.scaled, kmeans.km_2)

##### filtering the columns, which the max of it are larger than 0.7
km.type_cluster_df.scaled.cluster.filter<-my_cluster_filter(km.type_cluster_df.scaled.cluster, 0.7)

##### merge back the cluster
km.final_cluster_2<-my_cluster_merge_back(my_reshape=type_cluster_df, km.object=kmeans.km_2)
cluster_name<-cluster_name_table(km.type_cluster_df.scaled.cluster.filter,0.7)
km.final_cluster_2<-my_cluster_name(km.final_cluster_2,cluster_name)

##### 把汽車類型分群的結果，限制在下面這五種裡面，以防日後
fix_cluster_name = c('對「休旅車 & 轎車」感興趣的族群','對「休旅車 & 進口車」感興趣的族群',
                     '對「休旅車」感興趣的族群','對「跑車 & 進口車」感興趣的族群',
                     '對')
km.final_cluster_2<- km.final_cluster_2 %>% filter(cluster %in% fix_cluster_name)
print('type mapping finished')

##### 產生最終要輸出的資料
x = mydata3 %>% filter(clientId %in% km.final_cluster_2$clientId) %>% select(clientId, fid, cid, gid, actual_date)
x = merge(x, km.final_cluster_2, by ='clientId', all.x = TRUE)
x$methodName<-'product intent clustering'
colnames(x)[which(names(x) == "cluster")] <- "groupName"
x<- x %>% distinct %>% filter(!is.na(fid) | !is.na(cid) | !is.na(gid)) %>%
  dplyr::group_by(actual_date, methodName, groupName) %>% 
  dplyr::summarise(fids = list(as.character(fid)),
                   cids = list(as.character(cid)),
                   gids = list(as.character(gid)))  

print('依照汽車類型做分群 finished')


################################################################################
##################################### 依照對網站感興趣的程度做分群

##### 計算每個 id 的網站總停留時間
my_session_time<-mydata3 %>% dplyr::group_by(clientId, session) %>% 
  dplyr::summarize(session_duration=as.numeric(difftime(max(actual_time), min(actual_time), Asia/Taipei,units = "secs"))) %>% 
  dplyr::group_by(clientId) %>% 
  dplyr::summarize(total_time=sum(session_duration)) %>% 
  mutate(total_mins=total_time/60,total_hours=total_time/60/60) 

##### 依照計算出來的網站總停留時間百分比：(0, 網站總停留時間 50 百分位數, 網站總停留時間 75 百分位數, Inf)，把 ID 分為輕、中、高度興趣族群
my_session_time$Interest <- cut(my_session_time$total_mins, 
                                breaks = c(0,as.numeric(quantile(my_session_time$total_mins,0.5)),
                                           as.numeric(quantile(my_session_time$total_mins,0.75)),Inf), 
                                labels = c("輕度興趣族群","中度興趣族群","高度興趣族群"), 
                                right = FALSE) 
my_session_time<- my_session_time %>% dplyr::select(clientId,Interest)

# 產生最終要輸出的資料
y = mydata3 %>% filter(clientId %in% my_session_time$clientId) %>% select(clientId, fid, cid, gid, actual_date)
y = merge(y, my_session_time, by ='clientId', all.x = TRUE)
y$methodName<-'website interest clustering'
colnames(y)[which(names(y) == "Interest")] <- "groupName"
y<- y %>% distinct %>% filter(!is.na(fid) | !is.na(cid) | !is.na(gid)) %>%
  dplyr::group_by(actual_date, methodName, groupName) %>% 
  dplyr::summarise(fids = list(as.character(fid)),
                   cids = list(as.character(cid)),
                   gids = list(as.character(gid)))  

print('對網站感興趣的程度做分群 finished')


################################################################################
##################################### 依照對單一車種感興趣的程度做分群

##### 取每個 ID 車種欄位的最大值為感興趣的車種。若最大值重複，則隨意取一種車種為此 ID 感興趣的車種
km.final_cluster_3 <- data.frame(
  clientId = cluster_df$clientId,
  cluster=colnames(cluster_df[,-1])[max.col(cluster_df[,-1],ties.method="random")]
)
km.final_cluster_3$cluster = paste('對「',km.final_cluster_3$cluster,'」感興趣的族群',sep='')

##### 產生最終要輸出的資料
z = mydata3 %>% filter(clientId %in% km.final_cluster_3$clientId) %>% select(clientId, fid, cid, gid, actual_date)
z = merge(z, km.final_cluster_3, by ='clientId', all.x = TRUE)
z$methodName<-'product intent clustering'
colnames(z)[which(names(z) == "cluster")] <- "groupName"
z<- z %>% distinct %>% filter(!is.na(fid) | !is.na(cid) | !is.na(gid)) %>%
  dplyr::group_by(actual_date, methodName, groupName) %>% 
  dplyr::summarise(fids = list(as.character(fid)),
                   cids = list(as.character(cid)),
                   gids = list(as.character(gid)))  
print('依照對單一車種感興趣的程度做分群 finished')


################################################################################
##################################### 輸出最終要拿來 post 到 dmp 介面的 json 檔案

#### output today's result
x_today=x %>% filter(actual_date == Sys.Date()-1)
for(i in 1:dim(x_today)[1]){
  final_x<- jsonlite::toJSON(x_today[i,c('methodName','groupName','fids','cids','gids')], pretty=TRUE) # a better solution
  write(final_x, file=paste('./_cluster_data/',Sys.Date(),'/',Sys.Date()-1,'_cluster_CarInterest_',Sys.Date()-1,'_',i,'.json',sep=''))
}

y_today=y %>% filter(actual_date == Sys.Date()-1)
for(i in 1:dim(y_today)[1]){
  final_y<- jsonlite::toJSON(y_today[i,c('methodName','groupName','fids','cids','gids')], pretty=TRUE) # a better solution
  write(final_y, file=paste('./_cluster_data/',Sys.Date(),'/',Sys.Date()-1,'_cluster_WebIntent_',Sys.Date()-1,'_',i,'.json',sep=''))
}


z_today=z %>% filter(actual_date == Sys.Date()-1)
for(i in 1:dim(z_today)[1]){
  final_z<- jsonlite::toJSON(z_today[i,c('methodName','groupName','fids','cids','gids')], pretty=TRUE) # a better solution
  write(final_z, file=paste('./_cluster_data/',Sys.Date(),'/',Sys.Date()-1,'_cluster_CarInterest_Specific_',Sys.Date()-1,'_',i,'.json',sep=''))
}


system("say Just finished!")
allend<-Sys.time()
print(paste('cluster.R end:',Sys.time()))
difftime(allend, allstart, Asia/Taipei,units = "mins")

#### output recent days's result
# x_today=x %>% filter(actual_date >= Sys.Date()-6)
# for(date in as.character(unique(x_today$actual_date))){
#   x_temp= x_today %>% filter(actual_date == date)
#   for(i in 1:dim(x_temp)[1]){
#     final_x<- jsonlite::toJSON(x_temp[i,c('methodName','groupName','fids','cids','gids')], pretty=TRUE) # a better solution
#     write(final_x, file=paste('./_cluster_data/',Sys.Date(),'/',Sys.Date()-1,'_cluster_CarInterest_',date,'_',i,'.json',sep=''))
#   }
# }
# 
# y_today=y %>% filter(actual_date >= Sys.Date()-3)
# for(date in as.character(unique(y_today$actual_date))){
#   y_temp= y_today %>% filter(actual_date == date)
#   for(i in 1:dim(y_temp)[1]){
#     final_y<- jsonlite::toJSON(y_temp[i,c('methodName','groupName','fids','cids','gids')], pretty=TRUE) # a better solution
#     write(final_y, file=paste('./_cluster_data/',Sys.Date(),'/',Sys.Date()-1,'_cluster_WebIntent_',date,'_',i,'.json',sep=''))
#   }
# }
# 
# z_today=z %>% filter(actual_date >= Sys.Date()-3)
# for(date in as.character(unique(z_today$actual_date))){
#   z_temp= z_today %>% filter(actual_date == date)
#   for(i in 1:dim(z_temp)[1]){
#     final_z<- jsonlite::toJSON(z_temp[i,c('methodName','groupName','fids','cids','gids')], pretty=TRUE) # a better solution
#     write(final_z, file=paste('./_cluster_data/',Sys.Date(),'/',Sys.Date()-1,'_cluster_CarInterest_Specific_',date,'_',i,'.json',sep=''))
#   }
# }

#### output all
# 
# for(date in as.character(unique(x$actual_date))){
#   x_temp= x %>% filter(actual_date == date)
#   for(i in 1:dim(x_temp)[1]){
#     final_x<- jsonlite::toJSON(x_temp[i,c('methodName','groupName','fids','cids','gids')], pretty=TRUE) # a better solution
#     write(final_x, file=paste('./_cluster_data/',Sys.Date(),'/',Sys.Date()-1,'_cluster_CarInterest_',date,'_',i,'.json',sep=''))
#   }
# }
# 
# for(date in as.character(unique(y$actual_date))){
#   y_temp= y %>% filter(actual_date == date)
#   for(i in 1:dim(y_temp)[1]){
#     final_y<- jsonlite::toJSON(y_temp[i,c('methodName','groupName','fids','cids','gids')], pretty=TRUE) # a better solution
#     write(final_y, file=paste('./_cluster_data/',Sys.Date(),'/',Sys.Date()-1,'_cluster_WebIntent_',date,'_',i,'.json',sep=''))
#   }
# }
# 
# for(date in as.character(unique(z$actual_date))){
#   z_temp= z %>% filter(actual_date == date)
#   for(i in 1:dim(z_temp)[1]){
#     final_z<- jsonlite::toJSON(z_temp[i,c('methodName','groupName','fids','cids','gids')], pretty=TRUE) # a better solution
#     write(final_z, file=paste('./_cluster_data/',Sys.Date(),'/',Sys.Date()-1,'_cluster_CarInterest_Specific_',date,'_',i,'.json',sep=''))
#   }
# }



################################################################################################ Memo


# submit_id = mydata2 %>% filter(Event =='TestDrive_Submit_Click') %>% select(gid) %>% filter(!is.na(gid))
# submit_id<-as.character(submit_id$gid)
# write.csv(submit_id, file='submit_id.txt', row.names = FALSE)
# table(submit_id$actual_date)
# 



#####create final_nisssan.csv file
# myjson_new<-jsonlite::fromJSON('_data_new/nissan_com_tw.json', flatten = TRUE)
# myjson_new<-my_data_manipulate(myjson_new)
# myjson_new<-as.data.table(myjson_new)
# myjson_new$clientId <- myjson_new$gid
# myjson_new$clientId <- ifelse(is.na(myjson_new$clientId), myjson_new$fid, myjson_new$clientId)
# myjson_new$clientId <- ifelse(is.na(myjson_new$clientId), myjson_new$cid, myjson_new$clientId)
# 
# myjson_old<-jsonlite::fromJSON('_data_old/nissan_com_tw.json', flatten = TRUE)
# myjson_old<-my_data_manipulate(myjson_old)
# myjson_old<-as.data.table(myjson_old)
# myjson_old$clientId <- myjson_old$cid
# 
# mydata2 <- rbind.fill(myjson_new,myjson_old)
# write.csv(mydata2, file='final_nissan.csv')





# cluster_df.scaled<-my_normalize(cluster_df)
# 
# #######################################remove na rows and rows become na in type_cluster_df
# if(any(is.na(cluster_df.scaled$V1))){
#   cluster_df<-cluster_df[-which(is.na(cluster_df.scaled$V1)),]
#   cluster_df.scaled<-na.omit(cluster_df.scaled)
# }
# 
# kmeans.km_3<- clara(cluster_df.scaled, 14,samples=100,sampsize=500, pamLike=TRUE)  
# km.cluster_df.scaled.cluster<-my_cluster_df(cluster_df, cluster_df.scaled, kmeans.km_3)
# 
# ############### filtering the columns, which the max of it are larger than 0.7
# km.cluster_df.scaled.cluster.filter<-my_cluster_filter(km.cluster_df.scaled.cluster, 0.7)
# 
# ############### merge back the cluster and filter out the clientid that does show up for more than 90 days.
# ############### For the user with only one event, we only keep them for 7 days
# #km.final_cluster_2<-my_cluster_merge_back(original_df=mydata, my_reshape=type_cluster_df, km.object=kmeans.km_2, 1000, 1000)
# km.final_cluster_3<-my_cluster_merge_back(my_reshape=cluster_df, km.object=kmeans.km_3)
# cluster_name<-cluster_name_table(km.cluster_df.scaled.cluster.filter,0.7)
# km.final_cluster_3<-my_cluster_name(km.final_cluster_3,cluster_name)
# 
# fix_cluster_name = c('對「370Z」感興趣的族群','對「ALL NEW LIVINA」感興趣的族群','對「ALL NEW TEANA」感興趣的族群',
#                      '對「BIG TIIDA & TIIDA」感興趣的族群','對「BIG TIIDA Turbo」感興趣的族群','對「BIG TIIDA」感興趣的族群',
#                      '對「GT-R」感興趣的族群','對「JUKE」感興趣的族群','對「MURANO」感興趣的族群','對「NEW MARCH」感興趣的族群',
#                      '對「SENTRA aero」感興趣的族群','對「SUPER SENTRA」感興趣的族群','對「TIIDA」感興趣的族群','對「X-TRAIL」感興趣的族群')
# km.final_cluster_3<- km.final_cluster_3 %>% filter(cluster %in% fix_cluster_name)
# 





