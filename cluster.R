#http://new.nissan.com.tw/nissan
print(paste('cluster.R start:',Sys.time()))
allstart<-Sys.time()

setwd('/home/ubuntu/johnny/cluster_calculate/')
eval(parse("sop_function.R", encoding="UTF-8"))
eval(parse("cluster_function.R", encoding="UTF-8"))
Sys.setlocale("LC_ALL", "UTF-8")



mydata<-read.csv('final_nissan.csv',colClasses=c(rep("character",25)))
mydata$X<-NULL
mydata<-set_column_type(mydata)

myjson<-jsonlite::fromJSON('_data/nissan_com_tw.json', flatten = TRUE)
myjson<-my_data_manipulate(myjson)
myjson<-remove_id(myjson, c('1705117656.1467707925','1957040293.1471416113'))

mydata<-rbind.fill(mydata,myjson)

mydata<-my_session5(mydata)
print('mydata finished')
write.csv(mydata, file='final_nissan.csv')

################################################################################依照汽車型號做分群car
#######################################產生clustering 用的data frame
##############################1 * Event: page car、、page car停留時間
cluster_df_1<-my_reshape2(mydata, filter_argument = "Event=='Page_Car'", reshape_column = 'car', multiple_factor=1)

##############################3 * 下載型錄
cluster_df_3<-my_reshape2(mydata, filter_argument = "Event=='Click_Car_DownLoad'", reshape_column = 'car', multiple_factor=3)

##############################3 * 觀看詳細規格表“Page_Car_Spec”, type
mydata2 <- mydata %>% filter(Event=='Page_Car_Spec') 
mydata2$car[is.na(mydata2$car)] <- mydata2$type[is.na(mydata2$car)]
cluster_df_4<-my_reshape2(mydata2, filter_argument = "Event=='Page_Car_Spec'", reshape_column = 'car', multiple_factor=3)

##############################5 * 購車試算, car
cluster_df_5<-my_reshape2(mydata, filter_argument = "type=='購車試算_試算結果'", reshape_column = 'car', multiple_factor=5)

##############################5 * Click_Car_Build_2_Choose, car
cluster_df_6<-my_reshape2(mydata, filter_argument = "Event=='Click_Car_Build_2_Choose'", reshape_column = 'car', multiple_factor=5)

##############################8 * 選擇試乘 Page_TestDrive_PC_Chosen, car
cluster_df_7<-my_reshape2(mydata, filter_argument = "Event=='Page_TestDrive_PC_Chosen'", reshape_column = 'car', multiple_factor=8)

##############################10 * 預約試乘
cluster_df_8<-my_reshape2(mydata, filter_argument = "Event=='TestDrive_Submit_Click'", reshape_column = 'car', multiple_factor=10)

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




################################################################################依照汽車做分群type
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

#final_x<- jsonlite::toJSON(x, pretty=TRUE) # a better solution
#write(final_x, file=paste('./_cluster_data/',Sys.Date(),'_cluster_CarInterest.json',sep=''))

#final_y_1<- jsonlite::toJSON(y_1, pretty=TRUE) # a better solution
#write(final_y_1, file=paste('./_cluster_data/',Sys.Date(),'_cluster_WebIntent_1.json',sep=''))

#final_z<- jsonlite::toJSON(z, pretty=TRUE) # a better solution
#write(final_z, file=paste('./_cluster_data/',Sys.Date(),'_cluster_CarInterest_Specific.json',sep=''))


allend<-Sys.time()
print(paste('cluster.R end:',Sys.time()))
difftime(allend, allstart, Asia/Taipei,units = "mins")



