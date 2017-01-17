mydata3<-mydata2
mydata3$nchar<-nchar(mydata3$clientId)
mydata3<- mydata3 %>% filter(nchar==27)
mydata3$actual_date<-as.Date(mydata3$actual_time,'Asia/Taipei')


################################################################################依照汽車car
#######################################產生clustering 用的data frame
##############################1 * Event: page car、、page car停留時間
cluster_df_1<-my_reshape2(mydata3, filter_argument = "Event=='Page_Car'", reshape_column = 'car', multiple_factor=1)

##############################3 * 下載型錄
cluster_df_3<-my_reshape2(mydata3, filter_argument = "Event=='Click_Car_DownLoad'", reshape_column = 'car', multiple_factor=3)

##############################3 * 觀看詳細規格Page_Car_Spec”, type
mydata2 <- mydata3 %>% filter(Event=='Page_Car_Spec') 
mydata2$car[is.na(mydata2$car)] <- mydata2$type[is.na(mydata2$car)]
cluster_df_4<-my_reshape2(mydata2, filter_argument = "Event=='Page_Car_Spec'", reshape_column = 'car', multiple_factor=3)

##############################5 * 購車試算, car
cluster_df_5<-my_reshape2(mydata3, filter_argument = "type=='購車試算_試算結果'", reshape_column = 'car', multiple_factor=5)

##############################5 * Click_Car_Build_2_Choose, car
cluster_df_6<-my_reshape2(mydata3, filter_argument = "Event=='Click_Car_Build_2_Choose'", reshape_column = 'car', multiple_factor=5)

##############################8 * 選擇試乘 Page_TestDrive_PC_Chosen, car
cluster_df_7<-my_reshape2(mydata3, filter_argument = "Event=='Page_TestDrive_PC_Chosen'", reshape_column = 'car', multiple_factor=8)

##############################10 * 預約試乘
cluster_df_8<-my_reshape2(mydata3, filter_argument = "Event=='TestDrive_Submit_Click'", reshape_column = 'car', multiple_factor=10)

##############################合併
cluster_df<-plyr::rbind.fill(cluster_df_1,cluster_df_3,cluster_df_4,cluster_df_5,cluster_df_6,cluster_df_7,cluster_df_8)
cluster_df[is.na(cluster_df)] <- 0
cluster_df<-cluster_df %>% dplyr::group_by(clientId) %>% dplyr::summarise_each(funs(sum))

#######################################做一些欄位篩選，有的欄位不應該出現，如：ALL NEWティアナ
cluster_df <- cluster_df %>% dplyr::select(`clientId`, `370Z`, `ALL NEW LIVINA`, `ALL NEW TEANA`, `BIG TIIDA`, `BIG TIIDA Turbo`, `JUKE`, `MURANO`, `NEW MARCH`, `SENTRA aero`, `SUPER SENTRA`, `TIIDA`,`X-TRAIL`,`GT-R`)

print('cluster_df finished')

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










# 1. 對sentra aero有興趣  
list1_1<-km.final_cluster_3 %>% filter(cluster=="對「SENTRA aero」感興趣的族群")
list1<-as.character(list1_1$clientId)

write.csv(list1,file = "audience/nissan_sentra_aero.csv")

# 2. 對super sentra有興趣 
list2_1<-km.final_cluster_3 %>% filter(cluster=="對「SUPER SENTRA」感興趣的族群")
list2<-as.character(list2_1$clientId)

write.csv(list2,file = "audience/nissan_super_sentra.csv")

# 3. 對all new livina有興趣 
list3_1<-km.final_cluster_3 %>% filter(cluster=="對「ALL NEW LIVINA」感興趣的族群")
list3<-as.character(list3_1$clientId)

write.csv(list3,file = "audience/nissan_all_new_livina.csv")

# 4. 對x-trail有興趣 
list4_1<-km.final_cluster_3 %>% filter(cluster=="對「X-TRAIL」感興趣的族群")
list4<-as.character(list4_1$clientId)

write.csv(list4,file = "audience/nissan_x_trail.csv")

# 5. 對Nissan中、高度興趣  
list5_1<- mydata3 %>% group_by(clientId, session) %>% 
  dplyr::summarize(session_duration=as.numeric(difftime(max(actual_time), min(actual_time), Asia/Taipei,units = "secs")))
list5_2<- list5_1 %>%  group_by(clientId) %>% 
  dplyr::summarize(total_time=sum(session_duration)) %>% filter(total_time>=as.numeric(quantile(list8_2$total_time,0.75)))
list5<-as.character(list5_2$clientId)

write.csv(list5,file = "audience/nissan_mid_high_interest.csv")


# 6. 接觸時間30天以內  
list6_1<- mydata3 %>% filter(actual_date >= "2016-11-15") %>% group_by(clientId) %>% dplyr::summarise(count=n())
list6<- as.character(list6_1$clientId)

write.csv(list6,file = "audience/nissan_within30days.csv")

# 7. 進站次數超過3次以上  
list7_1<-mydata2 %>% group_by(clientId) %>% dplyr::summarise(max_session=max(session))
list7_1$nchar<- nchar(list7_1$clientId)
list7_2<-list7_1 %>% filter(max_session>3) %>% filter(nchar==27)
list7<-as.character(list7_2$clientId)
write.csv(list7,file = "audience/nissan_total_session_over3.csv")

# 8. 進站時間3分鐘以上
list8_1<-mydata2 %>% group_by(clientId, session) %>% 
  dplyr::summarize(session_duration=as.numeric(difftime(max(actual_time), min(actual_time), Asia/Taipei,units = "secs")))
list8_2<- list8_1 %>%  group_by(clientId) %>% 
  dplyr::summarize(total_time=sum(session_duration)) %>% 
  mutate(total_mins=total_time/60,total_hours=total_time/60/60)
list8_2$nchar<- nchar(list8_2$clientId)
list8_3<- list8_2 %>% filter(total_mins>3)
list8_3$nchar<- nchar(list8_3$clientId)
list8_3<- list8_3 %>% filter(nchar==27)

list8<-as.character(list8_3$clientId)
write.csv(list8,file = "8_total_mins_over3.csv")


