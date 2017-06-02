#- 在_data裡面新建立今天日期的的檔案夾
mkdir ./_data/nissan_flat_$(date '+%y%m%d')
location=/Users/JohnnyChiu/Desktop/MyFiles/2017_projects/coding/0217_nissan_interest_generator/_data/nissan_flat_$(date '+%y%m%d')

#- 在_cluster_data裡面新建立今天日期的檔案夾
mkdir ./_cluster_data/$(date '+%Y-%m-%d')

#- 把當日資料(前一天日期的檔案)抓到_data裡頭當日的資料夾
file_date=$(date -v-1d '+%Y%m%d')
new_nissan_com_tw_file_name=new_nissan_com_tw_"$file_date"_060.log
event_nissan_com_tw_file_name=event_nissan_com_tw_"$file_date"_060.log
nissan_bub3_com_file_name=nissan_bub3_com_"$file_date"_060.log
nissan_iux_com_tw_file_name=nissan_iux_com_tw_"$file_date"_060.log
nissan_svc_iux_com_tw_file_name=nissan-svc_iux_com_tw_"$file_date"_060.log
service_nissan_com_tw_file_name=service_nissan_com_tw_"$file_date"_060.log
www_nissan_com_tw_file_name=www_nissan_com_tw_"$file_date"_060.log
nissan_unidyna_com_file_name=nissan_unidyna_com_"$file_date"_060.log

scp -i ~/.ssh/DA-footprint johnny@footprints.urad.com.tw:/home/ec2-user/tracker/data/new_nissan_com_tw/"$new_nissan_com_tw_file_name" $location
scp -i ~/.ssh/DA-footprint johnny@footprints.urad.com.tw:/home/ec2-user/tracker/data/event_nissan_com_tw/"$event_nissan_com_tw_file_name" $location
scp -i ~/.ssh/DA-footprint johnny@footprints.urad.com.tw:/home/ec2-user/tracker/data/nissan_bub3_com/"$nissan_bub3_com_file_name" $location
scp -i ~/.ssh/DA-footprint johnny@footprints.urad.com.tw:/home/ec2-user/tracker/data/nissan_iux_com_tw/"$nissan_iux_com_tw_file_name" $location
scp -i ~/.ssh/DA-footprint johnny@footprints.urad.com.tw:/home/ec2-user/tracker/data/nissan-svc_iux_com_tw/"$nissan_svc_iux_com_tw_file_name" $location
scp -i ~/.ssh/DA-footprint johnny@footprints.urad.com.tw:/home/ec2-user/tracker/data/service_nissan_com_tw/"$service_nissan_com_tw_file_name" $location
scp -i ~/.ssh/DA-footprint johnny@footprints.urad.com.tw:/home/ec2-user/tracker/data/www_nissan_com_tw/"$www_nissan_com_tw_file_name" $location
scp -i ~/.ssh/DA-footprint johnny@footprints.urad.com.tw:/home/ec2-user/tracker/data/nissan_unidyna_com/"$nissan_unidyna_com_file_name" $location


#- jq 處理：cat xxx.log | … > nissan_data.json
cd $location
cat * | grep -v '[0-9]{' | grep -v '[a-zA-Z]{' | grep -v '\"utm{"\c' | grep -v '\"{' | /usr/local/bin/jq  '{cid: .session.cid, fid: .session.fid, gid: .session.gid, screenResolution: .general.sr, language: .general.la, Event: .gtm.event, car: .gtm.car, type: .gtm.type, URL_Current: .gtm.urlCurrent, URL_Referrer: .gtm.urlReferrer, UTM_Source: .gtm.utm.utmSource, UTM_Medium: .gtm.utm.utmMedium, UTM_Term: .gtm.utm.utmTerm, UTM_Content: .gtm.utm.utmContent, UTM_Compaign: .gtm.utm.utmCompaign, userAgent: .general.ua, IP: .general.ip, Time: .general.ts}' | /usr/local/bin/jq -s . -c > nissan_com_tw.json