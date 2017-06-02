post_url=https://sgcgm4pwu5.execute-api.us-west-2.amazonaws.com/prod/clusters-clients-inserting/
location=/Users/JohnnyChiu/Desktop/MyFiles/2017_projects/coding/0217_nissan_interest_generator/_cluster_data
cd $location

# 把每個cluster檔案加上前後需要的資訊，一個一個post回資料庫
directory_date=$(date -v-8d '+%Y-%m-%d')
filedate=$(date -v-9d '+%Y-%m-%d')
echo $filedate
find $location/$directory_date -name "*${filedate}*" | while read filename
do
    echo $filename
 	#echo "}" >> $filename
    #echo '{"domain": "nissan","data":' | /bin/cat - $filename > temp && /bin/mv temp $filename
	/usr/bin/curl --header "X-Api-Key: rb7I74sgPd5gCu7ezTw4I1enuZ0Dhimv4ReJDNt8"  -d @$filename $post_url?t=$RANDOM	
	/bin/sleep 300s
done
# 把今天的log檔移到新的位置; 把nissan_com_tw.json刪掉
file_date=$(date -v-1d '+%Y%m%d')
file_name=new_nissan_com_tw_"$file_date"_060.log
root_location=/Users/JohnnyChiu/Desktop/MyFiles/2017_projects/coding/0217_nissan_interest_generator/
cd $root_location
# mv ./_data/"$file_name" ./_data_new/new_nissan_com_tw  
rm ./_data/nissan_flat_$(date '+%y%m%d')/nissan_com_tw.json
# mv ./_data/nissan_flat_$(date '+%y%m%d') ./_data_new

#dev 5QzdQJXBHa8iWIVSRsDGG14Lvt63x8bm1BXZA3kh
#prod rb7I74sgPd5gCu7ezTw4I1enuZ0Dhimv4ReJDNt8


