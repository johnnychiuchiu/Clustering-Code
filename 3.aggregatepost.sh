#file=$(date '+%Y-%m-%d')_cluster.json
post_url=https://sgcgm4pwu5.execute-api.us-west-2.amazonaws.com/dev/clusters-clients-inserting/
location=/home/ubuntu/johnny/cluster_calculate/_cluster_data/


#echo $file
#echo "}" >> $location/$file
#echo '{"domain": "nissan","data":' | /bin/cat - $location/$file > temp && /bin/mv temp $location/$file
#/usr/bin/curl -d @$location/$file $post_url

filedate=$(date '+%Y-%m-%d')
echo $filedate
find $location -name "*${filedate}*" | while read filename
do
        echo $filename
 	echo "}" >> $filename
        echo '{"domain": "nissan","data":' | /bin/cat - $filename > temp && /bin/mv temp $filename
	/usr/bin/curl --header "X-Api-Key: 5QzdQJXBHa8iWIVSRsDGG14Lvt63x8bm1BXZA3kh"  -d @$filename $post_url
done


#file_CarInterest=$(date '+%Y-%m-%d')_cluster_CarInterest.json
#echo $file_CarInterest
#echo "}" >> $location/$file_CarInterest
#echo '{"domain": "nissan","data":' | /bin/cat - $location/$file_CarInterest > temp && /bin/mv temp $location/$file_CarInterest
#/usr/bin/curl -d @$location/$file_CarInterest $post_url


#file_WebIntent_1=$(date '+%Y-%m-%d')_cluster_WebIntent_1.json
#echo $file_WebIntent_1
#echo "}" >> $location/$file_WebIntent_1
#echo '{"domain": "nissan","data":' | /bin/cat - $location/$file_WebIntent_1 > temp && /bin/mv temp $location/$file_WebIntent_1
#/usr/bin/curl -d @$location/$file_WebIntent_1 $post_url

#file_WebIntent_2=$(date '+%Y-%m-%d')_cluster_WebIntent_2.json
#echo $file_WebIntent_2
#echo "}" >> $location/$file_WebIntent_2
#echo '{"domain": "nissan","data":' | /bin/cat - $location/$file_WebIntent_2 > temp && /bin/mv temp $location/$file_WebIntent_2
#/usr/bin/curl -d @$location/$file_WebIntent_2 $post_url

#file_WebIntent_3=$(date '+%Y-%m-%d')_cluster_WebIntent_3.json
#echo $file_WebIntent_3
#echo "}" >> $location/$file_WebIntent_3
#echo '{"domain": "nissan","data":' | /bin/cat - $location/$file_WebIntent_3 > temp && /bin/mv temp $location/$file_WebIntent_3
#/usr/bin/curl -d @$location/$file_WebIntent_3 $post_url

#file_CarInterest_Specific=$(date '+%Y-%m-%d')_cluster_CarInterest_Specific.json
#echo $file_CarInterest_Specific
#echo "}" >> $location/$file_CarInterest_Specific
#echo '{"domain": "nissan","data":' | /bin/cat - $location/$file_CarInterest_Specific > temp && /bin/mv temp $location/$file_CarInterest_Specific
#/usr/bin/curl -d @$location/$file_CarInterest_Specific $post_url


data_location=/home/ubuntu/johnny/cluster_calculate/_data/
/bin/rm -rf $data_location/*

date
