now=$(/bin/date --date="1 days ago" +%y%m%d)
url=https://footprints.urad.com.tw/data/nissan/nissan_"$now".tar.xz
file=nissan_"$now".tar.xz
/bin/echo $now
/bin/echo $url
/bin/echo $file
location=/home/ubuntu/johnny/cluster_calculate/_data/

cd $location

#/bin/rm -rf $location/*

/usr/bin/wget --user user --password uradhadoop $url -P $location
/bin/tar xJvf $file 
/usr/bin/find $location/. -type f -name "*.log" -exec /bin/cat {} \; |grep -v 'device\"{' | grep -v 'zh{' | /usr/bin/jq  '. | {clientId: .clientId, screenResolution: .screenResolution, viewportSize: .viewportSize, language: .language, Event: .uradTracker.event, car: .uradTracker.car, type: .uradTracker.type, URL_Current: .uradTracker.urlCurrent, URL_Referrer: .uradTracker.urlReferrer, Device: .uradTracker.device, UTM_Source: .uradTracker.utm.utmSource, UTM_Medium: .uradTracker.utm.utmMedium, UTM_Term: .uradTracker.utm.utmTerm, UTM_Content: .uradTracker.utm.utmContent, UTM_Compaign: .uradTracker.utm.utmCompaign, timeZone: .timeZone, userAgent: .userAgent, IP: .IP, Time: .Time}' | /usr/bin/jq -s . -c > nissan_com_tw.json
date
