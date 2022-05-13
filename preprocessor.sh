#!/bin/sh

# Splitting occurence.csv into several smaller, 1GB, files.
echo "Splitting files."
split -b 1000000000 Data/occurence.csv Data/occurrence_

for i in Data/*_*; do
	filename=$i".csv"
	touch $filename
	echo $(head -n1 Data/occurence.csv) > $filename
	cat $i >> $filename
	rm $i
done

echo "Reducing file sizes."
for i in Data/*_*; do
	Rscript preprocessor.R $i Data/
done

# Flushing the existing DB
redis-cli FLUSHDB

rm logs/redux_log.txt

echo "Uploading part."
for i in Data/*_*; do
	Rscript redis_uploader.R $i >> logs/redux_log.txt
done

# Setting the env. var to be used to store the data for the app
if [ $(cat ~/.Rprofile | grep "Sys.setenv(SHINY_DATA_PATH" | wc -l) -eq 0 ]; then 
	 echo "Sys.setenv(SHINY_DATA_PATH=\"'/home/user/shiny_data'\")" >> ~/.Rprofile
fi
