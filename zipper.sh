#!/bin/bash

mkdir -p ./package

cp -r core/src/main/scala/* package/
cp -r riddles.io/src/main/scala/* package/


skeleton1="package com.hnrklssn.fourinarow.riddles

/**
 * Generated by zipper.sh on `date --iso-8601`.
 */
object WeightsConfigContainer {
  val weights: Seq[String] = Seq("

skeleton2=")
}"
acc=$skeleton1
first=true
for file in $1/*
do
    #echo "$file"
    temp=$(<"$file")
    if $first
    then
        acc="$acc \"\"\"$temp\"\"\""
        first=false
        echo "first"
    else
        acc="$acc , \"\"\"$temp\"\"\""
    fi
done

acc="$acc $skeleton2"

echo "$acc" > package/com/hnrklssn/fourinarow/riddles/WeightsConfigContainer.scala

#curr=`pwd`
#cd ./src/main/scala
#while read path
#do
#    cp --parents $path $curr/package
#done < $curr/zip_include.txt

#cd $curr

#curl -L -o coursier https://git.io/vgvpD && chmod +x coursier

#mkdir -p package/libs
#cd package/libs

#../../coursier fetch com.zenecture:neuroflow-core_2.12:1.1.3 --sources --scala-version 2.12 | xargs -n1 jar xf

#cd package

#cp -r ./include-libs/* ./package/
rm ~/testbot.zip

cd package/com/hnrklssn
zip -r ~/testbot.zip ./fourinarow