# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#报错需要修改
read_herb_NEG <- function(file_raw_herb_NEG){
  rm(list = ls())
  library(tidyverse)
  library(readxl)
  library(writexl)
  filesNEG = list.files("file_raw_herb_NEG/", pattern = "xlsx",full.names = TRUE,)  #把所有的文件都进行读取
  filesNEG
  dir = grep('\\.xlsx',filesNEG,value = TRUE)
  n = length(dir)
  merge.data = read_excel(dir[1])
  for (i in 2:n){
    new.data = read_excel(dir[i])
    merge.data = rbind(merge.data,new.data)
  }
  #write.csv(merge.data,file = './merge_weiquchong.csv',row.names = FALSE)
  all_ion <- data.frame(merge.data)
  #上述操作是将所有的离子写入merge_weiquchong文件中
  all_ion$Area[all_ion$Area == 'N/A'] <- 0   #将N/A值变成0
  all_ion$Retention.Time[all_ion$Retention.Time == 'N/A'] <- 0
  all_ion_Area_more_than1000 <- all_ion
  #这边进行了修改,如果报错,就要重新定义all_ion_more_than1000
  head(all_ion_Area_more_than1000) #查看前几行
  class(all_ion_Area_more_than1000) #查看数据类型
  sapply(all_ion_Area_more_than1000,typeof)  #查看每一列的数据类型,发现Area和time是字符型数据
  all_ion_Area_more_than1000[,3:4] <- lapply(all_ion_Area_more_than1000[,3:4],as.numeric) #把Area和time转化成数值型数据
  all_ion_Area_more_than1000 <- subset(all_ion_Area_more_than1000,Area >= 1000)
  sapply(all_ion_Area_more_than1000,typeof)  #再次查看每一列的数据类型
  processed <- all_ion_Area_more_than1000 %>%
    select(Sample.Name,Area,Retention.Time,Precursor.Mass,Retention.Time,Precursor.Mass) %>%
    mutate(mz = round(Precursor.Mass * 0.5,2),time = round(Retention.Time * 1.5),0)  #对保留时间和母离子进行处理
  processed_2 <- processed%>%
    select(Sample.Name,Area,mz,time,Retention.Time,Precursor.Mass)%>%
    mutate(mz_time = paste(mz,time,sep = "_"))  #生成新的列


  processed_3 <- processed_2 %>%
    group_by(mz_time) %>%
    mutate(n=n()) %>%
    filter(n==1)%>%
    select(-n)
  processed_4 <- split(processed_3,processed_3$Sample.Name)
  setwd('./Character_herb_NEG')
  out_fileName <- sapply(names(processed_4),function(x){
    paste(x,".csv",sep="")
  })
  for(i in 1:length(processed_4)){
    write.csv(processed_4[[i]],file=out_fileName[i],row.names = F)
  }
  processed_4

  getwd()
  setwd("..")
  filesNEG_pro = list.files("Character_herb_NEG/", pattern = "csv",full.names = TRUE,)  #把所有的文件都进行读取
  filesNEG_pro


  dir_2 = grep('\\csv',filesNEG_pro,value = TRUE)
  dir_2
  new.data_ion_count_after = read.csv(dir_2[1])
  Sample.Name = unique(new.data_ion_count_after[,1])
  Ion_Count_after = nrow(new.data_ion_count_after)
  Ion_Count_before = Ion_Count_after
  Matching_rate = Ion_Count_after / Ion_Count_before
  Sum_matching2 <- data.frame(Sample.Name,Ion_Count_after,Ion_Count_before,Matching_rate)

  for (i in 2:length(dir_2)){
    new.data_ion_count_after = read.csv(dir_2[i])
    Sample.Name = unique(new.data_ion_count_after[,1])
    Ion_Count_after = nrow(new.data_ion_count_after)
    Ion_Count_before = Ion_Count_after
    Matching_rate = Ion_Count_after / Ion_Count_before
    Sum_matching <- data.frame(Sample.Name,Ion_Count_after,Ion_Count_before,Matching_rate)
    Sum_matching2 <- rbind(Sum_matching2,Sum_matching)
  }
  getwd()
  filesNEG2 = list.files("file_raw_herb_NEG/", pattern = "xlsx",full.names = TRUE,)  #把所有的文件都进行读取
  filesNEG2
  dir2 = grep('\\.xlsx',filesNEG2,value = TRUE)
  dir2

  all_ion2 <- read_excel(dir2[1])
  all_ion2$Area[all_ion2$Area == 'N/A'] <- 0
  all_ion_Area_more_than1000_2 <- all_ion2
  all_ion_Area_more_than1000_2[,3:4] <- lapply(all_ion_Area_more_than1000_2[,3:4],as.numeric)
  all_ion_Area_more_than1000_2 <- subset(all_ion_Area_more_than1000_2,all_ion_Area_more_than1000_2$Area >= 1000)
  Sample.Name2 = unique(all_ion_Area_more_than1000_2[,1])
  Ion_Count_before = nrow(all_ion_Area_more_than1000_2)
  Sum_matching4 <- data.frame(Sample.Name2,Ion_Count_before)
  for (i in 2:length(dir2)){
    all_ion2 <- read_excel(dir2[i])
    all_ion2$Area[all_ion2$Area == 'N/A'] <- 0
    all_ion_Area_more_than1000_2 <- all_ion2
    all_ion_Area_more_than1000_2[,3:4] <- lapply(all_ion_Area_more_than1000_2[,3:4],as.numeric)
    all_ion_Area_more_than1000_2 <- subset(all_ion_Area_more_than1000_2,all_ion_Area_more_than1000_2$Area >= 1000)
    Sample.Name2 = unique(all_ion_Area_more_than1000_2[,1])
    Ion_Count_before = nrow(all_ion_Area_more_than1000_2)
    Sum_matching3 <- data.frame(Sample.Name2,Ion_Count_before)
    Sum_matching4 <- rbind(Sum_matching4,Sum_matching3)

  }
  Sum_matching4


  NEG_Matching_Rate <- data.frame(Sum_matching4$Sample.Name,Sum_matching4$Ion_Count_before,Sum_matching2$Ion_Count_after,matching = Sum_matching2$Ion_Count_after/Sum_matching4$Ion_Count_before)
  NEG_Matching_Rate
  write.csv(NEG_Matching_Rate,file = "./herb_matching_rate_NEG.csv",row.names = FALSE)
}
read_herb_NEG(file_raw_herb_NEG = file_raw_herb_NEG)


#############################################################################################################


Character_herb_NEG_100 <- function(Character_herb_NEG){
  filesNEG = list.files("Character_herb_NEG/", pattern = "csv",full.names = TRUE,)  #把所有的文件都进行读取
  filesNEG
  dir = grep('\\.csv',filesNEG,value = TRUE)
  n = length(dir)
  merge.data = read.csv(dir[1])
  merge.data$Area <- sort(merge.data$Area,decreasing = TRUE)
  merge.data <- merge.data[1:100,]
  for (i in 2:n){
    new.data = read.csv(dir[i])
    new.data$Area <- sort(new.data$Area,decreasing = TRUE)
    new.data <- new.data[1:100,]
    merge.data = rbind(merge.data,new.data)
  }
  write.csv(merge.data,file = './Character_herb_NEG_100.csv',row.names = FALSE)
}
Character_herb_NEG_100(Character_herb_NEG = Character_herb_NEG4)
test <- read.csv("Character_herb_NEG_100.csv")
unique(test$Sample.Name)
#这边看看列名有个NA是咋回事

########################################################################################

Sample_match <- function(Sample_dir_NEG){
  rm(list = ls())
  filesNEG = list.files("./Sample_dir_NEG", pattern = "xlsx",full.names = TRUE,)  #把所有的文件都进行读取
  filesNEG
  dir = grep('\\.xlsx',filesNEG,value = TRUE)
  n = length(dir)
  n
  merge.data = read_excel(dir[1])
  dir.create('./Sample_dir_NEG/Result')
  write.csv(merge.data, file = "./Sample_dir_NEG/Result/raw_data.csv")
  head(merge.data)
  all_ion <- read.csv("./Sample_dir_NEG/Result/raw_data.csv")
  head(all_ion)
  all_ion <- select(all_ion,-X)  #这边发现了多了一列X,所以把X列进行了删除操作
  head(all_ion)

  all_ion$Area[all_ion$Area == 'N/A'] <- 0  #要先去除N/A再转化
  all_ion$Retention.Time[all_ion$Retention.Time == 'N/A'] <- 0
  head(all_ion)
  all_ion[,3:4] <- lapply(all_ion[,3:4],as.numeric)
  all_ion <- subset(all_ion,Area >= 1000)
  #  colnames(merge.data)  这个问题解决了,可以先导出csv格式文件,然后再读取,从而列名就有点了
  all_ion <- all_ion %>%
    select(Sample.Name,Area,Retention.Time,Precursor.Mass,Retention.Time,Precursor.Mass) %>%
    mutate(mz = round(Precursor.Mass * 0.5,2),time = round(Retention.Time * 1.5),0)  #对保留时间和母离子进行处理
  all_ion <- all_ion %>%
    select(Sample.Name,Area,mz,time,Retention.Time,Precursor.Mass)%>%
    mutate(mz_time = paste(mz,time,sep = "_"))  #生成新的列
  all_ion
  getwd()

  all_ion <- all_ion %>%
    group_by(mz_time) %>%
    mutate(n=n()) %>%
    filter(n==1)%>%
    select(-n)
  write.csv(all_ion,file = './Sample_dir_NEG/Result/Sample_all_ion_noNA_norepeat.csv')

  all_ion_yc = read.csv('Character_herb_NEG_100.csv')
  head(all_ion_yc)
  all_ion_yc$Area[all_ion_yc$Area == 'N/A'] <- 0   #将N/A值变成0
  all_ion_yc$Retention.Time[all_ion_yc$Retention.Time == 'N/A'] <- 0
  all_ion_yc[,3:4] <- lapply(all_ion_yc[,3:4],as.numeric)
  all_ion_yc <- subset(all_ion_yc,Area >= 1000)
  #  colnames(merge.data)  这个问题解决了,可以先导出csv格式文件,然后再读取,从而列名就有点了
  all_ion_yc <- all_ion_yc %>%
    select(Sample.Name,Area,Retention.Time,Precursor.Mass,Retention.Time,Precursor.Mass) %>%
    mutate(mz = round(Precursor.Mass * 0.5,2),time = round(Retention.Time * 1.5),0)  #对保留时间和母离子进行处理
  all_ion_yc <- all_ion_yc %>%
    select(Sample.Name,Area,mz,time,Retention.Time,Precursor.Mass)%>%
    mutate(mz_time = paste(mz,time,sep = "_"))  #生成新的列
  head(all_ion_yc)
  processed_1 <- all_ion_yc %>%
    group_by(mz_time) %>%
    mutate(n=n()) %>%
    filter(n==1)%>%
    select(-n)
  write.csv(processed_1,file = './Sample_dir_NEG/all_herb_no_repeat.csv')
  processed_1 <- read.csv('./Sample_dir_NEG/all_herb_no_repeat.csv')


  processed_2 <- split(processed_1,processed_1$Sample.Name)
  processed_3 <- select(processed_2[[1]],-X)
  processed_3 <- rbind(processed_3,all_ion)

  duplicate_name = processed_3 %>%
    group_by(mz_time)%>%
    summarise(freq = n()) %>%
    filter(freq > 1) %>%
    select(mz_time)
  duplicate_name
  duplicate_ion <- processed_3[processed_3$mz_time %in% duplicate_name$mz_time,]
  duplicate_ion
  out_fileName <- sapply(names(processed_2),function(x){
    paste(x,".csv",sep="")
  })
  length(processed_2)
  out_fileName  #for循环记得1:length()
  dir.create('./Sample_dir_NEG/Result/Sample_match_herbs_NEG')
  setwd('./Sample_dir_NEG/Result/Sample_match_herbs_NEG')

  for (i in 1:length(processed_2)) {
    processed_3 <- select(processed_2[[i]],-X)
    processed_3 <- rbind(processed_3,all_ion)
    duplicate_name = processed_3 %>%
      group_by(mz_time)%>%
      summarise(freq = n()) %>%
      filter(freq > 1) %>%
      select(mz_time)
    duplicate_name
    duplicate_ion <- processed_3[processed_3$mz_time %in% duplicate_name$mz_time,]
    write.csv(duplicate_ion,file = out_fileName[i],row.names = F)
  }
  setwd('..')
  setwd('..')
  setwd("..")
  yc_matching <- read.csv('./herb_matching_rate_NEG.csv')
  yc_matching

  filesNEG2 = list.files("./Sample_dir_NEG/Result/Sample_match_herbs_NEG", pattern = "csv",full.names = TRUE,)  #把所有的文件都进行读取
  filesNEG2
  dir2 = grep('\\.csv',filesNEG2,value = TRUE)
  n = length(dir2)
  merge.data2 = read.csv(dir2[1])
  Sample.Name2 = unique(merge.data2[,1])
  Ion_Count_match = (nrow(merge.data2))/2
  Sample.Name2 = Sample.Name2[1]
  Matching_rate <- data.frame(Sample.Name2,Ion_Count_match)
  for (i in 2:n){
    merge.data2 = read.csv(dir2[i])
    Sample.Name2 = unique(merge.data2[,1])
    Sample.Name2 = Sample.Name2[1]
    Ion_Count_match = (nrow(merge.data2))/2
    Matching_rate2 = data.frame(Sample.Name2,Ion_Count_match)
    Matching_rate = rbind(Matching_rate,Matching_rate2)
  }
  merge.data2
  head(yc_matching)
  Matching_rate3 = cbind(Matching_rate,yc_matching)
  Matching_rate3
  Matching_rate4 <- Matching_rate3 %>%
    select(Sample.Name2,Ion_Count_match,Sum_matching2.Ion_Count_after)%>%
    mutate(matching = Ion_Count_match / 100)
  lieming = read_excel('Name_herb_match.xlsx')
  Matching_rate5 <- merge(Matching_rate4,lieming)
  Matching_rate5 <- Matching_rate5[complete.cases(Matching_rate5),]
  Matching_rate5 <- Matching_rate5[order(Matching_rate5$matching,decreasing = TRUE),]
  write_excel_csv(Matching_rate5,"./Sample_dir_NEG/Result/Sample_herb_math_NEG.csv")
}
Sample_match(Sample_dir_NEG = Sample_dir_NEG)
