library(dplyr)
library(data.table)
install.packages('openair')
library(openair)
library(lubridate)
library(rpart)
library(rattle)
library(forecast)
library(class)
library(caret)

total=bulk_imbalance_1_
weather=X2022_01_22_weather

#weather_v2=reshape(weather,idvar = c('date','hour','lat','lon'),timevar = "variable",direction = 'Wide')
#weather_v3=dcast(weather, date + hour + lat + lon ~ variable, value.var = "value")
weather_v4=dcast(weather, date + hour ~ lon+lat+variable, value.var = "value")
#weather_fin=weather_v4[25:26976,]
#weather_fin=as.data.table(weather_fin)
#total=as.data.table(total)




#weather_fin[,datenew:=c(date,hour)]


#weather_fin[,datex:=as.POSIXct(c(date,hour),'%y/%m/%d/%h')]

#dat[,tst:=ymd_hm(paste(datex,hour))]
#dat[,date:=date(tst)]
#dat[,hour:=hour(tst)]

main_data=merge(weather_v4,total,by=c('date','hour'))

main_data$date=as.POSIXct(paste(main_data$date, main_data$hour), format="%Y-%m-%d %H")
main_data=main_data[order(main_data$date),]
main_data$hour=NULL 
classes=main_data$system_direction
main_data$system_direction=NULL

#main_data=nafill(main_data,type = 'locf')
#main_data=as.data.table(main_data)

sample_size = floor(0.8*nrow(main_data))
set.seed(1752)

picked = sample(seq_len(nrow(main_data)),size = sample_size)
train_main =main_data[picked,]
test_main =main_data[-picked,]


model_raw=lm(net~.-date-upRegulationZeroCoded-upRegulationOneCoded-upRegulationTwoCoded-
               downRegulationZeroCoded-downRegulationOneCoded-downRegulationTwoCoded-upRegulationDelivered-
               downRegulationDelivered,data = train_main)
summary(model_raw)


prediction_raw=predict(model_raw,test_main)
dummy_pred_raw=vector()
prediction_matrix_raw=as.matrix(prediction_raw)

# 
# for(i in 1:length(prediction_raw)){
#   if(prediction_matrix_raw[i]>=50){
#     dummy_pred_raw=c(dummy_pred_raw,1)
#   }
#   if(prediction_matrix_raw[i]<=(-50)){
#     dummy_pred_raw=c(dummy_pred_raw,-1)
#   }
#   if(prediction_matrix_raw[i]<50 & prediction_matrix_raw[i]>(-50)){
#     dummy_pred_raw=c(dummy_pred_raw,0)
#   }
# }
# 
# dummy_naive=vector()
# naive_pred=test_1$lagged24
# 
# for(i in 1:length(naive_pred)){
#   if(naive_pred[i]>=50){
#     dummy_naive=c(dummy_naive,1)
#   }
#   if(naive_pred[i]<=(-50)){
#     dummy_naive=c(dummy_naive,-1)
#   }
#   if(naive_pred[i]<50 & naive_pred[i]>(-50)){
#     dummy_naive=c(dummy_naive,0)
#   }
# }
# 
# 
# 
# dummy_real=vector()
# reality=test_1$net
# 
# for(i in 1:length(reality)){
#   if(reality[i]>=50){
#     dummy_real=c(dummy_real,1)
#   }
#   if(reality[i]<=(-50)){
#     dummy_real=c(dummy_real,-1)
#   }
#   if(reality[i]<50 & reality[i]>(-50)){
#     dummy_real=c(dummy_real,0)
#   }
# }
# mean(dummy_naive==dummy_real)
# mean(dummy_pred==dummy_real)




acf(main_data$net)




simple_data=NULL
simple_data=data.table()
simple_data$date=main_data$date
simple_data$average_dswrf=rowMeans(main_data[,c(2,8,14,20,26,32,38)])
simple_data$average_rh2m=rowMeans(main_data[,c(3,9,15,21,27,33,39)])
simple_data$average_tdcdlow=rowMeans(main_data[,c(seq(4,40,6))])
simple_data$average_temp=rowMeans(main_data[,c(seq(5,41,6))])
simple_data$average_winddir=rowMeans(main_data[,c(seq(6,42,6))])
simple_data$average_windstr=rowMeans(main_data[,c(seq(7,43,6))])
main_data=as.data.table(main_data)
dummy=main_data[,c(1,44:52)]
simple_data=merge(simple_data,dummy,by='date')
#simple_data$upRegulationTwoCoded=NULL
#simple_data$downRegulationTwoCoded=NULL
simple_data$lagged24=lag(simple_data$net,n=24)
simple_data$lagged168=lag(simple_data$net,n=168)


simple_data$day=wday(simple_data$date)
simple_data$day=as.character(simple_data$day)
simple_data$hour=as.character(hour(simple_data$date))

# today=Sys.Date()
# yesterday=today-1
# last_full_date=today-2
# test_simple_data=simple_data[simple_data$date>=yesterday]
# test_simple_data=selectByDate(simple_data,start = '2022-01-11',end = '2022-01-12')

 #simple_data=selectByDate(simple_data,end = '2022-01-11')

simple_data=simple_data[169:nrow(simple_data),]
simple_data$month=as.character(month(simple_data$date))
simple_data$class

simple_data[,confirmed:=upRegulationZeroCoded+upRegulationOneCoded+upRegulationTwoCoded-downRegulationTwoCoded-downRegulationZeroCoded-downRegulationOneCoded]

simple_data$class=simple_data$confirmed
simple_data$confirmed=NULL
index_pos=simple_data$net>50
index_neg=simple_data$net<(-50)
index_neut=(simple_data$net<=50&simple_data$net>=(-50))


simple_data$class[index_pos]='Positive'
simple_data$class[index_neg]='Negative'
simple_data$class[index_neut]='Neutral'



sample_size = floor(0.8*nrow(simple_data))
set.seed(1752)

# randomly split data in r
picked = sample(seq_len(nrow(simple_data)),size = sample_size)
train_1 =simple_data[picked,]
test_1 =simple_data[-picked,]


simple_model_1=lm(net~average_dswrf+average_rh2m+average_windstr+average_winddir+
                  average_temp+average_tdcdlow+lagged24+day+hour+month+lagged168,data = train_1)

# simple_model_1=lm(net~
#                     average_temp+day+hour+month,data = train_1)

summary(simple_model_1)

prediction_1=predict(simple_model_1,test_1)
dummy_pred=vector()
prediction_matrix=as.matrix(prediction_1)


for(i in 1:length(prediction_1)){
  if(prediction_matrix[i]>=50){
    dummy_pred=c(dummy_pred,1)
  }
  if(prediction_matrix[i]<=(-50)){
    dummy_pred=c(dummy_pred,-1)
  }
  if(prediction_matrix[i]<50 & prediction_matrix[i]>(-50)){
    dummy_pred=c(dummy_pred,0)
  }
}
dummy_naive=vector()
naive_pred=test_1$lagged24

for(i in 1:length(naive_pred)){
  if(naive_pred[i]>=50){
    dummy_naive=c(dummy_naive,1)
  }
  if(naive_pred[i]<=(-50)){
    dummy_naive=c(dummy_naive,-1)
  }
  if(naive_pred[i]<50 & naive_pred[i]>(-50)){
    dummy_naive=c(dummy_naive,0)
  }
}



dummy_real=vector()
reality=test_1$net

for(i in 1:length(reality)){
  if(reality[i]>=50){
    dummy_real=c(dummy_real,1)
  }
  if(reality[i]<=(-50)){
    dummy_real=c(dummy_real,-1)
  }
  if(reality[i]<50 & reality[i]>(-50)){
    dummy_real=c(dummy_real,0)
  }
}
mean(dummy_naive==dummy_real)
mean(dummy_pred==dummy_real)

test_1$residual_1=reality-prediction_1

res_tree=rpart(residual_1~average_dswrf+average_rh2m+average_windstr+average_winddir+
                 average_temp+average_tdcdlow+lagged24+day+hour+month,test_1,
               control=rpart.control(cp=0,maxdepth=4))
fancyRpartPlot(res_tree)



# 
# prediction_1[prediction_1>=50]='positive'
# prediction_1[prediction_1<=-50]='negative'
# prediction_1[(prediction_1>-50)&(prediction_1<50)]='neutral'
# 
# naive_pred[naive_pred>=50]='positive'
# naive_pred[naive_pred<=-50]='negative'
# naive_pred[(naive_pred>-50)&(prediction_1<50)]='neutral'
# 
# reality[reality>=50]='positive'
# reality[reality<=-50]='negative'
# reality[(reality>-50)&(prediction_1<50)]='neutral'
# 


#prediction_simple=predict(simple_model,test_simple_data)
#prediction_simple=as.vector(prediction_simple)
# test_classes=last(classes,n=48)
# test_classes=first(test_classes,n=24)
# prediction_simple[prediction_simple<(-50)]='Negative'
# prediction_simple[prediction_simple>(50)]='Positive'
# prediction_simple[is.numeric(prediction_simple)]='Neutral'
# 
# 
# 




tree=rpart(class~average_dswrf+average_rh2m+average_windstr+average_winddir+
                 average_temp+average_tdcdlow+lagged24+day+hour+month+lagged168,train_1,
               control=rpart.control(cp=0,maxdepth=4))
fancyRpartPlot(tree)


tree_pred=predict(tree,test_1,type = 'class')
test_1$class=as.factor(test_1$class)
test_1$tree_pred=tree_pred
index=test_1$tree_pred==test_1$class

mean(na.exclude(index))




test_1$tree_learner=as.factor(index)
# 
# learner_tree=rpart(tree_learner~average_dswrf+average_rh2m+average_windstr+average_winddir+
#              average_temp+average_tdcdlow+lagged24+day+hour+month,test_1,
#            control=rpart.control(cp=0,maxdepth=3))
# fancyRpartPlot(learner_tree)
# 
# simple_data$fix_a=as.numeric((simple_data$lagged24<354)&(simple_data$lagged24>=(-907)))
# simple_data$fix_b=as.numeric(simple_data$month %in% c('1','10','11','12','9'))

simple_data$fix_x=as.numeric(simple_data$lagged24>=1146)
simple_data$fix_y=as.numeric(simple_data$hour %in% c('2','4','5','6','7','8','14','15','16','17','21','22','23'))
simple_data$fix_z=as.numeric(simple_data$average_temp>=6.6)

weather_nn=simple_data[,2:8]
pca_rep=princomp(weather_nn[,c(2:4,6)])
summary(pca_rep,loadings = T)

pca_nn=as.data.table(pca_rep$scores)

simple_data$pca_weather=pca_nn$Comp.1






sample_size = floor(0.8*nrow(simple_data))
set.seed(48)

# randomly split data in r
picked = sample(seq_len(nrow(simple_data)),size = sample_size)
train_1 =simple_data[picked,]
test_1 =simple_data[-picked,]

simple_model_learnt=lm(net~average_dswrf+average_rh2m+average_windstr+average_winddir+
                    average_temp+average_tdcdlow+lagged24+day+hour+month+fix_x:fix_y+fix_y:fix_z,data = train_1)

summary(simple_model_learnt)
learnt_pred=predict(simple_model_learnt,test_1)

test_1$pred_lrnt
index_pos=learnt_pred>50
index_neg=learnt_pred<(-50)
index_neut=(learnt_pred<50&learnt_pred>(-50))
test_1$pred_lrnt[index_pos]='Positive'
test_1$pred_lrnt[index_neg]='Negative'
test_1$pred_lrnt[index_neut]='Neutral'

str(test_1)

index=(test_1$class==test_1$pred_lrnt)
mean(na.exclude(index))

tree=rpart(class~average_dswrf+average_rh2m+average_windstr+average_winddir+
             average_temp+average_tdcdlow+lagged24+day+hour+month,train_1,
           control=rpart.control(cp=0,maxdepth=4))
fancyRpartPlot(tree)

tree_pred=predict(tree,test_1,type = 'class')
test_1$tree_pred=tree_pred
#test_1$class=as.factor(test_1$class)
index=test_1$tree_pred==test_1$class

mean(na.exclude(index))
# 
# pca_matrix=NULL
# matrix(nrow = nrow(train_1),ncol = nrow(test_1))
# 
# for(i in 1:length(train_1)){
#   for (j in 1:length(test_1)) {
#      pca_matrix[i:j]=abs(test_1$pca_weather-train_1$pca_weather)
#   }
#   
# }


sum(is.na(train_1$pca_weather))

length(train_1$class)


pr=knn(train_1$pca_weather,test_1$pca_weather,cl=train_1$class)
str(test_1)

one_var=ts(main_data$net,frequency=840)
decomposed=decompose(one_var,type = "additive")
plot(decomposed)
plot.ts(decomposed$trend)

simple_data_knn=simple_data[,list(average_dswrf,average_rh2m,average_tdcdlow,average_temp,average_winddir,average_windstr)]
trainclass=simple_data$class
traindata=as.matrix(simple_data_knn)
traindata=data.table(cl=trainclass,traindata)
ctrl = trainControl(method="repeatedcv",repeats = 3,number=10) 
knnFit_raw =train(cl ~ ., data = traindata, method = "knn", 
                  trControl = ctrl, 
                  tuneLength = 10)
knnFit_raw


simple_data[,date1:=as.IDate(date)]


testdata=simple_data[date1=="2020-02-08"]
tree_pred=predict(tree,testdata,type = 'class')
linear_pred=predict(simple_model_learnt,testdata)
simple_model_naive=lm(net~average_dswrf+average_rh2m+average_windstr+average_winddir+average_temp+average_tdcdlow,data= testdata[1:12,])
linear_pred_naive=predict(simple_model_naive,testdata)


table(tree_pred,testdata[,class])

b=linear_pred
b=c(b)
c=c()

for(i in 1:24){
  if(b[i]>=50){
    c=c(c,"Positive")
  }
  if(b[i]<=(-50)){
    c=c(c,"Negative")
  }
  if(b[i]<50 & b[i]>(-50)){
    c=c(c,"Neutral")
  }
}

table(testdata[,class],c)
testingknn1=data.table(linear=c)
b=linear_pred_naive
b=c(b)
c=c()

for(i in 1:24){
  if(b[i]>=50){
    c=c(c,"Positive")
  }
  if(b[i]<=(-50)){
    c=c(c,"Negative")
  }
  if(b[i]<50 & b[i]>(-50)){
    c=c(c,"Neutral")
  }
}

table(testdata[,class],c)
testingknn1=cbind(testingknn1,naive=c)
testingknn1=cbind(testingknn1,as.data.table(tree_pred))

case_when()

testingknn1$linear= case_when(
  testingknn1$linear == 'Positive'  ~ 1,
  testingknn1$linear == 'Negative'  ~ -1,
  testingknn1$linear == 'Neutral'  ~ 0,
  TRUE ~ as.numeric(testingknn1$linear)
)

testingknn1$naive= case_when(
  testingknn1$naive == 'Positive'  ~ 1,
  testingknn1$naive == 'Negative'  ~ -1,
  testingknn1$naive == 'Neutral'  ~ 0,
  TRUE ~ as.numeric(testingknn1$naive)
)

testingknn1$tree_pred= case_when(
  testingknn1$tree_pred == 'Positive'  ~ 1,
  testingknn1$tree_pred == 'Negative'  ~ -1,
  testingknn1$tree_pred == 'Neutral'  ~ 0,
  TRUE ~ as.numeric(testingknn1$tree_pred)
)

testingknn1$hybrid=rowMeans(testingknn1)

testingknn1$hybrid= case_when(
  testingknn1$hybrid > 0 ~ 'Positive' ,
  testingknn1$hybrid < (-1) ~ 'Negative'  ,
  testingknn1$hybrid == 0 ~ 'Neutral'  ,
  TRUE ~ as.character(testingknn1$hybrid)
)

table(testdata[,class],testingknn1$hybrid)


