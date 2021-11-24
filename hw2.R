install.packages("data.table")
install.packages("ggplot2")
install.packages("repr")
install.packages("rpart")
install.packages("rattle")
install.packages("TSrepr")
install.packages("zoo")
install.packages("dtw")
install.packages("reshape")
install.packages("genlasso")
install.packages("rlang")
install.packages("data.table")
install.packages("tree")
library(tree)
library(genlasso)
require(dtw)
require(data.table)
require(ggplot2)
require(repr)
require(rpart)
require(rattle)
require(TSrepr)
require(zoo)
require(reshape)
require(rlang)
library(data.table)

current_folder=getwd()
dataset='CBF'

train_data_path=sprintf('%s/ClassificationData/%s/%s_TRAIN.txt',current_folder,dataset,dataset)

#traindata=as.matrix(fread(train_data_path))
traindata=fread(train_data_path)

head(traindata)

#first column is the class variable
trainclass=traindata[,1] # takes -1 and 1
#drop first column
traindata=traindata[,2:ncol(traindata)]
print(dim(traindata)) #shows that there 100 series (rows) of length 96 time units (columns)

tlength=ncol(traindata)
noftimeseries=nrow(traindata)

#let's plot some of the time series
plot(traindata[1,])
#let's use line format
plot(traindata[1,],type='l')

#multiple plots on single plot
par(mfrow=c(2,2)) #2x2 grid
plot(traindata[1,],type='l')
plot(traindata[2,],type='l')
plot(traindata[3,],type='l')	
plot(traindata[4,],type='l')

#let's plot them overlaid with colors representing their class
par(mfrow=c(1,1))
plot(traindata[1,],type='l',col=trainclass[1]+1)
points(traindata[2,],type='l',col=trainclass[2]+1)
points(traindata[3,],type='l',col=trainclass[3]+1)	
points(traindata[4,],type='l',col=trainclass[4]+1) 

#problem with limits?
lim=max(abs(traindata[1:4,])) #maximum of the absolute value of the observations for first 4 time series
plot(traindata[1,],type='l',col=trainclass[1]+2,ylim=c(-1.1*lim,1.1*lim))
points(traindata[2,],type='l',col=trainclass[2]+2)
points(traindata[3,],type='l',col=trainclass[3]+2)	
points(traindata[4,],type='l',col=trainclass[4]+2) 

euc_dist=dist(traindata)

euc_dist=as.matrix(euc_dist)
# apply MDS to understand what Euclidean distance reflects
reduced=cmdscale(euc_dist,k=2)
plot(reduced,col=trainclass+2)

head(euc_dist)

# evaluate 1-NN classifier on training data
large_number=1000
diag(euc_dist)=large_number

# check the nearest neighbor for first time series
neighborhood=order(euc_dist[1,]) #first array entry is 1, why?
neighborhood

#1-NN
neighbor=neighborhood[1] 
prediction=trainclass[neighbor]

par(mfrow=c(2,1)) #2x1 grid
plot(traindata[1,],type='l',main='Selected Series',col=trainclass[1]+2)
plot(traindata[neighbor,],type='l',main='Training data with minimum Euclidean distance',col=trainclass[neighbor]+2)

par(mfrow=c(1,1))
matplot(cbind(traindata[1,],traindata[neighbor,]),type='l')

testngh=neighborhood

neighborhood=apply(euc_dist,1,order) #first array entry is 1, why?
#neighborhood

predicted=trainclass[neighborhood[1,]]

table(trainclass,predicted)

acc=sum(trainclass==predicted)/length(predicted)
print(acc)

require(dtw)
#require(TSdist)

alignment=dtw(traindata[1,],traindata[2,],keep=TRUE)


str(alignment)

plot(alignment)

plot(alignment$index1,alignment$index2,main="Warping function")

dtwPlotThreeWay(alignment)

dtwPlotTwoWay(alignment);

query_ts=traindata[1,]
reference_ts=traindata[2,]
alignment=dtw(query_ts,reference_ts,window.type='itakura',keep=TRUE); 
dtwPlotTwoWay(alignment);

query_ts=traindata[1,]
reference_ts=traindata[5,]
alignment=dtw(query_ts,reference_ts,window.type='sakoechiba',keep=TRUE,window.size=5); 
dtwPlotTwoWay(alignment);

query_ts=traindata[1,]
reference_ts=traindata[4,]

plot(query_ts,col='black',type='l')
points(reference_ts,col='red',type='l')

alignment=dtw(query_ts,reference_ts,window.type='sakoechiba',keep=TRUE,window.size=10); 
dtwPlotTwoWay(alignment);

query_ts=traindata[1,]
reference_ts=traindata[2,]

par(mfrow=c(1,1)) #2x1 grid
query_length=length(query_ts)
ref_length=length(reference_ts)
alignment=dtw(query_ts,reference_ts,keep=TRUE)
contour(alignment$costMatrix,col=terrain.colors(length(traindata)),x=1:query_length,y=1:ref_length,
        xlab="Query",ylab="Reference");
lines(alignment$index1,alignment$index2,col="red",lwd=2);

dtw_dist_mat=dtwDist(traindata)

str(dtw_dist_mat)

diag(dtw_dist_mat)=large_number

neighborhood=apply(dtw_dist_mat,1,order) #first array entry is 1, why?
neighborhood[1,]

predicted=trainclass[neighborhood[1,]]

table(trainclass,predicted)

acc=sum(trainclass==predicted)/length(predicted)
print(acc)

dtw_dist_mat=dtwDist(traindata,window.type='sakoechiba',window.size=5)
diag(dtw_dist_mat)=large_number

neighborhood=apply(dtw_dist_mat,1,order) #first array entry is 1, why?
#neighborhood

predicted=trainclass[neighborhood[1,]]

table(trainclass,predicted)

acc=sum(trainclass==predicted)/length(predicted)
print(acc)

dtw_dist_mat=dtwDist(traindata,window.type='itakura')
diag(dtw_dist_mat)=large_number

neighborhood=apply(dtw_dist_mat,1,order) #first array entry is 1, why?
#neighborhood

predicted=trainclass[neighborhood[1,]]

table(trainclass,predicted)

acc=sum(trainclass==predicted)/length(predicted)
print(acc)


traindata=fread(train_data_path)

traindatamatrix=as.matrix(traindata)
traindatamatrix=traindatamatrix[,-1]
traindata=traindata[,-1]
traindata[,id:=1:.N]
traindatalong=melt(traindata,id.vars='id')
traindatalong[,time:=as.numeric(gsub("\\D", "", variable))-1]
traindatalong=traindatalong[,list(id,time,value)]
traindatalong=traindatalong[order(id,time)]

series_id=unique(traindatalong$id)


lambdafunc=function(X){
  X=X$value
  X=as.matrix(X)
  a=trendfilter(X,ord = 0)
  b=cv.trendfilter(a,k=10,mode = "lambda")
  return(b$lambda.min)
}
lambdas=lapply(series_id,function(x) lambdafunc(traindatalong[id==x]))

lambdas=as.data.table(lambdas)






out=trendfilter(traindatamatrix[1,],ord=0)



plot(out,lambda = 0.74)

cv=cv.trendfilter(out,k=10,mode="lambda")


fl1d=fusedlasso1d(traindatamatrix[1,],)
cv=cv.trendfilter(fl1d,k=10,mode = "lambda")
traindatamatrix=as.matrix(traindatamatrix)
lambmin=cv$lambda.min
col1=traindatamatrix[1,]
predgenlas=predict.genlasso(fl1d,lambda = lambmin)
plot(predgenlas$fit,type = "l")
points(traindatamatrix[1,],type = "l",col=2)
plot(traindatamatrix[1,],type = "l",col=2)
points(predgenlas$fit,type = "l")
genlas1d_values=as.data.table(predgenlas$fit)


genlas1d_values[,time:=1:128]


traindata=fread(train_data_path)
traindata=as.matrix(traindata)
trainclass=traindata[,1] # takes -1 and 1
#drop first column
traindata=traindata[,2:ncol(traindata)]
plot(traindata[1,])
plot(traindata[1,],type='l')
ts1=traindata[1,]

selected_series=1
selected_dt=traindatalong[id==selected_series]

tree_fit=rpart(value~time,selected_dt,control=rpart.control(minsplit=20,minbucket=10,cp=0))
fancyRpartPlot(tree_fit)
selected_dt[,tree_rep:=predict(tree_fit,selected_dt)]
selected_dt[,genlasso1d:=genlas1d_values]
data_plot=melt(selected_dt,id.vars='time',measure.vars=c('value','tree_rep','genlasso1d'))


ggplot(data_plot,aes(x=time,y=value,color=variable))+
  geom_line()

treemodel=rpart(ts1)








