####CHEN FANGZHENG  Projet Sim 202######
rm(list=objects())
library(keras) # require tensorflow and keras 
library(xgboost)
library(nlme)
library(magrittr)
library(lubridate)
library(mgcv)
library(ggplot2)
library(rpart)
library(randomForest)
library(rpart.plot)
library(ggthemes)
# criterion definition
rmse=function(eps){return(sqrt(mean(eps^2,na.rm=TRUE)))}


############## I. Data preprocessing#####################
setwd("~/R/STAT/SIM 202")
data2013=read.csv("Data0.csv", header=TRUE,sep=",")
str(data2013)
date_time=strsplit(as.character(data2013$Date)," ")
date2013=sapply(date_time,"[",1)
time2013=sapply(date_time,"[",2)
data2013=cbind(data2013,date2013,time2013)
data2013$Date=as.POSIXct(data2013$Date,format ="%Y-%m-%d %H:%M:%S")
data2013$Month=month(as.Date(data2013$date2013))
data2013$Wday=wday(as.Date(data2013$date2013))
data2013$Qday=qday(as.Date(data2013$date2013))
names(data2013)

data2014<-read.csv("Data1.csv", header=TRUE,sep=",")
str(data2014)
date_time=strsplit(as.character(data2014$Date)," ")
date2014=sapply(date_time,"[",1)
time2014=sapply(date_time,"[",2)
data2014=cbind(data2014,date2014,time2014)
data2014$Date=as.POSIXct(data2014$Date,format ="%Y-%m-%d %H:%M:%S")
data2014$Month=month(as.Date(data2014$date2014))
data2014$Wday=wday(as.Date(data2014$date2014))
data2014$Qday=qday(as.Date(data2014$date2014))
summary(data2014)

# fill the NA value in data2014
KNNmethod=function(x,k)
{
  index1=which(is.na(x))
  for (i in 1:(length(index1)))
  {
    s=0
    s=s+x[index1[i]-k]
    s=s+x[index1[i]+k]
    x[index1[i]]=s/2
  }
  return (x)
}
data2014$Temperature=KNNmethod(data2014$Temperature,4)
summary(data2014)

#remove the rows with NA in data2013
data2013.rvNA=data2013[rowSums(is.na(data2013)) == 0 ,]
summary(data2013.rvNA)

############# II. data visualisation  #################

meanconsom1=function(traindata)
{
  name=paste( c("X"), 1:30, sep="")
  d=data.frame()
  for(i in c(1:30))
  {
    d=rbind(d,data.frame(Month=c(1:12),
                         Meanconsom=tapply(traindata[,i+1],traindata[c("Month")],mean),
                         Meantemp=tapply(traindata[,32],traindata[c("Month")],mean),
                         label=as.factor(name[i])))
  }
  return(d)
}
meanconsom2=function(traindata)
{
  name=paste( c("X"), 1:30, sep="")
  d=data.frame()
  for(i in c(1:30))
  {
    d=rbind(d,data.frame(Temp=c(1:48),
                         Meanconsom=tapply(traindata[,i+1],traindata[c("time2013")],mean),
                         Meantemp=tapply(traindata[,32],traindata[c("time2013")],mean),
                         label=as.factor(name[i])))
  }
  return(d)
}
transdata1=meanconsom1(data2013.rvNA)
transdata2=meanconsom2(data2013.rvNA)
p1=ggplot(transdata1,aes(Month,Meanconsom,color=label))
p2=ggplot(transdata1,aes(Meantemp,Meanconsom,color=label))
p3=ggplot(transdata1,aes(Month,Meantemp))
p4=ggplot(transdata2,aes(Temp,Meanconsom,color=label))
p5=ggplot(transdata2,aes(Meantemp,Meanconsom,color=label))
p6=ggplot(transdata2,aes(Temp,Meantemp))
layer1=geom_point()
layer2=geom_line()
layer3=labs(title="monthly average consommation",y ="consommation")
layer4=labs(title="monthly average consommation",x="temperature",y ="consommation")
layer5=labs(title="monthly average temperature",y ="temperature")
layer6=labs(title="Average consommation every 30 minutes",y ="consommation")
layer7=labs(title="Average consommation every 30 minutes",x="temperature",y ="consommation")
layer8=labs(title="Average temperature every 30 minutes",y ="temperature")
q=theme(plot.title = element_text(hjust = 0.5))
p1+layer1+layer2+layer3+q
p2+layer1+layer2+layer4+q
p3+layer1+layer2+layer5+q
p4+layer1+layer2+layer6+q
p5+layer1+layer2+layer7+q
p6+layer1+layer2+layer8+q+theme_economist()+ scale_color_economist()

name=paste( c("X"), 1:30, sep="")
data_2013.rvNA=data.frame(data2013.rvNA$X1,data2013.rvNA$Temperature,as.factor("X1"))
names(data_2013.rvNA)=c("consommation","temp","label")
for(i in c(3:31))
{
  d=data.frame(data2013.rvNA[,i],data2013.rvNA[c("Temperature")],as.factor(name[i-1]))
  names(d)=c("consommation","temp","label")
  data_2013.rvNA=rbind(data_2013.rvNA,d)
}
ggplot(data_2013.rvNA, aes(label,consommation))+geom_boxplot()

# plot by group
par(mfrow=c(5,6))
name=paste( c("X"), 1:30, sep="")
for(i in c(1:30))
{
  sub=subset(data2013.rvNA,as.numeric(time2013)==36)
  plot(sub[,32],sub[,i+1],xlab = "temperature", ylab="consommation",main =name[i])
}
par(mfrow=c(1,1))

par(mfrow=c(3,2))
for(i in c(1:48))
{
  sub=subset(data2013.rvNA,as.numeric(time2013)==i)
  plot(sub[,32],sub[,2],xlab = "temperature", ylab="consommation",main ="X1" )
}
par(mfrow=c(1,1))

#basic plot for data2014
ggplot(data2014[c(1:2784),], aes(Date,Temperature))+geom_line()
ggplot(data2014[c(1:2784),], aes(Date,Temperature))+geom_boxplot()

############## III .Building model #####################

############## Random forest ########################
eq1 = data2013.rvNA[,2] ~ Temperature + time2013 + Month + Wday + Qday
eq2 = data2013.rvNA[,12] ~ Temperature + time2013 + Month + Wday + Qday
eq3 = data2013.rvNA[,22] ~ Temperature + time2013 + Month + Wday + Qday
rf1 <- randomForest(eq1,ntree=300,data=data2013.rvout, importance=TRUE)
rf2 <- randomForest(eq2,ntree=300,data=data2013.rvout, importance=TRUE)
rf3 <- randomForest(eq3,ntree=300,data=data2013.rvout, importance=TRUE)
imp1<-varImpPlot(rf1, type=1)
imp2<-varImpPlot(rf2, type=1)
imp3<-varImpPlot(rf3, type=1)
o1<-order(imp1,decreasing=TRUE)
o2<-order(imp2,decreasing=TRUE)
o3<-order(imp3,decreasing=TRUE)
par(mfrow=c(3,1))
plot(imp[o1],type='b',pch=20, axes=F, xlab="", ylab='importance',main="Feature importance for prediction X1")
axis(1,c(1:length(imp1)), rownames(imp1)[o], las=2, cex=0.5)
axis(2)
plot(imp[o2],type='b',pch=20, axes=F, xlab="", ylab='importance',main="Feature importance for prediction X11")
axis(1,c(1:length(imp2)), rownames(imp2)[o], las=2, cex=0.5)
axis(2)
plot(imp[o3],type='b',pch=20, axes=F, xlab="", ylab='importance',main="Feature importance for prediction X21")
axis(1,c(1:length(imp3)), rownames(imp3)[o], las=2, cex=0.5)
axis(2)
par(mfrow=c(1,1))

pred1<-function(traindata,newdata)
{
  vec=c();err=c();
  for (i in 2:31)
  {
    eq = traindata[,i] ~ Temperature + time2013 + Month+ Wday + Qday
    model= randomForest(eq,ntree=300,data=traindata, importance=TRUE)
    valfitted=predict(model,newdata=traindata)
    valpred=predict(model,newdata=newdata)
    err=c(err,rmse(valfitted-traindata[,i]))
    vec=c(vec,valpred)
  }
  return (list(err,vec))
}
sub=data2014[c(1:2784),]
newdata=data.frame(sub[,2],sub[,6],sub[,7],sub[,8],sub[,9])
names(newdata)=c("Temperature","time2013","Month","Wday","Qday")
z1=pred1(data2013.rvNA,newdata)
plot(unlist(z1[1]),xlab = "X", ylab="RMSE",main ="RMSE on trainset for RandomForest")
write.csv(data.frame(Prediction=unlist(z1[2])),file="RandomForest.csv",row.names = T)

########################## Linear Model #################################

pred2=function(traindata,newdata)
{
  vec=c();err=c();
  for (i in 2:11)
  {
    model=lm(traindata[,i] ~ Temperature + time2013 + Qday ,data = traindata)
    valfitted=predict(model,newdata=traindata)
    valpred=predict(model,newdata=newdata)
    err=c(err,rmse(valfitted-traindata[,i]))
    vec=c(vec,valpred)
  }
  for (i in 12:31)
  {
    model=lm(traindata[,i] ~ Temperature + time2013 + Wday ,data = traindata)
    valfitted=predict(model,newdata=traindata)
    valpred=predict(model,newdata=newdata)
    err=c(err,rmse(valfitted-traindata[,i]))
    vec=c(vec,valpred)
  }
  return (list(err,vec))
}
z2=pred2(data2013.rvNA,newdata)
plot(unlist(z2[1]),xlab = "X", ylab="RMSE",main ="RMSE on trainset for Linear regression before simplifying variables")
write.csv(data.frame(Prediction=unlist(z2[2])),file="LinearRegression.csv",row.names = T)

##############  GAM MODEL ###########################
pred3<-function(traindata,newdata)
{
  vec=c();err=c();
  for (i in 2:11)
  {
    model=gam(traindata[,i]~s(Temperature,Qday,time2013,bs="fs"), data=traindata)
    #model=gam(traindata[,i]~s(Temperature,Wday,time2013,bs="fs"), data=traindata)
    #model=gam(traindata[,i]~s(Temperature,Qday,time2013,bs="fs"), data=traindata)
    valfitted=predict(model,newdata=traindata)
    valpred=predict(model,newdata=newdata)
    err=c(err,rmse(valfitted-traindata[,i]))
    vec=c(vec,valpred)
    print(i)
  }
  for (i in 12:31)
  {
    model=gam(traindata[,i]~s(Temperature,time2013,bs="fs"), data=traindata)
    valfitted=predict(model,newdata=traindata)
    valpred=predict(model,newdata=newdata)
    err=c(err,rmse(valfitted-traindata[,i]))
    vec=c(vec,valpred)
    print(i)
  }
  return (list(err,vec))
}
z3=pred3(data2013.rvNA,newdata)
plot(unlist(z3[1]),xlab = "X", ylab="RMSE",main ="RMSE on trainset for GAM model before simplifying variables")
write.csv(data.frame(Prediction=unlist(z3[2])),file="GAM.csv",row.names = T)

subdata2013.rvNA=subset(data2013.rvNA,Temperature<14)
z7=pred3(subdata2013.rvNA,newdata)
plot(unlist(z3[1]),xlab = "X", ylab="RMSE",main ="RMSE on subtrainset for GAM model after simplifying variables")
write.csv(data.frame(Prediction=unlist(z7[2])),file="GAM2.csv",row.names = T)

###############xgboost######################

pred4<-function(traindata,newdata)
{
  vec=c();err1=c();err2=c();
  for (i in 2:31)
  {
    set.seed(2019)
    subtraindata=data.frame(traindata[,i],traindata[,32],as.numeric(traindata[,34]),
                            traindata[,35],traindata[,36],traindata[,37])
    names(subtraindata)=c("X","Temperature","time2013","Month","Wday","Qday")
    select=sample(1:nrow(subtraindata),12190,replace = F)  ## 17416*0.7 number of subsample
    dtrain=xgb.DMatrix(as.matrix(subtraindata[select,-1]),label=subtraindata[select,1])
    dtest=xgb.DMatrix(as.matrix(subtraindata[-select,-1]),label=subtraindata[-select,1])
    watchlist=list(train=dtrain,eval=dtest)
    para = list(subsample = 0.9,eta = 0.05,
                max.depth = 6, colsample_bytree= 1,
                objective = "reg:linear",nround =500,
                booster = "gbtree",seed=2019,eval_metric='rmse')
    model = xgb.train(para,dtrain,nround = 500,nthread = 4,
                      watchlist,early_stopping_rounds=50,verbose = 0)
    data=newdata
    data$time2013=as.numeric(newdata$time2013)
    valpred= predict(model,as.matrix(data))
    err1=c(err1,min(model$evaluation_log[,2]))
    err2=c(err2,min(model$evaluation_log[,3]))
    vec=c(vec,valpred)
  }
  return (list(err1,err2,vec))
}
z4=pred4(data2013.rvNA,newdata)
points(unlist(z4[2]),col="red")
legend(20, 0.05, legend=c("training error", "validation error"),col=c("black", "red"),pch =c(1,1))
plot(unlist(z4[1]),xlab = "X", ylab="RMSE",main ="RMSE on trainset for XgBoost")
write.csv(data.frame(Prediction=unlist(z4[3])),file="Xgboost.csv",row.names = T)      

###############LSTM ########################

pred5<-function(traindata,newdata)
{
  vec=c();err=c();
  for (i in 2:31)
  {
    set.seed(2019)
    subtraindata=data.frame(traindata[,i],traindata[,32],as.numeric(traindata[,34]),
                            traindata[,35],traindata[,36],traindata[,37])
    names(subtraindata)=c("X","Temperature","time2013","Month","Wday","Qday")
    dtrain=list(dataX=as.matrix(subtraindata[,-1]),dataY=as.matrix(subtraindata[,1]))
    dim_train = dim(dtrain$dataX)
    dim(dtrain$dataX)= c(dim_train[1], 1, dim_train[2])
    model = keras_model_sequential()
    model %>% layer_lstm( units = 16,input_shape = c(1,5)) %>%
      layer_dense(units = 1) %>%compile(loss = 'mean_squared_error',optimizer = 'adam') %>%
      fit(dtrain$dataX,dtrain$dataY,epochs = 300, batch_size = 300,verbose = 2) #validation_split = 0.3
    trainScore <- model %>%  evaluate(dtrain$dataX,dtrain$dataY, verbose = 0)
    newdata$time2013=as.numeric(newdata$time2013)
    dtest=list(dataX=as.matrix((newdata[,])))
    dim_test= dim(dtest$dataX)
    dim(dtest$dataX) = c(dim_test[1], 1, dim_test[2])
    valpred = model %>% predict(dtest$dataX)
    err=c(err,sqrt(trainScore))
    vec=c(vec,valpred)
    model%>%reset_states() 
  }
  return (list(err,vec))
}
z5=pred5(data2013.rvNA,newdata)
plot(unlist(z5[1]),xlab = "X", ylab="RMSE",main ="RMSE on trainset for LSTM")
write.csv(data.frame(Prediction=unlist(z5[2])),file="LSTM.csv",row.names = T) 

##################IV. Ensemble learning###################
##################  Stacking  ############################

redo=function(model,model.fitted,model.pred,traindata,newdata)
{
  valfitted=predict(model,newdata=traindata)
  valpred=predict(model,newdata=newdata)
  model.fitted=c(model.fitted,valfitted)
  model.pred=model.pred+valpred
  return (list(model.fitted,model.pred))
}
block=function(n,data)
{
  Nblock=n
  borne_block=seq(1, nrow(data), length=Nblock+1)%>%floor
  block_list=list()
  l=length(borne_block)
  for(i in c(2:(l-1)))
  {
    block_list[[i-1]] = c(borne_block[i-1]:(borne_block[i]-1))
  }
  block_list[[l-1]]=c(borne_block[l-1]:(borne_block[l]))
  return (block_list)
}
Cv=function(traindata,newdata,i,n)
{
  blocklist=block(n,data)
  model.pred=rep(0,nrow(newdata));model.fitted=c();
  z=randomForest(traindata[-unlist(blocklist[1]),i] ~ Temperature + time2013 + Month,
                 ntree=300,data=traindata[-unlist(blocklist[1]),],importance=TRUE)  %>%
    redo(model.fitted,model.pred,traindata[unlist(blocklist[1]),],newdata) 
  for( j in c(2:5))
  {
    z=randomForest(traindata[-unlist(blocklist[j]),i] ~ Temperature + time2013 + Month,
                   ntree=300,data=traindata[-unlist(blocklist[j]),],importance=TRUE)  %>%
      redo(model.fitted=unlist(z[1]),model.pred=unlist(z[2]),traindata[unlist(blocklist[j]),],newdata) 
  }
  print(i)
  return (list(unlist(z[1]),unlist(z[2])/5))
}
ensemble<-function(traindata,newdata,n)
{
  vec=c();err1=c();err2=c();
  for (i in 2:31)
  {
    z=Cv(traindata,newdata,i,n)
    subtraindata=data.frame(traindata[,i],traindata[,32],as.numeric(traindata[,34]),
                            traindata[,35],traindata[,36],traindata[,37],unlist(z[1]))
    names(subtraindata)=c("X","Temperature","time2013","Month","Wday","Qday","Fitted")
    set.seed(2019)
    select=sample(1:nrow(subtraindata),12190,replace = F)  ## 17416*0.7 number of subsample
    dtrain=xgb.DMatrix(as.matrix(subtraindata[select,-1]),label=subtraindata[select,1])
    dtest=xgb.DMatrix(as.matrix(subtraindata[-select,-1]),label=subtraindata[-select,1])
    watchlist=list(train=dtrain,eval=dtest)
    para = list(subsample = 0.9,eta = 0.05,
                max.depth = 8, colsample_bytree= 1,
                objective = "reg:linear",nround =500,
                booster = "gbtree",seed=2019,eval_metric='rmse')
    model = xgb.train(para,dtrain,nround = 500,nthread = 4,
                      watchlist,early_stopping_rounds=50,verbose = 0)
    data=newdata
    data$Fitted=as.numeric(unlist(z[2]))
    data$time2013=as.numeric(newdata$time2013)
    valpred= predict(model,as.matrix(data))
    err1=c(err1,min(model$evaluation_log[,2]))
    err2=c(err2,min(model$evaluation_log[,3]))
    vec=c(vec,valpred)
  }
  return (list(err1,err2,vec))
}
z6=ensemble(data2013.rvNA,newdata,5)
plot(unlist(z6[1]),xlab = "X", ylab="RMSE",main ="RMSE on trainset using Ensemble learning")
points(unlist(z6[2]),col="red")
legend(20, 0.05, legend=c("training error", "validation error"),col=c("black", "red"),pch =c(1,1))
write.csv(data.frame(Prediction=unlist(z6[3])),file="Ensemble.csv",row.names = T) 

