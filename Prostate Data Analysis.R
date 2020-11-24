library(ElemStatLearn)
library(glmnet)
library(pls)
library(leaps)
library(bootstrap)
library(boot)
data(prostate)
head(prostate)
names(prostate)
pros_cor = cor(prostate[,1:8])
pros_cor
training<-subset(prostate,train=="TRUE")[,1:9]
test<-subset(prostate,train=="FALSE")[,1:9]
y_training<-training$lpsa
y_test<-test$lpsa
regfit_full<-regsubsets(lpsa~.,data=training, nbest=1,nvmax=8,method="exhaustive")
my_sum <- summary(regfit_full)
my_sum
my_sum$cp
my_sum$bic
select = my_sum$outmat
training_error_store<-c()
test_error_store<-c()
aic_store<-c()
bic_store<-c()

for(i in 1:8){
  temp<-which(select[i,]=="*")
  red_training<-training[,c(9,temp)]
  red_test<-test[,c(9,temp)]
  red_fit<-lm(lpsa~.,data=red_training)
  aic<-AIC(red_fit)
  bic<-BIC(red_fit)
  predict_training<-predict(red_fit,newdata=red_training)
  predict_test<-predict(red_fit,newdata=red_test)
  training_error<-sum((predict_training-y_training)^2)/length(y_training)
  test_error<-sum((predict_test-y_test)^2)/length(y_test)
  training_error_store<-c(training_error_store,training_error)
  test_error_store<-c(test_error_store,test_error)
  aic_store<-c(aic_store,aic)
  bic_store<-c(bic_store,bic)
  
  
}

training_error_store
test_error_store
aic_store
bic_store

upper= max(aic_store,bic_store)
lower= min(aic_store,bic_store)
upper
lower
x11()
plot(aic_store,type="o",lty=2,col = "dark blue",ylim = c(lower-5,upper+5),xlab = "k",main="AIC & BIC",ylab="Value")
lines(bic_store,type="o",lty=1,col="orange")
legend("topright",c("AIC", "BIC"),lty=c(2,1),col=c("dark blue","orange"))

x11()
plot(training_error_store,type="o",lty=2,col = "dark blue",ylim = c(0,1),xlab = "k",ylab="error",main="Error")
lines(test_error_store,type="o",lty=1,col="orange")
legend("topright",c("training_error", "test_error"),lty=c(2,1),col=c("dark blue","orange"))


# cross-validation k=5,10
set.seed(124)
cv_error_k5_store<-c()
cv_error_k10_store<-c()
for(i in 1:8){
  temp<-which(select[i,]=="*")
  glm_data<-prostate[,c(9,temp)]
  glm_fit<-glm(lpsa~.,data =glm_data)
  cv_error_k5<-cv.glm(glm_data, glm_fit,K=5)$delta[2]
  cv_error_k10<-cv.glm(glm_data, glm_fit,K=10)$delta[2]
  cv_error_k5_store<-c(cv_error_k5_store,cv_error_k5)
  cv_error_k10_store<-c(cv_error_k10_store,cv_error_k10)
}

cv_error_k5_store
cv_error_k10_store

which.min(cv_error_k5_store)
which.min(cv_error_k10_store)

x11()
plot(cv_error_k5_store,type="o",lty=2,col = "dark blue",ylim = c(0,1),xlab = "k",ylab="error",main="Error")
lines(cv_error_k10_store,type="o",lty=1,col="orange")
legend("topright",c("error_k5", "error_k10"),lty=c(2,1),col=c("dark blue","orange"))

# bootstrap.632

x<-prostate[,1:8]
y<-prostate[,9]

theta_fit<-function(x,y){lsfit(x,y)}
theta_predict<-function(fit,x){cbind(1,x)%*%fit$coef}
sq_err<-function(y,yhat){(y-yhat)^2}

bootstrap.632_error_store<-c()
for(i in 1:8){
  temp<-which(select[i,]=="*")
  res<-bootpred(x[,temp],y,nboot = 50,theta_fit,theta_predict,err.meas=sq_err)
  bootstrap.632_error_store<-c(bootstrap.632_error_store,res[[3]])
}

bootstrap.632_error_store

x11()
plot(bootstrap.632_error_store,type="o",lty=5,col="orange",main="error",xlab = "k",ylab="error")
legend("topright",c(".632"),lty=1,col=c("orange"))

x11()
plot(training_error_store,type="o",lty=2,col = "dark blue",ylim = c(0.4,0.7),xlab = "k",ylab="error",main="comparision of models")
lines(test_error_store,type="o",lty=1,col="dark red")
lines(cv_error_k5_store,type="o",lty=3,col = "yellow")
lines(cv_error_k10_store,type="o",lty=4,col="orange")
lines(bootstrap.632_error_store,type="o",lty=5,col="black")
legend("topright",c("k=5", "k=10","training_lm", "test_lm",".632"),lty=c(5,1),col=c("yellow","orange","dark blue","dark red","black"))
  