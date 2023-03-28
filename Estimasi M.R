library(readr)
library(car)
library(MASS)
library(lmtest)
data <- read_csv("D:/KULIAH/ASDOS BU YULI/ROBUST/P1/produksi jagung.csv")
View(data)

sum(is.na(data))

mkt<-lm(Y~X1+X2, data=data)
str(data)

#mengubah menjadi numerik
data[,c(1:3)] <-lapply(data[,c(1:3)], function(x) as.numeric(as.character(x)))
head(data)
str(data)

res <- mkt$residuals
summary(mkt)

#mkt

#NORMALITAS
shapiro.test(res)

#HETEROGENITAS
bptest(mkt)

#AUTOKORELASI
dwtest(mkt)

#MULTIKOLINIERITAS
vif(mkt)

#OUTLIER
dffits(mkt)
nilai.pembanding.dffits= 2*(sqrt(3/15))
nilai.pembanding.dffits

View(data)


#===================================================#
#ESTIMASI M PEMBOBOT HUBER#
#menghitung nilai residu
res<-mkt$residuals
res

#Iterasi 1
#Menghitung sig
sig1<- median(abs(res-median(res)))/0.6745

#Menghitung nilai ui
u<- vector()
for(i in 1:length(res)){
  u[i]<- res[i]/sig1
}
u

#Menghitung Pembobot
w=weights(mkt)
for(i in 1:15){
  if((abs(u[i]))>1.345){w[i]=1.345/abs(u[i])}
  else w[i]=1
}
w

modelM1<-lm(Y~.,data=data, weights=w)
summary(modelM1)

#menghitung nilai residu
res1<-modelM1$residuals

#Iterasi 2
#Menghitung sig
sig2<- median(abs(res1-median(res1)))/0.6745

#Menghitung nilai ui
u2<- vector()
for(i in 1:length(res1)){
  u2[i]<- res1[i]/sig2
}
u2

#Menghitung Pembobot
w2=weights(modelM1)
for(i in 1:15){
  if((abs(u2[i]))>1.345){w2[i]=1.345/abs(u2[i])}
  else w2[i]=1
}
w2

modelM2<-lm(Y~.,data=data, weights=w2)
summary(modelM2)


#menghitung nilai residu
res2<-modelM2$residuals

#Iterasi 3
#Menghitung sig
sig3<- median(abs(res2-median(res2)))/0.6745

#Menghitung nilai ui
u3<- vector()
for(i in 1:length(res2)){
  u3[i]<- res2[i]/sig3
}
u3

#Menghitung Pembobot
w3=weights(modelM2)
for(i in 1:15){
  if((abs(u3[i]))>1.345){w3[i]=1.345/abs(u3[i])}
  else w3[i]=1
}
w3

modelM3<-lm(Y~.,data=data, weights=w3)
summary(modelM3)


#menghitung nilai residu
res3<-modelM3$residuals

#Iterasi 4
#Menghitung sig
sig4<- median(abs(res3-median(res3)))/0.6745

#Menghitung nilai ui
u4<- vector()
for(i in 1:length(res3)){
  u4[i]<- res3[i]/sig4
}
u4

#Menghitung Pembobot
w4=weights(modelM3)
for(i in 1:15){
  if((abs(u4[i]))>1.345){w4[i]=1.345/abs(u4[i])}
  else w4[i]=1
}
w4

modelM4<-lm(Y~.,data=data, weights=w4)
summary(modelM4)



#menghitung nilai residu
res4<-modelM4$residuals

#Iterasi 5
#Menghitung sig
sig5<- median(abs(res4-median(res4)))/0.6745

#Menghitung nilai ui
u5<- vector()
for(i in 1:length(res4)){
  u5[i]<- res4[i]/sig5
}
u5

#Menghitung Pembobot
w5=weights(modelM4)
for(i in 1:15){
  if((abs(u5[i]))>1.345){w5[i]=1.345/abs(u5[i])}
  else w5[i]=1
}
w5

modelM5<-lm(Y~.,data=data, weights=w5)
summary(modelM5)


#menghitung nilai residu
res5<-modelM5$residuals

#Iterasi 6
#Menghitung sig
sig6<- median(abs(res5-median(res5)))/0.6745

#Menghitung nilai ui
u6<- vector()
for(i in 1:length(res5)){
  u6[i]<- res5[i]/sig6
}
u6

#Menghitung Pembobot
w6=weights(modelM5)
for(i in 1:15){
  if((abs(u6[i]))>1.345){w6[i]=1.345/abs(u6[i])}
  else w6[i]=1
}
w6

modelM6<-lm(Y~.,data=data, weights=w6)
summary(modelM6)


#menghitung nilai residu
res6<-modelM6$residuals

#Iterasi 7
#Menghitung sig
sig7<- median(abs(res6-median(res6)))/0.6745

#Menghitung nilai ui
u7<- vector()
for(i in 1:length(res6)){
  u7[i]<- res6[i]/sig7
}
u7

#Menghitung Pembobot
w7=weights(modelM6)
for(i in 1:15){
  if((abs(u7[i]))>1.345){w7[i]=1.345/abs(u7[i])}
  else w7[i]=1
}
w7

modelM7<-lm(Y~.,data=data, weights=w7)
summary(modelM7)


#menghitung nilai residu
res7<-modelM7$residuals

#Iterasi 8
#Menghitung sig
sig8<- median(abs(res7-median(res7)))/0.6745

#Menghitung nilai ui
u8<- vector()
for(i in 1:length(res7)){
  u8[i]<- res7[i]/sig8
}
u8

#Menghitung Pembobot
w8=weights(modelM7)
for(i in 1:15){
  if((abs(u8[i]))>1.345){w8[i]=1.345/abs(u8[i])}
  else w8[i]=1
}
w8

modelM8<-lm(Y~.,data=data, weights=w8)
summary(modelM8)

#menghitung nilai residu
res8<-modelM8$residuals

#Iterasi 9
#Menghitung sig
sig9<- median(abs(res8-median(res8)))/0.6745

#Menghitung nilai ui
u9<- vector()
for(i in 1:length(res8)){
  u9[i]<- res8[i]/sig9
}
u9

#Menghitung Pembobot
w9=weights(modelM8)
for(i in 1:15){
  if((abs(u9[i]))>1.345){w9[i]=1.345/abs(u9[i])}
  else w9[i]=1
}
w9

modelM9<-lm(Y~.,data=data, weights=w9)
summary(modelM9)


#menghitung nilai residu
res9<-modelM9$residuals

#Iterasi 10
#Menghitung sig
sig10<- median(abs(res9-median(res9)))/0.6745

#Menghitung nilai ui
u10<- vector()
for(i in 1:length(res9)){
  u10[i]<- res9[i]/sig10
}
u10

#Menghitung Pembobot
w10=weights(modelM9)
for(i in 1:15){
  if((abs(u10[i]))>1.345){w10[i]=1.345/abs(u10[i])}
  else w10[i]=1
}
w10

modelM10<-lm(Y~.,data=data, weights=w10)
summary(modelM10)

#menghitung nilai residu
res10<-modelM10$residuals

#Iterasi 11
#Menghitung sig
sig11<- median(abs(res10-median(res10)))/0.6745

#Menghitung nilai ui
u11<- vector()
for(i in 1:length(res10)){
  u11[i]<- res10[i]/sig11
}
u11

#Menghitung Pembobot
w11=weights(modelM10)
for(i in 1:15){
  if((abs(u11[i]))>1.345){w11[i]=1.345/abs(u11[i])}
  else w11[i]=1
}
w11

modelM11<-lm(Y~.,data=data, weights=w11)
summary(modelM11)

#menghitung nilai residu
res11<-modelM11$residuals

#Iterasi 12
#Menghitung sig
sig12<- median(abs(res11-median(res11)))/0.6745

#Menghitung nilai ui
u12<- vector()
for(i in 1:length(res11)){
  u12[i]<- res11[i]/sig12
}
u12

#Menghitung Pembobot
w12=weights(modelM11)
for(i in 1:15){
  if((abs(u12[i]))>1.345){w12[i]=1.345/abs(u12[i])}
  else w12[i]=1
}
w12

modelM12<-lm(Y~.,data=data, weights=w12)
summary(modelM12)