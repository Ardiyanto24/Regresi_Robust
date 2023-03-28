data

#Iterasi 1
mkt=lm(Y~.,data=data)
summary(mkt)

#Iterasi 2
residumkt=resid(mkt)
residukuadrat=(residumkt)^2
med_1=median(residukuadrat)
med_1
urutkan_1=arrange(data,residukuadrat)
h1=(183/2)+((6+1)/2)
h1 #86.5
data1=slice(urutkan_1,1:95)
mkt1=lm(Y~.,data=data1)
summary(mkt1)

#Itersi 3
residumkt1=resid(mkt1)
reskuadrat_1=residumkt1^2
sigmareskuadrat_1=sum(reskuadrat_1)
med_2=median(reskuadrat_1)
med_2
urutkan_2=arrange(data1,reskuadrat_1)
h2=(95/2)+((6+1)/2)
h2
data2=slice(urutkan_2,1:51)
mkt2=lm(Y~.,data=data2)
summary(mkt2)

# Iterasi 4
residumkt2=resid(mkt2)
reskuadrat_2=residumkt2^2
med_3=median(reskuadrat_2)
h3=(51/2)+((6+1)/2)
h3
med_3
urutkan_3=arrange(data2,reskuadrat_2)
data3=slice(urutkan_3,1:29)
mkt3=lm(Y~.,data=data3)
summary(mkt3)

# Iterasi 5
residumkt_3=resid(mkt3)
reskuadrat_3=residumkt_3^2
med_4=median(reskuadrat_3)
med_4
h4=(29/2)+((6+1)/2)
h4
urutkan_4=arrange(data3,reskuadrat_3)
data4=slice(urutkan_4,1:18)
mkt4=lm(Y~.,data=data4)
summary(mkt4)

#Iterasi 6
residumkt4=resid(mkt4)
reskuadrat_4=residumkt4^2
med_5=median(reskuadrat_4)
med_5
h5=(18/2)+((6+1)/2)
h5
urutkan_5=arrange(data4,reskuadrat_4)
data5=slice(urutkan_5,1:13)
mkt5=lm(Y~.,data=data5)
summary(mkt5)

#Iterasi 7
residumkt5=resid(mkt5)
reskuadrat_5=residumkt5^2
med_6=median(reskuadrat_5)
med_6
h6=(13/2)+((6+1)/2)
h6
urutkan_6=arrange(data5,reskuadrat_5)
data6=slice(urutkan_6,1:10)
mkt6=lm(Y~.,data=data6)
summary(mkt6)

#Iterasi 8
residumkt6=resid(mkt6)
reskuadrat_6=residumkt6^2
med_7=median(reskuadrat_6)
med_7
h7=(10/2)+((6+1)/2)
h7
urutkan_7=arrange(data6,reskuadrat_6)
data7=slice(urutkan_7,1:9)
mkt7=lm(Y~.,data=data7)
summary(mkt7)

# Iterasi 9
residumkt7=resid(mkt7)
reskuadrat_7=residumkt7^2
med_8=median(reskuadrat_7)
med_8
h8=(9/2)+((6+1)/2)
h8
urutkan_8=arrange(data7,reskuadrat_7)
data8=slice(urutkan_8,1:8)
mkt8=lm(Y~.,data=data8)
summary(mkt8)

#Iterasi 10
residumkt8=resid(mkt8)
reskuadrat_8=residumkt8^2
med_9=median(reskuadrat_8)
med_9
h9=(8/2)+((6+1)/2)
h9
urutkan_9=arrange(data8,reskuadrat_8)
data9=slice(urutkan_9,1:8)
mkt9=lm(Y~.,data=data9)
summary(mkt9)


# MENCARI PEMBOBOT LMS
sigma=1.4826*(1+(5/(183-6)))*sqrt(med_9)
sigma
psi_e=weights(mkt)
e.star=residumkt/sigma
for(i in 1:183){
  if(abs(e.star[i])<=2.5){psi_e[i]=e.star[i]}
  else if (e.star[i]>2.5){psi_e[i]=2.5}
  else psi_e[i]=(-2.5)
  }
psi_e
w=psi_e/e.star
w

#MODEL LMS
model.lms=lm(Y~.,data=data,weight=w)
summary(model.lms)
