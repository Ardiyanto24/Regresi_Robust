Jagung_Karanganyar
data = Jagung_Karanganyar[2:4]
data

#Regresi linear berganda
install.packages(car)
install.packages(lmtest)
mkt=lm(Y~X1+X2, data = data)
residumkt = resid(mkt)
residumkt 

#Uji Normalitas
shapiro.test(residumkt)

#Uji Homokedastisitas
bptest(mkt)

#Uji autokorelasi
dwtest(mkt)

#Uji Multikolinearitas
vif(mkt)

#Uji 
dffits(mkt)
nilai.pembanding.dffits= 2*(sqrt(3/15))
nilai.pembanding.dffits

