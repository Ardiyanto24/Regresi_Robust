model.mkt=lm(Y~X, data = Sel_Darah_Merah)
summary(model.mkt)

#Uji Normalitas (Kolmogorov smirnov)
residu=resid(model.mkt)
ks.test(residu, "pnorm")

#Uji Homokedastisitas
bptest(model.mkt)

#Uji Autokorelasi
dwtest(model.mkt)

#Deteksi pencilan (DFFITS)
dffits(model.mkt)
nilai.pembanding.dffits= 2*(sqrt(2/13))
nilai.pembanding.dffits

summary(model.mkt)