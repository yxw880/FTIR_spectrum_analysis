library(hyperSpec)
library(baseline)
library(reshape2)
library(ggplot2)


##################################################################################################
##sa30151
files = Sys.glob("V:/vuv-data/proj/15PREDICTS2-BS/BP1/Task2/FTIR/NREL_Coupon_SubSamples/sa30151_12/*.spc")
f1 <- read.spc(files[1])
dfout <- f1
for(f in 2:length(files)){
  cat("\n",files[f]) 
  curfile <- read.spc(files[f])
  dfout <- rbind(dfout,curfile)
}

for (i in 1:8) {
  cat("\n",files[i])
  
  bl <- baseline(dfout[i][[]], method="modpolyfit", degree=17)
  dfout[i][[]] <- getCorrected(bl) 
  
}  

dff <- as.t.df(dfout)
colnames(dff) = c("wavenumber","mn1", "mn2","mn3","mn4", "mn5","mn6","mn7", "mn8")

for (i in 2:ncol(dff)){
  #dff[,i] = dff[,i]/dff[2541,i] #1633cm-1
  dff[,i] = dff[,i]/dff[2721,i] #1465cm-1
}
df = melt(dff, id = "wavenumber")
ggplot(data = df, aes(x = wavenumber, y = value, color = variable)) +
  geom_line(size = 0.5)  + #ggtitle("PA/PA/PA coupon exposed in Xenon#1: PA core layer")+
  ylab("Absorbance")+ scale_x_continuous(breaks=seq(0,4000,200))












##############################################################################################
df1 = dff[,c(1,8,3,2)]
df1[,2] = df1[,2]/df1[,2][2538]
df1[,3] = df1[,3]/df1[,3][2538]
df1[,4] = df1[,4]/df1[,4][2538]
df1[,5] = df1[,5]/df1[,5][2538]
df = melt(df1, id = "wavenumber")
ggplot(data = df, aes(x = wavenumber, y = value, color = variable)) +
  geom_line(size = 0.5)  + ggtitle("PA/PA/PA coupon exposed in Xenon#1: PA core layer")+
  ylab("Absorbance")+ scale_x_continuous(breaks=seq(0,4000,200))


df1 = dff[,c(1,14,6,15 )]
df1[,2] = df1[,2]/df1[,2][2538]
df1[,3] = df1[,3]/df1[,3][2538]
df1[,4] = df1[,4]/df1[,4][2538]
#df1[,5] = df1[,5]/df1[,5][2538]
df = melt(df1, id = "wavenumber")
ggplot(data = df, aes(x = wavenumber, y = value, color = as.factor(variable))) +
           geom_line()  + ggtitle("PA/PA/PA coupon exposed in Xenon#1: PA inner layer")+
          ylab("Absorbance")+ scale_x_continuous(breaks=seq(0,4000,200))


df1 = dff[,c(1,11,5,15)]
df1[,2] = df1[,2]/df1[,2][2538]
df1[,3] = df1[,3]/df1[,3][2538]
df1[,4] = df1[,4]/df1[,4][2538]
#df1[,5] = df1[,5]/df1[,5][2538]
#df1[,6] = df1[,6]/df1[,6][2538]
#df1[,7] = df1[,7]/df1[,7][2538]
#df1[,8] = df1[,8]/df1[,8][2538]
df = melt(df1, id = "wavenumber")
ggplot(data = df, aes(x = wavenumber, y = value, color = as.factor(variable))) +
  geom_line(size = 0.5)  + ggtitle("PA/PA/PA coupon exposed in Xenon#1: PA outer layer")+
  ylab("Absorbance")+ scale_x_continuous(breaks=seq(0,4000,200))
#################################################################################

files = Sys.glob("V:/vuv-data/proj/15PREDICTS2-BS/BP2/Data/FTIR/sa31004*.spc")
f1 <- read.spc(files[1])
dfout <- f1
for(f in 2:length(files)){
  cat("\n",files[f]) 
  curfile <- read.spc(files[f])
  dfout <- rbind(dfout,curfile)
}

for (i in 1:11) {
  cat("\n",files[i])
  
  bl <- baseline(dfout[i][[]], method="modpolyfit", degree=17)
  dfout[i][[]] <- getCorrected(bl) 
  
}  

dff <- as.t.df(dfout)

colnames(dff) = c("wavenumber","step0-EVA", "step0-PET","step0-PVF","step7-EVA", "step7-PET","step7-PVF","step4-EVA", "step4-PET","step4-PVF","step4-EVA inner","step4-PET core")
df1 = dff[,c(1,2,11,5)]
df1[,2] = df1[,2]/df1[,2][3521]
df1[,3] = df1[,3]/df1[,3][3521]
df1[,4] = df1[,4]/df1[,4][3521]
df1[,5] = df1[,5]/df1[,5][3521]
library(reshape2)
df = melt(df1, id = "wavenumber")
ggplot(data = df, aes(x = wavenumber, y = value, color = variable)) +xlab("Wavenumber (cm-1)")+
  geom_line(size = 0.8)  + ggtitle("PVF/PET/EVA coupon exposed in Xenon#1: EVA inner layer")+
  ylab("Absorbance (a.u.)")+ scale_x_reverse(breaks=seq(4000,0,-200)) +scale_color_discrete(name = "Exposure Hour", label = c("0 hr","2000 hrs","4000 hrs"))

df1[,2] = df1[,2]-df1[,2]
df1[,3] = df1[,3]-df1[,2]
df1[,4] = df1[,4]-df1[,2]

df = melt(df1, id = "wavenumber")
ggplot(data = df, aes(x = wavenumber, y = value, color = variable)) +
  geom_line(size = 0.8)  + ggtitle("PVF/PET/EVA coupon exposed in Xenon#1: EVA inner layer")+
  ylab("Absorbance")+ scale_x_continuous(breaks=seq(0,4000,200))



######################################################################################################
df1 = dff[,c(1,3,9,6)]
df1[,2] = df1[,2]/df1[,2][2782]
df1[,3] = df1[,3]/df1[,3][2782]
df1[,4] = df1[,4]/df1[,4][2782]

df = melt(df1, id = "wavenumber")
ggplot(data = df, aes(x = wavenumber, y = value, color = variable)) +
  geom_line(size = 0.8)+ scale_x_continuous(breaks=seq(0,4000,200)) + ggtitle("PVF/PET/EVA coupon exposed in Xenon#1: PET core layer")+
  ylab("Absorbance")

df1[,2] = df1[,2]-df1[,2]
df1[,3] = df1[,3]-df1[,2]
df1[,4] = df1[,4]-df1[,2]
df = melt(df1, id = "wavenumber")
ggplot(data = df, aes(x = wavenumber, y = value, color = variable)) +
  geom_line(size = 0.8)+ scale_x_continuous(breaks=seq(0,4000,200)) + ggtitle("PVF/PET/EVA coupon exposed in Xenon#1: PET core layer")+
  ylab("Absorbance")
#################################################################################################3

df1 = dff[,c(1,4,10,7)]
df1[,2] = df1[,2]/df1[,2][3125]
df1[,3] = df1[,3]/df1[,3][3125]
df1[,4] = df1[,4]/df1[,4][3125]

df = melt(df1, id = "wavenumber")
ggplot(data = df, aes(x = wavenumber, y = value, color = variable)) +xlab("Wavenumer (cm-1)")
  geom_line(size = 0.8)+ scale_x_reverse(breaks=seq(4000,0,-200)) +scale_color_discrete(name = "Exposure Hour", label = c("0 hr","2000 hrs","4000 hrs")) + ggtitle("PVF/PET/EVA coupon exposed in Xenon#1: PVF outer layer")+
  ylab("Absorbance (a.u.)")

df1[,2] = df1[,2]-df1[,2]
df1[,3] = df1[,3]-df1[,2]
df1[,4] = df1[,4]-df1[,2]
df = melt(df1, id = "wavenumber")

ggplot(data = df, aes(x = wavenumber, y = value, color = variable)) +
  geom_line(size = 0.8)+ scale_x_continuous(breaks=seq(0,4000,200)) + ggtitle("PVF/PET/EVA coupon exposed in Xenon#1: PVF outer layer")+
  ylab("Absorbance")
