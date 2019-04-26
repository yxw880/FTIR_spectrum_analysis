

source("C:/Users/wangy/Documents/Git/18-matls/scripts/bsm-FTIR_baseline_function.R")


dir = "C:/Users/wangy/Downloads/BS_Indoor/Xenon5/FTIR/PPE/"
ftir.files <- list.files(path = dir, pattern = "*.spc")

dir = "C:/Users/wangy/Downloads/BS_outdoot/retreived/FTIR/PET/outer-layer/"
ftir.files <- list.files(path = dir, pattern = "*.spc")
#-----------------------------------baseline----------------------------------
## read in each spectrum and baseline for individual one,
## the degree of baseline is decided by human
## after baseline, the data will be saved as txt file in the "baseline" folder
library(stringr)
library(hyperSpec)

for (i in 1:length(ftir.files)){
  
  # Read FTIR data with ".csv" format
  # comment this script for ".spc" format spectra
  
  #data <- read.csv(paste(dir,ftir.files[i],sep = "/"))
  # 
  #data <- data[which(data[1] > 600 & data[,1] < 1900), ]
  # 
  #ftir <- new("hyperSpec", spc = data[,2], wavelength = data[,1])
  
  # Read FTIR with ".spc" format
  
  ftir <- read.spc(paste(dir, ftir.files[i], sep = "/"))
  
  ftir <- ftir[,,c(700~1900)]
  #ftir <- ftir[,,c(2400~3800)]
  
  #ftir$spc <- ftir$spc - min(ftir[,,2400~2450])
  
  # baseline correction, depends on raw data, can be commented if not needed
  
  spc.bl <- ftir_baseline(spc= ftir, method = "polynomial", noise = 0, order = 3)
  
  #spc.bl <- ftir_baseline(spc= ftir, method = "rubberband", noise = 0)
  
}

#--------------------------------normalize----------------------------------
library(stringr)
library(hyperSpec)

kpe_peak <- c(763, 793,840,854, 870,975, 1069, 1149, 1184, 1209, 1275, 1383,1405,1423, 1715)
ppe_peak <- c(849,872, 880, 898, 935, 972, 1017,1024, 1044, 1098, 1120, 1080, 1174, 1223,1247, 1260,1340,1370,1425, 1458, 1506, 1578, 1685, 1713, 1775)
tpe_peak <- c(831, 888,1033,1092,1144,1232,1351,1368,1410,1427)
aaa_peak <-c(1120, 1370,1437,1540,1633,1710, 3420, 3300, 2919, 2860)

source("C:/Users/wangy/Documents/Git/18-matls/scripts/pac-findPeak.R")

dir = "C:/Users/wangy/Downloads/BS_Indoor/Xenon4/FTIR/PPE/baseline"

ftir.files <- list.files(path = dir, pattern = "*.txt")

dir = "C:/Users/wangy/Downloads/BS_Indoor/FTIR/AAA"
ftir.files <- list.files(path = dir, pattern = "*.txt")


dir = "C:/Users/wangy/Downloads/BS_outdoot/retreived/FTIR/PET/inner-layer/baseline"
ftir.files <- list.files(path = dir, pattern = "*.txt")

rm(ftir)
for (i in 1:length(ftir.files)){
  
  #m <- regexpr("sa[0-9]{5}_[0-9]{2}",ftir.files[i])
  inf <- strsplit(ftir.files[i], split = "[.]")[[1]][1]
  
  sa <- strsplit(ftir.files[i], split = "-")[[1]][1]
  
  # Get exposure step
  
  es <- as.numeric(gsub(".*?([0.0-9.9]+).*", "\\1", str_split(ftir.files[i],"-")[[1]][2]))
  
  # Get measurement number
  
  mn <- as.numeric(gsub(".*?([0.0-9.9]+).*", "\\1", str_split(ftir.files[i],"-")[[1]][4]))
  
  # Get exposure from key file
  
  exp <- gsub("_","-",strsplit(inf, split = "-")[[1]][5])
  
  if(exp == "Damp-Heat"){
    exp <- "Damp Heat"}else{
      exp <- exp
    }
  
  # Get material from key file
  
  mat <- strsplit(sa, split = "_")[[1]][1]
  
  # Get exposure time from key file
  
  time <- es*500
  
  
  rowkey <- strsplit(ftir.files[i], split = "[.]")[[1]][1]
  
  # Read FTIR data
  
  data <- read.delim(paste(dir,ftir.files[i],sep = "/"))
  
  #data <- read.csv(paste(dir,ftir.files[i],sep = "/"))
  #data <- subset.data.frame(data, data[,1] > 700)
  #data[,2] = data[,2] - min(data[,2])
  
  #a <- which(data[,1]<2700&data[,1]>2500)
  #data[,2] = data[,2] - mean(data[a,2])
  
  if (exists("ftir") == FALSE){
    ftir <- new("hyperSpec", spc = data[,2], wavelength = data[,1])
  
    #ftir <- read.spc(paste(dir, ftir.files[i], sep = "/"))
    
    #ftir <- svn(ftir)
    
    #ftir$spc <- ftir$spc - min(ftir)
    
    #ftir$spc <- ftir$spc - findValley(spectrum, 820,5)[2]
    
    # Add meta data to hyperSpec object
    
    ftir@data$Sample <- sa
    
    ftir@data$Exposure <- exp
    
    ftir@data$Material <- mat
    
    ftir@data$Time <- time
    
    ftir@data$Meas.num <- mn
    
    ftir@data$rowkey <- rowkey
    
    # normalization for each type of backsheet
    
    if(mat == "sa31001" | mat == "sa31006" | mat == "sa31010"){##PVDF
      peak <- 877
      ftir$spc <- ftir$spc/findPeak(ftir, peak)[2] 
      peaks <- kpe_peak
    }else if(mat == "sa31003" | mat == "sa31007" | mat == "sa31011"){##PET
      peak <- 1410
      ftir$spc <- ftir$spc/findPeak(ftir, peak)[2]
      peaks <- ppe_peak
    }else if(mat == "sa31004" | mat == "sa31008" | mat == "sa31012"){##PVF
      peak <- 1092
      ftir$spc <- ftir$spc/findPeak(ftir, peak)[2]
      peaks <- tpe_peak
    }else if(mat == "sa31005" | mat == "sa31009" | mat == "sa31013"){##PA
      peak <- 1633
      ftir$spc <- ftir$spc/findPeak(ftir, peak)[2]
      peaks <- aaa_peak
    }else{
      peak <- 2850
      ftir$spc <- ftir$spc/findPeak(ftir, peak)[2]
      peaks <- ppe_peak  ## manually decide for retreived backsheets
    }
     
    #find peak intensity after normalization for each spectrum
    res_peak <- NA
    for(j in 1:length(peaks)){
      peak_it <- findPeak(ftir, peaks[j], error = 5)[2]
      res_peak <-cbind(res_peak, peak_it)

      colnames(res_peak)[j+1] <- paste("I",peaks[j], sep="_")
    }

     res_peak <- data.frame(res_peak)
     res_peak <- res_peak[,-1]
     res_peak$time <- time
     res_peak$rowkey <- rowkey
     
    
  } else{
    spectrum <- new("hyperSpec", spc = data[,2], wavelength = data[,1])
    
    #spectrum  <- read.spc(paste(dir, ftir.files[i], sep = "/"))
  
    #spectrum <- svn(spectrum)
    
    #spectrum$spc <- spectrum$spc - min(spectrum)
    
    #spectrum$spc <- spectrum$spc - findValley(spectrum, 820,5)[2] ##use one specific valley value to baseline
    
    # Add meta data to hyperSpec object
    
    spectrum@data$Sample <- sa
    
    spectrum@data$Exposure <- exp
    
    spectrum@data$Material <- mat
    
    spectrum@data$Time <- time
    
    spectrum@data$Meas.num <- mn
    
    spectrum@data$rowkey <- rowkey
    
    # normalization for each type of backsheet

    if(mat == "sa31001" | mat == "sa31006" | mat == "sa31010"){##PVDF
      peak <- 877
      spectrum$spc <- spectrum$spc/findPeak(spectrum, peak)[2]
      peaks <- kpe_peak
    }else if(mat == "sa31003" | mat == "sa31007" | mat == "sa31011"){##PET
      peak <- 1410
      spectrum$spc <- spectrum$spc/findPeak(spectrum, peak)[2]
      peaks <- ppe_peak
    }else if(mat == "sa31004" | mat == "sa31008" | mat == "sa31012"){##PVF
      peak <- 1092
      spectrum$spc <- spectrum$spc/findPeak(spectrum, peak)[2]
      peaks <- tpe_peak
    }else if(mat == "sa31005" | mat == "sa31009" | mat == "sa31013"){##PA
      peak <- 1633
      spectrum$spc <- spectrum$spc/findPeak(spectrum, peak)[2]
      peaks <- aaa_peak
    }else{
      peak <- 2850
      spectrum$spc <- spectrum$spc/findPeak(spectrum, peak)[2]
      peaks <- ppe_peak  ## manually decide for retreived backsheets
    }
    
    res_peak_temp <- NA
    for(j in 1:length(peaks)){
      peak_it <- findPeak(spectrum, peaks[j],error = 5)[2]
      res_peak_temp <-cbind(res_peak_temp, peak_it)
      colnames(res_peak_temp)[j+1] <- paste("I",peaks[j], sep="_")
    }
    #
    res_peak_temp <- data.frame(res_peak_temp)
    #
    res_peak_temp <- res_peak_temp[,-1]
    #
    res_peak_temp$time <- time

    res_peak_temp$rowkey <- rowkey
    #
    res_peak <- rbind(res_peak, res_peak_temp)
    
    # Combine spectrum with other spectra
    
    
    # Adjust the wavenumber of different spectra
    # if(length(spectrum@wavelength) > length(ftir@wavelength)){
    #   spectrum <- spc.loess(spectrum,ftir@wavelength)
    # }else{
    #   if(length(spectrum@wavelength) < length(ftir@wavelength)){
    #     ftir <- spc.loess(ftir,spectrum@wavelength)
    #   }else{
    #     spectrum <- spectrum
    #   }
    # }

    ftir <- collapse(ftir,spectrum)
  }
  
}



# Order Wavelengths

ftir <- orderwl(ftir)

##Xenon3, PET
#ftir <- subset(ftir, rowkey != "sa31007_10-es01-ms01-mn02" & rowkey != ftir$rowkey[126])

# Plot Spectra with ggplot2 graphics

exposure_names <- c(
  `x1` = "Xenon-1",
  `x3` = "Xenon-3",
  `x4` = "Xenon-4"
)

manupalette <- c("#b563ec",
                 "#439f00",
                 "#624488",
                 "#917c00",
                 "#6ebaff",
                 "#ffaa78",
                 "#00694c",
                 "#9d1c4c",
                 "#ff9da7") ##set manual color palette

ftir$Sample <- gsub("_","-", ftir$Sample)

ftir$cry <- c("beta","alpha","alpha","beta","beta")##for outdoor pvdf

qplotspc(ftir, mapping = aes (x = .wavelength, y = spc, color = as.factor(Time), group = .rownames), spc.nmax = length(ftir)) +
  geom_line(size=1)+ facet_wrap(.~Exposure) +
  xlab (expression(Wavenumber~(cm^{-1}))) + ylab("Normalized Absorbance (a.u.)") +
  geom_line(size=1) + scale_x_reverse(limits = c(1800,800), breaks = seq(4000,0,-200))+ scale_colour_discrete(name = "Exposure\nTime (hours)")+ theme_bw()+ylim(-0.05, 1.2)+
  theme(legend.title = element_text(size = 20,face = "bold"), 
        legend.text = element_text(size = 20,face = "bold"),
        legend.position = "top",
        axis.text = element_text(size = 17),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title = element_text(size = 20,face = "bold"),
        strip.text= element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 20,face = "bold"))+ annotate("segment", x = 1635, xend = 1635, y = 1.2, yend = 1.05, colour = "blue", size=2, alpha=1, arrow=arrow())



equire(RColorBrewer)##manually choose ggplot palette color

qplotspc(subset(ftir, Time < 2200), mapping = aes (x = .wavelength, y = spc, color = as.factor(Time), group = .rownames), spc.nmax = length(ftir)) +
  xlab ("Wavenumber") + ylab("a.u.") +
  geom_line(size=1)+
  xlab (expression(Wavenumber~(cm^{-1}))) + ylab("Absorbance") +
  geom_line(size=1) + scale_x_continuous(limits = c(800,1900), breaks = seq(100,4000,100))+ theme_dark()+ylim(-0.5, 6.5)+scale_colour_manual(name = "Exposure\nTime (hrs)",values = brewer.pal(8,"Greens")[c(1,3,4,5,7)])+
  theme(legend.title = element_text(size = 15,face = "bold"), 
        legend.text = element_text(size = 20,face = "bold"),
        legend.position = "top",
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0, hjust = 1),
        axis.title = element_text(size = 20,face = "bold"),
        strip.text= element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 20,face = "bold"))+ annotate("segment", x = 1410, xend = 1410, y = 2.5, yend = 1.2, colour = "orange", size=3, alpha=1, arrow=arrow())



##---------------------retrieved backsheets-------------------------------
out <- length(unique(ftir$Sample))
out <- list()

for(i in 1:length(unique(ftir$Sample))){
   name <- paste("PA",i, sep = "-")
   if (i == length(unique(ftir$Sample))){
     name <- "Core layer"
   }
   out <- cbind(out, name)
}

##PET
plot(ftir, stacked= TRUE, wl.reverse = TRUE ,wl.range = c (800 ~ 1800,2700~3400), col = rainbow(7, start = 0.4, end = 1), xoffset = 750, title.args = list (xlab = expression (Wavenumber~(cm^{-1})), ylab = "Normalized Absorbance (a.u.)",cex.lab=1.5,cex.axis=25), plot.args = list (ylim = c (0, 35)))


legend(2500,38, gsub("_","-", ftir$Sample), lty=1, col=rainbow(7, start = 0.4, end = 1), bty='n', cex=0.85,ncol=4)


##PA
plot (ftir, stacked= TRUE, wl.reverse = TRUE ,wl.range = c (800 ~ 1800,2700~3400), col = rainbow(9, start = 0.4, end = 1), xoffset = 750, title.args = list (xlab = expression (Wavenumber~(cm^{-1})), ylab = "Normalized Absorbance (a.u.)",cex.lab=1.5,cex.axis=25), plot.args = list (ylim = c (0, 12)))


legend(2500,13, gsub("_","-", ftir$Sample), lty=1, col=rainbow(9, start = 0.4, end = 1), bty='n', cex=0.85,ncol=4)

plot (ftir,  wl.reverse = TRUE ,wl.range = c (850 ~ 1800,2700~3500), col = rainbow(9, start = 0.4, end = 1), xoffset = 750, title.args = list (xlab = expression (Wavenumber~(cm^{-1})), ylab = "Normalized Absorbance (a.u.)",cex.lab=1.5,cex.axis=25), plot.args = list (ylim = c (0, 1.6)))

legend(2500,1.6, gsub("_","-", ftir$Sample), lty=1, col=rainbow(9, start = 0.4, end = 1), bty='n', cex=0.85,ncol=4)


##PVDF
plot (ftir, stacked= TRUE, wl.reverse = TRUE ,wl.range = c (800 ~ 1800), col = rainbow(5, start = 0.4, end = 1), xoffset = 750, title.args = list (xlab = expression (Wavenumber~(cm^{-1})), ylab = "Normalized Absorbance (a.u.)",cex.lab=1.5,cex.axis=25), plot.args = list (ylim = c (0, 6)))

legend(1000,6.5, gsub("_","-", ftir$Sample), lty=1, col=rainbow(5, start = 0.4, end = 1), bty='n', cex=0.85,ncol=4)


##PVF
plot (ftir, stacked= TRUE, wl.reverse = TRUE ,wl.range = c (800 ~ 1800), col = rainbow(10, start = 0.4, end = 1), xoffset = 750, title.args = list (xlab = expression (Wavenumber~(cm^{-1})), ylab = "Normalized Absorbance (a.u.)",cex.lab=1.5,cex.axis=25), plot.args = list (ylim = c (0, 14)))

legend(1100,15, gsub("_","-", ftir$Sample), lty=1, col=rainbow(10, start = 0.4, end = 1), bty='n', cex=0.85,ncol=5)

##THV
plot (ftir, stacked= TRUE, wl.reverse = TRUE , col = rainbow(3, start = 0.4, end = 1), title.args = list (xlab = expression (Wavenumber~(cm^{-1})), ylab = "Normalized Absorbance (a.u.)",cex.lab=1.5,cex.axis=25), plot.args = list (ylim = c (0, 4.5)))

legend(1700,4.5, gsub("_","-", ftir$Sample), lty=1, col=rainbow(3, start = 0.4, end = 1), bty='n', cex=0.85,ncol=5)

ftir_cp1 <- subset(ftir, Time <600)

ftir_cp2 <- subset(ftir, Time >600)


##create mean spc hyperspec
#spc_mean <- aggregate(ftir, ftir$Time, mean)

rm(spc_mean)
for(i in unique(ftir$Exposure)){
  temp <- subset(ftir, Exposure == i)
  spc_mean_temp <- aggregate(temp, temp$Time, mean)
  if (exists("spc_mean") == FALSE){
    spc_mean <- spc_mean_temp 
  }else{
    spc_mean <- collapse(spc_mean, spc_mean_temp )
  }
}

qplotspc(spc_mean, mapping = aes (x = .wavelength, y = spc, color = as.factor(Time), group = .rownames), spc.nmax = length(ftir)) + facet_wrap(.~Exposure) +
  xlab (expression(Wavenumber~(cm^{-1}))) + ylab("Normalized Absorbance (a.u.)") +
  geom_line(size=1) + scale_x_reverse(limits = c(1800,700), breaks = seq(4000,0,-200))+ scale_colour_discrete(name = "Exposure\nTime (hours)")+ ylim(-0.02,1.2)+ theme_bw()+
  theme(legend.title = element_text(size = 15,face = "bold"), 
        legend.text = element_text(size = 20,face = "bold"),
        legend.position = "top",
        axis.text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title = element_text(size = 20,face = "bold"),
        strip.text= element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 20,face = "bold"))+ annotate("segment", x = 870, xend = 870, y = 1.2, yend = 1.05, colour = "blue", size=2, alpha=1, arrow=arrow())


plotspc(spc_mean,
         col = as.factor(Time),
         stacked = TRUE)

res_peak$coupon <- lapply(res_peak$rowkey, function(x) strsplit(strsplit(x, split = "-")[[1]][1], split = "_")[[1]][2])

res_peak$exposure <- lapply(res_peak$rowkey, function(x) strsplit(x, split = "-")[[1]][5])

res_peak$exposure <- as.character(res_peak$exposure)
res_peak$coupon <- as.character(res_peak$coupon)
res_peak$exposure <- gsub("_","-", res_peak$exposure)
res_peak$exposure <- gsub("Damp-Heat","Damp Heat", res_peak$exposure)
res_peak$beta_percentage <- res_peak$I_840/(res_peak$I_763 * 1.26 + res_peak$I_840) * 100

ggplot(res_peak, aes(time, beta_percentage,group= time, fill =as.factor(time))) +geom_boxplot()+xlab("Exposure Time (hours)") + ylab(expression(beta~"phase/Total crystal phase (%)"))+facet_wrap(.~exposure, scales = "free")+geom_jitter(height = 0, width = 0.1, alpha=0.75)+
  theme(legend.title = element_text(size = 15,face = "bold"), 
        legend.text = element_text(size = 20,face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 20, hjust = 1),
        axis.title = element_text(size = 20,face = "bold"),
        strip.text= element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 20,face = "bold"))


##----------------------plot the peak intensity----------------------------
res_peak_all <- res_peak

res_peak <- subset.data.frame(res_peak, time < 1600)
#res_peak$crystallinity <- res_peak$I_1120/res_peak$I_1098 ##PET

res_peak$beta_percentage <- res_peak$I_840/(res_peak$I_763 * 1.26 + res_peak$I_840) * 100

res_plot <- reshape::melt.data.frame(res_peak, id.vars = c("rowkey", "time"), measure.vars = c(1:15, 18))


res_plot$exposure <- 


ggplot(subset(res_peak, variable != "I_898" & variable != "I_1174" & variable != "I_1370" & variable != "I_1386" & variable != "I_1458"  & variable != "I_1102" & variable != "I_1123"), aes(time/1000, value, col = as.factor(time) )) + geom_point() + facet_wrap(~variable,  scales = "free") + xlab("Exposure Time (x1000 hrs)") + ylab("Peak intensity")+
  theme(legend.title = element_text(size = 15,face = "bold"), 
        legend.text = element_text(size = 20,face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0, hjust = 1),
        axis.title = element_text(size = 20),
        strip.text= element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 20,face = "bold"))

ggplot(subset(res_plot, variable != "I_898" & variable != "I_1174" & variable != "I_1370" & variable != "I_1386" & variable != "I_1458"  & variable != "I_1102" & variable != "I_1123"), aes(time/1000, value, fill = as.factor(time) )) + geom_boxplot(aes(group = time)) + facet_wrap(~variable,  scales = "free") + xlab("Exposure Time (x1000 hrs)") + ylab("Peak intensity")+
  theme(legend.title = element_text(size = 15,face = "bold"), 
        legend.text = element_text(size = 20,face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0, hjust = 1),
        axis.title = element_text(size = 20),
        strip.text= element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 20,face = "bold"))
##without1174: too weak peak




##plot peak intensity change with YI change
res_peak$Hours = res_peak$time/1000
res_peak <- res_peak[,c(1:28,30:32)]
res_peak_yi <- merge.data.frame(res_peak, t_df, by = c("rowkey","Hours"))

res_peak_yi <- res_peak_yi[,c(1:31,43)]
res_plot <- reshape::melt.data.frame(res_peak_yi, id.vars = c("rowkey", "Hours"), measure.vars = c(3:32))

res_plot$coupon <- lapply(res_plot$rowkey, function(x) strsplit(strsplit(x, split = "-")[[1]][1], split = "_")[[1]][2])

res_plot$coupon <- as.character(res_plot$coupon)

ggplot(subset(res_plot, variable != "I_898" & variable != "I_1174" & variable != "I_1370" & variable != "I_1386" & variable != "I_1458"  & variable != "I_1102" & variable != "I_1123"), aes(time/1000, value, col = as.factor(time) )) + geom_point() + facet_wrap(~variable,  scales = "free") + xlab("Exposure Time (x1000 hrs)") + ylab("Peak intensity")+
  theme(legend.title = element_text(size = 15,face = "bold"), 
        legend.text = element_text(size = 20,face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0, hjust = 1),
        axis.title = element_text(size = 20),
        strip.text= element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 20,face = "bold"))

#principal component analysis
library("factoextra")

prin_comp <- prcomp(res_peak[,c(-23)], scale. = T)

fviz_eig(prin_comp, addlabels = TRUE, ylim = c(0, 50))

##biplot of variable and individual sample
fviz_pca_biplot(prin_comp, label="var", habillage=res_peak$time, legend.title = "Exposure\nTime (hrs)")+ 
  theme(legend.title = element_text(,size = 15), 
        legend.text = element_text(size = 20),
        legend.position = "top", axis.text = element_text(size = 20),
        axis.title = element_text(size = 25))

##individual sample
fviz_pca_ind(prin_comp, label="none", habillage=res_peak$time,addEllipses=TRUE, ellipse.level=0.95)

##variable 
fviz_pca_var(prin_comp, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=3)

set.seed(123)
var <- get_pca_var(prin_comp)
head(var$coord, 4)
res.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)


fviz_pca_var(prin_comp, col.var=grp) + 
  theme(legend.position = "none", axis.text = element_text(size = 20),
        axis.title = element_text(size = 25))
##save size: 650*650



##step 1
step1 <- subset.data.frame(res_peak, time < 600)
prin_comp <- prcomp(step1[,c(-23)], scale. = T)

fviz_eig(prin_comp, addlabels = TRUE, ylim = c(0, 50))

##biplot of variable and individual sample
fviz_pca_biplot(prin_comp, label="var", habillage=step1$time, addEllipses =TRUE, legend.title = "Exposure\nTime (hrs)")+ theme(legend.title = element_text(size = 15), 
        legend.text = element_text(size = 20),
        legend.position = "top", axis.text = element_text(size = 20),
        axis.title = element_text(size = 25)) 

##individual sample
fviz_pca_ind(prin_comp, label="none", habillage=step1$time,addEllipses=TRUE, ellipse.level=0.95)

##variable 
fviz_pca_var(prin_comp, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=3)

set.seed(123)
var <- get_pca_var(prin_comp)
head(var$coord, 4)
res.km <- kmeans(var$coord, centers = 4, nstart = 25)
grp <- as.factor(res.km$cluster)


fviz_pca_var(prin_comp, col.var=grp) + 
  theme(legend.position = "none", axis.text = element_text(size = 20),
        axis.title = element_text(size = 25))
##save size: 650*650