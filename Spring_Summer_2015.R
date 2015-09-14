# Spring_Summer_2015.R
# Created by Chelsea K Morris

# The first part of this code calculates the nitrate removal rate and flux for the experimental bioreactors. The latter part of the code estimates the nitrate removal from some proposed bioreactors in a watershed based on the total area drained.

dir <-"~/Dropbox/WRI Grant/"
setwd(dir)

# Load Concentration (mg/L) and Flow (m3/s) Data from observations

reactor_name = "Chemung" #Freeville or Chemung?")

# Read in data files and prepare for removal function

source("Code/Read_in_data.r")
data_list = Read_in_data(dir, reactor_name) #Note: This will take a few seconds to complete

woodchip_reactor_data = data_list[[1]]
biochar_reactor_data = data_list[[2]]
WTemp = data_list[[3]]
Weather = data_list[[4]]
SiteData = data_list[[5]]

# Create data frames to pass to N_Removal_Rate function
Vs = SiteData$Sat_Volume_L
ne = SiteData$Effec_Porosity

# Load the removal function
source("Code/N_Removal_Rate.r")

Rno3_woodchip = N_Removal_Rate(woodchip_reactor_data, Vs, ne)
Rno3_biochar = N_Removal_Rate(biochar_reactor_data, Vs, ne)

# Results

# Constrain the precipitation record to first & laste date of bioreactor

begin = match(Rno3_Freeville$Date_Time[1], Weather$Date_Time)
Weather_subset = Weather[begin:length(Weather$Precipitation_Amt_cm),]

# Plot Nitrate Removal Rate on one axis and the precipitation on another

# Calculate the seasonal removal rate for these bioreactors

# Apply that rate to the number of acres in Greene County over the season and
# calculate the potential total mass of N removed.

# Presentation
Rno3_biochar$Type = "B"
Rno3_woodchip$Type = "W"

Rno3_Chemung = rbind(Rno3_biochar, Rno3_woodchip)

write.csv(Rno3_Chemung, "Data/Chemung_Removal_Rates.csv")
Rno3_Freeville=read.csv("Data/Freeville_Removal_Rates.csv")
Rno3_Freeville$Date_Time=as.POSIXct(Rno3_Freeville$Date_Time)
Rno3_Freeville=Rno3_Freeville[2:5]

settings= list(col_chemung=c("darkolivegreen", "blue4"), col_freeville=c("darkgoldenrod", "deeppink"))
plot(R_no3_mgLh~Date_Time, data=Rno3_Chemung,
     xlab="", ylab="Nitrate Removal Rate, mg/L per hour",
     main="",
     col=settings$col_chemung[as.factor(Rno3_Chemung$Type)],
     pch=16)
par(new=T)
plot(R_no3_mgLh~Date_Time, data=Rno3_Freeville,
     xlab="", ylab="", axes=F,
     col=settings$col_freeville[as.factor(Rno3_Freeville$Type)],
     pch=16)
legend("topleft", legend=c("Chemung:Biochar", "Chemung:Woodchip", "Tompkins:Biochar", "Tompkins:Woodchip"),
       col=c(settings$col_chemung, settings$col_freeville), pch=c(1,2), pt.cex=2, pt.lwd=3)
par(new=F)

Rno3_Chemung$Loc="C"
Rno3_Freeville$Loc="T"
Rno3=rbind(Rno3_Chemung,Rno3_Freeville)

settings= list(colors=c("darkolivegreen", "blue4"), shapes=c(16,8))
plot(R_no3_mgLh~Date_Time, data=Rno3,
     xlab="", ylab="Nitrate Removal Rate, mg/L per hour",
     main="",
     col=settings$colors[as.factor(Rno3$Type)],
     pch=as.numeric(settings$shapes[as.factor(Rno3$Loc)]))


plot(Weather_subset$Precipitation_Amt_cm~as.POSIXct(Weather_subset$Date_Time, format="%y%m%d %H:%M"), type='l', xlab="", ylab="Precipitation, cm")

settings= list(colors=c("darkolivegreen", "blue4"), shapes=c(16,8))
plot(Nitrate_mgL~Date_Time, data=Nitrogen, col=settings$colors[as.factor(Nitrogen$Sampling_location)], pch=20)

png(paste(dir,"/Maps & Figures/May11-13Conc.png", sep=""))
plot(Nitrogen$Nitrate_mgL[39:61]~Nitrogen$Date_Time[39:61], col="darkolivegreen",type='b', pch=20, ylim=c(0,max(Nitrogen$Nitrate_mgL[39:61])), cex=2,
     xlab="May 11 2015 to May 13 2015", ylab="Nitrate Concentration, mg/L")
par(new=T)
plot(Nitrogen$Nitrate_mgL[63:86]~Nitrogen$Date_Time[63:86], col="blue4",type='b', pch=20, ylim=c(0,max(Nitrogen$Nitrate_mgL[39:61])), cex=2, axes=F, xlab="", ylab="")
par(new=T)
plot(Nitrogen$Nitrate_mgL[87:108]~Nitrogen$Date_Time[87:108], col="darkgoldenrod",type='b', pch=20, ylim=c(0,max(Nitrogen$Nitrate_mgL[39:61])), cex=2, axes=F, xlab="", ylab="")
legend("right", legend=c("Inflow", "Biochar+Woodchip", "Woodchip"),pch=20, col=c("darkolivegreen", "blue4", "darkgoldenrod"))
par(new=F)
dev.off()

png(paste(dir,"/Maps & Figures/May26-28Conc.png", sep=""))
plot(Nitrogen$Nitrate_mgL[111:134]~Nitrogen$Date_Time[111:134], col="darkolivegreen",type='b', pch=20, ylim=c(0,max(Nitrogen$Nitrate_mgL[111:134])),
     xlab="May 26 2015 to May 28 2015", ylab="Nitrate Concentration, mg/L")
par(new=T)
plot(Nitrogen$Nitrate_mgL[135:158]~Nitrogen$Date_Time[135:158], col="blue4",type='b', pch=20, ylim=c(0,max(Nitrogen$Nitrate_mgL[111:134])), axes=F, xlab="", ylab="")
par(new=T)
plot(Nitrogen$Nitrate_mgL[159:182]~Nitrogen$Date_Time[159:182], col="darkgoldenrod",type='b', pch=20, ylim=c(0,max(Nitrogen$Nitrate_mgL[111:134])), axes=F, xlab="", ylab="")
par(new=F)
dev.off()

#Rno3_nonneg=subset(Rno3, Rno3$R_no3_mgLh>0)

png(paste(dir,"/Maps & Figures/RemovalHistogram_Chemung.png", sep=""))
hist(Rno3$R_no3_mgLh[which(Rno3$Loc=="C")],
     xlab="Nitrate Removal Rate, mg per liter bioreactor per hour",
     main="High Removal Rates at Site with Organic Fertilizer Application",
     col="darkolivegreen")
dev.off()
png(paste(dir,"/Maps & Figures/RemovalHistogram_Tompkins.png", sep=""))
hist(Rno3$R_no3_mgLh[which(Rno3$Loc=="T")],
     xlab="Nitrate Removal Rate, mg per liter bioreactor per hour",
     main="Low Removal Rates at Site with Inorganic Fertilizer Application",
     col="darkgoldenrod")
dev.off()

