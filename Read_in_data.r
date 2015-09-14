Read_in_data = function (dir, reactor_name){
# Read_in_data.R
# Last edited by Chelsea K Morris 09/2015
#
# This code reads in CSV files from the pathway given by the first variable "dir". These files should be created from the # template named "Template_SiteName_Data_For_Model.xlsx". The user of the function should organize the data into the
# template and save each sheet as a CSV file with the tab name appended to the title. 
#  
# For example, "Freeville_Data_For_Model_Nitrogen.csv" is the correct naming for the first sheet.
#
# The code then readies the data for the N_Removal_Rate function. Two reactor data sets (Woodchip & Biochar) are returned,
# along with water temperature, weather, and site data.
  
  Nitrogen= read.csv(paste(dir,"Data/", reactor_name, 
                           "_Data_For_Model_Nitrogen.csv", sep=""))
  Nitrogen$Date_Time = as.POSIXct(paste(Nitrogen$Date_YYMMDD, 
                                        Nitrogen$Time_24h, sep=" "), 
                                  format="%y%m%d %H:%M")

  WTemp = read.csv(paste(dir,"Data/", reactor_name, 
                         "_Data_For_Model_WaterTemperature.csv", sep=""))

  WTemp$Date_Time = as.POSIXct(paste(WTemp$Date_YYMMDD, 
                                     WTemp$Time_24h, sep=" "), 
                               format="%y%m%d %H:%M")

  WDepth = read.csv(paste(dir,"Data/", reactor_name, 
                          "_Data_For_Model_WaterDepth.csv", sep=""))

  WDepth$Date_Time = as.POSIXct(paste(WDepth$Date_YYMMDD, 
                                      WDepth$Time_24h, sep=" "),
                                format="%y%m%d %H:%M")

  Weather = na.omit(read.csv(paste(dir,"Data/", reactor_name, 
                                   "_Data_For_Model_Weather.csv", sep="")))

  Weather$Date_Time = as.POSIXct(paste(Weather$Date_YYMMDD, 
                                       Weather$Time_24h, sep=" "), 
                                 format="%y%m%d %H:%M")

  SiteData = read.csv(paste(dir,"Data/", reactor_name,
                            "_Data_For_Model_SiteData.csv", sep=""))
  
  # Prepare the data for feeding into the N Removal Rate Function
  # Creates separate data frames for woodchip and biochar reactors.
  # Each data frame includes
    #1. Date & Time, 
    #2. Inflow concentration (mg/L),
    #3. Outflow concentration (mg/L),
    #4  Depth over weir at Inflow (cm),
    #5  Depth over weir at Outflow (cm).
  
  inflow_match = match(Nitrogen$Date_Time[which(Nitrogen$Sampling_location=='I')], WDepth$Date_Time)
  inflow_data = data.frame(Date_Time = Nitrogen$Date_Time[which(Nitrogen$Sampling_location=='I')],
                           c_i=Nitrogen$Nitrate_mgL[which(Nitrogen$Sampling_location=='I')],
                           d_i=WDepth$Depth_Inflow_cm[inflow_match])
  woodchip_match = match(Nitrogen$Date_Time[which(Nitrogen$Sampling_location=='W')], WDepth$Date_Time)
  woodchip_only_data = data.frame(Date_Time = Nitrogen$Date_Time[which(Nitrogen$Sampling_location=='W')],
                                  c_o=Nitrogen$Nitrate_mgL[which(Nitrogen$Sampling_location=='W')],
                                  d_o=WDepth$Depth_outflow_Woodchip_cm[woodchip_match])
  biochar_match = match(Nitrogen$Date_Time[which(Nitrogen$Sampling_location=='B')], WDepth$Date_Time)
  biochar_only_data = data.frame(Date_Time = Nitrogen$Date_Time[which(Nitrogen$Sampling_location=='B')],
                                 c_o=Nitrogen$Nitrate_mgL[which(Nitrogen$Sampling_location=='B')],
                                 d_o=WDepth$Depth_outflow_Biochar_cm[biochar_match])
  
  woodchip_reactor_data = data.frame(Date_Time = woodchip_only_data$Date_Time,
                                     c_i=inflow_data$c_i[match(woodchip_only_data$Date_Time, inflow_data$Date_Time)],
                                     c_o=woodchip_only_data$c_o,
                                     d_i=inflow_data$d_i[match(woodchip_only_data$Date_Time, inflow_data$Date_Time)],
                                     d_o=woodchip_only_data$d_o)
  
  biochar_reactor_data = data.frame(Date_Time = biochar_only_data$Date_Time,
                                    c_i=inflow_data$c_i[match(biochar_only_data$Date_Time, inflow_data$Date_Time)],
                                    c_o=biochar_only_data$c_o,
                                    d_i=inflow_data$d_i[match(biochar_only_data$Date_Time, inflow_data$Date_Time)],
                                    d_o=biochar_only_data$d_o)
  
  
return(list(woodchip_reactor_data, biochar_reactor_data, WTemp, Weather, SiteData))
}