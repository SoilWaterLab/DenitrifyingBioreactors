N_Removal_Rate <- function(reactor_data, Vs, ne=0.4){
# N_Removal_Rate.R
# Last edited by Chelsea K Morris 09/2015
#
# This code calculates the nitrate removal rate (mgN L-1 h-1) from the equation
# for water flow through porous media such as denitrifying bioreactor, as 
# described by Kadlec and Wallace (2009), and obtained from Ghane et al. (2015).
#
# Feed this code the concentration and volumetric flow data for the subset of
# time you'd like to calculate a rate from (i.e. daily,weekly, monthly, 
# seasonally, annually). The function delivers an average for the given time period.
#  
# This code can then be used to calculate nitrate flux (mgN h-1) from a
# bioreactor draining a certain field size, when the bioreactor is designed for
# a drainage field area.  The corresponding file name is N_Flux.R.

# Input:
#   reactor data is a data frame with 4 columns:
#      Co <- concentration at outlet of bioreactor, mg/L
#      Ci <- concentration at inlet of bioreactor, mg/L
#      do <- depth over weir at outlet of bioreactor, cm
#      di <- depth over weir at inlet of bioreactor, cm
#  
#   Vs <- saturated volume of denitrifying bed, L
#   ne <- effective porosity or drainable porosity of reactor material
  
  
# Check that all columns of loaded data is of the same length 
# and does not contain "NA" vaules

  if (nrow(na.omit(reactor_data)) < nrow(reactor_data)){
  print("Observations are of uneven length or contain NA's. NA's will be removed.")
  reactor_data = na.omit(reactor_data)
  }
  
  C_o = reactor_data$c_o #outlet concentration (mg/L)
  C_i = reactor_data$c_i #inlet concentration (mg/L)
  d_o = reactor_data$d_o #outlet depth over weir (cm)
  d_i = reactor_data$d_i #inlet depth over weir (cm)

# Discharge from depth over weir

# Relationship developed from in-situ calibration data located in 
# "Freeville DNBR Water Sample Data.xlsx"
  
  Q = vector() # Discharge from reactor (L/s)
  q_i = vector() # Discharge that bypasses reactor (L/s)

  for (i in 1:length(d_o)){
    if(d_o[i]<0){
      Q[i]=0
      }else{
      Q[i] =0.0108*(d_o[i]^2.3151)
      }
    if(d_i[i]<0){
      q_i[i]=0
    }else{
      q_i[i] =0.0108*(d_i[i]^2.3151) #Is this weir calibration curve different for the inlet weir?
    }
  }

# Convert discharge from L/s to L/h
  q_i = q_i*3600
  Q = Q*3600

# Compute the nitrate removal rate for each event

  Rno3 = vector()

  for (i in 1:length(C_o)){
    if(q_i[i]>0){
      is.na(Rno3)=c(i) #Return NA for events where inlet is bypassed
    }else{
      Rno3[i] = ((C_i[i]-C_o[i])*Q[i])/(Vs*ne) #nitrate removal rate(mg-N L-1 h-1)
    }
  }
N_removal = data.frame(Date_Time=reactor_data$Date_Time, R_no3_mgLh=Rno3, Q_Lh=Q)
}