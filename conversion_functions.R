#Functions for unit conversion



### Dissolved oxygen
#calculate oxygen from saturation (as Yui does). 
#constants from Yui (Benson and Krause 1994)

#apply oxygen salinity correction
oxy_umolkg <- function(temperature_celsius, O2_sat, salinity){
  
  A0 = 5.80818
  A1 = 3.20684
  A2 = 4.11890
  A3 = 4.93845
  A4 = 1.01567
  A5 = 1.41575
  B0 = -7.01211e-3
  B1 = -7.25958e-3
  B2 = -7.93334e-3
  B3 = -5.54491e-3
  C0 = -1.32412e-7
  
  Temp = log((298.15-temperature_celsius)/(273.15+temperature_celsius))
  
  O2_sat/100 * exp((A0 + A1*Temp + A2*Temp^2 + A3*Temp^3 + A4*Temp^4 + A5*Temp^5)
                   + salinity*(B0 + B1*Temp + B2*Temp^2 + B3*Temp^3)
                   + C0*salinity^2) * (1/1.025)
}



oxy_saturation <- function(temperature_celsius, do_umolkg, salinity){
  
  A0 = 5.80818
  A1 = 3.20684
  A2 = 4.11890
  A3 = 4.93845
  A4 = 1.01567
  A5 = 1.41575
  B0 = -7.01211e-3
  B1 = -7.25958e-3
  B2 = -7.93334e-3
  B3 = -5.54491e-3
  C0 = -1.32412e-7
  
  #Correct/convert the temperature values... need the reference to understand this.
  Temp = log((298.15-temperature_celsius)/(273.15+temperature_celsius))
  
  #Solve for do_sat instead of do_umolkg
  100 * do_umolkg/exp((A0 + A1*Temp + A2*Temp^2 + A3*Temp^3 + A4*Temp^4 + A5*Temp^5)
                      + salinity*(B0 + B1*Temp + B2*Temp^2 + B3*Temp^3)
                      + C0*salinity^2)
}
