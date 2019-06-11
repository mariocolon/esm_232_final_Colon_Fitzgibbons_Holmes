#' @title Snowmelt Model - Warming 
#' 
#' @param ci = heat capacity of ice (constant), default = 2102 J kg -1 K-1
#' @param pw = density of water (constant), default = 1000 kg m-3
#' @param hm = water equivalent of snowpack (meters, input)
#' @param Ts = snowpack temp (C, input)
#' @param Tm = melting point of snow (C, constant), default = 0
#' 
#' 
#' @author Mario Colon & Eric Holmes 
#' 



snow_warm = function(hm, Ts, ci, pw, Tm) {
  Qcc = -ci*pw*hm*(Ts-Tm) 
  
  return(Qcc)
}

  

