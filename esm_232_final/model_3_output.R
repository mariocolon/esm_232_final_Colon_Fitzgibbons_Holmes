#' #' @title Snowmelt Model - Output  
#' 
#' 
#' @param hm = water equivalent of snow (input)
#' @param hwret = liquid water content (θret, i.e constant) * snow depth (hs, input) , typical =.03
#' @param pw = density of water (constant), = 1000 kg m-3
#' @param lambda = latent heat of fusion (constant), = 0.334 MJ kg-1
#' 
#' 
#' @return Ripe i.e. pores filled 
#' 
#' @author Mario Colon & Eric Holmes 
#' 

snow_output = function(hm, hwret, pw, lambda) {
  Qm3 = (hm - hwret)*pw*lambda
  
  return(Qm3)
}
