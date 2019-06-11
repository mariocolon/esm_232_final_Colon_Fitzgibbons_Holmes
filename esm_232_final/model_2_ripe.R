#' @title Snowmelt Model - Ripening  
#' 
#' 
#' @param porosity = ratio of pore volume to total snowpack volume, for typical snowpack = .49
#' @param theta = liquid water equivalent 
#' @param hs = snow depth (input)
#' @param pw = density of water (constant), = 1000 kg m-3
#' @param lambda = latent heat of fusion (constant), = 0.334 MJ kg-1
#' @param pi = ice density (constant), = 917 kg m-3
#' 
#' 
#' @return Ripe i.e. pores filled 
#' 
#' @authors Mario Colon, Paige Fitzgibbon, and Eric Holmes 
#' 

snow_ripe = function(hs, porosity , theta, pw , lambda, pi) {
  
  #determine ps i.e. snow density
  ps = (1 - porosity)*pi + theta*pw
  
  #determine theta_ret i.e. liquid water equivalent 
  theta_ret =  -0.0745*(ps/pw) + 0.000267*(ps/pw)
  
  
  Qm2 = theta_ret*hs*pw*lambda
  
  
  
  return(Qm2)
}

