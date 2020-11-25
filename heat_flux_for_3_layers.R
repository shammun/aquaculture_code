
calculate_heat_flux_layer_1 = function(i, place, three_hour, cloud_fraction){
  if(i > 365){
    day = i - 365
  } else{
    day = i
  }
  
  
  
  ########################################################################################################################################################################
  ######################################          Volume 1  ---- Calculate phi_net or net heat flux for the pond            ##############################################
  ######################################           phi_net = phi_sn + phi_at - phi_ws - phi_e + phi_c + phi_dz              ##############################################
  ########################################################################################################################################################################
  
  phi_sn = calculate_phi_sn(place, day, solar_data)
  phi_at = calculate_phi_at(three_hour, cloud_fraction)
  phi_ws = calculate_phi_ws(i, place, three_hour)
  phi_e =  calculate_phi_e(i, place, three_hour)
  phi_c = calculate_phi_c(i, place, three_hour)
  phi_dz = calculate_phi_dz(i, place, three_hour)
  phi_net = phi_sn + phi_at - phi_ws - phi_e + phi_c + phi_dz 
  return(phi_net)
}



calculate_heat_flux_layer_2 = function(i, place, three_hour, solar_data, sdd, layer_number){
  
  ########################################################################################################################################################################
  ######################################   Volume 2  ---- Calculate phi_net or net heat flux for the pond for volume 2    ################################################
  ######################################                          phi_net = phi_c - phi_snz + phi_dz                      ################################################
  ########################################################################################################################################################################
  
  phi_c = calculate_phi_c(i, place, three_hour)
  phi_snz = calculate_phi_sn_z(i, place, three_hour, solar_data, sdd, layer_number)
  phi_dz = calculate_phi_dz(i, place, three_hour)
  phi_net = phi_c - phi_snz + phi_dz
  return(phi_net)
}


calculate_heat_flux_layer_3 = function(i, place, three_hour, solar_data, sdd, layer_number){
  
  ########################################################################################################################################################################
  ######################################          Volume 3  ---- Calculate phi_net or net heat flux for the pond            ##############################################
  ######################################               phi_net = phi_c - phi_snz + phi_sed - phi_gw + phi_dz                ##############################################
  ########################################################################################################################################################################
  
  phi_c = calculate_phi_c(i, place, three_hour)
  phi_snz = calculate_phi_sn_z(i, place, three_hour, solar_data, sdd, layer_number)
  phi_sed = calculate_phi_sed(i, place, three_hour)
  phi_gw = calculate_phi_gw(i, place, three_hour)
  phi_dz = calculate_phi_dz(i, place, three_hour)
  
  phi_net = phi_c - phi_snz + phi_sed - phi_gw + phi_dz 
  return(phi_net)
}














