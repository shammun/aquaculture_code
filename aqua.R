# Pseudocode
# For a single district
# 
# tmax_air = a vector of 730 values corresponding to 2 years of maximum air temperature values
# tmin_air = a vector of 730 values corresponding to 2 years of minimum air temperature values
# tmax_water = a vector of 730 values corresponding to water temperature at afternoon (3 PM?)
# tmin_water = a vector of 730 values corresponding to water temperature at morning (6 AM?)
# solar_radiation_morning = a vector of 730 solar radiation values corresponding to 6 AM on every day for 2 years
# solar_radiation_afternoon = a vector of 730 solar radiation values corresponding to 3 PM on every day for 2 years
# pond_length =? # How do we get this data, we test with different values!!??
# pond_width =? # How do we get this data, we test with different values!!??
# pond_depth =? # How do we get this data, we test with different values!!??
# anemometer_height =? # How do we get this data, we test with different values!!??
# secchi_depth =? # How do we get this data, we test with different values!!??
# initial_surface_temperature = tmin_water # Am I right?
# initial_middle_temperature = tmin_water # Am I right?
# initial_bottom_temperature = tmin_water # Am I right?
# wind_speed_morning  =?
# wind_speed_afternoon = ?
# temp_volume_1 = an empty vector containing volume 1 temperature, the size of this vector will be 1460 after the cpmputation (2 values corresponding to morning and afternoon for every day for 2 years)
# temp_volume_2 = an empty vector containing volume 2 temperature, the size of this vector will be 1460 after the cpmputation (2 values corresponding to morning and afternoon for every day for 2 years)
# temp_volume_3 = an empty vector containing volume 3 temperature, the size of this vector will be 1460 after the cpmputation (2 values corresponding to morning and afternoon for every day for 2 years)
# lambda_morning = a vector of 730 values corresponding to solar altitude angles in the morning time
# lambda_afternoon = a vector of 730 values corresponding to solar altitude angles in the afternoon time 
# cloud_fraction = a vector with different values of fraction of the sky covered by cloud
# sigma = 2.07 * (10 ** (-7))
# t_d = dew-point temperature needed for evaporative heat transfer calculation>
# w_2 = ? # wind speed 2 m above the water surface
# A_v = 1 # idealized volume element surface area
# c_pw = 4.1816 # heat capacity of water
# rho_a = 1.1988 # density of air above the water surface
# c_z = 1.0 * (10 ** (-3)) # coefficient of aerodynamic resistance
# g = 9.81 # gravtational acceleration coefficient
# w_vz = ? # wind vector magnitude at a height of z meters above the ground
# w_sa = ? # wind shaer area = wind speed * length of wind fetch
# heat_volume_1 = 0 # the size of this vector will be 1460 after the cpmputation (2 values corresponding to morning and afternoon for every day for 2 years)
# heat_volume_2 = 0 # the size of this vector will be 1460 after the cpmputation (2 values corresponding to morning and afternoon for every day for 2 years)
# heat_volume_3 = 0 # the size of this vector will be 1460 after the cpmputation (2 values corresponding to morning and afternoon for every day for 2 years)
# phi_sn = 0
# k_sed = 2.53 # thermal conductivity coefficient for sediment
# t_sed = ? # temperature of the sediment volume element ???
# k_e = 2.5 # thermal conductivity coefficient for earth
# r = 0.03 # reflectance of the water surface to longwave radiation (decimal fraction)
# e = 0.398 * (10 ** (-5))*(t_ak)^2.148 # emittance of the atmosphere # 

phi_net = 0

# for all the days in 2 years
for(i in 1:730){
  # For morning and afternoon time
  
  # Here, j =1 means morning and j=2 means afternoon
  
  for(j in 1:2){
    # If this is the first observation, set the temperature in each volume to initial temperature or the morning (minimum) temperature. 
    # Also, set the heat content to 0 for all the pond layers
    if(i == 1 && j == 1){
      temp_volume_1 = tmin_water[1]
      temp_volume_2 = tmin_water[1]
      temp_volume_3 = tmin_water[1]
      heat_volume_1 = 0
      heat_volume_2 = 0
      heat_volume_3 = 0
    }
    else{
      # Compute net heat flux for each of the volume elements
      # Here k stands for different volume elements of the pond. As there are 3 layers, surface, middle and bottom; 
      # there are 3 values for k corresponding to these 3 different layers
      for(k in 1:3){
        
        # Check if Volume 1
        if(k == 1){
          
          ########################################################################################################################################################################
          ######################################          Volume 1  ---- Calculate phi_net or net heat flux for the pond            ##############################################
          ######################################           phi_net = phi_sn + phi_at - phi_ws - phi_e + phi_c + phi_dz              ##############################################
          ########################################################################################################################################################################
          
          
          
          
          ######################################################## If it is morning time  ###########################################################################
          
            
          ##
          ##################################################################################################
          ##########   phi_sn  ---- Calculate phi_sn or penetraing short-wave solar irradiance
          ##################################################################################################
            
          # phi_s = measured incident radiation   lambda = solar altitude angle
          if(j == 1){
            phi_s = solar_radiation_morning[i]
            r_s = 2.2 * (180 * (lambda_morning[i]/pi)) ** (-0.97)
            phi_sn = phi_s * (1 - r_s(1 - 0.08*wind_speed_morning[i]))
          } else {
            phi_s = solar_radiation_afternoon[i]
            r_s = 2.2 * (180 * (lambda_afternoon[i]/pi)) ** (-0.97)
            phi_sn = phi_s * (1 - r_s(1 - 0.08*wind_speed_afternoon[i]))
          }
            
          ##################################################################################################
          ##########   phi_at  ---- Calculate phi_at or net atmospheric radiation
          ##################################################################################################
            
          # absolute air temperature
          t_ak = (tmax_air + tmin_air)/2 + (tmax_air - tmin_air)*0.4484 *(sin(2*(pi/24)*(6) - 2.7489) + 0.2706 * sin(2 * (2 * (pi/24)*6 - 2.7489)))
          e = (0.398 * (10 ** (-5)))*(t_ak ** (2.148))
          phi_at = ((1 - r)* e * sigma * (t_ak ** (4))) * (1 + 0.17 * (cloud_fraction ** 2)) # 
            
            
          ##################################################################################################
          ##########   phi_ws  ---- Calculate phi_ws or water surface back radiation
          ##################################################################################################
            
          if(j==1){
            phi_ws = 0.97 * sigma * (tmin_water[i] ** 4)  
          } else{
            phi_ws = 0.97 * sigma * (tmax_water[i] ** 4)
          }
          
            
            
          ##################################################################################################
          ##########   phi_e  ---- Calculate phi_e or evaporative heat transfer
          ##################################################################################################
            
          # Couldn't understand which formula to use from Shuttleworth 
            
            
          ##################################################################################################
          ##########   phi_c 5  ---- Calculate phi_c or sensible heat transfer
          ##########   phi_c = 1.5701 * w_2 * (t_wc - t_ac) 
          ##########   t_wc = temperature of the water at surface = tmin_water or tmax_water based on whether it is morning or afternoon 
          ##########   t_ac = temperature of the air at the surface = tmin_air or tmax_air based on whether it is morning or afternoon  
          ##################################################################################################
          
          if(j == 1){
            phi_c = 1.5701 * w_2 * (tmin_water[i] - tmin_air[i])
          } else{
            phi_c = 1.5701 * w_2 * (tmax_water[i] - tmax_air[i])
          } 
          
            
            
            
          #####################################################################################################################################
          ##########   phi_dz  ---- Calculate phi_dz or effective diffusion of heat at upper volume element boundary
          ##########   phi_dz = rho_w * c_pw * A_v * e_zz * (delta_t/delta_z)
          ##########   rho_w = density of water (kg/m3)
          ##########   c_pw = heat capacity of water = 4.1816 (kJ/(Kkg)) 
          ##########   A_v = idealized volume element surface area = 1.0 (m2)
          ##########   e_z_z = effective diffusion coefficient at depth z
          ##########   delta_t/delta_z = tempearature versus depth gradient between volume centers of adjacent volume elements
          #####################################################################################################################################
            
          # rho_w = density of water
          if(j == 1){
            rho_w = (0.99987 + (0.69 * (10 ** (-5))) * tmin_water[i]) - ((8.89 * (10 ** (-6))) * (tmin_water[i])**2) + ((7.4 * (10 ** (-8))) * (tmin_water[i] ** 3)) * 1000  
          } else{
            rho_w = (0.99987 + (0.69 * (10 ** (-5))) * tmax_water[i]) - ((8.89 * (10 ** (-6))) * (tmax_water[i])**2) + ((7.4 * (10 ** (-8))) * (tmax_water[i] ** 3)) * 1000
          }
          
          # rho_a = 1.1988, density of air above the water surface 
          # c_z = 1.0 * (10 ** (-3)) # coefficient of aerodynamic resistance  
          # w_vz = ? # wind vector magnitude at a height of z meters above the ground
          tau_zero = rho_a * c_z * (w_vz ** 2) 
          w_s_star = (tau_zero / rho_w) ** 0.5 # w_s_star, frictional wind velocity due to wind stress (m/s)
          mu_s = 30 * w_s_star # mu_s, drift velocity (m/s)
          
          # w_sa = wind shear area = wind speed * length of wind fetch
          k_star = 6 * (w_sa ** (-1.84)) # k_star, decay coefficient
          # a_v = coefficient of expansion of water
          # a_v = (1.5 * (10 ** (-5)) * (t_av - 277)) - (2.0 * (10 ** (-7))* (t_av - 277) ** 2) where t_av is the average water volume of the adjacent volume elements (K)
          # As it is layer 1, we consider layer 2 as the adjacent volume and use its temperature or temp_volume_2 as t_av
          a_v = (1.5 * (10 ** (-5)) * (temp_volume_2 - 277)) - (2.0 * (10 ** (-7))* (temp_volume_2 - 277) ** 2) # replace t_av with temp_volume_2
          delta_t = temp_volume_1 - temp_volume_2 # delta_t = temperature difference between adjacent volume elements (K)
          delta_z = 0.665 # distance between adjacent volume element centers (m). Assuming 2 meter deep pond 1 - 0.335
          
          # ri_z, Richardson number at depth z (dimensionless)
          # ri_z = (a_v * g * (z ** 2)) * (delta_t - delta_z)
          ri_z = (a_v * g * (0.335 ** 2)) * (delta_t - delta_z) # Assuming 2 meter pond, the depth of first volume is 0.67 m and so the middle of this volume or z is 0.335 m deep
          e_theta_z = ((w_s_star ** 2)/(mu_s * k_star)) * exp(-1 * k_star * 0.335)
          e_z_z = e_theta_z * (1 + 0.05 * ri_z) ** (-1)
          phi_dz = rho_w * c_pw * A_v * e_zz * (delta_t/delta_z)
            
            
          ##################################################################################################
          ##########   phi_net -------    Calculate phi_net or net heat flux for the pond for layer 1    ###############
          ##################################################################################################
            
          phi_net = phi_sn + phi_at - phi_ws - phi_e + phi_c + phi_dz
          
          if(i != 1 && j !=1){
            # If morning time, add 
            last_temp_volume_1 = temp_volume_1[length(temp_volume_1)] # last saved element in the vector temp_volume_1 or the temperature in the previous period
            last_heat_volume_1 = heat_volume_1[length(heat_volume_1)] # last saved element in the vector heat_volume_1 or the heat in the previous period
            if(j==1){
              dt = 15 # from 3 PM to 6 AM next day
            } else{
              dt = 9 # from 6 AM to 3 PM
            }
            # t_vit = t_vi_0  + (h_vit_-1 + (delta_h_vi/delta_t) * delta_t), temperature of volume i at time t= t
            # t_vi_0 = temperature of volume i at time t= 0, using last_temp_volume_1 for this variable
            # h_vit_-1 = heat stored in vi at time t-1, this is the last saved element in the vector temp_volume_1. Use last_heat_volume_1 for this
            # delta_h_vi/delta_t = heat flux during the time interval (KJ/hr)
            new_temperature = last_temp_volume_1 + (last_heat_volume_1 + phi_net * dt)/(rho_w * c_pw * 1) # 1 stands for volume
            temp_volume_1 = c(temp_volume_1, new_temperature)
          }
          
        } else if(k==2){
          ########################################################################################################################################################################
          ######################################   Volume 2  ---- Calculate phi_net or net heat flux for the pond for volume 2    ################################################
          ######################################                          phi_net = phi_c - phi_snz + phi_dz                      ################################################
          ########################################################################################################################################################################

          
          ##################################################################################################
          ##########   phi_c  ---- Calculate phi_snz or sensible heat transfer
          ##################################################################################################
          
          if(j==1){
            phi_c = 1.5701 * w_2 * (tmin_water[i] - tmin_air[i])
          } else{
            phi_c = 1.5701 * w_2 * (tmax_water[i] - tmax_air[i])
          }
          
          
          
          ##################################################################################################
          ##########   phi_snz  ---- Calculate phi_snz or solar irradiance at lower volume element boundary
          ##################################################################################################
          
          if(j == 1){
            r_s = 2.2 * (180 * (lambda_morning[i]/pi)) ** (-0.97)
            one_minus_r = (1 - r_s(1 - 0.08*wind_speed_morning[i]))
          } else {
            r_s = 2.2 * (180 * (lambda_afternoon[i]/pi)) ** (-0.97)
            one_minus_r = (1 - r_s(1 - 0.08*wind_speed_afternoon[i]))
          }
          phi_snz = phi_sn * one_minus_r * (1 - 0.03) * exp(-1 * 1.7 / sdd) # solar irradiance absorbed at surface is 0.03 but what should be the value for Volume 2 of the pond
          
          
          
          #####################################################################################################################################
          ##########   phi_dz  ---- Calculate phi_dz or effective diffusion of heat at middle volume element boundaries
          ##########   phi_dz = rho_w * c_pw * A_v * E_zz * (delta_t/delta_z)
          #####################################################################################################################################
          
          # rho_w = density of water
          if(j == 1){
            rho_w = (0.99987 + (0.69 * (10 ** (-5))) * tmin_water[i]) - ((8.89 * (10 ** (-6))) * (tmin_water[i])**2) + ((7.4 * (10 ** (-8))) * (tmin_water[i] ** 3)) * 1000  
          } else{
            rho_w = (0.99987 + (0.69 * (10 ** (-5))) * tmax_water[i]) - ((8.89 * (10 ** (-6))) * (tmax_water[i])**2) + ((7.4 * (10 ** (-8))) * (tmax_water[i] ** 3)) * 1000
          }
          
          tau_zero = rho_a * c_z * (w_vz ** 2)
          w_s_star = (tau_zero / rho_w) ** 0.5
          mu_s = 30 * w_s_star 
          k_star = 6 * (w_sa ** (-1.84))
          # a_v = coefficient of expansion of water
          # Below is the value of T_av or the average water temperature of the adjacent volume elements
          # The line below gives us the last calculated temperature in volume 1
          last_temp_volume_1 = temp_volume_1[length(temp_volume_1)] # I have doubt here as I am taking the temperature of the volume_1 only and not considering volume 2
          a_v = (1.5 * (10 ** (-5)) * (last_temp_volume_1 - 277)) - (2.0 * (10 ** (-7))* (last_temp_volume_1 - 277) ** 2)
          delta_t = temp_volume_2 - temp_volume_1
          delta_z = 0.665 # Assuming 2 meter deep pond 1 - 0.335
          ri_z = (a_v * g * (0.335 ** 2)) * (delta_t - delta_z) # Assuming 2 meter pond, the depth of the second volume is 1.34 m and so the middle of this volume is 1 m deep
          e_theta_z = ((w_s_star ** 2)/(mu_s * k_star)) * exp(-1 * k_star * 1)
          e_z_z = e_theta_z * (1 + 0.05 * ri_z) ** (-1)
          phi_dz = rho_w * c_pw * A_v * e_zz * (delta_t/delta_z)
          
          
          ########################################################################################################
          ##########  Calculate phi_net or net heat flux for the pond for volume 2 or layer 2      ###############
          ########################################################################################################
          
          phi_net = phi_c - phi_snz + phi_dz
          
          if(i != 1 && j !=1){
            # If morning time, add 
            last_temp_volume_2 = temp_volume_2[length(temp_volume_2)]
            last_heat_volume_2 = heat_volume_2[length(heat_volume_2)]
            if(j==1){
              dt = 15 # from 3 PM to 6 AM next day
            } else{
              dt = 9 # from 6 AM to 3 PM
            }
            new_temperature = last_temp_volume_2 + (last_heat_volume_2 + phi_net * dt)/(rho_w * c_pw * 1) # 1 stands for volume
            temp_volume_2 = c(temp_volume_2, new_temperature)
          }
          
          
        } else if(k==3){
          
          
          ########################################################################################################################################################################
          ######################################          Volume 3  ---- Calculate phi_net or net heat flux for the pond            ##############################################
          ######################################               phi_net = phi_c - phi_snz + phi_sed - phi_gw + phi_dz                ##############################################
          ########################################################################################################################################################################

          
          
          ######################################################## If it is morning time  ###########################################################################
          
          
          ##################################################################################################
          ##########   phi_snz  ---- Calculate phi_snz or sensible heat transfer
          ##################################################################################################
          
          phi_c = 1.5701 * w_2 * (tmin_water[i] - tmin_air[i])
          
          
          ##################################################################################################
          ##########   phi_c       ---- Calculate phi_c or sensible heat transfer
          ##################################################################################################
          
          if(j == 1){
            r_s = 2.2 * (180 * (lambda_morning[i]/pi)) ** (-0.97)
            one_minus_r = (1 - r_s(1 - 0.08*wind_speed_morning[i]))
          } else {
            r_s = 2.2 * (180 * (lambda_morning[i]/pi)) ** (-0.97)
            one_minus_r = (1 - r_s(1 - 0.08*wind_speed_morning[i]))
          }
          phi_snz = phi_sn * one_minus_r * (1 - 0.03) * exp(-1 * 1.7 / sdd) # solar irradiance absorbed at surface is 0.03 but what should be the value for Volume 3 of the pond
          
          
          
          
          ############################################################################################################################################
          ##########   phi_sed       ---- Calculate phi_sed or heat transfer between sediment and bottom water volume element
          #############################################################################################################################################
          
          delta_z = 5 # distance between sediment and ground water -- 5 m 
          t_bot = temp_volume_3[length(temp_volume_3)] # t_bot -- temperature of the bottom volume water element
          phi_sed = k_sed * ((t_bot - t_sed)/delta_z)
          
          
          
          #############################################################################################################################################
          ##########   phi_gw       ---- Calculate phi_gw or heat loss from sediment volume element to ground water table
          #############################################################################################################################################
          
          phi_gw = k_e * ((t_sed - t_gw)/5) # here 5 is for delta_z or distance between sediment and ground water volume elements
          
          
          ##################################################################################################
          ##########   phi_dz  ---- Calculate phi_dz or effective diffusion of heat at upper and lower
          ##########                volume element boundaries
          ##########                phi_dz = rho_w * c_pw * A_v * E_zz * (delta_t/delta_z)
          ##################################################################################################
          
          # rho_w = density of water
          if(j == 1){
            rho_w = (0.99987 + (0.69 * (10 ** (-5))) * tmin_water[i]) - ((8.89 * (10 ** (-6))) * (tmin_water[i])**2) + ((7.4 * (10 ** (-8))) * (tmin_water[i] ** 3)) * 1000  
          } else{
            rho_w = (0.99987 + (0.69 * (10 ** (-5))) * tmax_water[i]) - ((8.89 * (10 ** (-6))) * (tmax_water[i])**2) + ((7.4 * (10 ** (-8))) * (tmax_water[i] ** 3)) * 1000
          }
          
          tau_zero = rho_a * c_z * (w_vz ** 2)
          w_s_star = (tau_zero / rho_w) ** 0.5
          mu_s = 30 * w_s_star 
          k_star = 6 * (w_sa ** (-1.84))
          last_temp_volume_3 = temp_volume_3[length(temp_volume_3)]
          # a_v = coefficient of expansion of water
          a_v = (1.5 * (10 ** (-5)) * (last_temp_volume_3 - 277)) - (2.0 * (10 ** (-7))* (last_temp_volume_3 - 277) ** 2)
          delta_t = temp_volume_3 - temp_volume_2
          delta_z = 0.665 # Assuming 2 meter deep pond 1 - 0.335
          ri_z = (a_v * g * (0.335 ** 2)) * (delta_t - delta_z) # Assuming 2 meter pond, the depth of the third volume is 2 m and so the middle of this volume is 1.67 m deep
          e_theta_z = ((w_s_star ** 2)/(mu_s * k_star)) * exp(-1 * k_star * 1.67)
          e_z_z = e_theta_z * (1 + 0.05 * ri_z) ** (-1)
          phi_dz = rho_w * c_pw * A_v * e_zz * (delta_t/delta_z)
          
          
          
          
          ##################################################################################################
          ##########          Calculate phi_net or net heat flux for the pond for layer 3    ###############
          ##################################################################################################
          
          phi_net = phi_c - phi_snz + phi_sed - phi_gw + phi_dz
          
          if(i != 1 && j !=1){
            # Get the last computed temperature and heat in volume 3
            last_temp_volume_3 = temp_volume_3[length(temp_volume_3)]
            last_heat_volume_3 = heat_volume_3[length(heat_volume_3)]
            if(j==1){
              dt = 15 # from 3 PM to 6 AM next day
            } else{
              dt = 9 # from 6 AM to 3 PM
            }
            new_temperature = last_temp_volume_3 + (last_heat_volume_3 + phi_net * dt)/(rho_w * c_pw * 1) # 1 stands for volume
            temp_volume_3 = c(temp_volume_3, new_temperature)
          }
          
          
        } 
        
        
        
      }
    }
  }
}
