
areas = c("srimangal", "khulna")
time_of_the_day = c(seq(6, 21, 3), 0, 3)

simulate_pond_temperature_for_a_day = function(i, place, cloud_fraction, solar_data, sdd){
  
  # First determine the day in a year and the year 
  if(i > 365){
    day = i - 365
    year = 2019
  } else{
    day = i
    year = 2018
  }
  
  # Now, simulate net heat flux, heat and temperature for all three layers in a pond
  # Do it for three-hourly time periods startign with 6 AM assuming the minimum temperature occurs at 6 AM
  
  for(j in 1:8){
    if(i==1 & j==1){
      # We initialize the simulation with initial temperature of each pond layer as the average of the long-term air temperature and set initial heat to 0
      data_air = read.csv("long_term_air_temp.csv")
      data_air = data_air[data_air$day == day, ]
      data_air = data_air[data_air$area == areas[place], ]
      data_air = data_air[data_air$hour == 6, ]
      initial_temp = data_air$mean_temp - 273.15
      initial_surface_temp = initial_temp
      middle_surface_temp = initial_temp
      bottom_surface_temp = initial_temp
      simulated_data = data.frame(time = time_of_the_day[j], day = 1, year = 2018, heat_layer1 = 0, heat_layer2 = 0, heat_layer3 = 0, temp_layer1 = initial_surface_temp, temp_layer2 = middle_surface_temp, temp_layer3 = bottom_surface_temp)
      write.csv(simulated_data, paste0("simulated_data_", areas[place], ".csv"))
    }else{
      net_heat_flux_layer_1 = calculate_heat_flux_layer_1(i, place, three_hour, cloud_fraction)
      net_heat_flux_layer_2 = calculate_heat_flux_layer_2(i, place, three_hour, solar_data, sdd, 2)
      net_heat_flux_layer_3 = calculate_heat_flux_layer_3(i, place, three_hour, solar_data, sdd, 3)
      
      # Get the data of the last record in the simulated_data_.csv
      loaded_data = read.csv(paste0("simulated_data_", areas[place], ".csv"))
      loaded_data = tail(loaded_data, n=1)
      
      # Get the heat in each of the layer
      heat_layer_1 = loaded_data$heat_layer1 + net_heat_flux_layer_1
      heat_layer_2 = loaded_data$heat_layer2 + net_heat_flux_layer_2
      heat_layer_3 = loaded_data$heat_layer2 + net_heat_flux_layer_3
      
      # Calculate T_wc so that we can calculate rho_w
      T_wc = calculate_water_surface_temperature(i, place, three_hour)
      rho_w = (0.99987 + (0.69 * (10 ** (-5))) * T_wc) - ((8.89 * (10 ** (-6))) * (T_wc)**2) + ((7.4 * (10 ** (-8))) * (T_wc ** 3)) * 1000
      # c_pw is the heat capacity of water
      c_pw = 4.1816 
      
      # Get the temperature in each of the layer
      temp_layer1 = loaded_data$temp_layer1 + (heat_layer_1)/(rho_w * c_pw)
      temp_layer2 = loaded_data$temp_layer2 + (heat_layer_2)/(rho_w * c_pw)
      temp_layer3 = loaded_data$temp_layer3 + (heat_layer_3)/(rho_w * c_pw)
      simulated_data = data.frame(layer = c(1,2,3), time = rep(time_of_the_day[j], 3), day = rep(day, 3), year = rep(year, 3), heat = c(heat_layer1, heat_layer2,
                                                                                                                                        heat_layer_3),
                                  temp_layer2 = c(temp_layer1, temp_layer2, temp_layer3))
      # Add data to the CSV file simulated_data_area.csv

      write.table(simulated_data, "heat_layers.csv", sep = ",", col.names = !file.exists("heat_layers.csv"), append = T)
    }
  }
  
}