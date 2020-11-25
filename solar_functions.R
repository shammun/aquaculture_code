
library(solrad)

# Constants
sigma_const = 2.07 * (10 ** (-7)) # Stefan-Boltzman constant, unit KJ.m2/hr/K^4

areas = c("srimangal", "khulna")
longitude = c(91.678491, 89.441359)
latitude = c(24.303856, 22.800875)

# 3-hourly time starting from 0 to 21
time_of_the_day = seq(0, 21, 3)

# get_day_length() function provides sunrise and sunset time and day length for a given 
# day, for a given place

total_days = 730
i = 1

if(i > 365){
  day = i - 365
  year = 2019
} else{
  day = i
  year = 2018
}


# Pond parameters
pond_depth = 1.5204 # in meters
each_layer_depth = pond_depth / 3

read_met_data = function(day){
  all_data = read.csv("long_data.csv")
  all_data = all_data[all_data$doy == day & all_data$year == year, ]
  return(all_data)
}

get_day_length = function(day, place){
  # As the count goes from day 1 to day 730 
  if(day > 365) day = day - 365
  
  # Get the latitude for the place
  lat = latitude[place]
  
  # Compute the sunrise and sunset time using functions from solrad package
  # This will give us the day length which we can then use for computing
  # solar irradiance for 3 hours computing from solar irradiance for a day as
  # provided in the NASA LARC dataset
  
  # sunrise = Sunrise(day, lat)
  sunset = Sunset(day, lat)
  
  # Compute the day length
  # Sunrise() gives back the time when the sun rises in AM
  # Sunset() gives the sunset time in PM.
  # So, to compute the day_length, we subtract sunrise from 12 and add the sunset time in PM
  # to get the day_length
  
  # day_length = (12 - sunrise) + sunset
  
  day_length = DayLength(day, lat)
  
  sunrise = 12 + sunset - day_length
  
  # Make a list comprising of sunrise, sunset and day_length and return this
  
  solar_list = list("sunrise" = sunrise, "sunset" = sunset, "day_length" = day_length)
  return(solar_list)
}

# solar_data_from_NASA retrieves daily solar data from NASA LARC for a given day and place
# solar_data_from_NASA = function(day, place){
#   
#   # TO DO ###########################
#   
#   
#   
#   solar_irradiance = c()
#   return(solar_irradiance)
# }

# i = 1
place = 1
solar_data = get_day_length(i, place)

# Calculating sediment temperature as the long-term average air temperature

# avg_air_temp = (as.vector(t(read.csv("long_data.csv")["max_air_temp"])) + as.vector(t(read.csv("long_data.csv")["max_air_temp"])))/2
all_data = read.csv("long_data.csv")
all_data = all_data[all_data$district == areas[place], "long_term_air_average"]
long_term_avergae_air_temp = unique(all_data)


# day = 1

# Get daily solar data for a given day and place
daily_data = read_met_data(day, "long_data.csv")

solar_daily = daily_data$insolation
wind_speed_2m = daily_data$wind_speed_2m

# Calculate hourly solar data
hourly_solar = solar_daily[1] / solar_data[["day_length"]]

# Get the maximum and minimum air temperature
tmax_air = daily_data$max_air_temp[place]
tmin_air = daily_data$min_air_temp[place] 

# Get the maximum and minimum water temperature
tmin_water = daily_data$morning_water_temp[place]
tmax_water = daily_data$afternoon_water_temp[place]

# This function calculates phi_s or measured incident radiation
calculate_phi_s = function(time, solar_data){
  # check if the time is before sunrise or if the time is after sunset
  if(((time_of_the_day[time] + 3) < solar_data[["sunrise"]]) | ((time_of_the_day[time]) > solar_data[["sunset"]] + 12)){
    # if the 3-hour time is before sunrise or after sunset, solar irradiance is 0
    sol_irradiance = 0
  } else if((time_of_the_day[time] < solar_data[["sunrise"]]) & ((time_of_the_day[time] + 3) > solar_data[["sunrise"]])){ 
    # check if the time starts before sunrise and there are some time in the 3-hourly periods that fall before sunrise or in the morning
      
      # Calculate the total duration of the 3-hours after the sunrise
      sunshine_duration = time_of_the_day[time] + 3 - solar_data[["sunrise"]]
      sol_irradiance = sunshine_duration * hourly_solar
    } else if((time_of_the_day[time] > solar_data[["sunrise"]]) & (time_of_the_day[time] < solar_data[["sunset"]] + 12) & ((time_of_the_day[time] + 3) > solar_data[["sunset"]] + 12)){
      # check if the time starts before sunset and there are some time in the 3-hourly periods that fall after sunset or in the evening
      
      # Calculate the total duration of the 3-hours before the sunset
      sunshine_duration = 12 + solar_data[["sunset"]] - time_of_the_day[time] 
      # e.g., if sunset is at 7 PM, 12 + solar_data[["sunset"]] gives us 19 
      # if the time of the day is 18 or 6 PM, the above equation stands as 12 + 7 - 6 = 1
      # Thus, it gives 1 hour of sunshine
      sol_irradiance = sunshine_duration * hourly_solar
    } else if(((time_of_the_day[time]) >= solar_data[["sunrise"]]) & ((time_of_the_day[time] + 3) <= solar_data[["sunset"]] + 12)){
      # Check if all the 3-hours are between sunshine and sunset
      
      sunshine_duration = 3
      sol_irradiance = sunshine_duration * hourly_solar
    }
    
    # return solar irradiance
    return(sol_irradiance)
}

# calculate_phi_s(6, solar_data)

# This function calculates phi_sn or penetrating short-wave solar radiation

calculate_phi_sn = function(place, day, three_hour, solar_data){
  # Calculate solar altitude angle lambda
  lambda = abs(Altitude(day, latitude[place], longitude[place], longitude[place], 0))
  
  # Calculate R_s
  R_s = 2.2 * (180 * lambda/pi)^(-0.97)
  
  # W_z is wind velocity at 2 meters above the pond surface
  W_z = wind_speed_2m[place]
  
  # Calculate R, reflectivity adjusted for surface temperature
  R = R_s * (1 - 0.08 * W_z)
  
  # Calculate phi_s
  phi_s = calculate_phi_s(three_hour, solar_data)
  
  # Calculate phi_sn = phi_s * (1 - R)
  phi_sn = phi_s * (1 - R)
  
  # return the value of phi_sn
  return(phi_sn)
}

calculate_phi_sn(1, 1, 3, solar_data)

# calculate_phi_at(place, day, three_hour, solar_data)

calculate_phi_at = function(three_hour, cloud_fraction){
  
  # Get the time in hour
  hour = time_of_the_day[three_hour]
  t_ak = ((tmax_air + tmin_air)/2 + (tmax_air - tmin_air)*0.4484 *(sin(2*(pi/24)*(hour + 6) - 2.7489) + 0.2706 * sin(2 * (2 * (pi/24)* (hour + 6) - 2.7489)))) + 273.15
  e = (0.398 * (10 ** (-5)))*(t_ak ** (2.148))
  r = 0.03 # reflectance of the water surface to longwave radiation
  sigma = 2.07 * (10 ** (-7)) # Stefan-Boltzman constant, unit Kg/m2/hr/K^4
  phi_at = ((1 - r)* e * sigma * (t_ak ** (4))) * (1 + 0.17 * (cloud_fraction ** 2)) # this calculates phi_at for one hour
  
  # phi_at for three hours
  three_hours_phi_at = phi_at * 3
  return(three_hours_phi_at)
}
  
calculate_phi_at(4, 4)

calculate_water_surface_temperature = function(i, place, three_hour){
  time_difference = 9
  hourly_temperature_increase = (tmax_water - tmin_water) / time_difference
  
  if(i > 365){
    day = i - 365
  } else{
    day = i
  }
  
  if(i == total_days){
    previous_day_climate = read_met_data(day - 1)
    previous_day_afternoon_water_temp = previous_day_climate$afternoon_water_temp[place]
    hourly_temperature_decrease = (previous_day_afternoon_water_temp - tmin_water) / 15
  } else{
    next_day_climate = read_met_data(day + 1)
    next_day_morning_water_temp = next_day_climate$morning_water_temp[place]
    hourly_temperature_decrease = (tmax_water - next_day_morning_water_temp) / 15
  }
  
  
  if(time_of_the_day[three_hour] == 6){
    T_wc = tmin_water
  } else if(time_of_the_day[three_hour] == 15){
    T_wc = tmax_water
  } else if(time_of_the_day[three_hour] > 6 & time_of_the_day[three_hour] < 15){
    # this condition checks if the time is after 6 AM and before 3 PM
    
    temp_increase_from_6_AM = hourly_temperature_increase * (time_of_the_day[three_hour] - 6)
    T_wc = tmin_water + temp_increase_from_6_AM 
  } else if(time_of_the_day[three_hour] < 6 | time_of_the_day[three_hour] > 15){
    # this condition checks if the time is after 3 PM and before 6 AM
    
    if(time_of_the_day[three_hour] < 6){
      temp_decrease_from_3_PM = hourly_temperature_increase * (time_of_the_day[three_hour] + 24 - 15) 
    } else{
      temp_decrease_from_3_PM = hourly_temperature_decrease * (time_of_the_day[three_hour] - 15)
    }
    T_wc = tmax_water - temp_decrease_from_3_PM
  }
  return(T_wc)
}

calculate_phi_ws = function(i, place, three_hour){
  # Check the time, if it is at the morning or 6 AM, the temperature of the water is the morning water temperature or the minimum water temperature
  # if the temperature is at the afternoon or at 3 PM, the temperature of the water is the afternoon temperature or the maximum water temperature
  
  # T_wk is the water surface temperature in Kelvin
  T_wk = calculate_water_surface_temperature(i, place, three_hour) + 273.15
  
  # Calculate hourly phi_ws
  phi_ws = 0.97 * sigma_const * (T_wk^4)
  # Now, calculate three-hourly phi_ws
  three_hours_phi_ws = phi_ws * 3
  
  # Return phi_ws for three hours or three_hours_phi_ws
  return(three_hours_phi_ws)
}

calculate_phi_ws(1, 1, 4)

calculate_phi_e = function(i, place, three_hour){
  # N, empirical coefficient from Lake Hefner, unit KJm-2km-1mmHg-1
  N = 5.0593
  
  # wind speed 2 m above the surface
  W2 = wind_speed_2m[place]
  
  # T_wc is water surface temperature in Celsius
  T_wc = calculate_water_surface_temperature(i, place, three_hour)
  
  # T_d is the average daily dew-point temperature
  # Using morning minimum temperature as the morning minimum dry-bulb temperature
  T_d = (tmin_air - 2) + 273.15 # from page 235 of the Culberson paper. Adding 273.15 to convert to Kelvin
  
  # e_s, saturated vapor pressure at T_wc; unit mmHg
  e_s = 25.375 * exp(17.62 - (5271 / T_wc))
  
  
  # e_a, water vapor pressure above the pond surface; unit mmHg
  e_a = 610.78 * exp(17.2694 *((T_d - 273.16)/(T_d - 35.86)))
  
  phi_e = N * W2 * (e_s - e_a)
  
  three_hours_phi_e = 3 * phi_e
  
  # Return phi_e for three hours or three_hours_phi_e
  return(three_hours_phi_e)
}



calculate_phi_c = function(i, place, three_hour){
  # wind speed 2 m above the surface
  W2 = wind_speed_2m[place]
  
  # T_wc is water surface temperature in Celsius
  T_wc = calculate_water_surface_temperature(i, place, three_hour)
  
  # Get the time in hour
  hour = time_of_the_day[three_hour]
  
  # Air temperature in degree Celsius
  T_ac = ((tmax_air + tmin_air)/2 + (tmax_air - tmin_air)*0.4484 *(sin(2*(pi/24)*(hour + 6) - 2.7489) + 0.2706 * sin(2 * (2 * (pi/24)* (hour + 6) - 2.7489))))
  
  # phi_c, sensible heat transfer
  phi_c = 1.5701 * W2 * (T_wc - T_ac)
  
  three_hours_phi_c = phi_c * 3
  
  # Return phi_c for three hours or three_hours_phi_c
  return(three_hours_phi_c)
}


calculate_phi_sn_z = function(i, place, three_hour, solar_data, sdd, layer_number){
  phi_sn = calculate_phi_sn(i, place, three_hour, solar_data)
  
  # Calculate solar altitude angle lambda
  lambda = abs(Altitude(day, latitude[place], longitude[place], longitude[place], 0))
  
  # Calculate R_s
  R_s = 2.2 * (180 * lambda/pi)^(-0.97)
  
  # W_z is wind velocity at 2 meters above the pond surface
  W_z = wind_speed_2m[place]
  
  # Calculate R, reflectivity adjusted for surface temperature
  R = R_s * (1 - 0.08 * W_z)
  
  # beta, fraction of solar irradiance absorbed at surface
  beta = 0.03
  
  # n_e, effective light extinction coefficient
  n_e = 1.7 /sdd # For now, assume sdd could be as much as the pond depth, 5 ft or 1.5204 m
  
  # Calculate z, the depth for a layer
  z = each_layer_depth + (layer_number - 1) * each_layer_depth
  
  # calculate phi_sn_z
  phi_sn_z = phi_sn * (1 - R) * (1 - beta) * exp(-1 * n_e * z)
  
  # calculate three-hourly phi_sn_z
  three_hours_phi_sn_z = 3 * phi_sn_z
  
  return(three_hours_phi_sn_z)
}

calculate_T_sed = function(place, day, three_hour){
  # Get the value of the long-term average temperature
  all_data = read.csv("long_term_air_temp.csv")
  all_data = all_data[all_data$area == areas[place], ]
  all_data = all_data[all_data$day == day, ]
  all_data = all_data[all_data$hour == time_of_the_day[three_hour], ]
  # long_term_avergae_air_temp = unique(all_data)
  long_term_avergae_air_temp = all_data$mean_temp - 273.15
  rm(all_data)
  
  # Set temperature of the sediment volume element to the long-term average temperature
  return(long_term_avergae_air_temp)
}



calculate_phi_sed = function(i, place, three_hour){
  k_sed = 2.53 # thermal conductivity coefficient for sediment, unit KJm-1h-1C-1
  if(i == 1){
    T_bot = 20 # Assume the bottom layer water temperature to be the temperature of the ground water
  } else{
    # T_bot = bottom_layer_temp[i, time_of_the_day[three_hour]]
    T_bot = 20 # This line will be removed later
  }
  
  # Calculate T_sed
  T_sed = calculate_T_sed(place, day, three_hour)
  
  # Calculate phi_sed 
  phi_sed = k_sed * ((T_bot - T_sed) / each_layer_depth) # Use layer depth as the value for delta_z or distance 
  # between the centers of the volume elements
  
  # Calculate and return three-hourly phi_sed
  three_hours_phi_sed = phi_sed * 3
  
  return(three_hours_phi_sed)
}

# calculate_phi_sed(1, 1, 4)

# Calculalting phi_gw, heat loss from sediment volume element to ground water table

calculate_phi_gw = function(i, place, three_hour){
  k_e = 2.5 # thermal conductivity coefficien for earth, unit kJmh-1r-1C-1
  T_gw = 20 # temperature of the ground water, unit degree Celsius
  
  delta_z = 0.2534 # distance between sediment and ground water volume elements, in meters
  
  # Calculate T_sed
  T_sed = calculate_T_sed(place, day, three_hour)
  
  phi_gw = k_e * ((T_sed - T_gw)/delta_z)
  
  # Calculate and return three-hourly phi_gw
  three_hours_phi_gw = phi_gw * 3
  return(three_hours_phi_gw)
}

# calculate_phi_gw(1,1,4)

calculate_phi_dz = function(i, place, three_hour){
  # T_wc is water surface temperature in Celsius
  
  c_pw = 4.1816 # heat capacity of water, unit kJK-1kg-1
  A_v = 1 # idealized volume element surface area
  
  phi_dz = T_wc * rho_w * c_pw * A_v
  
  # Calculate and return three-hourly phi_dz
  three_hours_phi_dz = phi_dz * 3
  
  return(three_hours_phi_dz) 
}

calculate_phi_dz(1,1,4)
