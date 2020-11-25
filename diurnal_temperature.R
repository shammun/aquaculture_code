
library(dplyr)

areas = c("srimangal", "khulna")

read_met_data_climatology = function(day, area, year_p){
  if(area == 1){
    district = "srimangal"
  }else{
    district = "khulna"
  }
  all_data = read.csv(paste0("min_max_air_", district, ".csv"))
  all_data = all_data[all_data$doy == day & all_data$year == year_p, ]
  return(all_data)
}

# temp_historical_srimangal = read_met_data_climatology(1)

calculate_diurnal_air_temperature_historical = function(i, area, year_p){
  temp_historical = read_met_data_climatology(i, area, year_p)
  tmax_air = temp_historical$max_air_temp
  tmin_air = temp_historical$min_air_temp
  hours = seq(0,21,3)
  
  temp_at_different_times = sapply(hours, function(hour){((tmax_air + tmin_air)/2 + (tmax_air - tmin_air)*0.4484 *(sin(2*(pi/24)*(hour + 6) - 2.7489) + 0.2706 * sin(2 * (2 * (pi/24)* (hour + 6) - 2.7489)))) + 273.15})
  
  area = rep(areas[area], 8)
  year = rep(year_p, 8)
  
  if((i != 365 & i != 366) | (year_p %%4 == 0 & i == 365)){
    temp_for_a_single_day = data.frame(time = c(6,9,12,15,18,21,0,3), day = c(i,i,i,i,i,i,i+1,i+1), area = area, year = year_p, air_temp = temp_at_different_times)
  } else if((year_p%%4 != 0 & i == 365) | (year_p%%4 == 0 & i == 366)){
    temp_for_a_single_day = data.frame(time = c(6,9,12,15,18,21,0,3), day = c(i,i,i,i,i,i,1,1), area = area, year = c(rep(year_p, 6), year_p + 1, year_p + 1), air_temp = temp_at_different_times)
  } 
  
  return(temp_for_a_single_day)
}

# calculate_diurnal_air_temperature_historical(1,1,1982)

diurnal_air_temp_sri = data.frame(time = numeric(0), day = numeric(0), area= character(), year=numeric(0), air_temp = numeric(0), stringsAsFactors=FALSE)

for(j in 1982:2019){
  
  # data_holder = data.frame(value = factor(), type = factor(), year = factor(), month = factor(), model = factor(), rcp = factor())
  if(j%%4 == 0){
    number_of_days = 366} else{
      number_of_days = 365
    }
  out = vector("list", length(number_of_days))
  for(i in 1:number_of_days){
    out[[i]] = calculate_diurnal_air_temperature_historical(i,1,j)
  }
  vv1 = as.data.frame(do.call(rbind, out))
  diurnal_air_temp_sri = rbind(diurnal_air_temp_sri, vv1)
}

write.csv(diurnal_air_temp_sri, "diurnal_air_temp_sri.csv")

diurnal_air_temp_khu = data.frame(time = numeric(0), day = numeric(0), area= character(), year=numeric(0), air_temp = numeric(0), stringsAsFactors=FALSE)

for(j in 1982:2019){
  
  # data_holder = data.frame(value = factor(), type = factor(), year = factor(), month = factor(), model = factor(), rcp = factor())
  if(j%%4 == 0){
    number_of_days = 366} else{
      number_of_days = 365
    }
  out = vector("list", length(number_of_days))
  for(i in 1:number_of_days){
    out[[i]] = calculate_diurnal_air_temperature_historical(i,2,j)
  }
  vv1 = as.data.frame(do.call(rbind, out))
  diurnal_air_temp_khu = rbind(diurnal_air_temp_khu, vv1)
}

write.csv(diurnal_air_temp_khu, "diurnal_air_temp_khu.csv")


# Adding two diurnal temperatures together

diurnal_air_temp = rbind(diurnal_air_temp_sri, diurnal_air_temp_khu)
write.csv(diurnal_air_temp, "diurnal_air_temp.csv")



# Diurnal air temperature climatology

diurnal_air_temp$time = factor(diurnal_air_temp$time)
diurnal_air_temp$day = factor(diurnal_air_temp$day)
diurnal_air_temp$area = factor(diurnal_air_temp$area)



long_term_average = diurnal_air_temp %>%
  group_by(area, day, time) %>%
  summarize(mean_temp = mean(air_temp))

long_term_average$area = as.character(long_term_average$area)
long_term_average$day = as.numeric(long_term_average$day)
long_term_average$hour = as.numeric(as.character(long_term_average$time))
long_term_average$time = NULL

write.csv(long_term_average, "long_term_air_temp.csv")





# T_ak = calculate_diurnal_air_temperature_historical(i, 1, 3, 1982) 

# lapply(1:)





