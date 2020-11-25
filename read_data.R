
library(dplyr)

wide_data = read.csv("Air_water_temp_dataset_Sylhet_Khulna_modified.csv", stringsAsFactors = FALSE)
wide_data$X = NULL

date = c(wide_data$date, wide_data$date)
max_air_temp = c(wide_data$max_air_temp_syl, wide_data$max_air_temp_khu)

min_air_temp = c(wide_data$min_air_temp_syl, wide_data$min_air_temp_khu)

average_air_temp_syl = (wide_data$max_air_temp_syl + wide_data$min_air_temp_syl)/2

long_term_air_average_syl = rep(mean(average_air_temp_syl), 730)

average_air_temp_khu = (wide_data$max_air_temp_khu + wide_data$min_air_temp_khu)/2

long_term_air_average_khu = rep(mean(average_air_temp_khu), 730)

long_term_air_average = c(long_term_air_average_syl, long_term_air_average_khu)

morning_water_temp = c(wide_data$morning_water_temp_syl, wide_data$morning_water_temp_khu)

afternoon_water_temp = c(wide_data$afternoon_water_temp_syl, wide_data$afternoon_water_temp_khu)

rainfall = c(wide_data$rainfall_syl, wide_data$rainfall_khu)

srimangal_air = read.csv("srimangal_modified.csv")
srimangal_air = srimangal_air[, c("YEAR", "DOY", "wind_speed_2m", "insolation_incident")]

srimangal_air = srimangal_air %>% filter(YEAR>=2018)

khulna_air = read.csv("khulna_modified.csv")
khulna_air = khulna_air[, c("YEAR", "DOY", "wind_speed_2m", "insolation_incident")]

khulna_air = khulna_air %>% filter(YEAR>=2018)

year = c(srimangal_air$YEAR, khulna_air$YEAR)

doy = c(srimangal_air$DOY, khulna_air$DOY)

wind_speed_2m = c(srimangal_air$wind_speed_2m, khulna_air$wind_speed_2m)

insolation_speed = c(srimangal_air$insolation_incident, khulna_air$insolation_incident)

district = c(rep("Srimangal", 730), rep("Khulna", 730))

long_data = data.frame(date = date, year = year, doy = doy, district = district, max_air_temp = max_air_temp,
                       min_air_temp = min_air_temp, long_term_air_average = long_term_air_average,
                       morning_water_temp = morning_water_temp,
                       afternoon_water_temp = afternoon_water_temp, rainfall = rainfall,
                       wind_speed_2m = wind_speed_2m, insolation_speed = insolation_speed)

write.csv(long_data, "long_data.csv")



# Reads and returns meteorological data from Excel file for a given day
# This function takes the day number as argument

read_met_data = function(day){
  if(day > 365){
    day = day - 365
    year = 2019
  } else{
    day = day
    year = 2018
  }
  all_data = read.csv("long_data.csv")
  all_data = all_data[all_data$doy == day & all_data$year == year, ]
  return(all_data)
}