
# install.packages("solrad")
library(solrad)

#Calculating solar altitude angle for two consecutive days
DOY <- seq(0, 2, .05)
alpha <- Altitude(DOY, Lat = 24.303856, Lon=91.678491, SLon=90, DS=0)
#Note: only the difference between Lon and SLon matters not each value
plot(DOY, alpha)

### This function returns the apparent solar time (in minutes) for a given day of year and location.

DOY <- seq(0, 2, .05)
ast <- AST(1, Lon=91.678491, SLon=90, DS=0)
#Note: only the difference between Lon and SLon matters not each value
plot(DOY, ast)

# This function estimates day length (in hours) for a given day of year and latitude.

DOY <- 1:365
Lat = 24.303856
dl <- DayLength(DOY, Lat)
plot(DOY, dl)

# This function estimates sunrise time (in continuous hour values) for a given day of year and latitude.

sunrise <- Sunset(DOY, Lat)
plot(DOY, sunrise)


# This function estimates sunset time (in continuous hour values) for a given day of year and latitude.

sunset <- Sunset(DOY, Lat)
plot(DOY, sunset)

time_of_the_day = seq(0,21,by=3) 
for(i in seq_along(time_of_the_day)){
  print(time_of_the_day[i])
}










