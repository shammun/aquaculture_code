
simulated_data = data.frame(layer = 1, time = 6, day = 1, year = 2018, heat_layer1 = 0, heat_layer2 = 0, heat_layer3 = 0, temp_layer1 = 23, temp_layer2 = 23, temp_layer3 = 23)
write.csv(data_1, "layer1_heat.csv")

data_2 = data.frame(layer = 2, time = 9, day = 23, year = 2019, heat = 399, temp = 27)
# write.csv(data_2, "layer1_heat.csv", append = TRUE)
write.table(data_2, "layer1_heat.csv", sep = ",", col.names = !file.exists("layer1_heat.csv"), append = T)
