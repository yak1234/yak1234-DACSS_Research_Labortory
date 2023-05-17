install.packages("dplyr")
install.packages("WDI")
options(warn=-1)
library(WDI)
library(dplyr)
# Set the countries and indicators
countries <- c("FRA", "DEU", "HUN","ITA", "POL", "ESP", "SWE", "NLD", "NOR", "DNK")
indicators <- c( "SL.UEM.TOTL.ZS", "SI.POV.GINI","NY.GDP.PCAP.PP.KD", "SM.POP.NETM")
# Pull the data from the World Bank API
wb_data <- WDI(country = countries, indicator = indicators)
print(paste("Script is pulling", length(countries), "countries:  ", paste(countries, collapse = ", ")))
print(paste("Script is pulling", length(indicators), "World Bank Indicators: ", paste(indicators, collapse = ", ")))
# Save the data to a csv file
write.csv(wb_data, "wb_data.csv", row.names = FALSE)

unique_countries <- unique(wb_data$country)
print(paste("There are", length(unique_countries), "Countries in the wb_data.csv: ", paste(unique_countries, collapse = ", ")))

# Print unique years
print(paste("All Years in the wb_data.csv ", paste(unique(wb_data$year), collapse = ", ")))
