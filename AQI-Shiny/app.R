#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(highcharter)
library(dplyr)
library(stringr)
library(plyr)
library(lubridate)

my_dates = list()
temperature_df = data.frame()
atm_hpa_df = data.frame()
humidity_df = data.frame()
co_df = data.frame()
dust_pm1 = data.frame()
dust_pm10 = data.frame()
dust_pm2.5 = data.frame()
no2_df = data.frame()
ozone_df = data.frame()
so2_df = data.frame()

same_date = F
new_date = F

device_num = ''
device_num_temperature = ''
device_num_atm_hpa = ''
device_num_humidity = ''
device_num_co = ''
device_num_dpm1 = ''
device_num_dpm10 = ''
device_num_dpm2.5 = ''
device_num_no2 = ''
device_num_o3 = ''
device_num_so2 = ''

process_file <- function(f) {
  filename = basename(f)
  file_components = strsplit(filename, '_')
  
  special_char = '_'
  
  unit_name = file_components[[1]][1]
  unit_number = file_components[[1]][2]
  extracted_date = substr(file_components[[1]][3], 1, nchar(file_components[[1]][3])-4)
  
  device_num <<- paste(unit_name, unit_number, sep='_')
  
  date = as.Date(extracted_date, format="%Y-%m-%d")
  
  if(extracted_date %in% my_dates) {
    
    same_date <<- T
    new_date <<- F
    
  } else {
    same_date <<- F
    new_date <<- T
    my_dates <<- append(my_dates, extracted_date)
    year = as.numeric(format(date, '%Y'))
    month = as.numeric(format(date, '%m'))
    day = as.numeric(format(date, '%d'))
    
    timeseries = seq.POSIXt(ISOdatetime(year,month,day,0,0,0), ISOdatetime(year,month,day,23,50,0), by = '10 min')
    temp_df <<- data.frame(Time = timeseries)
    
    if (new_date == T && length(my_dates) > 1) {
      temperature_df <<- merge(x=temperature_df, y=temp_df, all=T)
      atm_hpa_df <<- merge(x=atm_hpa_df, y=temp_df, all=T)
      humidity_df <<- merge(x=humidity_df, y=temp_df, all=T)
      co_df <<- merge(x=co_df, y=temp_df, all=T)
      dust_pm1 <<- merge(x=dust_pm1, y=temp_df, all=T)
      dust_pm10 <<- merge(x=dust_pm10, y=temp_df, all=T)
      dust_pm2.5 <<- merge(x=dust_pm2.5, y=temp_df, all=T)
      no2_df <<- merge(x=no2_df, y=temp_df, all=T)
      ozone_df <<- merge(x=ozone_df, y=temp_df, all=T)
      so2_df <<- merge(x=atm_hpa_df, y=temp_df, all=T)
    }
  }
  
  input_df = data.frame(read.csv(f))
  input_df[['Time']] = as.POSIXct(input_df[['Time']], format="%Y-%m-%d %H:%M:%S")
  
  device_num_temperature <<- paste(device_num, 't', sep='_')
  device_num_atm_hpa <<- paste(device_num, 'atm', sep='_')
  device_num_humidity <<- paste(device_num, 'hum', sep='_')
  device_num_co <<- paste(device_num, 'co', sep='_')
  device_num_dpm1 <<- paste(device_num, 'dpm1', sep='_')
  device_num_dpm10 <<- paste(device_num, 'dpm10', sep='_')
  device_num_dpm2.5 <<- paste(device_num, 'dpm2.5', sep='_')
  device_num_no2 <<- paste(device_num, 'no2', sep='_')
  device_num_o3 <<- paste(device_num, 'o3', sep='_')
  device_num_so2 <<- paste(device_num, 'so2', sep='_')
  
  if("Temperature_C" %in% colnames(input_df)) {
    colnames(input_df)[which(names(input_df) == "Temperature_C")] = device_num_temperature
  } else if("Temperature" %in% colnames(input_df)) {
    colnames(input_df)[which(names(input_df) == "Temperature")] = device_num_temperature
  }
  
  if("Humidity_percentage" %in% colnames(input_df)) {
    colnames(input_df)[which(names(input_df) == "Humidity_percentage")] = device_num_humidity
  }
  else if ("Humidity" %in% colnames(input_df)) {
    colnames(input_df)[which(names(input_df) == "Humidity")] = device_num_humidity
  }
  
  if("Ozone" %in% colnames(input_df)) {
    colnames(input_df)[which(names(input_df) == "Ozone")] = device_num_o3
  }
  else if ("Ozone(O3)" %in% colnames(input_df)) {
    colnames(input_df)[which(names(input_df) == "Ozone(O3)")] = device_num_o3
  }
  else if ("Ozone.O3." %in% colnames(input_df)) {
    colnames(input_df)[which(names(input_df) == "Ozone.O3.")] = device_num_o3
  }
  
  colnames(input_df)[which(names(input_df) == "AtmosphericPressure_hPa")] = device_num_atm_hpa
  colnames(input_df)[which(names(input_df) == "Carbon_Monoxide")] = device_num_co
  colnames(input_df)[which(names(input_df) == "Dust_pm_1")] = device_num_dpm1
  colnames(input_df)[which(names(input_df) == "Dust_pm_10")] = device_num_dpm10
  colnames(input_df)[which(names(input_df) == "Dust_pm_2_5")] = device_num_dpm2.5
  colnames(input_df)[which(names(input_df) == "Nitrogen_Dioxide")] = device_num_no2
  colnames(input_df)[which(names(input_df) == "Sulfur_Dioxide")] = device_num_so2
  
  #View(input_df)
  
  if (empty(temperature_df)) {
    
    temperature_df <<- merge(x=temp_df, y=input_df[,c("Time", device_num_temperature)], all=T)
    atm_hpa_df <<- merge(x=temp_df, y=input_df[,c("Time", device_num_atm_hpa)], all=T)
    humidity_df <<- merge(x=temp_df, y=input_df[,c("Time", device_num_humidity)], all=T)
    co_df <<- merge(x=temp_df, y=input_df[,c("Time", device_num_co)], all=T)
    dust_pm1 <<- merge(x=temp_df, y=input_df[,c("Time", device_num_dpm1)], all=T)
    dust_pm10 <<- merge(x=temp_df, y=input_df[,c("Time", device_num_dpm10)], all=T)
    dust_pm2.5 <<- merge(x=temp_df, y=input_df[,c("Time", device_num_dpm2.5)], all=T)
    no2_df <<- merge(x=temp_df, y=input_df[,c("Time", device_num_no2)], all=T)
    ozone_df <<- merge(x=temp_df, y=input_df[,c("Time", device_num_o3)], all=T)
    so2_df <<- merge(x=temp_df, y=input_df[,c("Time", device_num_so2)], all=T)
    
  } else {
    
    device_num_temperaturex <<- paste(device_num_temperature, '.x', sep='')
    device_num_temperaturey <<- paste(device_num_temperature, '.y', sep='')
    
    device_num_atm_hpax <<- paste(device_num_atm_hpa, '.x', sep='')
    device_num_atm_hpay <<- paste(device_num_atm_hpa, '.y', sep='')
    
    device_num_humidityx <<- paste(device_num_humidity, '.x', sep='')
    device_num_humidityy <<- paste(device_num_humidity, '.y', sep='')
    
    device_num_cox <<- paste(device_num_co, '.x', sep='')
    device_num_coy <<- paste(device_num_co, '.y', sep='')
    
    device_num_dpm1x <<- paste(device_num_dpm1, '.x', sep='')
    device_num_dpm1y <<- paste(device_num_dpm1, '.y', sep='')
    
    device_num_dpm10x <<- paste(device_num_dpm10, '.x', sep='')
    device_num_dpm10y <<- paste(device_num_dpm10, '.y', sep='')
    
    device_num_dpm2.5x <<- paste(device_num_dpm2.5, '.x', sep='')
    device_num_dpm2.5y <<- paste(device_num_dpm2.5, '.y', sep='')
    
    device_num_no2x <<- paste(device_num_no2, '.x', sep='')
    device_num_no2y <<- paste(device_num_no2, '.y', sep='')
    
    device_num_so2x <<- paste(device_num_so2, '.x', sep='')
    device_num_so2y <<- paste(device_num_so2, '.y', sep='')
    
    device_num_o3x <<- paste(device_num_o3, '.x', sep='')
    device_num_o3y <<- paste(device_num_o3, '.y', sep='')
    
    temperature_df <<- merge(x=temperature_df, y=input_df[,c("Time", device_num_temperature)], by='Time', all.x=T)
    atm_hpa_df <<- merge(x=atm_hpa_df, y=input_df[,c("Time", device_num_atm_hpa)], by='Time', all.x=T)
    humidity_df <<- merge(x=humidity_df, y=input_df[,c("Time", device_num_humidity)], by='Time', all.x=T)
    co_df <<- merge(x=co_df, y=input_df[,c("Time", device_num_co)], by='Time', all.x=T)
    dust_pm1 <<- merge(x=dust_pm1, y=input_df[,c("Time", device_num_dpm1)], by='Time', all.x=T)
    dust_pm2.5 <<- merge(x=dust_pm2.5, y=input_df[,c("Time", device_num_dpm2.5)], by='Time', all.x=T)
    dust_pm10 <<- merge(x=dust_pm10, y=input_df[,c("Time", device_num_dpm10)], by='Time', all.x=T)
    no2_df <<- merge(x=no2_df, y=input_df[,c("Time", device_num_no2)], by='Time', all.x=T)
    so2_df <<- merge(x=so2_df, y=input_df[,c("Time", device_num_so2)], by='Time', all.x=T)
    ozone_df <<- merge(x=ozone_df, y=input_df[,c("Time", device_num_o3)], by='Time', all.x=T)
    
    if (device_num_temperaturex %in% colnames(temperature_df) || device_num_temperaturey %in% colnames(temperature_df)) {
      temperature_df[[device_num_temperaturex]] <<- ifelse(as.Date(ymd_hms(temperature_df$Time)) == extracted_date, temperature_df[[device_num_temperaturey]], temperature_df[[device_num_temperaturex]])
      
      temperature_df <<- subset(temperature_df, select = names(temperature_df) != device_num_temperaturey)
      
      colnames(temperature_df)[which(names(temperature_df) == device_num_temperaturex)] <<- device_num_temperature
    }
    
    if (device_num_atm_hpax %in% colnames(atm_hpa_df) || device_num_atm_hpay %in% colnames(atm_hpa_df)) {
      atm_hpa_df[[device_num_atm_hpax]] <<- ifelse(as.Date(ymd_hms(atm_hpa_df$Time)) == extracted_date, atm_hpa_df[[device_num_atm_hpay]], atm_hpa_df[[device_num_atm_hpax]])
      
      atm_hpa_df <<- subset(atm_hpa_df, select = names(atm_hpa_df) != device_num_atm_hpay)
      
      colnames(atm_hpa_df)[which(names(atm_hpa_df) == device_num_atm_hpax)] <<- device_num_atm_hpa
    }
    
    if (device_num_humidityx %in% colnames(humidity_df) || device_num_humidityy %in% colnames(humidity_df)) {
      humidity_df[[device_num_humidityx]] <<- ifelse(as.Date(ymd_hms(humidity_df$Time)) == extracted_date, humidity_df[[device_num_humidityy]], humidity_df[[device_num_humidityx]])
      
      humidity_df <<- subset(humidity_df, select = names(humidity_df) != device_num_humidityy)
      
      colnames(humidity_df)[which(names(humidity_df) == device_num_humidityx)] <<- device_num_humidity
    }
    
    if (device_num_cox %in% colnames(co_df) || device_num_coy %in% colnames(co_df)) {
      co_df[[device_num_cox]] <<- ifelse(as.Date(ymd_hms(co_df$Time)) == extracted_date, co_df[[device_num_coy]], co_df[[device_num_cox]])
      
      co_df <<- subset(co_df, select = names(co_df) != device_num_coy)
      
      colnames(co_df)[which(names(co_df) == device_num_cox)] <<- device_num_co
    }
    
    if (device_num_dpm1x %in% colnames(dust_pm1) || device_num_dpm1y %in% colnames(dust_pm1)) {
      dust_pm1[[device_num_dpm1x]] <<- ifelse(as.Date(ymd_hms(dust_pm1$Time)) == extracted_date, dust_pm1[[device_num_dpm1y]], dust_pm1[[device_num_dpm1x]])
      
      dust_pm1 <<- subset(dust_pm1, select = names(dust_pm1) != device_num_dpm1y)
      
      colnames(dust_pm1)[which(names(dust_pm1) == device_num_dpm1x)] <<- device_num_dpm1
    }
    
    if (device_num_dpm2.5x %in% colnames(dust_pm2.5) || device_num_dpm2.5y %in% colnames(dust_pm2.5)) {
      dust_pm2.5[[device_num_dpm2.5x]] <<- ifelse(as.Date(ymd_hms(dust_pm2.5$Time)) == extracted_date, dust_pm2.5[[device_num_dpm2.5y]], dust_pm2.5[[device_num_dpm2.5x]])
      
      dust_pm2.5 <<- subset(dust_pm2.5, select = names(dust_pm2.5) != device_num_dpm2.5y)
      
      colnames(dust_pm2.5)[which(names(dust_pm2.5) == device_num_dpm2.5x)] <<- device_num_dpm2.5
    }
    
    if (device_num_dpm10x %in% colnames(dust_pm10) || device_num_dpm10y %in% colnames(dust_pm10)) {
      dust_pm10[[device_num_dpm10x]] <<- ifelse(as.Date(ymd_hms(dust_pm10$Time)) == extracted_date, dust_pm10[[device_num_dpm10y]], dust_pm10[[device_num_dpm10x]])
      
      dust_pm10 <<- subset(dust_pm10, select = names(dust_pm10) != device_num_dpm10y)
      
      colnames(dust_pm10)[which(names(dust_pm10) == device_num_dpm10x)] <<- device_num_dpm10
    }
    
    if (device_num_no2x %in% colnames(no2_df) || device_num_no2y %in% colnames(no2_df)) {
      no2_df[[device_num_no2x]] <<- ifelse(as.Date(ymd_hms(no2_df$Time)) == extracted_date, no2_df[[device_num_no2y]], no2_df[[device_num_no2x]])
      
      no2_df <<- subset(no2_df, select = names(no2_df) != device_num_no2y)
      
      colnames(no2_df)[which(names(no2_df) == device_num_no2x)] <<- device_num_no2
    }
    
    if (device_num_so2x %in% colnames(so2_df) || device_num_so2y %in% colnames(so2_df)) {
      so2_df[[device_num_so2x]] <<- ifelse(as.Date(ymd_hms(so2_df$Time)) == extracted_date, so2_df[[device_num_so2y]], so2_df[[device_num_so2x]])
      
      so2_df <<- subset(so2_df, select = names(so2_df) != device_num_so2y)
      
      colnames(so2_df)[which(names(so2_df) == device_num_so2x)] <<- device_num_so2
    }
    
    if (device_num_o3x %in% colnames(ozone_df) || device_num_o3y %in% colnames(ozone_df)) {
      ozone_df[[device_num_o3x]] <<- ifelse(as.Date(ymd_hms(ozone_df$Time)) == extracted_date, ozone_df[[device_num_o3y]], ozone_df[[device_num_o3x]])
      
      ozone_df <<- subset(ozone_df, select = names(ozone_df) != device_num_o3y)
      
      colnames(ozone_df)[which(names(ozone_df) == device_num_o3x)] <<- device_num_o3
    }
    
  }
  
}

process_file("/home/shahrose/Downloads/AQI/AQI_Data/Data//2022-04-29/Unit_13_2022-04-29.csv")

files = dir('/home/shahrose/Downloads/AQI/AQI_Data/Data/', recursive = T, full.names = T, pattern = '\\.csv')

data_frames_list <- lapply(files, process_file)

process_dataframe <- function(data_frame) {
  hc <- highchart() %>%
    hc_xAxis(categories = data_frame[[1]])
  
  for(i in 2:length(colnames(data_frame))) {
    hc <- hc %>% hc_add_series(name=colnames(data_frame)[i], data = data_frame[[i]], type='line')
  }
  hc
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Plots"),
    
    selectInput('pick_dataframe', 'Please choose a chemical compund', c('Temperature',
                                                                        'Atmospheric Pressure',
                                                                        'Humidity',
                                                                        'Carbon Monoxide',
                                                                        'Dust PM 1',
                                                                        'Dust PM 10',
                                                                        'Dust PM 2.5',
                                                                        'Nitrogen Dioxide',
                                                                        'Sulphur Dioxide',
                                                                        'Ozone')),
    
    highchartOutput('hc_plot'),

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$hc_plot <- renderHighchart(
    if (input$pick_dataframe == 'Temperature') {
      process_dataframe(temperature_df)
    } else if (input$pick_dataframe == 'Atmospheric Pressure') {
      process_dataframe(atm_hpa_df)
    } else if (input$pick_dataframe == 'Carbon Monoxide') {
      process_dataframe(co_df)
    } else if (input$pick_dataframe == 'Humidity') {
      process_dataframe(humidity_df)
    } else if (input$pick_dataframe == 'Dust PM 1') {
      process_dataframe(dust_pm1)
    } else if (input$pick_dataframe == 'Dust PM 10') {
      process_dataframe(dust_pm10)
    } else if (input$pick_dataframe == 'Dust PM 2.5') {
      process_dataframe(dust_pm2.5)
    } else if (input$pick_dataframe == 'Nitrogen Dioxide') {
      process_dataframe(no2_df)
    } else if (input$pick_dataframe == 'Sulphur Dioxide') {
      process_dataframe(so2_df)
    } else if (input$pick_dataframe == 'Ozone') {
      process_dataframe(ozone_df)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
