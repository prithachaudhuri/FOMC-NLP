#----------Extracting Data from FRED and Quandl-----------------------------------------
# Author: Pritha Chaudhuri
# Date: 5/15/2020
#---------------------------------------------------------------------------------------

#----------FRED Data--------------------------------------------------------------------
# Get fred codes from csv file
fredmetrics <- read.csv("data/fredmetrics.csv")
# Set API
apikey <- "FRED API key here"
fredr_set_key(apikey)

# catch the fredcodes with changed names
error <- c()
for (i in 1:length(fredmetrics$fred_code)){
  try <- try(fredr(series_id=fredmetrics$fred_code[i],
                   observation_start=as.Date("1992-01-01"),
                   observation_end=as.Date("2019-12-31")))
  if("try-error" %in% class(try)){
    error <- union(error,fredmetrics$fred_code[i])
  } 
}

# Error shows ISM codes abd S&P500-PE could not be found. Get these from Quandl
error <- data.frame(error)%>%
  left_join(select(fredmetrics,fred_code,description),by=c("error"="fred_code"))

# Remove codes that could not be found before extracting data
fredmetrics <- filter(fredmetrics,!(fred_code %in% error$error))

# Extract data
freddata <- lapply(fredmetrics$fred_code, function(x)
  fredr(series_id=x,
        observation_start=as.Date("1992-01-01"),
        observation_end=as.Date("2019-12-31"),
        frequency = "m"))

freddata <- bind_rows(freddata) %>% 
  left_join(select(fredmetrics, transform_code, slow_move_code, fred_code), by = c("series_id"= "fred_code"))

# Save in csv file
write.csv(freddata, file = "data/freddata.csv")

#----------Quandl Data------------------------------------------------------------------
# Get Quandl codes from csv file
quandlmetrics <- read.csv("data/quandlmetrics.csv")
Quandl.api_key("Quandl API key here")

# Extract data, to make sure there is parity with names the series_id is the fred code
quandldata <- lapply(1:length(quandlmetrics$quandl_code), function(x)
  Quandl(quandlmetrics$quandl_code[x], 
         type = "raw", 
         start_date="1992-01-01", 
         end_date="2019-12-31",
         order = "asc")%>%
    rename_all(tolower)%>%
    select(contains("date"),contains("index"),contains("value"))%>%
    rename_at(vars(-"date"),~"value")%>%
    mutate(series_id=quandlmetrics$fred_code[x])) 

quandldata <- bind_rows(quandldata) %>% 
  left_join(select(quandlmetrics, transform_code, slow_move_code, fred_code), by = c("series_id"= "fred_code"))

# Save in csv file
write.csv(quandldata, file = "data/quandldata.csv")


#----------Combine Data-----------------------------------------------------------------
# Combine Fred and Quandl data and transform variables using transform_code
# 1: no transformation, 2: first difference (no such variables)
# 4: log, 5: first difference of log

favardata <- bind_rows(freddata, quandldata) %>% 
  group_by(series_id) %>% 
  mutate(trans_value = case_when(transform_code == 1 ~ value, 
                                 transform_code == 4 ~ log(value),
                                 transform_code == 5 ~ log(value)-lag(log(value))),
         ym = format(as.Date(date),"%Y-%m"),
         year = year(date),
         month = month(date)) %>% 
  ungroup() %>% 
  select(ym, year, month, series_id, trans_value) 

# Delete some variables that have NAs, add the LSAP index to create the dataset for FAVAR analysis
favardata <- favardata[-c(24998,24990,25333),] %>%
  spread(series_id, trans_value) %>% 
  left_join(index_lsap, by = c("year", "month")) %>% 
  mutate(lag = na.locf(indexlsap, na.rm = F),
         index_lsap = if_else(is.na(indexlsap), round(lag, 4), round(indexlsap, 4)),
         index_lsap = if_else(is.na(index_lsap), 0, index_lsap)) %>% 
  select(-document, -doc, -indexlsap, -lag, -SP500) %>% 
  filter(year %in% c(1993:2019))

# save in csv
write.csv(favardata, "data/favardata.csv", row.names = F)

# visualize dataset
head(favardata)