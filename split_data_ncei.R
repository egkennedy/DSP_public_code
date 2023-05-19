#Split data into geographic chunks with no more than 1 million rows each

library("tidyverse")

load("full_nd.Rdata")

#Okay, let's do this as easily as possible.By lat and by date
filepath <- "NCEI_submission/subset_data/"

test_df <- function(latmin, latmax, time){
  total <- full %>% filter(latitude >= latmin & latitude < latmax) %>% 
    nrow()
  
  print(total)
  
  if (total > 1000000){
    pre <- full %>%
      mutate(year = lubridate::year(time_utc)) %>% 
      filter(latitude >= latmin & latitude < latmax & year < time) %>% 
      nrow()
    print(pre)
    
    post <- full %>% 
      mutate(year = lubridate::year(time_utc)) %>% 
      filter(latitude >= latmin & latitude < latmax & year >= time) %>% 
      nrow()
    print(post)
  } 
}

export_df <- function(latmin, latmax, time){
  if (is.na(time)){
    df <- full %>% filter(latitude >= latmin & latitude < latmax)
    
    write_csv(df, file = paste0(filepath, latmin, "_to_", latmax, "_allyears.csv"))
  } else {
    
    df <- full %>% 
      mutate(year = lubridate::year(time_utc)) %>% 
      filter(latitude >= latmin & latitude < latmax & year < time) %>% 
      select(-year)
    
    write_csv(df, file = paste0(filepath, latmin, "_to_", latmax, "_pre_", time, ".csv"))
    
    df <- full %>% 
      mutate(year = lubridate::year(time_utc)) %>% 
      filter(latitude >= latmin & latitude < latmax & year >= time) %>% 
      select(-year)
    
    write_csv(df, file = paste0(filepath, latmin, "_to_", latmax, "_post_", time, ".csv"))
  }
}


#47 N
test_df(47, 49, 2015)
export_df(47, 49, 2015)


## 45N
test_df(45, 47, 2015)
export_df(45, 47, 2015)

## 44 N
test_df(44, 45, 2015)
export_df(44, 45, NA)

## 43
test_df(43, 44, 2015)
export_df(43, 44, 2015)

## 42
test_df(42, 43, 2015)
export_df(42, 43, 2015)

## 40
test_df(40, 42, 2015)
export_df(40, 42, 2015)

## 38
test_df(38, 40, 2015)
export_df(30, 40, NA)

## 37
test_df(37, 38, 2015)
export_df(37, 38, 2015)

## 36
test_df(36, 37, 2015)
export_df(36, 37, 2015)

## 35
test_df(35, 36, 2015)
export_df(35, 36, NA)

## 34
test_df(34, 35, 2015)
export_df(34, 35, 2015)

## 32
test_df(32, 34, 2015)
export_df(32, 34, 2015)
