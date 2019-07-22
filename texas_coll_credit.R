
library(tidyverse)
library(rvest)

urls <- c(
  'https://rptsvr1.tea.texas.gov/adhocrpt/Standard_Reports/CTE_Students_College_Credit/2014/CTE_Student_College_Credit_2013_14.html',
  'https://rptsvr1.tea.texas.gov/adhocrpt/Standard_Reports/CTE_Students_College_Credit/2015/CTE_Student_College_Credit_2014_15.html',
  'https://rptsvr1.tea.texas.gov/adhocrpt/Standard_Reports/CTE_Students_College_Credit/2016/CTE_Student_College_Credit_2015_16.html',
  'https://rptsvr1.tea.texas.gov/adhocrpt/Standard_Reports/CTE_Students_College_Credit/2017/CTE_Student_College_Credit_2016_17.html',
  'https://rptsvr1.tea.texas.gov/adhocrpt/Standard_Reports/CTE_Students_College_Credit/2018/CTE_Student_College_Credit_2017_18.html'
  )



for (url in urls){
  # get the page
  webpage <- read_html(url)
  
  # get the text from the tables
  test <- webpage %>% 
    html_nodes('table') %>% 
    html_nodes('td') %>% 
    html_text()
  
  # convert to tibble
  test <- enframe(test)
  
  # get rid of blank value cells (actually ASCII code 160 character instead of blank or space)
  test <- test %>% 
    filter(value != intToUtf8(160))
  
  # convert asterisk values to N/A, which is what's used in latest report
  test <- test %>% 
    mutate(value = ifelse(value == '*','N/A',value))
  
  
  test <- test %>% 
    # first round identifies values with regular expressions
    mutate(category = case_when(
      grepl('Counts of', value) ~ 'boilerplate',
      value == 'Texas Education Agency' ~ 'boilerplate',
      str_detect(value,'indicates counts or percentages are not available') ~ 'boilerplate',
      str_detect(value,'November.*at.*[PM|AM]') ~ 'boilerplate',
      str_detect(value, '20\\d\\d-\\d{2,4}') ~ 'school_yr',
      !is.na(as.numeric(value)) & nchar(value) == 6 ~ 'district_id',
      !is.na(as.numeric(value)) & nchar(value) == 8 ~ 'service_id',
      value == 'Total' ~ 'service_description',
      TRUE ~ NA_character_
    )) %>% 
    # second round identifies values by what's in the cell above them
    mutate(category = case_when(
      lag(category) == 'district_id' ~ 'district_name',
      lag(category) == 'service_id' ~ 'service_description',
      TRUE ~ category
    )) %>%
    # third round identifies values by what was just added to the cell above them
    mutate(category = case_when(
      # 2013-14 doesn't include Charter Status, so have to check for NA category
      # to prevent overwriting service_id
      is.na(category) & lag(category) == 'district_name' ~ 'charter_status',
      TRUE ~ category
    )) %>%
    # fourth round, add labels for data columns
    mutate(category = case_when(
      lag(category) == 'service_description' ~ 'CTE_earn_CC',
      lag(category,2) == 'service_description' ~ 'CTE_CC_hours',
      lag(category,3) == 'service_description' ~ 'CTE_avg_hours',
      lag(category,4) == 'service_description' ~ 'All_earn_CC',
      lag(category,5) == 'service_description' ~ 'All_CC_hours',
      lag(category,6) == 'service_description' ~ 'All_avg_hours',
      TRUE ~ category
    ))
  
  # get rid of boilerplate values
  test <- test %>% 
    filter(category != 'boilerplate')
  
  # create type category so can get rid of non-measurement columns
  test <- test %>% 
    mutate(type = ifelse(category %in% c('school_yr', 'district_id','district_name','charter_status',
                                         'service_id','service_description'),
                         'id','measure'))
  
  
  # add id columns
  test <- test %>% 
    mutate(school_yr = ifelse(category=='school_yr',value, NA)) %>%
    fill(school_yr) %>% 
    mutate(dist_id = ifelse(category == 'district_id',value,NA)) %>% 
    fill(dist_id) %>% 
    mutate(district = ifelse(category=='district_name',value,NA)) %>% 
    fill(district) %>% 
    mutate(charter_status = ifelse(category=='charter_status',value,NA)) %>% 
    fill(charter_status) %>% 
    mutate(service_id = ifelse(category=='service_id',value,NA)) %>% 
    fill(service_id) %>% 
    mutate(service = ifelse(category=='service_description',value,NA)) %>% 
    fill(service)
  
  # get rid of the id rows, rearrange columns
  test <- test %>% 
    filter(type=='measure') %>% 
    select(school_yr, dist_id, district, charter_status, service_id, service, category, value)
  
  
  # add to cc_data
  if (exists('cc_data')) {
    cc_data <- bind_rows(cc_data, test)
  } else {
    cc_data <- test
  }
  
  # pause for 10 seconds so website doesn't think it's under attack
  Sys.sleep(10)
}



