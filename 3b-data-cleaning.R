library(tidyverse)

raw <- read_csv('https://www.dropbox.com/scl/fi/ug8tbxsdd2qtsfqwnnox1/dollar_store.csv?rlkey=fu36g6uhfpx8u644d1rpsq11i&dl=1')

ds <- janitor::clean_names(raw)


ds %>% 
  select(unit_size) %>% 
  mutate(unit_size_c = parse_number(unit_size)) %>% 
  slice(142,192,194, 1:10)
#i got an error so I am just using a slice vector to see what happened on those rows

#not super efficient
ds %>% 
  select(unit_size) %>% 
  mutate(unit_size_c = parse_number(unit_size)) %>% 
  mutate(unit_type_each = if_else(str_detect(unit_size, 'each'),'each', if_else(str_detect(unit_size, 'ounce'), 'ounce', 'changeme')))
  #filter(str_detect(unit_size,'each'))

#a better way to do this would be...
ds %>% 
  select(unit_size) %>% 
  mutate(unit_size_c = parse_number(unit_size)) %>% 
  mutate(unit_typ = str_extract(unit_size,'each'|'ounce'|'pound')) %>% 
  count(unit_typ)

ds %>% 
  select(product_info) %>% 
  mutate(product_info_c = str_remove(product_info,'Brand:'))

#remove all [] in the description
ds %>% 
  select(description) %>% 
  mutate(description = str_remove_all(description,'\\[|\\]')) %>% 
  #how to turn empty strings into NA
  mutate(description = na_if(description, ''))



# Convert to number formats: price, star_rating, review_count, stock_status, unit_size
#   Goals:   (1) Don't lose information
#            (2) Missing where appropriate, but only where appropriate



# Create usable brand and product_name columns from the product_info column
#   Hints:  (1) Ensure missingness is appropriate. 
#           (2) If possible, exploit any repeated structure in the column


# Create usable brand and product_name columns from the product_info column
#   Hints:  (1) Ensure missingness is appropriate. 
#           (2) If possible, exploit any repeated structure in the column

ds %>% 
  select(product_info) %>% 
  #got an error that said Expected 2 pieces in each element of `product_info`... so we used too_many = 'merge'
  separate_wider_delim(product_info, delim = ' - ', names = c('usable_brand','product_name'), too_many = 'merge')


# Convert date columns to date and/or date-time columns, then calculate how many
# days each product sat on the shelf before its first sale.

ds %>% 
  select(date_added,first_sold_day,first_sold_time)

library(lubridate)

ds %>%
  mutate(date_added_c = dmy(date_added)) %>% 
  #mutate(first_sold_day_c = mdy(first_sold_day))%>% 
  mutate(first_sold_date_time=str_c(first_sold_day,first_sold_time)) %>% 
  mutate(first_sold_date_time_c = mdy_hms(first_sold_date_time)) %>% 
  select(first_sold_date_time,first_sold_date_time_c)

#how to combine a date and time column into one column.
ds %>% 
  select(date_added, first_sold_day, first_sold_time) %>% 
  mutate(date_added_c = dmy(date_added)) %>% 
  mutate(first_together = str_c(first_sold_day, first_sold_time, sep = ' ')) %>% 
  mutate(first_dt = mdy_hms(first_together))

