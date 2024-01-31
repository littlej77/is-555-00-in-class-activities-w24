bob <- read_csv('https://www.dropbox.com/s/mozqpceit51hia7/bob_ross.csv!dl=1')
#data starts in wide format

bob_long <- bob %>% 
  pivot_longer(
    cols = !1:4, #choosing by column index
    names_to = 'object',
    values_to = 'is_present'
  )

#which objects occur most frequently?
bob_long %>% 
  group_by(object) %>% 
  summarize(object_count = sum(is_present == 1)) %>% #summing the times where is_present is 1
  arrange(desc(object_count))

#what was the season when Bob painted the most mountains?
#we need object to me mountain and is_present to be 1
bob_long %>% 
  filter(str_detect(object, 'mountain')) %>% 
  group_by(season) %>% 
  summarize(mtn_count = sum(is_present == 1))

#other option
bob_long %>% 
  group_by(season) %>% 
  summarize(mtn_count = sum(is_present == 1 & str_detect(object, 'mountain'))) #counts the times that both conditions are true


#create a table that displays one line per attribute with a count of times that
#object was used in each season (one column per season)
bob_long %>% 
  select(season, object, is_present) %>% 
  pivot_wider(
    names_from = season,
    values_from = is_present
  ) #this gives you a warning and a really weird looking tibble
#each value is another vector of objects per episode per season
#he showed this just so we can know what this warning message means and how to fix it
bob_long %>% 
  select(season, object, is_present) %>% 
  group_by(season, object) %>% #this fixes that issue we were having
  summarize(count = sum(is_present == 1)) %>% #and this
  pivot_wider( #now you can pivot the same as above and it works
    names_from = season,
    values_from = count
  )
