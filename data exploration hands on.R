library(tidyverse)

df1 <- read_csv('https://www.dropbox.com/s/df2w04r0c09kw3f/df_1.csv?dl=1')
df2 <- read_csv('https://www.dropbox.com/s/xfp1qzxvo19ym0x/df_2.csv?dl=1')
df3 <- read_csv('https://www.dropbox.com/s/uzusr9723ffn546/df_3.csv?dl=1')
df4 <- read_csv('https://www.dropbox.com/s/js8tehtsk7btpeq/df_4.csv?dl=1')

big_df <-  rbind(df1, df2,df3,df4)
bind_rows(df1,df2,df3,df4, .id= 'df') %>% #have it tell you which df it came from
  ggplot(aes(x=x, fill=df))+
  geom_density(alpha=.4)


#trying to understand the dataset and see whats going on. 
bind_rows(df1,df2,df3,df4, .id= 'df') %>% #have it tell you which df it came from
  ggplot(aes(x=x, y=y, color=df))+
  geom_point()+
  geom_smooth(method=lm, se=F)+
  facet_wrap(~df)


# A few sample calculations:
df1 %>% summarize(across(everything(), list(mean = ~mean(.x), 
                                            sd = ~sd(.x))))

# correlation matrix:
df1 %>% cor()

# simple regression analysis:
lm(y ~ x, data = df1) %>% coefficients()

