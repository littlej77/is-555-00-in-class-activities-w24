library(tidyverse)
library(tidymodels)

# Install these if you haven't:
# install.packages('vetiver')
# install.packages('pins')
# install.packages('plumber')
# install.packages('ranger')

library(vetiver)
library(pins)
library(plumber)
library(ranger)

install.packages("ranger")
# Data and Model Setup
housing <-  read_csv('https://www.dropbox.com/scl/fi/9s2pkk23rsqokh2hymp92/housing_training.csv?rlkey=45f14zqlghrzwrmrhu47xapy9&dl=1')

set.seed(42)
housing_split <- initial_split(housing, strata = sale_price)

housing_training <- housing_split %>% training()
housing_testing <- housing_split %>% testing()

housing_rec <- recipe(sale_price ~ .,
                      data = housing_training) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_novel(all_nominal_predictors()) #handles categories that might come in the future. like "poor". anything that it hasnt seen, it will replace with NEW 
  step_other(ms_sub_class,ms_zoning,exterior_1st,exterior_2nd,utilities,electrical) %>% 
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_nzv(all_predictors())


housing_rf_model <- rand_forest() %>% 
  set_engine('ranger') %>% 
  set_mode('regression')

housing_wkfl <- workflow() %>% 
  add_model(housing_rf_model) %>% 
  add_recipe(housing_rec)

# Assume we did all the things (crossvalidation, tuning, etc.), 
# and that we're ready to deploy. Drumroll, please.....

# What's first? (Or, I should say last?)
final_model <- housing_wkfl %>% 
  last_fit(split=housing_split)

# Now we need to extract the trained workflow from the larger final fit result:
trained_workflow <- extract_workflow(final_model)


library(vetiver) 
# And create from that (trained) workflow a deployable model object. 
# We'll use vetiver for this. Give it a name, too.
deployable_model <- vetiver_model(trained_workflow, 'housing_pricing') #takes the workflow we extracted, and then we must name our model.

# a bundled workflow, that is ready for deployment. it has a name. rebundling into an object that is compatibile with the deployement stuff

# Now let's create a plumber-based API server for local testing
# pr(), vetiver_api(), pr_run()
pr() %>% #creates a launchable plumber object. 
  vetiver_api(deployable_model) #pass it the object we just preped for deployment in the vetiver_model
  pr_run(port = 8888) #given what we have been appending here, go ahead and launch an API

  #this console is now unusable. our server is running. 
  
  
# ##### Let's go check out our temporary API.
# ##### Copy the code below into a new session (Session >> New Session)
# 
# library(tidyverse)
# library(tidymodels)
# library(vetiver)
# 
# next_months_data <- read_csv('https://www.dropbox.com/scl/fi/ztf8ipi4i4n5rfwtwcmh0/housing_testing.csv?rlkey=yecihhbp6hbf88rfr9w3kysbk&dl=1')
# 
#  donest work bc this new data has "poor" but the recipe data didn't have it so it says "missing data in columns" 
#  next_months_data %>% 
#    count(heating_qc)
# 
# local_api <- vetiver_endpoint("http://127.0.0.1:8888/predict") #address that we made above in line 65.
# 
# # Now use the standard predict function to hit the api with our new data:
# api_preds <- predict(local_api,
#                      new_data = next_months_data)
# 
# # Look at performance over the past month:
# next_months_data %>%
#   bind_cols(api_preds) %>%
#   rsq(truth = sale_price, estimate = .pred)
# 
# 
# ##### [--End Copy--]





# Let's look at publishing with the pins package:
# First, create a "pin board"
pin_board <- pins::board_folder('~/Desktop/in-class-demo/')

# Write the (deployable) vetiver model object to the board:
vetiver_pin_write(pin_board, deployable_model)


# Boards can hold lots of different models (and versions). These can be listed 
# and retreived. 
pin_board %>% pin_list()
pin_board %>% pin_versions('housing_pricing')

# Moving from a pin board to a docker-based plumber API is as simple as preparing
# the docker scripts:
vetiver_prepare_docker(pin_board, 'housing_pricing')#prepares a docker build process for us. give pin board, model name we want to instantiate in our docker build process








