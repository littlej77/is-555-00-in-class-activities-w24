library(tidyverse)
library(tidymodels)

# Super simple scenario: build a regression model to predict `hwy` mpg from `cty` mpg and engine `displ`.
# The `mpg` dataset is preloaded into every R session:
mpg

# The initial split operation will handle the train/test split for you.
set.seed(42)
#allows us to randomize the same as other people. 
mpg_split <- initial_split(mpg,         # <-- dataset to be split
                           prop = 0.75, # <-- default is .75, only needed if you want a different split.
                           strata = hwy) # <-- give the dv to strata so that train/test are similar in distribution 
                            #strata = our dependent variable (the thing you are going to be predicting)
mpg_split

# Then you get your training and test sets out with appropriately named functions:
mpg_training <- mpg_split %>% training()
mpg_testing <- mpg_split %>% testing()

# Next you'll specify/plan your model:
#relevant model types: logistic_reg(),linear_reg(), decision_tree(), rand_forest(), boost_tree(), 
#show_engines('logistic_reg')
 
mpg_model <- linear_reg() %>%  # <-- start with the type of model you're fitting. linear regression model 
  set_engine('lm') %>%         # <-- "engine" is the estimation technique you'd like to use. "here is how i want you to do the math"
  set_mode('regression')       # <-- "mode" is the type of dv you're predicting (classification vs. regression). how you say between regression or classification (binary or classes)

show_engines('linear_reg') #to see the engine tyeps

# Now we can fit the model using the training data:
mpg_fit <- mpg_model %>% 
  fit(hwy ~ cty + displ,    # <-- first argument is the model equation.  fit takes 2 things: the equation and the data. 
                            #hwy (dependent variable) predicted by the city and display. you can also do "." for everything else
      data = mpg_training)  # <-- here you give it the data you're training with.

# Check results with the `tidy()` function
mpg_fit %>% tidy()

# We use `predict()` to get predictions on new data.
# Note: ALWAYS returns the predictions in the same row order as the source data.
mpg_predictions <- mpg_fit %>% 
  predict(new_data = mpg_testing) # <-- new_data is the data "to score" with

# Usually we bind these predictions with a few columns from the data, including the "ground truth" column.
mpg_test_results <- mpg_testing %>% 
  select(hwy, cty, displ) %>% 
  bind_cols(mpg_predictions) #tacks on the mgp_predictions column for us to check

# We can then use these to evaluate whatever metrics feel important to us.
mpg_test_results %>%
  rmse(truth = hwy,       # <-- truth parameter is the thing the model was attempting to predict
       estimate = .pred)  # <-- estimate is the model's prediction 

# Note that all of the tidymodels evaluation functions use the same format:
mpg_test_results %>%
  rsq(truth = hwy,        # <-- here's the r-squared function
      estimate = .pred)
mpg_test_results %>%
  mae(truth = hwy,        # <-- and here's the mean absolute error function
      estimate = .pred)


#-------------step 5

# When we're done modeling, we use `last_fit()` to "finalize" a model and get associated predictions:
mpg_last_fit <- mpg_model %>% 
  last_fit(hwy ~ cty + displ,  # <-- provide the final equation (in case you dropped variables, etc.)
           split = mpg_split)  # <-- provide that split object from the beginning.

# The last fit return object is a bit complex, so we use various functions to pull things out:
mpg_last_fit %>% 
  collect_metrics()      # <-- extracts various performance metrics
mpg_last_fit %>% 
  collect_predictions()  # <-- extracts predictions from the test data