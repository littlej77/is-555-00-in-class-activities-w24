library(tidyverse)
library(tidymodels)

# Okay let's work through the (cleaned) titanic data from last week:
titanic <- read_csv('https://www.dropbox.com/s/92funarubgk5rzh/titanic_clean.csv?dl=1')

#------------------------general flow-------------------------------------------

# 1. setup train/test split
# 2. model setup
# 3. fit the model
# 4. evaluate
# 5. finalize

#-------------------------------------------------------------------------------
# First let's make sure factors are factors
#be aware of categories that are intended to be treated as a category. factors have different levels, 
#so "had_cabin" is binary but they are essentially categories (yes they did or no they didnt). so we need to convert them
leo <- titanic %>% 
  mutate(across(c(survived,had_cabin, sex), ~as.factor(.x)))
#see now they are <fct> instead of <dbl>


# Now let's do a train/test split
#intial split() function takes in some data and splits (75,25)
set.seed(42)
leo_split <- initial_split(leo,
                               prop= 0.75,
                               strata= survived) #dependent variable. what we are trying to predict.handles disproportionately split dependent variables

leo_training <- leo_split %>% training()
leo_testing <-  leo_split %>% testing()

# Plan the model setup, including the engine and mode
leo_spec <- logistic_reg() %>% 
  set_engine('glm') %>% 
  set_mode('classification')

show_engines('logistic_reg') #to see the engines options and mode

# relevant model types: logistic_reg(), linear_reg(), decision_tree(), rand_forest(), boost_tree()
# show_engines('logistic_reg')




# Now fit a model, look at output with tidy()
leo_fit <- leo_spec %>% 
  fit(survived ~ pclass+had_cabin+sex+age+sib_sp+parch+fare,#what you are predicting ~ as predicted by.... can also just do survived~ .
      data= leo_training) #data you want to train

leo_fit %>% tidy() #tidys up the output


# Calculate predictions, 
# including class predictions _and_ probabilities
#to get predictions from our test set

leo_preds <- leo_fit %>% 
  predict(new_data = leo_testing)

#see our data with the predictions for us to see it more clearly and evaluate
leo_test <- leo_testing %>% 
  bind_cols(leo_preds)

#-------------------------------------------------------------------------------
#trying another model
#-------------------------------------------------------------------------------
# First let's make sure factors are factors
#be aware of categories that are intended to be treated as a category. factors have different levels, 
#so "had_cabin" is binary but they are essentially categories (yes they did or no they didnt). so we need to convert them
rose <- titanic %>% 
  mutate(across(c(survived,had_cabin, sex), ~as.factor(.x)))
#see now they are <fct> instead of <dbl>


# Now let's do a train/test split
#intial split() function takes in some data and splits (75,25)
set.seed(42)
rose_split <- initial_split(leo,
                           prop= 0.75,
                           strata= survived) #dependent variable. what we are trying to predict.handles disproportionately split dependent variables

rose_training <- rose_split %>% training()
rose_testing <-  rose_split %>% testing()

# Plan the model setup, including the engine and mode
rose_spec <- decision_tree() %>% 
  set_engine('rpart') %>% 
  set_mode('classification')

show_engines('decision_tree') #to see the engines options and mode

# relevant model types: logistic_reg(), linear_reg(), decision_tree(), rand_forest(), boost_tree()
# show_engines('logistic_reg')

# Now fit a model, look at output with tidy()
rose_fit <- rose_spec %>% 
  fit(survived ~ pclass+had_cabin+sex+age+sib_sp+parch+fare,#what you are predicting ~ as predicted by.... can also just do survived~ .
      data= rose_training) #data you want to train

rose_fit %>% tidy() #tidys up the output


# Calculate predictions, 
# including class predictions _and_ probabilities
#to get predictions from our test set

rose_preds <- rose_fit %>% 
  predict(new_data = rose_testing)
#when you ask for predictions. its giving us a class prediction. but we can also add a ", type="prob"" parameter, and it produce pred_0 and pred_1. which show the propability of a given row belonging to 1 or 0
#you might not want to do a .5 threshold for the prediction
rose_probs <- rose_fit %>% 
  predict(new_data = rose_testing, type = "prob")

#see our data with the predictions and probability for us to see it more clearly and evaluate
rose_test <- rose_testing %>% 
  bind_cols(rose_preds, rose_probs)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Now let's build a confusion matrix and explore a few of the related metrics.
# conf_mat(), sens()

#see individual metrics
leo_test %>% 
  sens(truth= survived,
       estimate= .pred_class) #gives a sensitivity score
#see individual metrics

leo_test %>% 
  accuracy(truth= survived,
       estimate= .pred_class) #gives a accuracy score
 
#to see most of the metrics in one code
leo_test %>% 
  conf_mat(truth= survived,
           estimate= .pred_class) %>% 
  summary()


 #yardstick::accuracy()



# Let's get fancy:
# roc_curve(), roc_auc(), autoplot()
roc_data <- leo_results %>% 
  roc_curve(truth = survived, .pred_0)

roc_data %>% 
  autoplot()

leo_results %>% 
  roc_auc(truth = survived, .pred_0)

# Finalize the model with last_fit()
#designed to take a split object & model specification and allows us to package everything up
leo_final <- leo_spec %>% 
  last_fit(survived ~ ., 
    split=leo_split)

# finalized object, extract predictions, metrics 
# with dedicated collect_* functions: