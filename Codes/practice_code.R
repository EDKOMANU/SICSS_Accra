
library (caret)
library (tidyverse)
library (pROC)
library (Boruta)
library (C50)
library (caretEnsemble)
library (randomForest)


# load("Data/model_data.RData")
# full_data <- new_full_data %>% 
#              select(-c(orig.id, region, g_equitable_v2))
# 
# saveRDS(full_data, "full_data.RDS")

rm(list=ls())
full_data <- readRDS("full_data.RDS")


dta_wo_country <- full_data %>% 
                  select(-c(country))

## We set seed so that our shuffling can always begin from the same random point.
set.seed(2022)

## Shuffle the data
shuffle(dta_wo_country)

#' Boruta feature extraction method.
      #' There are several other feature extraction method that the participants should explore.
      #' In the example below, I've asked the model to select the most important variables for 
      #' predicting gender equitable attitudes `g_equitable`
      
      boruta_output <- Boruta(g_equitable ~ ., 
                              data=na.omit(dta_wo_country),
                              doTrace=2)
      
      #' You can inspect the outcome of the extraction method
      #' Whether variables are confirmed to be important, tentative or rejected. 
      head(boruta_output$finalDecision)
      
      
      # Plot variable importance
      plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  
      
      
      #' Keep only the confirmed important covariates.
      confirmed_df <-  attStats(boruta_output) %>% 
                       filter (decision == "Confirmed")
      
      #' We can also use the function/code below to select only confirmed attributes.
      #' Change  withTentative = T to get tentative (important) variables.
      boruta_conf <- getSelectedAttributes(boruta_output, withTentative = F)
      
      ## Inspect the mean/median importance of the 37 variables.
      head(confirmed_df)

      
###
      
      dta_clean <- full_data %>% 
        select(all_of(rownames(confirmed_df[confirmed_df$meanImp >=5,])),
               g_equitable, country) %>% 
        dplyr::rename(religion_accept = "q170_the_only_acceptable_religion_is_my_religion",
                      stigma_aids = "q20_neighbors_people_who_have_aids",
                      work_changes = "q43_future_changes_less_importance_placed_on_work",
                      labour_migrat = "q122_immigration_in_your_country_fills_useful_jobs_in_the_workforce",
                      reli_demo = "q242_democracy_religious_authorities_interpret_the_laws",
                      realWoman = "h339_only_when_a_woman_has_a_child_is_she_a_real_woman",
                      marriage_ready = "h332_a_girl_is_ready_for_marriage_once_she_starts_menstruating",
                      famDec_giMarriage = "h333_a_girl_should_honour_the_decisions_wishes_of_her_family_even_if_she_does_not_want_to_marry",
                      famDec_boMarriage = "h334_a_boy_should_honour_the_decisions_wishes_of_his_family_even_if_he_does_not_want_to_marry",
                      social_activ = "q287_social_class_subjective",
                      demo_equal = "q249_democracy_women_have_the_same_rights_as_men") %>% 
        mutate(stigma_aids = ifelse((stigma_aids == 2), "Not Desired", "Don't Mind") %>% as.factor(),
               religion_accept = derivedFactor("Strongly Agree" = (religion_accept == 1),
                                               "Agree" = (religion_accept == 2),
                                               "Disagree" = (religion_accept == 3),
                                               "Strongly Disagree" = (religion_accept == 4),
                                               .default = NA),
               labour_migrat = derivedFactor("Disagree" = (labour_migrat == 0),
                                             "Hard to say" = (labour_migrat == 1),
                                             "Agree" = (labour_migrat == 2),
                                             .default = NA),
               marriage_ready = derivedFactor("Strongly Agree" = (marriage_ready == 1),
                                              "Agree" = (marriage_ready == 2),
                                              "Disagree" = (marriage_ready == 3),
                                              "Strongly Disagree" = (marriage_ready == 4),
                                              .default = NA),
               famDec_boMarriage = derivedFactor("Strongly Agree" = (famDec_boMarriage == 1),
                                                 "Agree" = (famDec_boMarriage == 2),
                                                 "Disagree" = (famDec_boMarriage == 3),
                                                 "Strongly Disagree" = (famDec_boMarriage == 4),
                                                 .default = NA),
               famDec_giMarriage = derivedFactor("Strongly Agree" = (famDec_giMarriage == 1),
                                                 "Agree" = (famDec_giMarriage == 2),
                                                 "Disagree" = (famDec_giMarriage == 3),
                                                 "Strongly Disagree" = (famDec_giMarriage == 4),
                                                 .default = NA),
               realWoman = derivedFactor("Strongly Agree" = (realWoman == 1),
                                         "Agree" = (realWoman == 2),
                                         "Disagree" = (realWoman == 3),
                                         "Strongly Disagree" = (realWoman == 4),
                                         .default = NA),
               social_activ = derivedFactor("Upper class" = (social_activ == 1),
                                            "Upper middle class" =(social_activ == 2),
                                            "Lower middle class" = (social_activ == 3),
                                            "Working class" =(social_activ == 4),
                                            "Lower class" = (social_activ == 5),
                                            .default=NA),
               
               
               work_changes = derivedFactor("Good" = (work_changes == 1),
                                            "Don't mind" =(work_changes == 2),
                                            "Bad" = (work_changes == 3),  .default=NA),
               demo_equal = as.numeric(demo_equal),
               reli_demo = as.numeric(reli_demo),
               demo_equal = ifelse(demo_equal > 5,
                                    "Not so important",
                                    "Important") %>% as.factor(),
                reli_demo = ifelse(reli_demo > 5,
                                   "Not so essential",
                                   "Essential") %>% as.factor()
        ) %>% drop_na()
      

      
### Modelling --- Specification
      
      set.seed(2022)
      #' Create a partition index.
      validation_index <- createDataPartition(dta_clean$g_equitable,
                                              p=0.70, list=FALSE)
      
      #' We are training our model based on 70% of the data
      #' and we will validate with 30%.
      test_data <- dta_clean[-validation_index,]
      train_data <- dta_clean[validation_index,]
      
      

      #' Training the model
      #' You should read more about the different methods here: 
      #' https://topepo.github.io/caret/model-training-and-tuning.html
      #' I've set it as 5 for this practice but I've usually used 10 and 10 repeats in real life.
      #' 
      #' There are a few things worth highlighting here:
      #' sampling==smote is usually very helpful to help balance the distribution of samples.
      #
      
      fitControl <- trainControl(method= "repeatedcv",
                                 number = 5,
                                 repeats = 5,
                                 sampling="smote",
                                 search = "random",
                                 classProbs = TRUE,
                                 summaryFunction = twoClassSummary)
      
      ## You can also read more about the different models supported by caret, the tunning parameters, etc
      # https://topepo.github.io/caret/available-models.html
      ### Logistic Reg
      set.seed(2022)
      model_logis <- train(g_equitable ~.,
                           train_data,
                           tuneLength = 10,
                           method="glm",metric="ROC",
                           trControl= fitControl)
      
      
      #svm
      set.seed(2022)
      model_svm <-train(g_equitable~.,
                        base_data,
                        method="svmRadial",
                        tuneLength = 10,
                        trControl= fitControl,
                        tuneGrid = expand.grid(sigma = 0.029,
                                               C = 0.819),
                        metric= "ROC")
      

      #' randomforest
      #' In the code below, I'm letting caret find an optimal 
      #' value between 1 and 3 for mtry and the best performing model.
      #' You can adjust the value but also bear in mind that any change
      #' will affect how long it takes for the model to converge.
      
      set.seed(2022)
      model_rf <- train(g_equitable~.,
                        data=base_data,
                        method="rf", 
                        tuneLength = 10,
                        tuneGrid = expand.grid(mtry=c(1:3)),
                        trControl= fitControl, 
                        metric= "ROC")
      
      
      # ## GBM
      set.seed(2022)
      model_gb <- train(g_equitable~.,
                        base_data,
                        method="gbm",
                        metric= "ROC",
                        tuneLength=8,
                        tuneGrid = expand.grid(interaction.depth = 17,
                                               n.trees = 300,
                                               shrinkage = 0.1,
                                               n.minobsinnode = 20),
                        trControl= fitControl)
      
      
      #R-part 2
      set.seed(2022)
      model_rpt <- train(g_equitable~.,
                         base_data,
                         method="rpart2",
                         metric= "ROC",
                         tuneLength=10,
                         tuneGrid = expand.grid(maxdepth=c(4:6)),
                         trControl= fitControl)
      
      

### Modelling --- Validating the models.
      
      #' You can change the model_rpt object below to 
      #' evaluate the accuracy of the other models
      
      prediction <- predict(model_rpt, test_data)
      cm_knn <- confusionMatrix(prediction, test_data$g_equitable,
                                positive = "Equitable",
                                mode = "everything")
      
   
### Ensemble Modelling ===
      
      
      #Training the model
      fitControl <- trainControl(method= "repeatedcv",
                                 number = 10,
                                 repeats = 10,
                                 sampling="smote",
                                 verbose=TRUE,
                                 search = "random",
                                 classProbs = TRUE,
                                 savePredictions = "final",
                                 # index=createResample(base_data$g_equitable, 25),
                                 summaryFunction = twoClassSummary)
      
      
      ## Ensemble
      set.seed(2022)
      model_list <- caretList(g_equitable ~.,
                              train_data,
                              trControl=fitControl,
                              metric="ROC",
                              methodList=c("glm"),
                              tuneList=list(
                                rpt=caretModelSpec(method="rpart2",
                                                   tuneGrid = expand.grid(maxdepth = 6)),
                                rf=caretModelSpec(method="rf",
                                                  tuneGrid = expand.grid(mtry=6)),
                                xgb=caretModelSpec(method="gbm",
                                                   tuneGrid = expand.grid(interaction.depth = 17,
                                                                          n.trees = 300,
                                                                          shrinkage = 0.1,
                                                                          n.minobsinnode = 20)))
      )

      #' You can access the correlation between the models using the code below.
      #' Some models may be good at finding people with less equitable attitudes
      #' while others are good at finding those with equitable attitudes.
      modelCor(resamples(model_list))
      
      
      greedy_ensemble <- caretEnsemble(
        model_list,
        metric="ROC",
        trControl=trainControl(
          sampling="smote",
          method="repeatedcv",
          savePredictions="final",
          number=5,
          repeats = 5,
          classProbs=TRUE,
          summaryFunction=twoClassSummary
        ))
      
      summary(greedy_ensemble)
      
      # Again you can evaluate the performance of the model and compare with the individual models
      #' is the ensemble (combination of models)  better than the individual models?
      
      prediction <- predict(greedy_ensemble, test_data)
      cm_knn <- confusionMatrix(prediction, test_data$g_equitable,
                                positive = "Equitable",
                                mode = "everything")
      
      