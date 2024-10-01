library(tidyverse)
library(mongolite)

umps23 <- read_csv("data/ump_db_cape23.csv") %>%
      select(
            ump = home_plate_umpire,
            trackmanID = tm_game_id
      ) %>%
      filter(!grepl("[/;]", ump)) # remove games that were double umped

umps24 <- read_csv("data/trackman 2024 umps.csv") %>%
      select(
            ump = fullName,
            trackmanID
      )


cape23 <- read_csv("data/cape trackman 2023.csv") %>%
      filter(PitchCall %in% c("BallCalled", "StrikeCalled")) %>%
      mutate(CalledStrike = 
                   case_when(PitchCall == "StrikeCalled" ~ 1,
                             PitchCall == "BallCalled" ~ 0)
      ) %>%
      filter(!is.na(PlateLocSide)) %>%
      select(GameID, PitchCall, PlateLocHeight, PlateLocSide) %>%
      mutate(GameID = substr(GameID, 1, nchar(GameID) - 2),
             GameID = case_when(
                   GameID == '20230726-DunkinDonuts' ~ '20230726-CCBLHyannis',
                   TRUE ~ GameID
                   )
             )

connection_string = # secret
connection = # secret

cape24 <- connection$find(
      query = '{"PitchCall": {"$in": ["BallCalled", "StrikeCalled"]}}',
      fields = '{"PlateLocSide": 1, "PlateLocHeight": 1, "PitchCall": 1, "GameID": 1, "_id": 0}'  # Select specific fields and exclude the _id field
) 

cape24 <- cape24 %>%
      mutate(GameID = substr(GameID, 1, nchar(GameID) - 2))


full23 <- full_join(
      cape23,
      umps23,
      join_by(GameID == trackmanID)
) %>% drop_na()

full24 <- full_join(
      cape24,
      umps24,
      join_by(GameID == trackmanID)
) %>% drop_na() 

full_df <- rbind(full23, full24) %>%
      mutate(CalledStrike = 
                   case_when(PitchCall == "StrikeCalled" ~ 1,
                             PitchCall == "BallCalled" ~ 0)
      )

# run the overall model ####

library(caret)
set.seed(255)

full_df$CalledStrike <- factor(full_df$CalledStrike, levels = c(0, 1)) # switch it to factors instead of numbers

# partition data
trainIndex <- createDataPartition(full_df$CalledStrike, 
                                  times=1, 
                                  p = .8, 
                                  list = FALSE)
train <- full_df[trainIndex, ]
test <- full_df[-trainIndex, ]

# train the knn model, use crossvalidation (ten fold specifically) to choose best k value
knnModel <- train(
      CalledStrike ~ PlateLocSide + PlateLocHeight, 
      data = train, 
      method = "knn", 
      trControl = trainControl(method = "cv"), 
      tuneGrid = data.frame(k = c(7,8,9,10,11,12))
)

# print best k value
knnModel$bestTune$k
# its 12 it turns out

# train the actual best model
best_model<- knn3(
      CalledStrike ~ PlateLocSide + PlateLocHeight,
      data = train,
      k = knnModel$bestTune$k
)

# testing the model
predictions <- predict(best_model, test,type = "class")
# Calculate confusion matrix
cm <- confusionMatrix(predictions, test$CalledStrike)
cm

# 88% accuracy, decent

# creating the individual models and then running them ####

# model training function
train_model <- function(data) {
      knn3(
            CalledStrike ~ PlateLocSide + PlateLocHeight,
            data = data,
            k = 9 # im gonna go 3 less than the best for the overall model because the size for each ump will be so much smaller
      )
}

umpire_names <- unique(full_df$ump) # list of unique ump names
grouped_models <- vector("list", length = length(umpire_names)) # empty vector for the ump models

# loop through each umps name and then generate that umps model
# this may seem worse than a map but it is better because it accurately names each model
# when I was mapping before I was getting a glitch where the wrong ump names where being assigned to each model
for (i in seq_along(umpire_names)) {
      umpire <- umpire_names[i]
      umpire_data <- filter(full_df, ump == umpire)
      grouped_models[[i]] <- train_model(umpire_data)
}

# add the names on, for some reason this works instead of the map, I don't understand why but fuck it we ball
names(grouped_models) <- umpire_names

# now we make predictions
predictions <- map(names(grouped_models), ~{
      umpire <- .x
      umpire_data <- filter(full_df, ump == umpire)
      if (nrow(umpire_data) > 0) {
            preds <- predict(grouped_models[[umpire]], umpire_data, type = "prob")[, 2]
            data.frame(ump = umpire, PlateLocSide = umpire_data$PlateLocSide, PlateLocHeight = umpire_data$PlateLocHeight, indiv_prob = preds)
      } else {
            NULL
      }
}) %>%
      bind_rows()



# running the overal model on the dataframe that has the individual predictions ####
predictions <- cbind(predictions, predict(best_model, predictions, type = "prob")[, 2]) %>%
      rename(
            ovr_prob = 'predict(best_model, predictions, type = "prob")[, 2]'
      )

predictions$prob_diff <- (predictions$indiv_prob - predictions$ovr_prob) * 100

home_plate_segments <- data.frame(
      x = c(0, 0.71, 0.71, 0, -0.71, -0.71),
      y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15),
      xend = c(0.71, 0.71, 0, -0.71, -0.71, 0),
      yend = c(0.15, 0.3, 0.5, 0.3, 0.15, 0.15)
)

# plots ####
predictions %>% 
      filter(ump == "Mike Hinojosa", between(PlateLocHeight, 1, 6)) %>%
      ggplot(aes(PlateLocSide, PlateLocHeight, z = prob_diff)) +
      geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                   inherit.aes = FALSE, color = "black") +
      #facet_wrap(~home_plate_umpire, ncol = 5) +
      stat_summary_hex(bins = 15) +
      # scale_fill_gradient2(#limits = c(-35, 35), 
      #       breaks = c(-20, 0, 20),
      #       labels = c("-20%", "0%", "20%"),
      #       low = "lightblue", mid = "white", high = "red") +  # Adjust color gradient as needed
      labs(x = "Plate side (ft.)", y = "Plate height (ft.)", title = "Difference from average", fill = "") +
      geom_rect(
            xmin = -(22.2/2)/12,
            xmax = (22.2/2)/12,
            ymin = 1.5,
            ymax = 3.6,
            color = "black",
            alpha = 0
      ) +
      coord_equal() + 
      theme_classic(base_size = 20) +
      xlim(-2.5, 2.5) + ylim(0, 6.5)

# average zone
predictions %>% 
      filter(ump == "Mike Hinojosa", between(PlateLocHeight, 1, 6)) %>%
      ggplot(aes(PlateLocSide, PlateLocHeight, z = indiv_prob)) +
      geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                   inherit.aes = FALSE, color = "black") +
      #facet_wrap(~home_plate_umpire, ncol = 5) +
      stat_summary_hex(bins = 15) +
      scale_fill_gradient2(#limits = c(-35, 35), 
            breaks = c(.2, .50, .8),
            labels = c("20%", "50%", "80%"),
            low = "white", high = "red") +  # Adjust color gradient as needed
      labs(x = "Plate side (ft.)", y = "Plate height (ft.)", title = "Chance of a strike", fill = "") +
      geom_rect(
            xmin = -(22.2/2)/12,
            xmax = (22.2/2)/12,
            ymin = 1.5,
            ymax = 3.6,
            color = "black",
            alpha = 0
      ) +
      coord_equal() + 
      theme_classic(base_size = 20) +
      xlim(-2.5, 2.5) + ylim(0, 6.5)
