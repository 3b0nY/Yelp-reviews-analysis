# REQUIRED LIBRARIES + OPTIONS ---------------------------------------------

library(stringr)             # Regex
library(sf)                  # Geo package
library(rnaturalearth)       # Geo package
library(rnaturalearthdata)   # Geo package
library(ggspatial)           # Geo package
library(wordcloud)           # Text visualisation
library(caret)               # Useful functions
library(tictoc)              # Counting time
library(tidytext)            # Required libraries for text mining
library(tm)                  # Required libraries for text mining
library(SnowballC)           # Required libraries for text mining
library(xgboost)             # Required libraries for xgboost
library(Ckmeans.1d.dp)       # Required libraries for xgboost
library(randomForest)        # Required library for rf
library(glmnet)              # Required library for lr
library(tidyverse)           # Data manipulation

options(warn = -1)



# WD + DATA LOADING -------------------------------------------------------

# Setting working directory
setwd("C:/Users/PC/Downloads/Yelp/")

# Convert JSON files to data frames
business <- jsonlite::stream_in(file("yelp_academic_dataset_business.json"),pagesize = 100000) %>% jsonlite::flatten()
# S tymito som sa nakoniec rozhodol nepracovat
# checkin  <- jsonlite::stream_in(file("yelp_academic_dataset_checkin.json"),pagesize = 100000)
# tip      <- jsonlite::stream_in(file("yelp_academic_dataset_tip.json"),pagesize = 100000)

# Before loading run read_json.r
review <- read.csv("review.csv/part-00000-a13d76fb-3c2a-407f-a195-39c18f6fb041-c000.csv")
user   <- read.csv("user.csv/part-00000-b7938a9f-db7d-4314-a48d-5c40f4d55530-c000.csv")



# DATA FILTERING + VISUALISATIONS ------------------------------------------

# Check NA values ratio and remove columns with lot of NAs (ca.50%) & only restau - business dataset
na_count <- sapply(business, function(x) sum(length(which(is.na(x)))))/nrow(business)
business <- business %>% filter(is_open == 1) %>% select(-starts_with("attributes"))

# Analyzing only restaurants/gastronomy
restaurants <- business %>% filter(grepl('Restaurant', categories)); rm(business)
restaurants$business_id %>% unique %>% length()    # Check for unique restaurants
restaurants <- restaurants %>% 
  mutate(state=if_else(state=="MB","AB",state),
         state=if_else(state=="AL","AB",state),
         longitude=if_else(state=="TX",-98,longitude),
         state=if_else(city=="Macedonia","OH",state),
         longitude=if_else(city=="Omaha",-95,longitude),
         latitude=if_else(city=="Omaha",41.5,latitude),
         state=if_else(state=="WA","PA",state),
         state=if_else(state=="FL","OH",state),
         state=if_else(state=="BC","ON",state))%>%
  filter(state!="HPL")

# Data preparation for analysis
sp_data <- restaurants %>% 
  group_by(state,city) %>% 
  summarize(longitude = median(longitude), latitude = median(latitude), count = n()) %>%
  arrange(desc(count)) %>% as.data.frame()

# All restaurants per state
sum_per_state <- sp_data%>%group_by(state)%>%
  summarise(count=sum(count), longitude=median(longitude), latitude=median(latitude))%>%
  arrange(desc(count))

# Cities with highest num of reviewed restaurants per state
max_per_state <- sp_data%>%group_by(state)%>%summarise(count = max(count))%>%arrange(desc(count))%>%
  inner_join(sp_data, by = c("count","state"))%>%
  as.data.frame()

# Where is the most 5 stars restaurants?
top_rest <- restaurants %>% filter(stars==5) %>%group_by(state)%>%
  summarise(count=n(), longitude=median(longitude), latitude=median(latitude))%>%
  arrange(desc(count))


### World visualization
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")

# Visualisation of aggregated counts of restaurants per state
plt1 <- ggplot(data = world) +
  geom_sf(fill= "antiquewhite") +
  geom_point(data=sum_per_state,aes(x=longitude, y=latitude, size=count)) +
  geom_text(data=sum_per_state,aes(x=longitude, y=latitude+1.2, label=paste0(state,", ",count))) +
  geom_point(data=max_per_state%>%filter(count>100),aes(x=longitude, y=latitude, color=count)) +
  geom_text(data=max_per_state%>%filter(count>100),aes(x=longitude, y=latitude, label=city)) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.55, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-125, -60), ylim = c(25, 53), expand = FALSE)+ 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))+ 
  ggtitle("Aggregated counts of restaurants reviewed in Yelp per state")

# Visualisation of number of 5 stars restaurants per state
plt2 <- ggplot(data = world) +
  geom_sf(fill= "antiquewhite") +
  geom_point(data=top_rest,aes(x=longitude, y=latitude, size=count)) +
  geom_text(data=top_rest,aes(x=longitude, y=latitude+1.2, label=paste0(state,", ",count))) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.55, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-125, -60), ylim = c(25, 53), expand = FALSE)+ 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue")) + 
  ggtitle("Number of 5 stars restaurants reviewed in Yelp per state")

# Combined plot
cow <- cowplot::plot_grid(plotlist = list(plt1,plt2), nrow = 2)
print(cow)


# TOP influencers by useful reviews and by number of fans 
# - joining review, user and business dataset together
joined_data <- review %>% select(business_id,user_id) %>% 
                  inner_join(user, by = "user_id") %>%
                  inner_join(restaurants %>% 
                    select(city, state, latitude, longitude, business_id), by = "business_id") %>% 
                  mutate(ones=1)
# Grouping by users, cities and states
grouped_data <- joined_data %>% group_by(user_id,city,state,name) %>% 
                  summarise(average_stars=mean(average_stars), cool=mean(cool),
                            fans=mean(fans), funny=mean(funny), review_count=mean(review_count),
                            useful=mean(useful),latitude=mean(latitude),
                            longitude=mean(longitude),count=sum(ones)) %>% 
                  mutate(usefulness_ratio=round(useful/review_count,2))

# Analysis of influencers by usefulness
usefulness <- grouped_data %>% arrange(desc(useful),desc(count)) %>% ungroup()
top_10_use <- usefulness[c(1,3,6,7,9,10,11,12,15,17),]
g1 <- ggplot(data=top_10_use, aes(x=reorder(name, -useful), y=useful, fill = state)) +
        geom_bar(stat="identity", width=0.75) +
        geom_text(aes(label=useful), vjust=1.5, color="white", size=3)+
        geom_text(aes(label=city), vjust=3, color="white", size=3)+
        theme_minimal() + xlab(label = "Top 10 influencers by usefulness of reviews")

# Analysis of influencers by fans
fans <- grouped_data %>% arrange(desc(fans),desc(count)) %>% ungroup()
top_10_fans <- fans[c(1,3,4,5,8,10,11,13,16,19),]
g2 <- ggplot(data=top_10_fans, aes(x=reorder(name, -fans), y=fans, fill = state)) +
        geom_bar(stat="identity", width=0.75) +
        geom_text(aes(label=fans), vjust=1.5, color="white", size=3)+
        geom_text(aes(label=city), vjust=3, color="white", size=3)+
        theme_minimal() + xlab(label = "Top 10 influencers by fans")

# Analysis of influencers by usefulness/review_count ratio
use_ratio <- grouped_data %>% arrange(desc(usefulness_ratio),desc(count)) %>% ungroup()
top_10_use_ratio <- use_ratio[c(1,3,4,5,6,7,9,10,11,12),]
g3 <- ggplot(data=top_10_use, aes(x=reorder(name, -usefulness_ratio), y=usefulness_ratio, fill = state)) +
  geom_bar(stat="identity", width=0.75) +
  geom_text(aes(label=usefulness_ratio), vjust=1.5, color="white", size=3)+
  geom_text(aes(label=city), vjust=3, color="white", size=3)+
  theme_minimal() + xlab(label = "Top 10 influencers by useful/review number ratio")

# Combined plot
cow3 <- cowplot::plot_grid(plotlist = list(g1,g2,g3), nrow = 3)
plot(cow3)


# S tymto som sa nakoniec rozhodol nepracovat
# Merging tables
# checkin <- checkin %>% mutate(num_checkin = str_count(date, ",")+1)  # Checkin counts
# checkin$business_id %>% unique %>% length()     # Check for unique restaurants
# restaurants <- restaurants %>% inner_join(checkin%>%select(-date), by = "business_id"); rm(checkin)
# tip <- tip %>% filter(business_id %in% restaurants$business_id)




# NLP - Natural language processing ---------------------------------------


# Reviews filtering and transforming
review <- review %>% filter(business_id %in% restaurants$business_id) %>% 
  mutate(cool=as.numeric(cool), funny=as.numeric(funny), useful=as.numeric(useful)) %>%
  filter(!is.na(useful))
# review <- review %>% mutate(years=as.numeric(2018-lubridate::year(as.Date(date))))

# Start of NLP
corpus <- Corpus(VectorSource(review$text))     # Creating document
corpus[[7]]$content
corpus <- tm_map(corpus, tolower)               # Lowering text
corpus[[7]]$content

fun <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, fun, "\\n")                                      # Removing new line sign
corpus <- tm_map(corpus, content_transformer(qdap::replace_contraction))  # Replacing contractions
corpus[[7]]$content
corpus <- tm_map(corpus, content_transformer(qdap::replace_abbreviation)) # Replacing abbreviations
corpus <- tm_map(corpus, content_transformer(qdap::replace_symbol))       # Replacing symbols for words

corpus <- tm_map(corpus, removeNumbers)                                       # Removing numbers
corpus[[7]]$content
corpus <- tm_map(corpus, removePunctuation)                                   # Removing punctuation
corpus[[7]]$content
corpus1 <- tm_map(corpus, removeWords, c("restaurant", stopwords("english"))) # Removing stopwords
corpus1[[7]]$content
corpus <- tm_map(corpus1, stemDocument)                                       # Lemmatize words
corpus[[7]]$content


# Creating document matrix based on weighting tf/tf-idf
tf     <- DocumentTermMatrix(corpus)
tf_idf <- DocumentTermMatrix(corpus, control=list(weighting = weightTfIdf))

# Removing words with low appearance
tf_sparse     <- removeSparseTerms(tf, 0.99)
tf_idf_sparse <- removeSparseTerms(tf_idf, 0.99)

# Creating dataframes
tf_df     <- as.data.frame(as.matrix(tf_sparse));     rm(tf, tf_sparse)
tf_idf_df <- as.data.frame(as.matrix(tf_idf_sparse)); rm(tf_idf, tf_idf_sparse)




# VISUALISATION -----------------------------------------------------------

# Most frequent words
wordcloud(corpus1, scale = c(3, 0.5), max.words = 50, min.freq = 5, random.order = , 
          rot.per = 0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))


# Dataset with words frequency
visual_df <- cbind(stars=review$stars, tf_df) %>%
  group_by(stars) %>% summarise_all(sum) %>% 
  select(-c("stars","food","one","just","get","place","will","can","got")) %>% 
  t() %>% as.data.frame()
visual_df <- visual_df %>% mutate(words=rownames(visual_df)) %>%
  select(words,everything())
colnames(visual_df)[2:6] <- c("one_star","two_stars","three_stars","four_stars","five_stars")


# Top 10 most frequent words per stars
st1 <- visual_df %>% select(words, one_star)    %>% arrange(desc(one_star))    %>% top_n(10)
st2 <- visual_df %>% select(words, two_stars)   %>% arrange(desc(two_stars))   %>% top_n(10)
st3 <- visual_df %>% select(words, three_stars) %>% arrange(desc(three_stars)) %>% top_n(10)
st4 <- visual_df %>% select(words, four_stars)  %>% arrange(desc(four_stars))  %>% top_n(10)
st5 <- visual_df %>% select(words, five_stars)  %>% arrange(desc(five_stars))  %>% top_n(10)
# Creating plots
p1 <- ggplot(data=st1, aes(x=reorder(words, -one_star), y=one_star)) +
  geom_bar(stat="identity", width=0.75, fill="steelblue") +
  geom_text(aes(label=one_star), vjust=1.6, color="white", size=3.5)+
  theme_minimal() + xlab(label = "top 10 words")
p2 <- ggplot(data=st2, aes(x=reorder(words, -two_stars), y=two_stars)) +
  geom_bar(stat="identity", width=0.75, fill="steelblue") +
  geom_text(aes(label=two_stars), vjust=1.6, color="white", size=3.5)+
  theme_minimal() + xlab(label = "top 10 words")
p3 <- ggplot(data=st3, aes(x=reorder(words, -three_stars), y=three_stars)) +
  geom_bar(stat="identity", width=0.75, fill="steelblue") +
  geom_text(aes(label=three_stars), vjust=1.6, color="white", size=3.5)+
  theme_minimal() + xlab(label = "top 10 words")
p4 <- ggplot(data=st4, aes(x=reorder(words, -four_stars), y=four_stars)) +
  geom_bar(stat="identity", width=0.75, fill="steelblue") +
  geom_text(aes(label=four_stars), vjust=1.6, color="white", size=3.5)+
  theme_minimal() + xlab(label = "top 10 words")
p5 <- ggplot(data=st5, aes(x=reorder(words, -five_stars), y=five_stars)) +
  geom_bar(stat="identity", width=0.75, fill="steelblue") +
  geom_text(aes(label=five_stars), vjust=1.6, color="white", size=3.5)+
  theme_minimal() + xlab(label = "top 10 words")

# Combined plot
cow1 <- cowplot::plot_grid(plotlist = list(p1,p2,p3,p4,p5), nrow = 3); rm(st1,st2,st3,st4,st5,p1,p2,p3,p4,p5)
print(cow1)


# Top 10 most frequent words if word max used by specific star category - other insight
visual_df$best <- visual_df[,2:6] %>% apply(1, max)
star1 <- visual_df %>% filter(one_star==best) %>% select(words, one_star) %>% 
  arrange(desc(one_star)) %>% top_n(10)
star2 <- visual_df %>% filter(two_stars==best) %>% select(words, two_stars) %>% 
  arrange(desc(two_stars)) %>% top_n(10)
star3 <- visual_df %>% filter(three_stars==best) %>% select(words, three_stars) %>% 
  arrange(desc(three_stars)) %>% top_n(10)
star4 <- visual_df %>% filter(four_stars==best) %>% select(words, four_stars) %>% 
  arrange(desc(four_stars)) %>% top_n(10)
star5 <- visual_df %>% filter(five_stars==best) %>% select(words, five_stars) %>% 
  arrange(desc(five_stars)) %>% top_n(10)

# Creating plots
p1 <- ggplot(data=star1, aes(x=reorder(words, -one_star), y=one_star)) +
  geom_bar(stat="identity", width=0.75, fill="steelblue") +
  geom_text(aes(label=one_star), vjust=1.6, color="white", size=3.5)+
  theme_minimal() + xlab(label = "top 10 words")
p2 <- ggplot(data=star2, aes(x=reorder(words, -two_stars), y=two_stars)) +
  geom_bar(stat="identity", width=0.75, fill="steelblue") +
  geom_text(aes(label=two_stars), vjust=1.6, color="white", size=3.5)+
  theme_minimal() + xlab(label = "top 10 words")
p3 <- ggplot(data=star3, aes(x=reorder(words, -three_stars), y=three_stars)) +
  geom_bar(stat="identity", width=0.75, fill="steelblue") +
  geom_text(aes(label=three_stars), vjust=1.6, color="white", size=3.5)+
  theme_minimal() + xlab(label = "top 10 words")
p4 <- ggplot(data=star4, aes(x=reorder(words, -four_stars), y=four_stars)) +
  geom_bar(stat="identity", width=0.75, fill="steelblue") +
  geom_text(aes(label=four_stars), vjust=1.6, color="white", size=3.5)+
  theme_minimal() + xlab(label = "top 10 words")
p5 <- ggplot(data=star5, aes(x=reorder(words, -five_stars), y=five_stars)) +
  geom_bar(stat="identity", width=0.75, fill="steelblue") +
  geom_text(aes(label=five_stars), vjust=1.6, color="white", size=3.5)+
  theme_minimal() + xlab(label = "top 10 words")

# Combined plot
cow2 <- cowplot::plot_grid(plotlist = list(p1,p2,p3,p4,p5), nrow = 3); rm(star1,star2,star3,star4,star5,p1,p2,p3,p4,p5)
print(cow2)




# ML MODELS ---> TRAINING + TESTING ---------------------------------------

# Final dataset
stars <- review$stars %>% as.factor()
varX  <- review %>% select(cool, funny, useful)
colnames(varX) <- c("coolX","funnyX","usefulX")

### HAS TO CHOOSE BETWEEN tf_df AND tf_idf_df !!! THEN RUN ALL CODE BELOW ###
data <- cbind(stars,varX,tf_df)

# Train/test split and min-max scaling
set.seed(1234)
index <- createDataPartition(data$stars, p = 0.75, list = FALSE)
train <- data[index,]
test  <- data[-index,]

maxx <- apply(train[,c(2,3,4)], 2, max)
minn <- apply(train[,c(2,3,4)], 2, min)
train[,c(2,3,4)] <- as.data.frame(scale(train[,c(2,3,4)],center = minn, scale = maxx - minn))
test[,c(2,3,4)]  <- as.data.frame(scale(test[,c(2,3,4)], center = minn, scale = maxx - minn))
# Stars count and proportions
tab <- rbind(table(train$stars), round(prop.table(table(train$stars)),4)) %>% t() %>% as.data.frame()
colnames(tab) <- c("count","proportion")

# Evaluation metrics - crossEntropy
crossEntropy <- function(pred, test){
  eps <- 1e-5
  pred <- pred %>% as.data.frame() + eps
  test <- test %>% as.numeric()
  suma <- 0
  for (i in 1:nrow(pred)) {
    suma <- suma + log2(pred[i,test[i]])
  }
  return(-suma)
}



### RF model - training + testing

# Training model
tic("Time used for training RF")
set.seed(1)
RF_model <- randomForest(x=train[,-1], y=train$stars, ntree = 200)
toc()
# Predict hold-out test set
RF_pred <- predict(RF_model, newdata=test, type = "prob")
rf_prediction <- matrix(RF_pred, nrow=length(RF_pred)/numOfClasses, ncol=numOfClasses) %>%
  data.frame() %>%
  mutate(label = as.numeric(test$stars),
         max_prob = max.col(., "last"))
# Confusion matrix of test set
cM_rf <- confusionMatrix(factor(rf_prediction$max_prob),
                         factor(rf_prediction$label))
# Computing cross-entropy metrics
rf_CE <- crossEntropy(RF_pred,test$stars)
# Plotting variable importance
varImpPlot(RF_model,type=2,n.var=20)



### XGBoost model - training + testing

# Train/test matrices
train_matrix <- xgb.DMatrix(data = as.matrix(train[,-1]), label = as.numeric(train$stars)-1)
test_matrix  <- xgb.DMatrix(data = as.matrix(test[,-1]), label = as.numeric(test$stars)-1)
# Settings
numOfClasses <- length(unique(train$stars))
xgb_params   <- list("objective"   = "multi:softprob",
                     "eval_metric" = "mlogloss",
                     "num_class"   = numOfClasses)
# Training model
tic("Time used for training XGBoost")
set.seed(1)
xgb_model <- xgb.train(params  = xgb_params,
                       data    = train_matrix,
                       nrounds = 200)
toc()
# Predict hold-out test set
xgb_pred <- predict(xgb_model, newdata = test_matrix)
xgb_prediction <- matrix(xgb_pred, nrow=numOfClasses, ncol=length(xgb_pred)/numOfClasses) %>%
  t() %>% data.frame() %>%
  mutate(label = as.numeric(test$stars),
         max_prob = max.col(., "last"))
# Confusion matrix of test set
cM_xgb <- confusionMatrix(factor(xgb_prediction$max_prob),
                          factor(xgb_prediction$label))
# Computing cross-entropy metrics
xgb_CE <- crossEntropy(xgb_prediction[,1:5],test$stars)
# compute feature importance matrix
importance_matrix = xgb.importance(feature_names = colnames(train[,-1]), model = xgb_model)
head(importance_matrix)
# Plotting importance
gp <- xgb.plot.importance(importance_matrix,
                            top_n = 20, n_clusters = 1)
print(gp)



### Logistic regresion

tic("Time used for training Logistic Regression")
set.seed(1)
lr_model <- glmnet(x = as.matrix(train[,-1]),y = as.factor(train$stars), 
                       family = "multinomial", standardize=FALSE,
                       type.multinomial = "grouped")
toc()
# Prediction
lr_pred <- predict(lr_model, newx = as.matrix(test[-1]), s = min(lr_model$lambda),
                      type = "response") %>% as.numeric()
lr_prediction <- matrix(lr_pred, nrow=length(lr_pred)/numOfClasses, ncol=numOfClasses) %>%
  data.frame() %>%
  mutate(label = as.numeric(test$stars),
         max_prob = max.col(., "last"))
# Confusion matrix
cM_lr <- confusionMatrix(as.factor(lr_prediction$max_prob),
                         as.factor(lr_prediction$label))
# Computing cross-entropy metrics
lr_CE <- crossEntropy(lr_prediction[,1:5],test$stars)
# Computing and plotting variable importance
lr_varimp <- varImp(lr_model, lambda = min(lr_model$lambda), scale = FALSE) %>% 
                apply(1, mean) %>% sort(decreasing = TRUE) 
lr_varimp <- lr_varimp %>%
                cbind(words=names(lr_varimp)) %>% as.data.frame() %>% head(20) %>% 
                mutate(imp=round(as.numeric(.),2)) %>% select(words,imp)
                
varimp_plot <- ggplot(data=lr_varimp, aes(x=reorder(words, -imp), y=imp)) +
  geom_bar(stat="identity", width=0.75, fill="steelblue") +
  geom_text(aes(label=imp), vjust=1.5, color="white", size=3)+
  theme_minimal() + xlab(label = "Top importance variables")
print(varimp_plot)



### NEURAL NETWORK
h2o::h2o.init(nthreads = -1)

# Fit the model
tic("Time used for training NeuralNet")
set.seed(1)
nn_model <- h2o::h2o.deeplearning(y = 'stars',
                                  training_frame = h2o::as.h2o(train),
                                  activation = 'Tanh',
                                  sparse=TRUE,
                                  hidden = c(50,8,2),
                                  loss = "CrossEntropy",
                                  epochs = 5,
                                  variable_importances=T)
toc()
# Make predictions
nn_pred <- h2o::h2o.predict(nn_model,h2o::as.h2o(test)) %>% as.data.frame()
# Confusion matrix
cM_nn <- confusionMatrix(nn_pred$predict,
                         as.factor(test$stars))
# Computing cross-entropy metrics
nn_CE <- crossEntropy(nn_pred[,2:6],test$stars)
# Variable importance
head(as.data.frame(h2o::h2o.varimp(nn_model)))
h2o::h2o.varimp_plot(nn_model, num_of_features = 20)

# Shuting down cluster
h2o::h2o.shutdown(prompt = FALSE)



### SAVING RESULTS TO TABLES - DEPENDING ON WHICH METOD WAS CHOSEN !!! (tf or tf_idf) ###

# Results from tf method
tab_tf     <- rbind(c(cM_rf$overall[1],cM_xgb$overall[1],cM_lr$overall[1],cM_nn$overall[1]),
                c(rf_CE,xgb_CE,lr_CE,nn_CE)) %>% round(4)
colnames(tab_tf) <- c("RF","XGB","LR","NN")
rownames(tab_tf) <- c("Accuracy","CrossEntropy")


# Results from tf_idf method
# tab_tf_idf <- rbind(c(cM_rf$overall[1],cM_xgb$overall[1],cM_lr$overall[1],cM_nn$overall[1]),
#                 c(rf_CE,xgb_CE,lr_CE,nn_CE)) %>% round(4)
# colnames(tab_tf_idf) <- c("RF","XGB","LR","NN")
# rownames(tab_tf_idf) <- c("Accuracy","CrossEntropy")


# Image of table
grid::grid.newpage()
gridExtra::grid.table(tab_tf)

# grid::grid.newpage()
# gridExtra::grid.table(tab_tf_idf)


# rm(RF_model,xgb_model, lr_model, nn_model)

# RF_model=RF_model_tf_idf
# xgb_model=xgb_model_tf_idf
# lr_model=lr_model_tf_idf
# nn_model=nn_model_tf_idf
# 
# RF_model=RF_model_tf
# xgb_model=xgb_model_tf
# lr_model=lr_model_tf
# nn_model=nn_model_tf

# save.image("C:/Users/PC/Downloads/Yelp/all.RData")
