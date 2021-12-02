library(sparklyr)
library(jsonlite)
library(tidyverse)

# Setting working directory
setwd("C:/Users/PC/Downloads/Yelp/")

# Convert JSON files to data frames
business <- jsonlite::stream_in(file("yelp_academic_dataset_business.json"),pagesize = 100000) %>% 
                jsonlite::flatten()
# Only open & restau 
business <- business %>% filter(is_open == 1) %>% select(-starts_with("attributes"))
restaurants <- business %>% filter(grepl('Restaurant', categories)); rm(business)

# Configure cluster (c3.4xlarge 30G 16core 320disk)
conf <- spark_config()
conf$'sparklyr.shell.executor-memory' <- "7g"
conf$'sparklyr.shell.driver-memory' <- "7g"
conf$spark.executor.cores <- 20
conf$spark.executor.memory <- "7G"
conf$spark.yarn.am.cores  <- 20
conf$spark.yarn.am.memory <- "7G"
conf$spark.executor.instances <- 20
conf$spark.dynamicAllocation.enabled <- "false"
conf$maximizeResourceAllocation <- "true"
conf$spark.default.parallelism <- 32

sc <- spark_connect(master = "local", config = conf, version = '2.2.0')

# Review - transforming json to dataset form and selecting a subset (only the commented code)
review <- spark_read_json(sc,name="example2",path="yelp_academic_dataset_review.json", 
                          header = TRUE, memory = FALSE, overwrite = TRUE)
review <- review %>% filter(business_id %in% !!restaurants$business_id); rm(restaurants)
spark_write_csv(review, "review.csv", header = TRUE, delimiter = ",")
# df_review <- review %>% filter(business_id %in% !!restaurants$business_id) %>%
#               select(-c("review_id", "user_id", "business_id", "date")) %>%
#               sdf_with_sequential_id(id = "index") %>%
#               filter(index<=100000)
# review_final <- df_review %>% collect()
# write.csv(review_final,"review_adj.csv", row.names = FALSE)



# User - transforming json to dataset form and selecting a subset (only the commented code)
user <- spark_read_json(sc,name="example1",path="yelp_academic_dataset_user.json", 
                        header = TRUE, memory = FALSE, overwrite = TRUE)
user <- user %>% select(-c(starts_with("compliment"),"elite"))
spark_write_csv(user, "user.csv", header = TRUE, delimiter = ",")
# df_user <- user %>%
#             sdf_with_sequential_id(id = "index") %>%
#             filter(index<=100000) %>% collect()
# write.csv(df_user,"user_adj.csv", row.names = FALSE)

spark_disconnect(sc)
