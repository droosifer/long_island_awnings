##no sale amounts for data reported before 2014. Moving forawrd will only include date post-2014

##add note column to 2014 data set to match total and correct columns

data_2014$Notes <- ''

##reordering to match the order of all the other dataframes

data_2014 <- data_2014[,c(1:15,22,16:21)]

##creating list of the dataframes with all similar columns

final_df_list <- list(data_2014, data_2015, data_2016, data_2017, data_2018)


##standardizing column names across datasets, picked 5 because most recent naming standard from company

new_col_names <- colnames(final_df_list[[5]])


new_name_df_list <- lapply(final_df_list, set_names, new_col_names)

##checking new df names are all the same

col_names_dfs <- as.data.frame(lapply(new_name_df_list, colnames))


##checking classes of all of the columns

class_types <- as.data.frame(lapply(new_name_df_list, class))

final_df <- bind_rows(new_name_df_list)

write.csv(final_df, file = paste0(wd$data, "/cleaned_data.csv"))
