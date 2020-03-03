category_test <- str_to_lower(sales$Order_Category) 

test <- grepl(pattern = "rollout", x = category_test)

test_index_pos <- grep(pattern = "rollout", x = category_test)

most_pop <- sales %>%
  ungroup() %>%
  slice(test_index_pos) %>%
  select(Order)

removed_characters <- str_remove_all(most_pop$Order, pattern = "[^x|[:digit:]|\\'|]")

as.data.frame(removed_characters)

final_most_pop <- as.data.frame(as.character(removed_characters))
 
final_most_pop %>%
  count(`as.character(removed_characters)`) %>%
  top_n(`as.character(removed_characters)`,5)
