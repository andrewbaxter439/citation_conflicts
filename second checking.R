library(tidyverse)
library(googlesheets)

sheet <- gs_title("new_conflicts")

conflicts <- gs_read(sheet, ws = "conflicts_1203")

# test_sample <- conflicts %>% 
#   sample_n(100)
# 
# 
# blanked_test <-
#   test_sample %>% 
#   mutate_at(vars(`no mention`:`done`), ~ 0)
# 
# gs_ws_new(sheet, "conflicts_second_check", input = blanked_test)  

conflicts_check <- gs_read(sheet, ws = "conflicts_second_check")


out <- conflicts_check %>% 
  semi_join(conflicts, by = c("Reference", "done")) %>%
  arrange(Reference) %>% 
  select(Reference, `none declared`:`tobacco control advocate`)


check <- conflicts %>% 
  semi_join(conflicts_check, by = c("Reference", "done")) %>% 
  arrange(Reference) %>% 
  select(Reference, `none declared`:`tobacco control advocate`)

out[as.logical(rowSums(!out == check)),] %>% 
  bind_rows(check[as.logical(rowSums(!out == check)),])
