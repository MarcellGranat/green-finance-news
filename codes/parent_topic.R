fit_stm_df <- list.files("data/", full.names = TRUE) %>% 
  keep(str_detect, "fit_stm") %>% 
  enframe(NULL, "file_name") %>%
  transmute(
    k = parse_number(file_name),
    fit = map(file_name, ~ {load(.); mod})
  ) %>% 
  arrange(k)


kk <- fit_stm_df %>% 
  pull(k) %>% 
  tail(-1)

parent_topic_df <- tibble()
for (x in kk) {
  message(x)
  current_tidy <- fit_stm_df %>% 
    filter(k == x) %>% 
    pull(fit) %>% 
    first() %>% 
    broom::tidy() %>% 
    rename(current_beta = beta, current_topic = topic)
  
  prev_tidy <- fit_stm_df %>% 
    filter(k == x - 2) %>% 
    pull(fit) %>% 
    first() %>% 
    broom::tidy() %>% 
    rename(prev_beta = beta, prev_topic = topic)
  
  parent_topic_df <- current_tidy %>% 
    left_join(prev_tidy) %>% 
    group_by(current_topic, prev_topic) %>% 
    summarise(d = sum((current_beta - prev_beta)^2)) %>% 
    ungroup() %>% 
    arrange(d) %>% 
    distinct(current_topic, .keep_all = TRUE) %>% 
    mutate(k = x) %>% 
    bind_rows(parent_topic_df)
}

parent_topic_df <- parent_topic_df %>% 
  select(-d) %>% 
  arrange(k, prev_topic)

parent_topic_df

topic_row_df <- tibble(row_n = 1:2, topic = 1:2, k = 2)

for (kk in seq(from = 4, to = 6, by = 2)) {
  
  # kk = 4
  
  topic_row_df <-
    topic_row_df %>% 
    mutate(k = k + 2) %>% 
    filter(k == kk) %>% 
    left_join(
      parent_topic_df,
      by = c("k", "topic" = "prev_topic")
    ) %>% 
    arrange(row_n, current_topic) %>% 
    mutate(row_n = row_number()) %>% 
    select(row_n, topic = current_topic, k) %>% 
    bind_rows(topic_row_df)
  parent_topic_df %>% 
    filter(k == kk) %>% 
    data.frame() %>% 
    print()
  
  data.frame(topic_row_df) %>% 
    print()
}


