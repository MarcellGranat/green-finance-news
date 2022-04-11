pdftools::pdf_text("data/sic.pdf") %>% 
  enframe(NULL, "raw") %>% 
  slice(-1) %>% # title page
  mutate(
    text = str_replace_all(raw, "\\D", "_"),
    text = str_split(text, "_")
  ) %>% 
  select(last_col()) %>% 
  unnest() %>% 
  filter(text != "") %>% 
  slice(-1) %>% # date of classification
  mutate(
    code2 = ifelse(str_length(text) == 2, text, NA),
    code3 = ifelse(str_length(text) == 3, text, NA),
    code4 = ifelse(str_length(text) == 4, text, NA)
  ) %>% 
  fill(code2, code3, .direction = "down") %>% 
  drop_na(code3, code4)


