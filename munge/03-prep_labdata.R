lab_avg <- lab %>%
  filter(test %in% c(
    "bnp",
    "potas",
    "sodium",
    "regular Hb",
    "crea"
  )) %>%
  group_by(LopNr, test, datum) %>%
  summarise(result = mean(result, na.rm = TRUE), .groups = "drop") %>%
  mutate(labdtm = ymd(datum)) %>%
  select(-datum)

lab_avg <- lab_avg %>%
  pivot_wider(id_cols = c(LopNr, labdtm), names_from = test, values_from = result) %>%
  rename(hb = `regular Hb`)


labnt <- lab_avg %>%
  filter(!is.na(bnp)) %>%
  select(LopNr, labdtm, bnp)
