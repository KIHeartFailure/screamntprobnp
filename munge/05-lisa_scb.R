rsdata <- rsdata %>%
  mutate(scbyear = shf_indexyear - 1)

rsdata <- left_join(
  rsdata,
  lisa,
  by = c("LopNr" = "LopNr", "scbyear" = "year")
) %>%
  select(-scbyear)

## income
inc <- rsdata %>%
  group_by(shf_indexyear) %>%
  summarise(
    incmed = quantile(scb_dispincome,
      probs = 0.5,
      na.rm = TRUE
    ),
    .groups = "drop_last"
  )

rsdata <- left_join(
  rsdata,
  inc,
  by = c("shf_indexyear")
) %>%
  mutate(
    scb_dispincome_cat2 = case_when(
      scb_dispincome < incmed ~ 1,
      scb_dispincome >= incmed ~ 2
    ),
    scb_dispincome_cat2 = factor(scb_dispincome_cat2,
      levels = 1:2,
      labels = c("Below median", "Above median")
    )
  ) %>%
  select(-incmed)