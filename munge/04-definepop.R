

# Inclusion/exclusion criteria --------------------------------------------------------

rsdata <- shfdata001 %>%
  rename(LopNr = lopnr)
flow <- c("Number of posts in SwedeHF in SCREAM2", nrow(rsdata))

# already removed duplicated indexdates

rsdata <- left_join(rsdata,
  pininfo,
  by = "LopNr"
)
rsdata <- rsdata %>%
  filter(scb_reusedpin == 0 & scb_changedpin == 0) %>% # reused/changed personr
  select(-scb_reusedpin, -scb_changedpin)
flow <- rbind(flow, c("Exclude posts with reused or changed PINs", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_age >= 18 & !is.na(shf_age))
flow <- rbind(flow, c("Exclude posts < 18 years", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_indexdtm >= ymd("2011-01-15"))
flow <- rbind(flow, c("Exclude posts with index date < 2011-01-15", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_indexdtm <= ymd("2018-12-17"))
flow <- rbind(flow, c("Exclude posts with index date > 2018-12-17", nrow(rsdata)))

hf6m <- patreg %>%
  mutate(hf = str_detect(DIA_all, " I110| I130| I132| I255| I420| I423| I425| I426| I427| I428| I429| I43| I50| J81| K761| R570")) %>%
  group_by(LopNr) %>%
  arrange(INDATUM) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(firsthfdtm = INDATUM) %>%
  select(LopNr, firsthfdtm)

rsdata <- left_join(rsdata, hf6m, by = "LopNr") %>%
  filter(shf_indexdtm - 365 / 2 >= firsthfdtm)
flow <- rbind(flow, c("Exclude posts without ICD-10 code (I110| I130| I132| I255| I420| I423| I425| I426| I427| I428| I429| I43| I50| J81| K761| R570)
                      diagnosis for HF > 6 months before", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_durationhf))
flow <- rbind(flow, c("Exclude posts with missing duration of HF in SwedeHF", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_durationhf == ">6mo")
flow <- rbind(flow, c("Exclude posts with duration of HF < 6 mo in SwedeHF", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_ef))
flow <- rbind(flow, c("Exclude posts with missing EF", nrow(rsdata)))

tmp_rsdataflytt <- inner_join(rsdata %>% select(LopNr, shf_indexdtm),
  flytt %>% mutate(flyttdtm = ymd(hdat)) %>% select(-hdat, -hdat_c),
  by = "LopNr"
)
tmp_rsdataflytt <- tmp_rsdataflytt %>%
  filter(shf_indexdtm > flyttdtm) %>%
  group_by(LopNr, shf_indexdtm) %>%
  arrange(flyttdtm) %>%
  slice(n()) %>%
  ungroup() %>%
  filter(hkod == "U")

rsdata <- left_join(rsdata,
  tmp_rsdataflytt,
  by = c("LopNr", "shf_indexdtm")
) %>%
  filter(is.na(hkod)) %>%
  select(-flyttdtm, -hkod)

flow <- rbind(flow, c("Exclude posts emigrated from Stockholm prior to indexdate in SwedeHF", nrow(rsdata)))

# WHEF defined from VAL/KON -----------------------------------------------

hf <- inner_join(
  rsdata %>% select(LopNr, shf_indexdtm, shf_indexhosptime),
  patreg,
  by = "LopNr"
) %>%
  mutate(
    tmpindexdtm = if_else(!is.na(shf_indexhosptime), shf_indexdtm - shf_indexhosptime, shf_indexdtm),
    diff = tmpindexdtm - INDATUM,
    tmp_hfsos = stringr::str_detect(HDIA, " I110| I130| I132| I255| I420| I423| I425| I426| I427| I428| I429| I43| I50| J81| K761| R570")
  )

hfhosp <- hf %>%
  filter(abs(diff) < 14 & tmp_hfsos & sos_source == "sv") %>%
  group_by(LopNr, shf_indexdtm) %>%
  arrange(abs(diff)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(sos_locationhf = 1) %>%
  select(LopNr, shf_indexdtm, sos_locationhf)

anypost <- hf %>%
  filter(abs(diff) < 14) %>%
  group_by(LopNr, shf_indexdtm) %>%
  arrange(abs(diff)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(sos_locationany = if_else(sos_source == "sv", 1, 0)) %>%
  select(LopNr, shf_indexdtm, sos_locationany)

allhosp <- full_join(
  hfhosp,
  anypost,
  by = c("LopNr", "shf_indexdtm")
)

rsdata <- left_join(
  rsdata,
  allhosp,
  by = c("LopNr", "shf_indexdtm")
) %>%
  mutate(
    sos_location = factor(case_when(
      sos_locationhf == 1 ~ 1,
      sos_locationany == 1 ~ 2,
      sos_locationany == 0 ~ 3
    ),
    levels = 1:3, labels = c("HF in-patient", "Other in-patient", "Out-patient")
    ),
    whfe = factor(case_when(
      sos_locationhf == 1 ~ 1,
      TRUE ~ 2
    ),
    levels = 1:2, labels = c("WHFE", "NWHFE")
    )
  ) %>%
  select(-sos_locationhf, -sos_locationany)

rsdata <- rsdata %>%
  filter(!is.na(sos_location))
flow <- rbind(flow, c("Exclude posts without match within +/- 14 days in VAL", nrow(rsdata)))

rsdata <- rsdata %>%
  group_by(LopNr) %>%
  arrange(shf_indexdtm) %>%
  slice(1) %>%
  ungroup()

flow <- rbind(flow, c("First post / patient", nrow(rsdata)))

colnames(flow) <- c("Criteria", "N")
