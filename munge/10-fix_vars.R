

# Fix variables ------------------------------------

rsdata <- rsdata %>%
  mutate(
    sos_location2 = factor(case_when(
      sos_location %in% c("Out-patient") ~ 1,
      sos_location %in% c("HF in-patient", "Other in-patient") ~ 2
    ),
    levels = 1:2, labels = c("Out-patient", "In-patient")
    ),
    scream_anemia = case_when(
      is.na(scream_hb) ~ NA_character_,
      shf_sex == "Female" & scream_hb < 120 | shf_sex == "Male" & scream_hb < 130 ~ "Yes",
      TRUE ~ "No"
    ),
    shf_indexyear_cat = case_when(
      shf_indexyear <= 2014 ~ "2011-2014",
      shf_indexyear <= 2018 ~ "2015-2018"
    ),
    shf_nyha_cat = case_when(
      shf_nyha %in% c("I", "II") ~ "I - II",
      shf_nyha %in% c("III", "IV") ~ "III-IV"
    ),
    shf_age_cat = case_when(
      shf_age < 75 ~ "<75",
      shf_age >= 75 ~ ">=75"
    ),
    shf_ef_cat = factor(case_when(
      shf_ef == ">=50" ~ 3,
      shf_ef == "40-49" ~ 2,
      shf_ef %in% c("30-39", "<30") ~ 1
    ),
    labels = c("HFrEF", "HFmrEF", "HFpEF"),
    levels = 1:3
    ),
    shf_smoking_cat = factor(case_when(
      shf_smoking %in% c("Former", "Never") ~ 0,
      shf_smoking %in% c("Current") ~ 1
    ),
    labels = c("No", "Yes"),
    levels = 0:1
    ),
    shf_map_cat = case_when(
      shf_map <= 90 ~ "<=90",
      shf_map > 90 ~ ">90"
    ),
    scream_potassium_cat = factor(
      case_when(
        is.na(scream_potassium) ~ NA_real_,
        scream_potassium < 3.5 ~ 2,
        scream_potassium <= 5 ~ 1,
        scream_potassium > 5 ~ 3
      ),
      labels = c("normakalemia", "hypokalemia", "hyperkalemia"),
      levels = 1:3
    ),
    scream_sodium_cat = factor(
      case_when(
        is.na(scream_sodium) ~ NA_real_,
        scream_sodium <= 135 ~ 1,
        scream_sodium > 135 ~ 2
      ),
      labels = c("<=135", ">135"),
      levels = 1:2
    ),
    shf_heartrate_cat = case_when(
      shf_heartrate <= 70 ~ "<=70",
      shf_heartrate > 70 ~ ">70"
    ),
    shf_device_cat = factor(case_when(
      is.na(shf_device) ~ NA_real_,
      shf_device %in% c("CRT", "CRT & ICD", "ICD") ~ 2,
      TRUE ~ 1
    ),
    labels = c("No", "CRT/ICD"),
    levels = 1:2
    ),
    shf_bmi_cat = case_when(
      is.na(shf_bmi) ~ NA_character_,
      shf_bmi < 30 ~ "<30",
      shf_bmi >= 30 ~ ">=30"
    ),

    # eGFR according to CKD-EPI 2021 https://www.nejm.org/doi/full/10.1056/NEJMoa2102953
    tmp_k = if_else(shf_sex == "Female", 0.7, 0.9),
    tmp_a = if_else(shf_sex == "Female", -0.241, -0.302),
    tmp_add = if_else(shf_sex == "Female", 1.012, 1),
    scream_gfrckdepi = 142 * pmin(scream_creatinine / 88.4 / tmp_k, 1)^tmp_a * pmax(scream_creatinine / 88.4 / tmp_k, 1)^-1.200 * 0.9938^shf_age * tmp_add,
    scream_gfrckdepi_cat = factor(case_when(
      is.na(scream_gfrckdepi) ~ NA_real_,
      scream_gfrckdepi <= 30 ~ 1,
      scream_gfrckdepi < 60 ~ 2,
      scream_gfrckdepi >= 60 ~ 3,
    ),
    labels = c("<=30", ">30-<60", ">=60"),
    levels = 1:3
    ),
    scream_nttest = factor(case_when(
      is.na(scream_ntprobnp) ~ 0,
      TRUE ~ 1
    ),
    levels = 0:1,
    labels = c("No", "Yes")
    ),
    scream_ntprobnp_cat = factor(case_when(
      is.na(scream_ntprobnp) ~ NA_real_,
      scream_ntprobnp <= 1000 ~ 1,
      scream_ntprobnp <= 2000 ~ 2,
      scream_ntprobnp <= 3000 ~ 3,
      scream_ntprobnp <= 4000 ~ 4,
      scream_ntprobnp <= 5000 ~ 5,
      scream_ntprobnp <= 6000 ~ 6,
      scream_ntprobnp <= 7000 ~ 7,
      scream_ntprobnp <= 8000 ~ 8,
      scream_ntprobnp <= 9000 ~ 9,
      scream_ntprobnp <= 10000 ~ 10,
      scream_ntprobnp <= 11000 ~ 11,
      scream_ntprobnp <= 12000 ~ 12,
      scream_ntprobnp <= 13000 ~ 13,
      scream_ntprobnp <= 14000 ~ 14,
      scream_ntprobnp > 14000 ~ 15,
    ),
    labels = c(
      "<=1000", ">1000-2000", ">2000-3000", ">3000-4000", ">4000-5000", ">5000-6000", ">6000-7000", ">7000-8000", ">8000-9000",
      ">9000-10000", ">10000-11000", ">11000-12000", ">12000-13000", ">13000-14000", ">14000"
    ),
    levels = 1:15
    ),
    scream_ntprobnpstableprior = factor(
      case_when(
        is.na(scream_ntprobnp) | is.na(scream_ntprobnp6moprior) ~ NA_real_,
        scream_ntprobnp <= 5000 & scream_ntprobnp6moprior <= 5000 ~ 1,
        scream_ntprobnp <= 5000 & scream_ntprobnp6moprior > 5000 ~ 2,
        scream_ntprobnp > 5000 & scream_ntprobnp6moprior <= 5000 ~ 3,
        scream_ntprobnp > 5000 & scream_ntprobnp6moprior > 5000 ~ 4
      ),
      levels = 1:4,
      labels = c("Stable low", "Decreased", "Increased", "Stable high")
    ),
    scream_ntprobnpstablepost = factor(
      case_when(
        is.na(scream_ntprobnp) | is.na(scream_ntprobnp6mopost) ~ NA_real_,
        scream_ntprobnp <= 3000 & scream_ntprobnp6mopost <= 3000 ~ 1,
        scream_ntprobnp <= 3000 & scream_ntprobnp6mopost > 3000 ~ 3,
        scream_ntprobnp > 3000 & scream_ntprobnp6mopost <= 3000 ~ 2,
        scream_ntprobnp > 3000 & scream_ntprobnp6mopost > 3000 ~ 4
      ),
      levels = 1:4,
      labels = c("Stable low", "Decreased", "Increased", "Stable high")
    ),
    diff_ntprobnppost = (scream_ntprobnp6mopost - scream_ntprobnp) / scream_ntprobnp * 100,
    shf_sos_com_af = case_when(
      sos_com_af == "Yes" |
        shf_af == "Yes" |
        shf_ekg == "Atrial fibrillation" ~ "Yes",
      TRUE ~ "No"
    ),
    shf_sos_com_ihd = case_when(
      sos_com_ihd == "Yes" |
        shf_revasc == "Yes" |
        sos_com_pci == "Yes" |
        sos_com_cabg == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    shf_sos_com_hypertension = case_when(
      shf_hypertension == "Yes" |
        sos_com_hypertension == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    shf_sos_com_diabetes = case_when(
      shf_diabetes == "Yes" |
        sos_com_diabetes == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    shf_sos_com_valvular = case_when(
      shf_valvedisease == "Yes" |
        sos_com_valvular == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    shf_followuplocation_cat = if_else(shf_followuplocation %in% c("Primary care", "Other"), "Primary care/Other",
      as.character(shf_followuplocation)
    ),
    scb_education_cat = case_when(
      scb_education %in% c("Compulsory school", "Secondary school") ~ "No university",
      scb_education %in% c("University") ~ "University"
    )
  ) %>%
  select(-starts_with("tmp_"))

# limit outcomes to 1 and 3 yr

rsdata <- cut_surv(rsdata, sos_out_deathcv, sos_outtime_death, 365, rename = "1y", cuttime = F)
rsdata <- cut_surv(rsdata, sos_out_death, sos_outtime_death, 365, rename = "1y")
rsdata <- cut_surv(rsdata, sos_out_deathcv, sos_outtime_death, 365 * 3, rename = "3y", cuttime = F)
rsdata <- cut_surv(rsdata, sos_out_death, sos_outtime_death, 365 * 3, rename = "3y")

rsdata <- rsdata %>%
  mutate(
    sos_outtime_death1y = sos_outtime_death1y - 365 / 2,
    sos_outtime_death1y = if_else(sos_outtime_death1y < 0, NA_real_, sos_outtime_death1y),
    sos_outtime_death3y = sos_outtime_death3y - 365 / 2,
    sos_outtime_death3y = if_else(sos_outtime_death3y < 0, NA_real_, sos_outtime_death3y)
  )

rsdata <- rsdata %>%
  mutate(across(where(is_character), factor))
