
# Charlson comobidity index -----------------------------------------------

# Myocardial infarction

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_mi",
  diakod = " I21| I22| I252",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "VAL/KON (in, out, pc)"
)

# Congestive heart failure

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_chf",
  diakod = " I110| I130| I132| I255| I420| I42[6-9]| I43| I50",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "VAL/KON (in, out, pc)"
)

# Peripheral vascular disease

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_pvd",
  diakod = " I70| I71| I731| I738| I739| I771| I790| I792| K55",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "VAL/KON (in, out, pc)"
)

# Cerebrovascular disease

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_cd",
  diakod = " G45| I6[0-4]| I67| I69",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "VAL/KON (in, out, pc)"
)

# Chronic pulmonary disease

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_copd",
  diakod = " J4[3-4]",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "VAL/KON (in, out, pc)"
)

# Other chronic pulmonary disease

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_copdother",
  diakod = " J41| J42| J4[5-7]| J6[0-9]| J70",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "VAL/KON (in, out, pc)"
)

# Rheumatic disease

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_rheumatic",
  diakod = " M05| M06| M123| M07[0-3]| M08| M13| M30| M31[3-6]| M3[2-4]| M350| M351| M353| M45| M46",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "VAL/KON (in, out, pc)"
)

# Dementia

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_dementia",
  diakod = " F0[0-3]| F051| G30| G311| G319",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "VAL/KON (in, out, pc)"
)

# Hemiplegia or paraplegia

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_plegia",
  diakod = " G114| G8[0-2]| G83[0-3]| G838",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "VAL/KON (in, out, pc)"
)

# Diabetes

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_diabetes",
  diakod = " E100| E101| E110| E111| E120| E121| E130| E131| E140| E141",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "VAL/KON (in, out, pc)"
)

# Diabetes with end organ damage

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_diabetescompliation",
  diakod = " E10[2-5]| E107| E11[2-7]| E12[2-7]| E13[2-7]| E14[2-7]",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "VAL/KON (in, out, pc)"
)

# Renal disease

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_renal",
  diakod = " N03[2-7]| N05[2-7]| N11| N18| N19| N250| I120| I131| Q61[1-4]| Z49| Z940| Z992",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "VAL/KON (in, out, pc)"
)

# Mild liver disease

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_livermild",
  diakod = " B1[5-9]| K709| K703| K73| K746| K703| K754",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "VAL/KON (in, out, pc)"
)

## liver spec

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_liverspec",
  diakod = " R18",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "VAL/KON (in, out, pc)"
)


# Moderate or severe liver disease

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_livermodsev",
  diakod = " I850| I859| I982| I983| R18",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "VAL/KON (in, out, pc)"
)

# Peptic ulcer disease

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_pud",
  diakod = " K2[5-8]",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "VAL/KON (in, out, pc)"
)

# Any malignancy, including lymphoma and leukemia, except malignant neoplasm of skin

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_malignancy",
  diakod = " C[0-7]| C8[0-6]| C8[8-9]| C9[0-7]",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "VAL/KON (in, out, pc)"
)

# Metastatic solid tumor

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_metastatictumor",
  diakod = " C7[7-9]| C80",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "VAL/KON (in, out, pc)"
)

# AIDS/HIV

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "cci_hiv",
  diakod = " B2[0-4]| F024| Z219| Z711| R75| Z114",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "VAL/KON (in, out, pc)"
)

rsdata <- rsdata %>%
  mutate(
    sos_com_cci_diabetes = if_else(sos_com_cci_diabetescompliation == 1, 0, sos_com_cci_diabetes),
    sos_com_cci_livermodsev = if_else(sos_com_cci_livermild == 1 & sos_com_cci_liverspec == 1, 1, sos_com_cci_livermodsev),
    sos_com_cci_livermild = if_else(sos_com_cci_livermodsev == 1, 0, sos_com_cci_livermild),
    sos_com_cci_malignancy = if_else(sos_com_cci_metastatictumor == 1, 0, sos_com_cci_malignancy),
    sos_com_cci_diabetescompliation = sos_com_cci_diabetescompliation * 2,
    sos_com_cci_plegia = sos_com_cci_plegia * 2,
    sos_com_cci_renal = sos_com_cci_renal * 2,
    sos_com_cci_malignancy = sos_com_cci_malignancy * 2,
    sos_com_cci_livermodsev = sos_com_cci_livermodsev * 3,
    sos_com_cci_metastatictumor = sos_com_cci_metastatictumor * 6,
    sos_com_cci_hiv = sos_com_cci_hiv * 6
  ) %>%
  mutate(sos_com_charlsonci = rowSums(select(., starts_with("sos_com_cci_")))) %>%
  select(-starts_with("sos_com_cci_"))

ccimeta <- metaout
rm(metaout)
