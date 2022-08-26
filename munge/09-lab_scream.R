
# at index

tmp_rsdatalab <- left_join(rsdata %>% select(LopNr, shf_indexdtm), lab_avg, by = "LopNr") %>%
  mutate(diff = as.numeric(shf_indexdtm - labdtm)) %>%
  filter(diff <= 14 & diff >= 0)

labfunc <- function(lab, labname) {
  labname <- paste0("scream_", labname)
  tmp_rsdatalab2 <- tmp_rsdatalab %>%
    filter(!is.na(!!sym(lab))) %>%
    group_by(LopNr) %>%
    arrange(abs(diff)) %>%
    slice(1) %>%
    ungroup() %>%
    rename(!!labname := !!sym(lab)) %>%
    select(LopNr, shf_indexdtm, !!sym(labname))

  rsdata <<- left_join(rsdata,
    tmp_rsdatalab2,
    by = c("LopNr", "shf_indexdtm")
  )
}

labfunc("bnp", "ntprobnp")
labfunc("hb", "hb")
labfunc("crea", "creatinine")
labfunc("sodium", "sodium")
labfunc("potas", "potassium")

# 6 mo prior/post index

labfunc6mo <- function(start, stop, time, name) {
  labname <- paste0("scream_ntprobnp", name)

  tmp_rsdatalab <- left_join(
    rsdata %>% select(LopNr, shf_indexdtm),
    labnt,
    by = "LopNr"
  ) %>%
    mutate(
      diff = as.numeric(labdtm - shf_indexdtm),
      difftime = abs(diff - time)
    ) %>%
    filter(diff >= start & diff <= stop) %>%
    group_by(LopNr) %>%
    arrange(difftime) %>%
    slice(1) %>%
    ungroup() %>%
    rename(!!labname := bnp) %>%
    select(LopNr, shf_indexdtm, !!sym(labname))

  rsdata <<- left_join(rsdata,
    tmp_rsdatalab,
    by = c("LopNr", "shf_indexdtm")
  )
}

labfunc6mo(start = -9 * 30.5, stop = -3 * 30.5, -6 * 30.5, name = "6moprior")
labfunc6mo(start = 3 * 30.5, stop = 9 * 30.5, 6 * 30.5, name = "6mopost")
#labfunc6mo(start = -3 * 30.5, stop = 0, -3 * 30.5, name = "3moprior")
#labfunc6mo(start = 1, stop = 3 * 30.5, 3 * 30.5, name = "3mopost")
