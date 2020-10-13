# Libraries

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(FuzzyDateJoin)


# Source configs / helpers

source("/Users/ldmay/Box/Documents/R_helpers/config.R")
source("/Users/ldmay/Box/Documents/R_helpers/helpers.R")


# EXTRACT

# Get target IDs
target_ids <- read_excel("Copy of 2019-03-26_Pruitt_39_IDs_to_Neuropath.xlsx") %>%
  pull(ptid) %>%
  sort() %>%
  unique()

fields_ug_raw <- c("subject_id",
                   "mri_date")

fields_u2u3_a1 <- c("birthmo",
                    "birthyr",
                    "sex",
                    "hispanic",
                    "hispor",
                    "hisporx",
                    "race",
                    "racex",
                    "racesec",
                    "racesecx",
                    "raceter",
                    "raceterx",
                    "primlang",
                    "primlangx",
                    "educ",
                    "handed")

fields_u3_raw_hd <- c("ptid",
                      "form_date")

fields_u3_raw_b1 <- c("height",
                      "weight",
                      "bpsys",
                      "bpdias",
                      "hrate")

fields_u3_raw_b1 <- c(fields_u3_raw_b1,
                      paste0("fu_", fields_u3_raw_b1))

fields_u3_raw_b9 <- c("decsub",
                      "decin",
                      "decclcog",
                      "cogmem",
                      "cogori",
                      "cogjudg",
                      "coglang",
                      "cogvis",
                      "cogattn",
                      "cogfluc",
                      "cogflago",
                      "cogothr",
                      "cogothrx")

fileds_u3_raw_b9 <- c(fields_u3_raw_b9,
                      paste0("fu_", fields_u3_raw_b9))

fields_u3_raw_c2 <- c("mocacomp",
                      "mocareas",
                      "mocaloc",
                      "mocalan",
                      "mocalanx",
                      "mocavis",
                      "mocahear",
                      "mocatots",
                      "mocatrai",
                      "mocacube",
                      "mocacloc",
                      "mocaclon",
                      "mocacloh",
                      "mocanami",
                      "mocaregi",
                      "mocadigi",
                      "mocalett",
                      "mocaser7",
                      "mocarepe",
                      "mocaflue",
                      "mocaabst",
                      "mocarecn",
                      "mocarecc",
                      "mocarecr",
                      "mocaordt",
                      "mocaormo",
                      "mocaoryr",
                      "mocaordy",
                      "mocaorpl",
                      "mocaorct",
                      "npsycloc_c2",
                      "npsylan_c2",
                      "npsylanx_c2",
                      "craftvrs",
                      "crafturs",
                      "udsbentc",
                      "digforct",
                      "digforsl",
                      "digbacct",
                      "digbacls",
                      "animals_c2",
                      "veg_c2",
                      "traila_c2",
                      "trailarr_c2",
                      "trailali_c2",
                      "trailb_c2",
                      "trailbrr_c2",
                      "trailbli_c2",
                      "craftdvr",
                      "craftdre",
                      "craftdti",
                      "craftcue",
                      "udsbentd",
                      "udsbenrs",
                      "minttots",
                      "minttotw",
                      "mintscng",
                      "mintscnc",
                      "mintpcng",
                      "mintpcnc",
                      "udsverfc",
                      "udsverfn",
                      "udsvernf",
                      "udsverlc",
                      "udsverlr",
                      "udsverln",
                      "udsvertn",
                      "udsverte",
                      "udsverti",
                      "cogstat_c2")

fields_u3_raw_c2 <- c(fields_u3_raw_c2,
                      paste0("fu_", fields_u3_raw_c2))

fields_u3_raw_hvlt <- c("hvlt_ttpr",
                        "hvlt_srfpe",
                        "hvlt_sufpe",
                        "hvlt_tfpe",
                        "hvlt_trr",
                        "hvlt_drr",
                        "hvlt_rr",
                        "hvlt_rdir")

fields_u3_raw_jolo <- c("jolo_total_correct_raw")

fields_u3_raw_wcst <- c("wcst_nosas",
                        "wcst_nosam",
                        "wcst_nosae",
                        "wcst_noes",
                        "wcst_nocs",
                        "wcst_te",
                        "wcst_pe")

fields_u3_raw_d1 <- c("dxmethod",
                      "normcog",
                      "demented",
                      "amndem",
                      "pca",
                      "ppasyn",
                      "ppasynt",
                      "ftdsyn",
                      "lbdsyn",
                      "namndem",
                      "mciamem",
                      "mciaplus",
                      "mciaplan",
                      "mciapatt",
                      "mciapex",
                      "mciapvis",
                      "mcinon1",
                      "mcin1lan",
                      "mcin1att",
                      "mcin1ex",
                      "mcin1vis",
                      "mcinon2",
                      "mcin2lan",
                      "mcin2att",
                      "mcin2ex",
                      "mcin2vis",
                      "impnomci",
                      "amylpet",
                      "amylcsf",
                      "fdgad",
                      "hippatr",
                      "taupetad",
                      "csftau",
                      "fdgftld",
                      "tpetftld",
                      "mrftld",
                      "datscan",
                      "othbiom",
                      "othbiomx",
                      "imaglinf",
                      "imaglac",
                      "imagmach",
                      "imagmich",
                      "imagmwmh",
                      "imagewmh",
                      "admut",
                      "ftldmut",
                      "othmut",
                      "othmutx",
                      "alzdis",
                      "alzdisif",
                      "lbdis",
                      "lbdif",
                      "park",
                      "msa",
                      "msaif",
                      "psp",
                      "pspif",
                      "cort",
                      "cortif",
                      "ftldmo",
                      "ftldmoif",
                      "ftldnos",
                      "ftldnoif",
                      "ftldsubt",
                      "ftldsubx",
                      "cvd",
                      "cvdif")

fields_u3_raw_d1 <- c(fields_u3_raw_d1,
                      paste0("fu_", fields_u3_raw_d1))

fields_u3_raw <- c(fields_u3_raw_hd,
                   fields_u3_raw_b1,
                   fields_u3_raw_b9,
                   fields_u3_raw_c2,
                   fields_u3_raw_d1,
                   fields_u3_raw_hvlt,
                   fields_u3_raw_jolo,
                   fields_u3_raw_wcst)

forms_u3_raw <- c("ivp_a3", "fvp_a3",
                  "ivp_a4", "fvp_a4",
                  "ivp_a5", "fvp_a5",
                  "ivp_b4", "fvp_b4",
                  "ivp_b6", "fvp_b6")

fields_bl_raw <- c("subject_id",
                   "blood_draw_date")

fields_ap_raw <- c("subject_id",
                   "allele")

# Export REDCap data
df_ug_raw <- export_redcap_records(uri = REDCAP_API_URI,
                                   token = REDCAP_API_TOKEN_UMMAP_GEN,
                                   fields = paste(fields_ug_raw, collapse = ",")) %>%
  jsonlite::fromJSON()

df_u2_a1_raw <- export_redcap_records(uri = REDCAP_API_URI,
                                      token = REDCAP_API_TOKEN_UDS2,
                                      fields = paste(c("subject_id", fields_u2u3_a1), collapse = ",")) %>%
  jsonlite::fromJSON()

df_u3_a1_raw <- export_redcap_records(uri = REDCAP_API_URI,
                                      token = REDCAP_API_TOKEN_UDS3n,
                                      fields = paste(c("ptid", fields_u2u3_a1), collapse = ",")) %>%
  jsonlite::fromJSON()

df_u3_a3_raw <- export_redcap_records(uri = REDCAP_API_URI,
                                      token = REDCAP_API_TOKEN_UDS3n, 
                                      fields = "ptid",
                                      forms = "ivp_a3") %>% 
  jsonlite::fromJSON()

df_u3_a5_raw <- export_redcap_records(uri = REDCAP_API_URI,
                                      token = REDCAP_API_TOKEN_UDS3n,
                                      fields = "ptid",
                                      forms = "ivp_a5") %>% 
  jsonlite::fromJSON()

df_u3_raw <- export_redcap_records(uri = REDCAP_API_URI,
                                   token = REDCAP_API_TOKEN_UDS3n,
                                   fields = paste(fields_u3_raw, collapse = ","),
                                   forms = paste(forms_u3_raw, collapse = ",")) %>%
  jsonlite::fromJSON()

df_bl_raw <- export_redcap_records(uri = REDCAP_API_URI,
                                   token = REDCAP_API_TOKEN_UMMAP_GEN,
                                   fields = paste(fields_bl_raw, collapse = ",")) %>%
  jsonlite::fromJSON()

df_ap_raw <- export_redcap_records(uri = REDCAP_API_URI,
                                   token = REDCAP_API_TOKEN_UMMAP_GEN,
                                   fields = paste(fields_ap_raw, collapse = ",")) %>%
  jsonlite::fromJSON()


# TRANSFORM

# Clean; Coërce data types
df_ug_cln <- df_ug_raw %>%
  na_if("") %>%
  filter(str_detect(mri_date, "^\\d{4}-\\d{2}-\\d{2}$")) %>%
  select(-redcap_event_name) %>%
  mutate(mri_date = as_date(mri_date))

df_u2_a1_cln <- df_u2_a1_raw %>%
  na_if("") %>%
  filter(str_detect(subject_id, "^UM\\d{8}$")) %>%
  filter(!is.na(birthmo)) %>%
  filter(redcap_event_name == "visit_01_arm_1") %>%
  rename(ptid = subject_id) %>%
  select(-redcap_event_name)

df_u3_a1_cln <- df_u3_a1_raw %>%
  na_if("") %>%
  filter(str_detect(ptid, "^UM\\d{8}$")) %>%
  filter(!is.na(birthmo)) %>%
  filter(redcap_event_name == "visit_1_arm_1") %>%
  select(-redcap_event_name)

df_u3_a3_cln <- df_u3_a3_raw %>% 
  na_if("") %>% 
  filter(str_detect(ptid, "^UM\\d{8}$")) %>% 
  filter(redcap_event_name == "visit_1_arm_1") %>% 
  filter(!is.na(afffamm)) %>% 
  rename_with(~ if_else(.x %in% c("ptid", "redcap_event_name"), .x, paste0(.x, "_iv"))) %>% 
  select(-redcap_event_name)

df_u3_a5_cln <- df_u3_a5_raw %>% 
  na_if("") %>% 
  filter(str_detect(ptid, "^UM\\d{8}$")) %>% 
  filter(redcap_event_name == "visit_1_arm_1") %>% 
  filter(!is.na(tobac30)) %>% 
  rename_with(~ if_else(.x %in% c("ptid", "redcap_event_name"), .x, paste0(.x, "_iv"))) %>% 
  select(-redcap_event_name)

df_u3_cln <- df_u3_raw %>%
  na_if("") %>%
  rename(fu_sib17pdx = fusib17pdx,
         fu_kid9agd = fukid9agd) %>%
  filter(str_detect(ptid, "^UM\\d{8}$")) %>%
  filter(str_detect(form_date, "^\\d{4}-\\d{2}-\\d{2}$")) %>%
  mutate_at(all_of(c(fields_u3_raw_c2,
                     fields_u3_raw_hvlt,
                     fields_u3_raw_jolo,
                     fields_u3_raw_wcst,
                     fields_u3_raw_d1[!(fields_u3_raw_d1 %in% c("othbiomx", "ftldsubx"))])),
            as.integer) %>%
  derive_consensus_dx() %>% 
  coalesce_ift_cols() %>%
  mutate(form_date = as_date(form_date)) %>%
  mutate(visit_num = str_match(redcap_event_name, "^visit_(\\d+)_arm_1$")[, 2])

df_bl_cln <- df_bl_raw %>%
  filter(str_detect(subject_id, "^UM\\d{8}$")) %>%
  filter(str_detect(blood_draw_date, "^\\d{4}-\\d{2}-\\d{2}$")) %>%
  mutate(across(blood_draw_date, as_date)) %>%
  rename(blood_date = blood_draw_date) %>%
  select(-redcap_event_name)

df_ap_cln <- df_ap_raw %>%
  filter(str_detect(subject_id, "^UM\\d{8}$")) %>%
  filter(str_detect(allele, "^\\d+$")) %>%
  rename(ptid = subject_id) %>%
  mutate(across(allele, as.integer)) %>%
  select(-redcap_event_name)

# Mutate

df_u2u3_a1 <- bind_rows(df_u2_a1_cln, df_u3_a1_cln) %>%
  distinct(ptid, .keep_all = TRUE)

df_u3_mut <- df_u3_cln %>% 
  left_join(df_u2u3_a1, by = c("ptid" = "ptid")) %>% 
  select(ptid, form_date, madc_dx, 
         birthmo, birthyr, sex, hispanic, hispor, hisporx,
         race, racex, racesec, racesecx, raceter, raceterx,
         primlang, educ, handed)

# Ensure that all target_ids are in df_ug_cln and df_u3_cln
assertthat::assert_that(all(target_ids %in% df_ug_cln$subject_id))
assertthat::assert_that(all(target_ids %in% df_u3_mut$ptid))

df_ug_flt <- df_ug_cln %>% 
  filter(subject_id %in% target_ids)

# Left join: df_ug_flt, df_u3_cln => df_ugu3
df_ugu3_raw <- inner(df_ug_flt, df_u3_cln,
                     x_id_col = "subject_id", y_id_col = "ptid",
                     x_date_col = "mri_date", y_date_col = "form_date",
                     x_intvl_less = 183L, x_intvl_more = 183L)

df_ugu3_cln <- df_ugu3_raw %>% 
  mutate(mri_clinic_diff = form_date - mri_date) %>% 
  select(ptid, subject_id, madc_dx,
         redcap_event_name, visit_num, 
         form_date, mri_date, mri_clinic_diff,
         everything()) %>% 
  rename(clinic_date = form_date) %>%
  select(-subject_id, -redcap_event_name)

# df_ugu3_cln %>% pull(ptid) %>% length()  # 252
# df_ugu3_cln %>% pull(ptid) %>% unique() %>% length()  # 204

# Left join: df_ugu3, df_ugblood => df_ugu3blood
df_ugu3bl_raw <- inner(df_ugu3_cln, df_bl_cln,
                          x_id_col = "ptid", y_id_col = "subject_id",
                          x_date_col = "mri_date", y_date_col = "blood_date",
                          x_intvl_less = 183L, x_intvl_more = 183L)

df_ugu3bl_cln <- df_ugu3bl_raw %>% 
  mutate(mri_blood_diff = blood_date - mri_date) %>% 
  select(ptid, madc_dx, 
         mri_date, clinic_date, blood_date,
         mri_clinic_diff, mri_blood_diff,
         everything()) %>% 
  select(-subject_id)

# Left join: df_ugu3bl_cln, df_ap_cln => df_ugu3blap
df_ugu3blap_raw <- left_join(df_ugu3bl_cln, df_ap_cln, by = c("ptid" = "ptid"))

# Drop fields that only appear in FVP A3
df_ugu3blap_cln <- df_ugu3blap_raw %>% 
  select(-fu_nwinfmut, -fu_nwinfpar, -fu_nwinfsib, -fu_nwinfkid)

# df_ugu3blap_cln %>% pull(ptid) %>% length() # 162
# df_ugu3blap_cln %>% pull(ptid) %>% unique() %>% length() # 149

# Left join IVP Form A3 and IVP Form A5 data
df_ugu3blap_join <- df_ugu3blap_cln %>% 
  left_join(df_u3_a3_cln, by = c("ptid" = "ptid")) %>% 
  left_join(df_u3_a5_cln, by = c("ptid" = "ptid"))


# LOAD

# today_str = today()
today_str = "2020-09-15"

write_csv(x = df_ugu3blap_join,
          file = str_glue("DataSet_2020-07-31_Paulson_Damoiseau_Counts_27_{today_str}.csv"),
          na = "")

# Build tailored data dictionary

dd_u3 <- export_redcap_data_dictionary(uri = REDCAP_API_URI,
                                       token = REDCAP_API_TOKEN_UDS3n) %>% 
  jsonlite::fromJSON() %>% 
  filter(field_name %in% names(df_ugu3blap_join))

dd_ug <- export_redcap_data_dictionary(uri = REDCAP_API_URI,
                                       token = REDCAP_API_TOKEN_UMMAP_GEN) %>%
  jsonlite::fromJSON() %>% 
  filter(field_name %in% c("mri_date", "allele"))

dd <- bind_rows(dd_u3, dd_ug)

write_csv(x = dd,
          file = str_glue("DataDictionary_2017-07-31_Paulson_Damoiseau_Counts_27_{today_str}.csv"),
          na = "")
