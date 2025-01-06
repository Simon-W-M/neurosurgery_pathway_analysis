# data load

# read in main dataset
analytics_exercise <- read_excel("Analytics exercise.xlsx",
  sheet = "Spell level data",
  col_types = c(
    "numeric", "text", "numeric",
    "numeric", "date", "date",
    "text", "numeric", "text",
    "text", "text", "text",
    "text", "text", "numeric",
    "numeric", "text", "numeric",
    "numeric", "text", "text",
    "text", "text"
  )
)

# read in weighted population data
url <- "https://www.england.nhs.uk/wp-content/uploads/2016/04/j-overall-weighted-populations.xlsx"
destfile <- "pops.xlsx"
curl::curl_download(url, destfile)

pops <- read_excel("pops.xlsx",
  sheet = "CCG weighted populations",
  skip = 4
) |>
  janitor::clean_names()

# create data table for diagnosis
diag_codes <- data.frame(
  stringsAsFactors = FALSE,
  Spell_Primary_Diagnosis = c(
    "I609", "I601",
    "I602", "I608", "I607", "I603", "I604", "I606",
    "I600", "I605", "I60X"
  ),
  Primary.Diagnosis = c(
    "I609: Subarachnoid haemorrhage, unspecified",
    "I601: Subarachnoid haemorrhage from middle cerebral artery",
    "I602: Subarachnoid haemorrhage from anterior communicating artery", 
    "I608: Other subarachnoid haemorrhage",
    "I607: Subarachnoid haemorrhage from intracranial artery, unspecified",
    "I603: Subarachnoid haemorrhage from posterior communicating artery",
    "I604: Subarachnoid haemorrhage from basilar artery",
    "I606: Subarachnoid haemorrhage from other intracranial arteries",
    "I600: Subarachnoid haemorrhage from carotid siphon and bifurcation",
    "I605: Subarachnoid haemorrhage from vertebral artery",
    "Other Subarachnoid haemorrhage"
  )
)

# create list of neuro centres
neuro_centres <- c(
  "R1H",
  "RET", "RF4", "RGT", "RHM",
  "RHQ", "RJ7", "RJE", "RJZ",
  "RK9", "RKB", "RM3", "RR1", "RR8",
  "RRK", "RRV", "RTD", "RTH",
  "RTR", "RVJ", "RWA", "RX1",
  "RXH", "RXN", "RYJ"
)

# create data table of admission methods (to determine elective/emergency)
ad_meth <- data.frame(
  stringsAsFactors = FALSE,
  ad_code = c(
    "11", "12", "13", "21", "22",
    "23", "24", "25", "2A", "2B", "2C", "2D", "28", "31", "32",
    "82", "83", "81"
  ),
  ad_desc = c(
    "Elective: Waiting list",
    "Elective: Booked",
    "Elective: Planned",
    "Emergency: ED",
    "Emergency: GP",
    "Emergency: Bed bureau",
    "Emergency: Consultant Clinic",
    "Emergency: Mental Health",
    "Emergency: ED other provider",
    "Emergency: Transfer",
    "Emergency: Baby born at home as intended",
    "Emergency: Other emergency admission",
    "Emergency: Other means",
    "Maternity: Admitted ante partum",
    "Maternity: Admitted post partum",
    "Other: Birth in this Health Care Provider",
    "Other: Baby outside this Health Care Provider",
    "Other: Transfer non emergency"
  )
)

# create table of management types
man_type <- data.frame(
  stringsAsFactors = FALSE,
  code = c("DC", "EL", "EM", "NE", "RDA", "UNK"),
  man_desc = c(
    "Day case",
    "Elective",
    "Emergency",
    "Non elective",
    "Elective", ## check this one
    "Unknown"
  )
)

# create table of discharge destinations
dis_dest <- data.frame(
  stringsAsFactors = FALSE,
  Code = c(
    19, 29, 30, 37, 40, 42,
    48, 49, 50, 51, 52, 53, 55, 56, 66, 79, 84, 87,
    88, 89
  ),
  dis_desc = c(
    "Usual place of residence",
    "Temporary place of residence",
    "Repatriation high security psychiatric",
    "Court",
    "Penal establishment",
    "Police Station",
    "High Security Psychiatric Hospital, Scotland",
    "High security psychiatric accommodation",
    "Medium secure unit",
    "General ward",
    "Maternity",
    "MH/LD ward",
    "Care Home Services with Nursing",
    "Care Home Services without Nursing",
    "Foster care",
    "Patient died",
    "Independent - medium secure unit",
    "Independent - excluding medium secure unit",
    "Hospice",
    "Forced repatriation"
  )
)

# read list of prescribed specialised services
pss_list <- read_excel("Analytics exercise.xlsx",
  sheet = "Useful links and info", skip = 51
)

# add simple impatient flag in order to join
pss_list <- pss_list |>
  mutate(admit_flag = if_else(Setting == "APC", 1, 0))

# join additional tables to main dataset
# add flags for child ( age < 19),
#               admitted patient (assumes 0 LOS is day case)
#               neuro centre

data <- analytics_exercise |>
  left_join(diag_codes,
    by = "Spell_Primary_Diagnosis"
  ) |>
  mutate(
    neuro_centre_flag = if_else(Der_Provider_Code %in% neuro_centres, 1, 0),
    neuro_centre = if_else(Der_Provider_Code %in% neuro_centres, 
                           "Neuro centre", "Acute"),
    admit_flag = if_else(Spell_Adj_LoS == 0, 0, 1),
    child = if_else(Age_At_CDS_Activity_Date < 19, 1, 0)
  ) |>
  left_join(
    pops |>
      dplyr::select(
        ccg,
        total_weighted_populations_uplifted_by_ons_population_growth_to_2020
      ),
    by = c("Final_Derived_CCG" = "ccg")
  ) |>
  left_join(ad_meth,
    by = c("Admission_Method" = "ad_code")
  ) |>
  left_join(dis_dest,
    by = c("Discharge_Destination" = "Code")
  ) |>
  left_join(pss_list,
    by = c(
      "NCBFinal_Spell_ServiceLine" = "Code",
      "admit_flag" = "admit_flag"
    )
  ) |>
  left_join(man_type,
    by = c("Der_Management_Type" = "code")
  ) |>
  mutate(death = if_else(Discharge_Destination == "Patient died", 1, 0)) |>
  mutate(
    hrg_tot = n(),
    .by = c("HRG_Code", "neuro_centre")
  ) |>
  janitor::clean_names() |>
  rename(npoc_category = "n_po_c_category")
