OUTPUT_COUNTY_CSV <- "county_demand_index_rebuilt.csv"
OUTPUT_ALLOCATION_AUDIT_CSV <- "utility_allocation_audit_rebuilt.csv"
OUTPUT_MAP_HTML <- "emerald_demand_map_rebuilt.html"
# PHASE 0: LIBRARIES
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(tidycensus)
library(jsonlite)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)
# PHASE 1: RAW DATA INGESTION
meters_raw <- read_excel(
  "Meters Data 2024.xlsx",
  sheet = "Combined Meter Data"
) %>%
  clean_names() %>%
  mutate(utility_number = as.character(utility_number))

territory_raw <- read_excel(
  "Service_Territory_2024.xlsx",
  sheet = "Counties_States"
) %>%
  clean_names() %>%
  mutate(utility_number = as.character(utility_number))
# PHASE 2: HELPERS

# Standardise county/state names for name-based joins
normalize_county_state <- function(df, county_col = "county", state_col = "st
ate") {
  state_lookup <- c(
    stats::setNames(state.name, state.abb),
    DC = "District of Columbia"
  )
  geo_suffix_pattern <- regex(
    " (County|Parish|Borough|Census Area|City and Borough|Municipality|Distri
ct)$",
    ignore_case = TRUE
  )

  df %>%
    mutate(
      !!state_col := str_squish(as.character(.data[[state_col]])),
      .state_lookup_value = unname(state_lookup[.data[[state_col]]]),
      !!state_col := ifelse(
        nchar(.data[[state_col]]) == 2 & !is.na(.state_lookup_value),
        .state_lookup_value,
        .data[[state_col]]
      ),
      !!county_col := str_squish(as.character(.data[[county_col]])),
      .default_suffix = case_when(
        .data[[state_col]] == "Louisiana" ~ "Parish",
        .data[[state_col]] %in% c("Alaska", "District of Columbia") ~ "",
        str_detect(.data[[county_col]], regex(" city$", ignore_case = TRUE))
&
          !(.data[[state_col]] == "Virginia" &
              .data[[county_col]] %in% c("Charles City", "James City")) ~ "",
        TRUE ~ "County"
      ),
      !!county_col := case_when(
        is.na(.data[[county_col]]) ~ .data[[county_col]],
        str_detect(.data[[county_col]], geo_suffix_pattern) ~ .data[[county_c
ol]],
        .default_suffix == "" ~ .data[[county_col]],
        TRUE ~ paste(.data[[county_col]], .default_suffix)
      )
    ) %>%
    select(-.state_lookup_value, -.default_suffix)
}

# Canonical key for the one unavoidable name-based county join.
canonical_county_key <- function(x) {
  x %>%
    iconv(from = "", to = "ASCII//TRANSLIT") %>%
    str_to_lower() %>%
    str_replace_all("[.'`-]", " ") %>%
    str_replace_all("&", " and ") %>%

    str_replace_all("\\bsaint\\b", "st") %>%
    str_replace_all("\\bst\\.?\\b", "st") %>%
    str_replace_all("\\bste\\.?\\b", "ste") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

# Apply only one-to-one county-equivalent aliases. Ambiguous historical split
s
# are handled later via HIFLD county-FIPS backfill instead of guessed remaps.
apply_county_aliases <- function(df, county_col = "county", state_col = "stat
e") {
  alias_map <- tibble::tribble(
    ~state,    ~county_alias,           ~county_standard,
    "Alaska",  "Aleutians East",        "Aleutians East Borough",
    "Alaska",  "Aleutians West",        "Aleutians West Census Area",
    "Alaska",  "Anchorage",             "Anchorage Municipality",
    "Alaska",  "Bethel",                "Bethel Census Area",
    "Alaska",  "Dillingham",            "Dillingham Census Area",
    "Alaska",  "Fairbanks North Star",  "Fairbanks North Star Borough",
    "Alaska",  "Haines",                "Haines Borough",
    "Alaska",  "Kenai Peninsula",       "Kenai Peninsula Borough",
    "Alaska",  "Kodiak Island",         "Kodiak Island Borough",
    "Alaska",  "Lake and Peninsula",    "Lake and Peninsula Borough",
    "Alaska",  "Nome",                  "Nome Census Area",
    "Alaska",  "North Slope",           "North Slope Borough",
    "Alaska",  "Northwest Arctic",      "Northwest Arctic Borough",
    "Alaska",  "Yukon Koyukuk",         "Yukon-Koyukuk Census Area"
  )

  df %>%
    mutate(
      .state_alias = .data[[state_col]],
      .county_alias = .data[[county_col]]
    ) %>%
    left_join(
      alias_map,
      by = c(".state_alias" = "state", ".county_alias" = "county_alias")
    ) %>%
    mutate(
      !!county_col := coalesce(county_standard, .data[[county_col]])
    ) %>%
    select(-.state_alias, -.county_alias, -county_standard)
}

add_county_join_keys <- function(df, county_col = "county", state_col = "stat
e") {
  df %>%
    mutate(
      county_join_key = canonical_county_key(.data[[county_col]]),

      state_join_key  = str_to_lower(str_squish(as.character(.data[[state_col
]])))
    )
}

# Left join that silently skips NULL right-hand tables
safe_left_join <- function(base, new_df, by = "fips") {
  if (!is.null(new_df)) left_join(base, new_df, by = by) else base
}

# Ensure a column exists, filling with NA if absent
fill_missing_col <- function(df, col, val = NA_real_) {
  if (!col %in% names(df)) df[[col]] <- val
  df
}

territory_clean <- territory_raw %>%
  normalize_county_state("county", "state") %>%
  apply_county_aliases("county", "state") %>%
  add_county_join_keys("county", "state")
# PHASE 3: ACS COUNTY FEATURES
YEAR <- 2023  # ACS 5-year (2019-2023) - most recent available

get_census_key <- function() {
  candidate_keys <- c(
    Sys.getenv("CENSUS_API_KEY", unset = ""),
    Sys.getenv("TIDYCENSUS_API_KEY", unset = "")
  )
  key <- candidate_keys[nzchar(candidate_keys)][1]

  if (length(key) == 0 || is.na(key) || !nzchar(key)) {
    stop(
      "Set CENSUS_API_KEY (or TIDYCENSUS_API_KEY) in your environment or .Ren
viron before knitting this notebook."
    )
  }

  key
}

census_api_key(get_census_key(), install = FALSE, overwrite = TRUE)
county_acs <- get_acs(
  geography = "county",
  variables = c(
    "B01001_001",  # total population
    "B25001_001",  # total housing units

    "B19013_001",  # median household income
    "B25077_001",  # median home value
    "B25035_001"   # median year structure built
  ),
  year    = YEAR,
  survey  = "acs5",
  output  = "wide"
) %>%
  select(-matches("M$")) %>%
  transmute(
    fips                = GEOID,
    county              = str_remove(NAME, ",.*$"),
    state               = str_remove(NAME, "^.*?,\\s*"),
    total_pop           = B01001_001E,
    total_housing_units = B25001_001E,
    median_income       = B19013_001E,
    median_home_value   = B25077_001E,
    median_year_built   = B25035_001E
  ) %>%
  filter(state != "Puerto Rico")

cat("ACS county rows:", nrow(county_acs), "\n")
# NOAA Climate at a Glance county exports. For the rebuilt index we only use
# annual heating and cooling degree days directly from the NOAA county files.
# These contiguous-US files still leave Alaska, Hawaii, and a small number of
# county-equivalent gaps unmatched; those counties continue through the
# existing HDD/CDD mean-imputation path later.
# Connecticut is a special case: NOAA still publishes the legacy 8-county vie
w,
# while ACS 2023 returns 9 planning regions. Expand those legacy rows to the
# planning-region names the rebuilt pipeline uses so CT climate values are no
t
# lost to avoidable mean imputation.
ct_noaa_planning_region_xwalk <- tribble(
  ~county,             ~state,         ~county_planning_region,
  "Fairfield County",  "Connecticut",  "Greater Bridgeport Planning Region",
  "Fairfield County",  "Connecticut",  "Western Connecticut Planning Region",
  "Hartford County",   "Connecticut",  "Capitol Planning Region",
  "Litchfield County", "Connecticut",  "Northwest Hills Planning Region",
  "Middlesex County",  "Connecticut",  "Lower Connecticut River Valley Planni
ng Region",
  "New Haven County",  "Connecticut",  "Naugatuck Valley Planning Region",
  "New Haven County",  "Connecticut",  "South Central Connecticut Planning Re
gion",
  "New London County", "Connecticut",  "Southeastern Connecticut Planning Reg
ion",
  "Windham County",    "Connecticut",  "Northeastern Connecticut Planning Reg

ion"
)

expand_ct_noaa_regions <- function(df) {
  ct_expanded <- df %>%
    filter(state == "Connecticut") %>%
    inner_join(
      ct_noaa_planning_region_xwalk,
      by = c("county", "state"),
      relationship = "many-to-many"
    ) %>%
    transmute(
      county = county_planning_region,
      state,
      across(-c(county, state))
    )

  bind_rows(
    df %>% filter(state != "Connecticut"),
    ct_expanded
  )
}

read_noaa_degree_days <- function(file, value_name) {
  read_csv(file, skip = 3, show_col_types = FALSE) %>%
    transmute(
      county = str_trim(Name) %>%
        str_replace_all("&#039;", "'"),
      state  = str_trim(State),
      !!value_name := suppressWarnings(as.numeric(Value))
    ) %>%
    mutate(
      county = if_else(
        state == "District of Columbia" & county == "Washington",
        "District of Columbia",
        county
      ),
      county = if_else(
        state == "Louisiana" & county == "La Salle Parish",
        "LaSalle Parish",
        county
      )
    ) %>%
    normalize_county_state("county", "state") %>%
    expand_ct_noaa_regions() %>%
    add_county_join_keys("county", "state")
}

county_hdd <- read_noaa_degree_days(
  "heatingdays.csv",

  "annual_heating_degree_days"
)

county_cdd <- read_noaa_degree_days(
  "coolingdays.csv",
  "annual_cooling_degree_days"
) %>%
  select(county_join_key, state_join_key, annual_cooling_degree_days)

county_climate <- county_hdd %>%
  left_join(
    county_cdd,
    by = c("county_join_key", "state_join_key"),
    relationship = "one-to-one"
  )

climate_key_dupes <- county_climate %>%
  count(county_join_key, state_join_key, name = "n") %>%
  filter(n > 1)

if (nrow(climate_key_dupes) > 0) {
  stop("NOAA degree-day inputs contain duplicate county keys.")
}

# Base county features keyed by FIPS
county_features <- county_acs %>%
  add_county_join_keys("county", "state") %>%
  left_join(
    county_climate %>%
      select(
        county_join_key,
        state_join_key,
        annual_heating_degree_days,
        annual_cooling_degree_days
      ),
    by = c("county_join_key", "state_join_key"),
    relationship = "many-to-one"
  ) %>%
  select(-county_join_key, -state_join_key)

county_features_join <- county_features %>%
  add_county_join_keys("county", "state")

cat("NOAA heating rows:", nrow(county_hdd), "\n")
cat("NOAA cooling rows:", nrow(county_cdd), "\n")

cat("County feature rows:", nrow(county_features), "\n")
cat("FIPS populated:     ", sum(!is.na(county_features$fips)), "\n")
cat("Counties with NOAA heating matches:",
    sum(!is.na(county_features$annual_heating_degree_days)), "\n")
cat("Counties with NOAA cooling matches:",
    sum(!is.na(county_features$annual_cooling_degree_days)), "\n")
# PHASE 4: ADDITIONAL DATA SOURCES
# Census Population Estimates 2020-2024
county_growth <- read_csv("co-est2024-alldata.csv",
                          show_col_types = FALSE) %>%
  clean_names() %>%
  filter(sumlev == "050") %>%
  transmute(
    fips = paste0(str_pad(as.character(state),  2, "left", "0"),
                  str_pad(as.character(county), 3, "left", "0")),
    pop_growth_pct         = (popestimate2024 - popestimate2020) /
                              pmax(popestimate2020, 1) * 100,
    pop_growth_1yr_pct     = (popestimate2024 - popestimate2023) /
                              pmax(popestimate2023, 1) * 100,
    net_domestic_migration = domesticmig2024
  )

cat("Population growth rows:", nrow(county_growth), "\n")
# Census Building Permits - 3-year average (2022, 2023, 2024)
# co2022a.txt, co2023a.txt, co2024a.txt all share the same layout:
#   3-row compound header (skip = 3), same column names.
# Averaging across years smooths one-time large subdivision filings.
col_names <- c(
  "survey_date", "state_fips", "county_fips", "region_code", "division_code",
"county_name",
  "u1_bldgs", "u1_units", "u1_value",
  "u2_bldgs", "u2_units", "u2_value",
  "u34_bldgs", "u34_units", "u34_value",
  "u5p_bldgs", "u5p_units", "u5p_value",
  "r1_bldgs", "r1_units", "r1_value",

  "r2_bldgs", "r2_units", "r2_value",
  "r34_bldgs", "r34_units", "r34_value",
  "r5p_bldgs", "r5p_units", "r5p_value"
)

read_permit_year <- function(file) {
  read_csv(file, skip = 3, col_names = col_names,
           col_types = cols(.default = col_character()),
           show_col_types = FALSE) %>%
    filter(!is.na(state_fips), nchar(trimws(state_fips)) > 0) %>%
    mutate(
      fips       = paste0(str_pad(trimws(state_fips),  2, "left", "0"),
                          str_pad(trimws(county_fips), 3, "left", "0")),
      sf         = suppressWarnings(as.numeric(u1_units)),
      mf         = suppressWarnings(as.numeric(u2_units))  +
                   suppressWarnings(as.numeric(u34_units)) +
                   suppressWarnings(as.numeric(u5p_units)),
      total      = replace_na(sf, 0) + replace_na(mf, 0)
    ) %>%
    group_by(fips) %>%
    summarise(total = sum(total, na.rm = TRUE),
              sf    = sum(sf,    na.rm = TRUE),
              mf    = sum(mf,    na.rm = TRUE),
              .groups = "drop") %>%
    filter(nchar(fips) == 5, fips != "00000")
}

p2022 <- read_permit_year("co2022a.txt")
p2023 <- read_permit_year("co2023a.txt")
p2024 <- read_permit_year("co2024a.txt")

# 3-year average - rowMeans(na.rm=TRUE) keeps counties missing one year
county_permits <- p2022 %>%
  full_join(p2023, by = "fips", suffix = c("_22", "_23")) %>%
  full_join(p2024 %>% rename(total_24 = total, sf_24 = sf, mf_24 = mf), by =
"fips") %>%
  mutate(
    avg_annual_permits = rowMeans(cbind(total_22, total_23, total_24), na.rm
= TRUE),
    avg_sf_permits     = rowMeans(cbind(sf_22,    sf_23,    sf_24),    na.rm
= TRUE),
    avg_mf_permits     = rowMeans(cbind(mf_22,    mf_23,    mf_24),    na.rm
= TRUE),
    # permit_growth_pct: 2022-2024 change; exported to CSV for reference, not
in index
    permit_growth_pct  = (replace_na(total_24, 0) - replace_na(total_22, 0))
/
                          pmax(replace_na(total_22, 0), 1) * 100
  ) %>%
  select(fips, avg_annual_permits, avg_sf_permits, avg_mf_permits, permit_gro

wth_pct)

cat("Building permit counties:", nrow(county_permits), "\n")
# Housing unit growth: ACS B25001_001 (total units), 2019 vs 2023.
# 4-year growth rate annualised. This pull is required for the canonical work
flow.
hu_2019 <- get_acs(geography = "county", variables = "B25001_001",
                   year = 2019, survey = "acs5", output = "wide") %>%
  transmute(fips = GEOID, hu_2019 = B25001_001E)

hu_2023 <- get_acs(geography = "county", variables = "B25001_001",
                   year = 2023, survey = "acs5", output = "wide") %>%
  transmute(fips = GEOID, hu_2023 = B25001_001E)

county_hu <- hu_2019 %>%
  inner_join(hu_2023, by = "fips") %>%
  mutate(
    hu_growth_pct = (hu_2023 - hu_2019) / pmax(hu_2019, 1) * 100,
    hu_growth_1yr = hu_growth_pct / 4   # annualised over 4-year span
  ) %>%
  select(fips, hu_growth_pct, hu_growth_1yr)

cat("Housing unit growth rows (ACS 2019 vs 2023):", nrow(county_hu), "\n")
# County Business Patterns 2023 - economy-wide totals only
county_cbp <- read_csv("cbp23co.txt", show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    fipstate = str_pad(as.character(fipstate), 2, "left", "0"),
    fipscty  = str_pad(as.character(fipscty),  3, "left", "0"),
    fips     = paste0(fipstate, fipscty),
    est      = suppressWarnings(as.numeric(est)),
    emp      = suppressWarnings(as.numeric(emp)),
    ap       = suppressWarnings(as.numeric(ap))
  ) %>%
  filter(naics == "------") %>%
  transmute(fips,
            total_establishments = est,
            total_employment     = emp,
            annual_payroll_1000s = ap)

cat("CBP county rows:", nrow(county_cbp), "\n")

# FEMA National Risk Index - only Expected Annual Loss (eal_valt)
# Drop risk_score, sovi_score, and hazard-specific sub-scores
county_nri <- read_csv("nri_county_trimmed.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  transmute(
    fips          = str_pad(as.character(stcofips), 5, "left", "0"),
    nri_eal_score = eal_valt   # dollar-value expected annual loss
  )

cat("NRI county rows:", nrow(county_nri), "\n")
# Hardcoded 2024 state residential electricity prices (cents/kWh).
state_elec_prices <- tribble(
  ~state,                    ~res_price_cents_kwh,
  "Alabama",                  12.86,  "Alaska",                22.47,
  "Arizona",                  13.12,  "Arkansas",              10.71,
  "California",               27.94,  "Colorado",              14.21,
  "Connecticut",              27.32,  "Delaware",              13.44,
  "Florida",                  13.73,  "Georgia",               13.09,
  "Hawaii",                   37.55,  "Idaho",                 10.37,
  "Illinois",                 14.31,  "Indiana",               13.82,
  "Iowa",                     11.38,  "Kansas",                12.71,
  "Kentucky",                 11.04,  "Louisiana",             11.30,
  "Maine",                    25.57,  "Maryland",              14.84,
  "Massachusetts",            28.76,  "Michigan",              17.61,
  "Minnesota",                14.04,  "Mississippi",           11.87,
  "Missouri",                 12.18,  "Montana",               11.78,
  "Nebraska",                 10.83,  "Nevada",                11.58,
  "New Hampshire",            25.37,  "New Jersey",            17.44,
  "New Mexico",               13.30,  "New York",              21.98,
  "North Carolina",           12.40,  "North Dakota",          10.18,
  "Ohio",                     14.54,  "Oklahoma",              11.02,
  "Oregon",                   10.82,  "Pennsylvania",          15.94,
  "Rhode Island",             28.82,  "South Carolina",        12.88,
  "South Dakota",             11.67,  "Tennessee",             12.02,
  "Texas",                    13.42,  "Utah",                  10.29,
  "Vermont",                  20.45,  "Virginia",              13.43,
  "Washington",               10.08,  "West Virginia",         12.18,
  "Wisconsin",                15.52,  "Wyoming",               11.41,
  "District of Columbia",     16.04
)

cat("Hardcoded state electricity prices:", nrow(state_elec_prices), "states\n
")

# PHASE 5: MERGE ALL SOURCES + NA AUDIT
county_features <- county_features %>%
  safe_left_join(
    county_growth %>% select(fips, pop_growth_pct, pop_growth_1yr_pct,
                             net_domestic_migration)
  ) %>%
  safe_left_join(county_permits) %>%
  safe_left_join(county_hu) %>%
  safe_left_join(county_cbp) %>%
  safe_left_join(county_nri) %>%
  left_join(state_elec_prices, by = "state") %>%
  mutate(
    # Rate: avg annual permits per 1,000 existing housing units (3-year avg,
2022-2024).
    # Normalises for county size so small fast-growing counties aren't penali
sed.
    permits_per_1k_hu  = avg_annual_permits / pmax(total_housing_units, 1) *
1000,
    # Per-capita migration rate: removes large-county bias from raw migration
counts.
    net_migration_rate = net_domestic_migration / pmax(total_pop, 1) * 1000
  )

# Stub any missing columns so downstream code never errors on missing columns
for (col in c(
  "pop_growth_pct", "pop_growth_1yr_pct", "net_domestic_migration", "net_migr
ation_rate",
  "avg_annual_permits", "avg_sf_permits", "avg_mf_permits", "permit_growth_pc
t", "permits_per_1k_hu",
  "hu_growth_pct", "hu_growth_1yr",
  "total_establishments", "total_employment", "annual_payroll_1000s",
  "nri_eal_score", "res_price_cents_kwh"
)) county_features <- fill_missing_col(county_features, col)
# NA audit for every column fed into the index. Printed for the presentation
record.
cat("=== NA AUDIT: INDEX INPUT COLUMNS ===\n")
audit_tbl <- tibble(
  column = c(
    "total_pop", "total_housing_units",
    "median_income", "median_home_value", "median_year_built",
    "annual_heating_degree_days", "annual_cooling_degree_days",
    "pop_growth_pct", "net_domestic_migration", "net_migration_rate",
    "avg_annual_permits", "permits_per_1k_hu",
    "hu_growth_pct",
    "total_establishments", "annual_payroll_1000s",

    "nri_eal_score", "res_price_cents_kwh"
  ),
  handling = c(
    "coalesce(0) -> log-transformed in Market Size",
    "coalesce(0) -> log-transformed in Market Size; permit rate denominator",
    "mean imputation -> Economic Capacity (zero income is nonsensical)",
    "mean imputation -> Economic Capacity (zero home value is nonsensical)",
    "mean imputation -> Infrastructure Age (year 0 is nonsensical)",
    "mean imputation -> Climate Stress HDD/CDD branch (zero degree-days are n
onsensical)",
    "mean imputation -> Climate Stress HDD/CDD branch (zero degree-days are n
onsensical)",
    "coalesce(0) -> treated as zero population growth",
    "coalesce(0) -> used in net_migration_rate",
    "coalesce(0) -> treated as no net migration",
    "coalesce(0) -> treated as no new construction",
    "coalesce(0) -> treated as zero permit rate",
    "coalesce(0) -> treated as zero housing growth",
    "coalesce(0) -> treated as zero establishment density",
    "coalesce(0) -> treated as zero payroll per capita",
    "coalesce(0) -> treated as zero climate risk",
    "state-level -> always populated via hardcoded values"
  )
)

for (i in seq_len(nrow(audit_tbl))) {
  col <- audit_tbl$column[i]
  if (!col %in% names(county_features)) next
  n_na  <- sum(is.na(county_features[[col]]))
  pct   <- round(n_na / nrow(county_features) * 100, 1)
  cat(sprintf("%-28s %4d NA (%5.1f%%)  -> %s\n",
              col, n_na, pct, audit_tbl$handling[i]))
}
formed in Market Size
formed in Market Size; permit rate denominator
mic Capacity (zero income is nonsensical)
mic Capacity (zero home value is nonsensical)
structure Age (year 0 is nonsensical)
te Stress HDD/CDD branch (zero degree-days are nonsensical)
te Stress HDD/CDD branch (zero degree-days are nonsensical)
s zero population growth

et_migration_rate
s no net migration
s no new construction
s zero permit rate
s zero housing growth
s zero establishment density
s zero payroll per capita
s zero climate risk
pulated via hardcoded values
cat("\nNote: est_total_meters and pct_iou_meters are added in Phase 7 (meter
allocation).\n")
cation).
# PHASE 6: UTILITY ASSEMBLY
# Phase 6A. Roll Up Meter Workbook to One Row per Utility
# Combined Meter Data can contain multiple rows per utility (for example,
# multi-state utilities). Aggregate additive meter fields to utility level
# instead of keeping an arbitrary first row.
first_non_missing <- function(x) {
  x_chr <- trimws(as.character(x))
  keep <- !is.na(x) & nzchar(x_chr)
  if (any(keep)) x[which(keep)[1]] else x[1]
}

sum_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

meter_value_cols <- c(
  "x2019_pickups", "x2020_pickups", "x2021_pickups", "x2022_pickups",
  "x2023_pickups", "x2024_pickups", "x2025_pickups",
  "residential", "commercial", "industrial", "transportation", "total_meters"
)

meter_meta_cols <- c(
  "data_year", "utility_name", "ownership", "emerald_customer_y_n",
  "emerald_customer_name", "emerald_customer_id", "emerald_customer_state",
  "state", "short_form", "ba_code"
)

meter_utility_audit <- meters_raw %>%
  group_by(utility_number) %>%
  summarise(
    row_count = n(),
    ownership_values = n_distinct(ownership, na.rm = TRUE),
    .groups = "drop"
  )

if (any(meter_utility_audit$ownership_values > 1, na.rm = TRUE)) {
  stop("Combined Meter Data contains utilities with conflicting ownership val
ues across rows.")
}

meters_one <- meters_raw %>%
  group_by(utility_number) %>%
  summarise(
    across(all_of(meter_meta_cols), first_non_missing),
    across(all_of(meter_value_cols), sum_or_na),
    .groups = "drop"
  )

cat("Meter workbook rows:", nrow(meters_raw), "\n")
cat("Distinct utilities:", nrow(meters_one), "\n")
cat("Utilities aggregated from multiple rows:",
    sum(meter_utility_audit$row_count > 1, na.rm = TRUE), "\n")
# Phase 6B. Join Service Territory Rows to County Features
# Attach territory + meter data. The service-territory workbook does not
# provide county FIPS, so this is the one major county join that must remain
# name-based; downstream county joins switch to FIPS once it is available.
utility_county <- territory_clean %>%
  right_join(meters_one, by = "utility_number", relationship = "many-to-one")
%>%
  select(-ends_with(".y")) %>%
  rename_with(~ str_remove(.x, "\\.x$"))

utility_county_features <- utility_county %>%
  left_join(
    county_features_join %>%
      rename(
        county_matched = county,
        state_matched = state
      ),
    by = c("county_join_key", "state_join_key"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    county = coalesce(county_matched, county),
    state  = coalesce(state_matched, state)
  ) %>%
  select(-county_matched, -state_matched, -county_join_key, -state_join_key)
%>%
  # Collapse alias variants that resolve to the same utility/county pair.
  distinct(utility_number, county, state, fips, .keep_all = TRUE)

cat("Utility-county pairs:", nrow(utility_county_features), "\n")
cat("Utility-county pairs missing county FIPS after name join:",
    sum(is.na(utility_county_features$fips)), "\n")
cat("Utilities with at least one county missing FIPS:",
    n_distinct(utility_county_features$utility_number[is.na(utility_county_fe
atures$fips)]), "\n")
# PHASE 7: HIFLD GEOGRAPHIC ALLOCATION +
HOUSING-UNIT FALLBACK
# Phase 7A. Load and Standardize HIFLD Territories
hifld_path <- "electric-retail-service-territories-shapefile/Electric_Retail_
Service_Territories.shp"

# Known source-ID crosswalks between HIFLD polygons and the meter workbook.
# 4 Rivers is present in HIFLD as ID 64196, while the meter workbook uses
# utility_number 63286 for the same Kansas cooperative. FirstEnergy
# Pennsylvania is represented in HIFLD by subsidiary/opco polygons
# (Met-Ed, Penelec, Penn Power, West Penn), so those IDs roll up to the
# consolidated meter-workbook utility_number 66101 before geometry dissolve.
hifld_id_to_meter_id <- c(

  "64196" = "63286",
  "12390" = "66101",
  "14711" = "66101",
  "14716" = "66101",
  "20387" = "66101"
)

if (file.exists(hifld_path)) {
  hifld_sf <- st_read(
    hifld_path,
    quiet = TRUE
  ) %>%
    filter(is.na(COUNTRY) | COUNTRY == "USA") %>%
    st_make_valid() %>%
    st_transform(5070)   # Albers Equal Area - area-preserving

  cat("HIFLD loaded:", nrow(hifld_sf), "utility territory polygons\n")
} else {
  message("HIFLD shapefile not found - using housing-unit allocation only")
  hifld_sf <- NULL
}
# Phase 7B. Intersect HIFLD Territory with Counties
if (!is.null(hifld_sf)) {
  counties_sf <- counties(cb = TRUE, year = 2022, progress_bar = FALSE) %>%
    st_transform(5070) %>%
    mutate(county_area_m2 = as.numeric(st_area(.))) %>%
    st_make_valid() %>%
    st_buffer(dist = 0)

  hifld_clean <- hifld_sf %>%
    mutate(
      hifld_utility_id = as.character(ID),
      utility_number = coalesce(
        unname(hifld_id_to_meter_id[hifld_utility_id]),
        hifld_utility_id
      )
    ) %>%
    group_by(utility_number) %>%
    summarise(
      NAME = first(NAME[!is.na(NAME) & nzchar(NAME)]),
      .groups = "drop"
    ) %>%
    st_make_valid() %>%
    st_buffer(dist = 0)

  cat("Computing utility x county geographic intersections...\n")

  suppressWarnings({
    intersection <- st_intersection(
      hifld_clean %>% select(utility_number, NAME),
      counties_sf  %>% select(GEOID, county_area_m2)
    )
  })
  intersection$overlap_m2 <- as.numeric(st_area(intersection))

  geo_weights <- intersection %>%
    st_drop_geometry() %>%
    group_by(utility_number, county_fips = GEOID) %>%
    summarise(
      overlap_m2 = sum(overlap_m2),
      county_area_m2 = max(county_area_m2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      county_overlap_share = pmin(overlap_m2 / pmax(county_area_m2, 1), 1)
    ) %>%
    group_by(utility_number) %>%
    mutate(geo_weight = overlap_m2 / sum(overlap_m2)) %>%
    ungroup()

  cat("Geo weight pairs:", nrow(geo_weights),
      "covering", n_distinct(geo_weights$utility_number), "utilities\n")
  # Extra diagnostics: utility match rate and represented meter volume.
  matched_utilities <- meters_one %>%
    filter(!is.na(utility_number)) %>%
    distinct(utility_number, total_meters) %>%
    mutate(has_geo = utility_number %in% geo_weights$utility_number)

  cat("Utilities matched to HIFLD:",
      sum(matched_utilities$has_geo), "of", nrow(matched_utilities),
      "(", round(mean(matched_utilities$has_geo) * 100, 1), "%)\n")
  cat("Unmatched utilities:", sum(!matched_utilities$has_geo), "\n")
  cat("Meter volume covered by matched utilities:",
      round(
        sum(coalesce(matched_utilities$total_meters[matched_utilities$has_geo
], 0), na.rm = TRUE) /
          pmax(sum(coalesce(matched_utilities$total_meters, 0), na.rm = TRUE)
, 1) * 100,
      ),
      "%\n")
  hifld_available <- TRUE
} else {
  hifld_available <- FALSE
}

# Phase 7C. Add HIFLD -Only County Backfill Rows
allocation_input <- utility_county_features %>%
  mutate(
    is_hifld_backfill = FALSE,
    county_overlap_share = NA_real_
  )

geo_backfill <- utility_county_features %>%
  slice(0) %>%
  mutate(
    is_hifld_backfill = FALSE,
    county_overlap_share = NA_real_
  )

if (hifld_available) {
  matched_service_pairs <- utility_county_features %>%
    filter(!is.na(fips)) %>%
    distinct(utility_number, fips)

  # When workbook county names lag current Census geography (for example,
  # Connecticut planning regions or renamed Alaska county equivalents),
  # backfill county rows from HIFLD FIPS coverage rather than guessing names.
  # For these HIFLD-only rows, scale the housing-side denominator by the
  # share of county area actually covered so tiny polygon clips do not inheri
t
  # the county's full housing stock in the 50% housing half.
  geo_backfill <- geo_weights %>%
    select(utility_number, fips = county_fips, county_overlap_share) %>%
    distinct() %>%
    anti_join(matched_service_pairs, by = c("utility_number", "fips")) %>%
    # Only meter-workbook utilities should enter county allocation. HIFLD and
    # service-territory sources can include extra utility IDs that are out of
    # scope for this meter-based pipeline and would otherwise inflate the
    # backfill diagnostics without affecting exported allocations.
    semi_join(
      meters_one %>% distinct(utility_number),
      by = "utility_number"
    ) %>%
    left_join(county_features, by = "fips", relationship = "many-to-one") %>%
    filter(!is.na(total_housing_units)) %>%
    left_join(
      meters_one %>% select(-state),
      by = "utility_number",

      relationship = "many-to-one"
    ) %>%
    mutate(is_hifld_backfill = TRUE)

  if (nrow(geo_backfill) > 0) {
    if (any(is.na(geo_backfill$county_overlap_share))) {
      stop("HIFLD backfill rows are missing county_overlap_share values.")
    }

    backfill_utility_status <- geo_backfill %>%
      distinct(utility_number) %>%
      left_join(
        territory_clean %>%
          count(utility_number, name = "territory_row_count"),
        by = "utility_number"
      ) %>%
      left_join(
        utility_county_features %>%
          group_by(utility_number) %>%
          summarise(
            county_rows_with_fips_after_name_join = sum(!is.na(fips)),
            county_rows_missing_fips_after_name_join = sum(is.na(fips)),
            .groups = "drop"
          ),
        by = "utility_number"
      ) %>%
      mutate(
        territory_row_count = coalesce(territory_row_count, 0L),
        county_rows_with_fips_after_name_join = coalesce(county_rows_with_fip
s_after_name_join, 0L),
        county_rows_missing_fips_after_name_join = coalesce(county_rows_missi
ng_fips_after_name_join, 0L)
      )

    cat("Intermediate HIFLD county backfill rows (pre-audit):", nrow(geo_back
fill), "\n")
    cat("Intermediate utilities with HIFLD-added counties (pre-audit):",
        nrow(backfill_utility_status), "\n")
    cat("Median county-area overlap for HIFLD backfill rows:",
        round(median(geo_backfill$county_overlap_share, na.rm = TRUE) * 100,
2), "%\n")
  }

  allocation_input <- bind_rows(allocation_input, geo_backfill)
}

# Phase 7D. Compute Housing and Geographic Allocation Shares
# Step 1: housing-unit weights (baseline - always available). Workbook rows u
se
# full county housing units; HIFLD-only backfill rows use overlap-adjusted
# housing so small polygon clips do not receive the whole county's housing st
ock.
county_meter_allocation <- allocation_input %>%
  filter(!is.na(total_housing_units)) %>%
  mutate(
    is_hifld_backfill = coalesce(is_hifld_backfill, FALSE),
    county_overlap_share = if_else(
      is_hifld_backfill,
      pmin(pmax(coalesce(county_overlap_share, 0), 0), 1),
      NA_real_
    ),
    housing_units_for_weight = case_when(
      is_hifld_backfill ~ total_housing_units * county_overlap_share,
      TRUE ~ total_housing_units
    )
  ) %>%
  group_by(utility_number) %>%
  mutate(
    utility_total_housing = sum(housing_units_for_weight, na.rm = TRUE),
    housing_weight = ifelse(utility_total_housing > 0,
                            housing_units_for_weight / utility_total_housing,
                            1 / n())
  ) %>%
  ungroup()

# Step 2: attach geographic weights when HIFLD succeeded
if (hifld_available) {
  county_meter_allocation <- county_meter_allocation %>%
    left_join(
      geo_weights %>% select(utility_number, county_fips, geo_weight),
      by = c("utility_number", "fips" = "county_fips")
    )
} else {
  county_meter_allocation <- county_meter_allocation %>%
    mutate(geo_weight = NA_real_)
}

# Step 3: hybrid share = 50% housing for every row, plus 50% geographic
# spread only across counties with matched geo coverage. Geographic weights
# are renormalized after the join so each utility still sums to 100%.
county_meter_allocation <- county_meter_allocation %>%
  group_by(utility_number) %>%
  mutate(
    geo_total_weight = sum(coalesce(geo_weight, 0), na.rm = TRUE),
    utility_has_geo  = geo_total_weight > 0,

    geo_weight_local = ifelse(
      !is.na(geo_weight) & utility_has_geo,
      geo_weight / geo_total_weight,
      NA_real_
    ),
    county_share = case_when(
      utility_has_geo & !is.na(geo_weight_local) ~ 0.50 * geo_weight_local +
0.50 * housing_weight,
      utility_has_geo                            ~ 0.50 * housing_weight,
      TRUE                                       ~ housing_weight
    ),
    ownership_bucket = case_when(
      str_detect(coalesce(ownership, ""), regex("^Investor", ignore_case = TR
UE)) ~ "IOU",
      str_detect(coalesce(ownership, ""), regex("Cooperative", ignore_case =
TRUE)) ~ "Coop",
      str_detect(coalesce(ownership, ""), regex("Municipal", ignore_case = TR
UE)) ~ "Muni",
      TRUE ~ "Other"
    ),
    allocated_residential  = residential  * county_share,
    allocated_commercial   = commercial   * county_share,
    allocated_industrial   = industrial   * county_share,
    allocated_total_meters = total_meters * county_share,
    allocated_pickups_2024 = coalesce(x2024_pickups, 0) * county_share
  ) %>%
  ungroup()
# Phase 7E. Validate Allocation Shares
allocation_check <- county_meter_allocation %>%
  group_by(utility_number) %>%
  summarise(total_share = sum(county_share, na.rm = TRUE), .groups = "drop")

bad_allocation <- allocation_check %>%
  filter(abs(total_share - 1) > 1e-6)

if (nrow(bad_allocation) > 0) {
  stop(
    "Hybrid allocation failed normalization for ",
    nrow(bad_allocation),
    " utilities. Max deviation = ",
    round(max(abs(bad_allocation$total_share - 1)), 8)
  )
}

cat("Allocation method:", ifelse(hifld_available,
    "Hybrid (50% geographic + 50% housing units)",
    "Housing units only"), "\n")

pct_geo <- if (hifld_available)
  round(mean(!is.na(county_meter_allocation$geo_weight)) * 100, 1) else 0
cat("Pairs with geo weight:", pct_geo, "%\n")
# Phase 7F. Roll Utility Shares Up to County Meter Totals
# Once county FIPS is attached, roll county totals up by FIPS instead of name
s.
county_meters <- county_meter_allocation %>%
  filter(!is.na(fips)) %>%
  group_by(fips) %>%
  summarise(
    n_utilities            = n_distinct(utility_number),
    est_residential_meters = sum(allocated_residential,    na.rm = TRUE),
    est_commercial_meters  = sum(allocated_commercial,     na.rm = TRUE),
    est_industrial_meters  = sum(allocated_industrial,     na.rm = TRUE),
    est_total_meters       = sum(allocated_total_meters,   na.rm = TRUE),
    est_iou_meters         = sum(allocated_total_meters[ownership_bucket == "
IOU"],
                                 na.rm = TRUE),
    n_iou   = sum(ownership_bucket == "IOU",   na.rm = TRUE),
    n_coop  = sum(ownership_bucket == "Coop",  na.rm = TRUE),
    n_muni  = sum(ownership_bucket == "Muni",  na.rm = TRUE),
    n_other = sum(ownership_bucket == "Other", na.rm = TRUE),
    est_emerald_pickups = sum(allocated_pickups_2024,  na.rm = TRUE),
    has_emerald         = sum(allocated_pickups_2024,  na.rm = TRUE) > 10,
    n_emerald_utils     = n_distinct(utility_number[coalesce(x2024_pickups, 0
) > 0]),
    pct_geo_allocated   = sum(allocated_total_meters[!is.na(geo_weight)], na.
rm = TRUE) /
                          pmax(sum(allocated_total_meters, na.rm = TRUE), 1)
* 100,
    .groups = "drop"
  ) %>%
  mutate(
    pct_iou_meters = est_iou_meters / pmax(est_total_meters, 1)
  )

cat("Counties with meter data:     ", nrow(county_meters), "\n")
cat("Counties with Emerald activity:", sum(county_meters$has_emerald), "\n")

# PHASE 8: COUNTY DEMAND INDEX - 6
COMPONENTS
# Phase 8A. Join County Meter Totals Back to the County Feature Table
# Join county meter totals back by FIPS to avoid a second name-based county j
oin.
county_index_data <- county_features %>%
  left_join(county_meters, by = "fips", relationship = "one-to-one")

# Stub meter columns for counties not in service territory data
for (col in c("est_total_meters", "est_residential_meters", "est_commercial_m
eters",
              "est_industrial_meters", "est_iou_meters", "pct_iou_meters",
              "n_utilities", "n_iou", "n_coop", "n_muni", "n_other",
              "est_emerald_pickups", "n_emerald_utils", "has_emerald",
              "pct_geo_allocated")) {
  county_index_data <- fill_missing_col(county_index_data, col)
}

cat("Total counties:", nrow(county_index_data), "\n")
cat("With meter data before known exceptions:",
    sum(!is.na(county_index_data$est_total_meters)), "\n")
# Phase 8B. Enforce Meter Coverage and Apply Known Zero -Meter
Exceptions
missing_meter_coverage <- county_index_data %>%
  filter(is.na(est_total_meters) | is.na(pct_iou_meters))

if (nrow(missing_meter_coverage) > 0) {
  zero_meter_fallback_fips <- c("02060", "02220")
  unsupported_missing_meter_coverage <- missing_meter_coverage %>%
    filter(!fips %in% zero_meter_fallback_fips)

  if (nrow(unsupported_missing_meter_coverage) > 0) {
    print(
      unsupported_missing_meter_coverage %>%
        select(fips, county, state, est_total_meters, pct_iou_meters) %>%
        head(20)
    )
    stop(
      sprintf(
        paste0(

          "Meter allocation coverage is incomplete: %d counties are missing "
,
          "est_total_meters or pct_iou_meters. Fix Phase 7 allocation inputs
",
          "before scoring."
        ),
        nrow(unsupported_missing_meter_coverage)
      )
    )
  }

  cat(
    "Applying zero-meter fallback for known Alaska exceptions:",
    paste(missing_meter_coverage$fips, collapse = ", "),
    "\n"
  )
  print(
    missing_meter_coverage %>%
      select(fips, county, state, est_total_meters, pct_iou_meters) %>%
      head(20)
  )

  county_index_data <- county_index_data %>%
    mutate(
      est_total_meters = if_else(
        fips %in% zero_meter_fallback_fips & is.na(est_total_meters),
        0,
        est_total_meters
      ),
      est_residential_meters = if_else(
        fips %in% zero_meter_fallback_fips & is.na(est_residential_meters),
        0,
        est_residential_meters
      ),
      est_commercial_meters = if_else(
        fips %in% zero_meter_fallback_fips & is.na(est_commercial_meters),
        0,
        est_commercial_meters
      ),
      est_industrial_meters = if_else(
        fips %in% zero_meter_fallback_fips & is.na(est_industrial_meters),
        0,
        est_industrial_meters
      ),
      est_iou_meters = if_else(
        fips %in% zero_meter_fallback_fips & is.na(est_iou_meters),
        0,
        est_iou_meters
      ),
      pct_iou_meters = if_else(

        fips %in% zero_meter_fallback_fips & is.na(pct_iou_meters),
        0,
        pct_iou_meters
      ),
      n_utilities = if_else(
        fips %in% zero_meter_fallback_fips & is.na(n_utilities),
        0,
        n_utilities
      ),
      n_iou = if_else(
        fips %in% zero_meter_fallback_fips & is.na(n_iou),
        0,
        n_iou
      ),
      n_coop = if_else(
        fips %in% zero_meter_fallback_fips & is.na(n_coop),
        0,
        n_coop
      ),
      n_muni = if_else(
        fips %in% zero_meter_fallback_fips & is.na(n_muni),
        0,
        n_muni
      ),
      n_other = if_else(
        fips %in% zero_meter_fallback_fips & is.na(n_other),
        0,
        n_other
      ),
      est_emerald_pickups = if_else(
        fips %in% zero_meter_fallback_fips & is.na(est_emerald_pickups),
        0,
        est_emerald_pickups
      ),
      n_emerald_utils = if_else(
        fips %in% zero_meter_fallback_fips & is.na(n_emerald_utils),
        0,
        n_emerald_utils
      ),
      has_emerald = if_else(
        fips %in% zero_meter_fallback_fips & is.na(has_emerald),
        FALSE,
        has_emerald
      ),
      pct_geo_allocated = if_else(
        fips %in% zero_meter_fallback_fips & is.na(pct_geo_allocated),
        0,
        pct_geo_allocated
      )

    )
}
cat("With meter data after known exceptions:",
    sum(!is.na(county_index_data$est_total_meters)), "\n")
# Phase 8C. Define Scaling Helpers and Imputation Anchors
# Min-max scaler [0,1] - winsorized at 99th pct to prevent outlier compressio
n.
# Without winsorisation, LA/NYC extremes crush mid-distribution variation.
minmax <- function(x, invert = FALSE, winsor_pct = 0.99) {
  lo  <- quantile(x, 1 - winsor_pct, na.rm = TRUE)
  hi  <- quantile(x, winsor_pct,     na.rm = TRUE)
  x   <- pmax(pmin(x, hi), lo)
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(0.5, length(x)))
  scaled <- (x - rng[1]) / (rng[2] - rng[1])
  if (invert) scaled <- 1 - scaled
  scaled
}

# Mean-imputation for variables where 0 is nonsensical
mean_year_built  <- mean(county_index_data$median_year_built,         na.rm =
TRUE)
mean_income      <- mean(county_index_data$median_income,             na.rm =
TRUE)
mean_home_value  <- mean(county_index_data$median_home_value,         na.rm =
TRUE)
mean_hdd         <- mean(county_index_data$annual_heating_degree_days, na.rm
= TRUE)
mean_cdd         <- mean(county_index_data$annual_cooling_degree_days, na.rm
= TRUE)
# Phase 8D. Score the Six Demand Index Components
county_index <- county_index_data %>%
  mutate(

    # ----------------------------------------------------------
    # 1. MARKET SIZE  (W = 0.30)
    # log transforms: compressed scale prevents mega-counties from dominating

.
    # Meter coverage is required; the notebook stops before scoring if any
    # county is missing allocated meters.
    # ----------------------------------------------------------
    ms_meters       = minmax(log1p(est_total_meters)),
    ms_housing      = minmax(log1p(coalesce(total_housing_units, 0))),
    ms_pop          = minmax(log1p(coalesce(total_pop, 0))),
    ms_nonres_share = minmax(coalesce(
      (est_commercial_meters + est_industrial_meters) / pmax(est_total_meters
, 1), 0
    )),
    idx_market_size = 0.45 * ms_meters + 0.25 * ms_housing +
                      0.15 * ms_pop + 0.15 * ms_nonres_share,

    # ----------------------------------------------------------
    # 2. INFRASTRUCTURE AGE  (W = 0.05)
    # Older median year built = higher score because older housing stock
    # is more likely to imply aging electric infrastructure.
    # Mean imputation: year 0 is nonsensical; missing counties get the nation
al average.
    # ----------------------------------------------------------
    idx_infra_age = minmax(coalesce(median_year_built, mean_year_built), inve
rt = TRUE),

    # ----------------------------------------------------------
    # 3. CLIMATE STRESS  (W = 0.05)
    # NRI EAL (expected annual loss) captures multi-hazard dollar risk.
    # HDD + CDD captures heating/cooling demand burden.
    # Mean imputation for HDD/CDD: zero degree-days implies no climate demand
, which
    # is wrong for nearly all US counties. Missing counties get the national
mean.
    # ----------------------------------------------------------
    cs_hdd_cdd = minmax(coalesce(annual_heating_degree_days, mean_hdd) +
                          coalesce(annual_cooling_degree_days, mean_cdd)),
    cs_nri_eal = minmax(coalesce(nri_eal_score, 0)),
    idx_climate_stress = 0.60 * cs_nri_eal + 0.40 * cs_hdd_cdd,

    # ----------------------------------------------------------
    # 4. ECONOMIC CAPACITY  (W = 0.15)
    # Mean imputation for income/home value: zero is nonsensical (no county h
as
    # zero income or zero home values). Missing counties get the national mea
n.
    # Establishments/payroll keep 0-fill: genuinely can be zero (log1p handle
s it).
    # ----------------------------------------------------------
    ec_income  = minmax(coalesce(median_income,    mean_income)),
    ec_home    = minmax(coalesce(median_home_value, mean_home_value)),
    ec_estabs  = minmax(log1p(coalesce(total_establishments, 0))),

    ec_payroll = minmax(log1p(coalesce(annual_payroll_1000s,  0))),
    ec_price   = minmax(coalesce(res_price_cents_kwh, 0)),
    idx_economic_capacity =
      0.25 * ec_income + 0.25 * ec_home + 0.20 * ec_estabs +
      0.15 * ec_payroll + 0.15 * ec_price,

    # ----------------------------------------------------------
    # 5. OPERATIONAL COMPLEXITY  (W = 0.15)
    # Higher IOU meter share = higher score because Emerald's target strategy
    # is more aligned with investor-owned utility territories.
    # Meter coverage is required; pct_iou_meters must be present for every co
unty.
    # ----------------------------------------------------------
    idx_operational_complexity = minmax(pct_iou_meters),

    # ----------------------------------------------------------
    # 6. GROWTH POTENTIAL  (W = 0.30)
    # All inputs coalesced to 0: missing data = assumed zero growth/migration
.
    # Conservative but prevents NA propagation through the index.
    # net_migration_rate = net_domestic_migration / total_pop * 1000 (per-cap
ita).
    # ----------------------------------------------------------
    gp_permits_rate = minmax(coalesce(permits_per_1k_hu, 0)),
    gp_pop_growth   = minmax(coalesce(pop_growth_pct, 0)),
    gp_hu_growth    = minmax(coalesce(hu_growth_pct, 0)),
    gp_migration    = minmax(coalesce(net_migration_rate, 0)),
    idx_growth_potential =
      0.30 * gp_permits_rate + 0.30 * gp_pop_growth +
      0.20 * gp_hu_growth + 0.20 * gp_migration
  )

cat("Active component formulas:\n")
cat("  Market:   meters(45%) + housing(25%) + pop(15%) + nonres_share(15%); m
eter coverage required\n")
er coverage required
cat("  Climate:  0.60*nri_eal + 0.40*hdd_cdd\n")
cat("  Growth:   permits(30%) + pop_growth(30%) + hu_growth(20%) + migration(
20%)\n")
%)

cat("  Economic: income(25%) + home(25%) + estabs(20%) + payroll(15%) + price
(15%)\n")
5%)
# Phase 8E. Combine Component Scores into the Composite Index
W_MARKET   <- 0.30
W_GROWTH   <- 0.30
W_ECONOMIC <- 0.15
W_OPER     <- 0.15
W_INFRA    <- 0.05
W_CLIMATE  <- 0.05

county_index <- county_index %>%
  mutate(
    demand_index =
      W_MARKET   * idx_market_size            +
      W_GROWTH   * idx_growth_potential       +
      W_ECONOMIC * idx_economic_capacity      +
      W_OPER     * idx_operational_complexity +
      W_INFRA    * idx_infra_age              +
      W_CLIMATE  * idx_climate_stress,
    demand_index_100 = minmax(demand_index) * 100,
    demand_tier      = ntile(demand_index, 5)
  )

cat("\n=== DEMAND INDEX SUMMARY ===\n")
county_index %>%
  group_by(demand_tier) %>%
  summarise(
    n           = n(),
    avg         = round(mean(demand_index_100),               1),
    min         = round(min(demand_index_100),                1),
    max         = round(max(demand_index_100),                1),
    avg_meters  = round(mean(est_total_meters, na.rm = TRUE), 0),
    pct_emerald = round(mean(coalesce(has_emerald, FALSE)) * 100, 1),
    .groups = "drop"
  ) %>%
  print()

cat("\n=== TOP 20 COUNTIES ===\n")
county_index %>%
  arrange(desc(demand_index_100)) %>%
  select(county, state, demand_index_100, demand_tier,
         est_total_meters, total_pop, pop_growth_pct,
         avg_annual_permits, has_emerald, n_utilities) %>%
  head(20) %>%
  print(n = 20)
al_pop
<dbl>

# PHASE 9: CT FIPS CROSSWALK + OUTPUT NA AUDIT
# ACS 2023 returns planning-region FIPS (09110-09190) for Connecticut, but th
e
# TopoJSON map file uses the 8 pre-2022 county FIPS (09001-09015).
# old_fips = best-match legacy county FIPS for map display; NA for all other
states.
# When two planning regions share the same old FIPS (09001/Fairfield & 09009/
New Haven),
# the map-build chunk keeps the higher-scoring region.
ct_fips_xwalk <- tribble(
  ~fips,   ~old_fips,
  "09110", "09003",   # Capitol               -> Hartford (dominant county)
  "09120", "09001",   # Greater Bridgeport    -> Fairfield (western)
  "09130", "09007",   # Lower CT River Valley -> Middlesex
  "09140", "09009",   # Naugatuck Valley      -> New Haven
  "09150", "09015",   # Northeastern CT       -> Windham (primary)
  "09160", "09005",   # Northwest Hills       -> Litchfield
  "09170", "09009",   # South Central CT      -> New Haven (city core)
  "09180", "09011",   # Southeastern CT       -> New London
  "09190", "09001"    # Western CT            -> Fairfield (eastern)
)

county_index <- county_index %>%
  left_join(ct_fips_xwalk, by = "fips")

cat("CT planning region rows:", sum(county_index$state == "Connecticut", na.r
m = TRUE), "\n")
cat("CT rows with old_fips:  ", sum(!is.na(county_index$old_fips)), "\n")
cat("=== NA AUDIT: OUTPUT COLUMNS ===\n")

for (col in c("demand_index_100", "demand_tier",
              "idx_market_size", "idx_infra_age", "idx_climate_stress",
              "idx_economic_capacity", "idx_operational_complexity",
              "idx_growth_potential")) {
  n_na <- sum(is.na(county_index[[col]]))
  pct  <- round(n_na / nrow(county_index) * 100, 1)
  cat(sprintf("%-30s %4d NA (%5.1f%%)\n", col, n_na, pct))
}
# PHASE 10: EXPORT CSVS
# Phase 10A. Export the County -Level Index Output
county_index %>%
  select(
    fips, old_fips, county, state,
    demand_index_100, demand_tier,
    idx_market_size, idx_infra_age, idx_climate_stress,
    idx_economic_capacity, idx_operational_complexity, idx_growth_potential,
    est_total_meters, est_residential_meters, est_commercial_meters,
    est_industrial_meters, est_iou_meters, pct_iou_meters,
    total_pop, total_housing_units, median_income, median_year_built,
    pop_growth_pct, pop_growth_1yr_pct, net_domestic_migration, net_migration
_rate,
    avg_annual_permits, avg_sf_permits, avg_mf_permits, permit_growth_pct, pe
rmits_per_1k_hu, hu_growth_pct,
    total_establishments, total_employment, annual_payroll_1000s,
    res_price_cents_kwh, nri_eal_score,
    n_utilities, n_iou, n_coop, n_muni, n_other,
    has_emerald, est_emerald_pickups, n_emerald_utils,
    pct_geo_allocated
  ) %>%
  write_csv(OUTPUT_COUNTY_CSV)

cat("Exported:", OUTPUT_COUNTY_CSV, "-", nrow(county_index), "rows\n")

# Phase 10B. Build the Utility-Level Allocation Audit
territory_audit <- territory_clean %>%
  count(utility_number, name = "territory_row_count")

name_match_audit <- utility_county_features %>%
  group_by(utility_number) %>%
  summarise(
    county_rows_after_name_join = n(),
    county_rows_with_fips_after_name_join = sum(!is.na(fips)),
    county_rows_missing_fips_after_name_join = sum(is.na(fips)),
    .groups = "drop"
  )

backfill_audit <- geo_backfill %>%
  group_by(utility_number) %>%
  summarise(
    hifld_backfill_rows = n(),
    .groups = "drop"
  )

allocation_audit <- county_meter_allocation %>%
  group_by(utility_number) %>%
  summarise(
    allocation_row_count = n(),
    allocated_counties = n_distinct(fips[!is.na(fips)]),
    counties_with_geo = sum(!is.na(geo_weight)),
    counties_without_geo = sum(is.na(geo_weight)),
    utility_has_geo = any(utility_has_geo),
    pct_geo_county_rows = mean(!is.na(geo_weight)) * 100,
    total_share = sum(county_share, na.rm = TRUE),
    allocated_residential = sum(allocated_residential, na.rm = TRUE),
    allocated_commercial = sum(allocated_commercial, na.rm = TRUE),
    allocated_industrial = sum(allocated_industrial, na.rm = TRUE),
    allocated_total_meters = sum(allocated_total_meters, na.rm = TRUE),
    allocated_pickups_2024 = sum(allocated_pickups_2024, na.rm = TRUE),
    .groups = "drop"
  )

utility_allocation_audit <- meters_one %>%
  transmute(
    utility_number,
    utility_name,
    ownership,
    source_residential = residential,
    source_commercial = commercial,
    source_industrial = industrial,
    source_total_meters = total_meters,
    source_pickups_2024 = x2024_pickups
  ) %>%

  left_join(territory_audit, by = "utility_number") %>%
  left_join(name_match_audit, by = "utility_number") %>%
  left_join(backfill_audit, by = "utility_number") %>%
  left_join(allocation_audit, by = "utility_number") %>%
  mutate(
    across(
      c(
        territory_row_count,
        county_rows_after_name_join,
        county_rows_with_fips_after_name_join,
        county_rows_missing_fips_after_name_join,
        hifld_backfill_rows,
        allocation_row_count,
        allocated_counties,
        counties_with_geo,
        counties_without_geo
      ),
      ~ coalesce(.x, 0L)
    ),
    utility_has_geo = coalesce(utility_has_geo, FALSE),
    pct_geo_county_rows = coalesce(pct_geo_county_rows, 0),
    total_share = coalesce(total_share, 0),
    share_gap = if_else(allocation_row_count > 0L, total_share - 1, 0),
    allocated_residential = coalesce(allocated_residential, 0),
    allocated_commercial = coalesce(allocated_commercial, 0),
    allocated_industrial = coalesce(allocated_industrial, 0),
    allocated_total_meters = coalesce(allocated_total_meters, 0),
    allocated_pickups_2024 = coalesce(allocated_pickups_2024, 0),
    meter_gap = allocated_total_meters - coalesce(source_total_meters, 0),
    pickup_gap = allocated_pickups_2024 - coalesce(source_pickups_2024, 0),
    allocation_status = case_when(
      allocation_row_count == 0 & territory_row_count == 0 ~ "no_territory_ro
ws",
      allocation_row_count == 0 ~ "territory_rows_but_not_allocable",
      utility_has_geo ~ "hybrid_geo_plus_housing",
      TRUE ~ "housing_only"
    )
  ) %>%
  arrange(desc(source_total_meters), utility_number)

final_backfill_summary <- utility_allocation_audit %>%
  filter(hifld_backfill_rows > 0)

cat("Final HIFLD county backfill rows:", sum(final_backfill_summary$hifld_bac
kfill_rows), "\n")
cat("Final utilities with HIFLD-added counties:", nrow(final_backfill_summary
), "\n")

cat("  No workbook territory rows:",
    sum(final_backfill_summary$territory_row_count == 0), "\n")
cat("  Workbook rows but zero matched county FIPS:",
    sum(
      final_backfill_summary$territory_row_count > 0 &
        final_backfill_summary$county_rows_with_fips_after_name_join == 0
    ),
    "\n")
cat("  Some workbook counties matched, some missed:",
    sum(
      final_backfill_summary$county_rows_with_fips_after_name_join > 0 &
        final_backfill_summary$county_rows_missing_fips_after_name_join > 0
    ),
    "\n")
cat("  Workbook counties matched cleanly; HIFLD added more counties:",
    sum(
      final_backfill_summary$county_rows_with_fips_after_name_join > 0 &
        final_backfill_summary$county_rows_missing_fips_after_name_join == 0
    ),
    "\n")
# Phase 10C. Export the Utility-Level Allocation Audit
utility_allocation_audit %>%
  write_csv(OUTPUT_ALLOCATION_AUDIT_CSV)

cat("Exported:", OUTPUT_ALLOCATION_AUDIT_CSV, "-", nrow(utility_allocation_au
dit), "rows\n")
# PHASE 11: BUILD INTERACTIVE MAP
# Phase 11A. Prepare County -Level Map Data
csv_data <- read_csv(OUTPUT_COUNTY_CSV, show_col_types = FALSE)

# --- CT crosswalk: map planning-region FIPS to old county FIPS ----------

# Two planning regions share old FIPS 09001 and 09009; keep higher score.
map_data <- csv_data %>%
  mutate(map_key = coalesce(old_fips, fips)) %>%
  filter(!is.na(map_key), nchar(map_key) == 5) %>%
  group_by(map_key) %>%
  slice_max(demand_index_100, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    ms100  = round(coalesce(idx_market_size,            0) * 100, 1),
    ia100  = round(coalesce(idx_infra_age,              0) * 100, 1),
    cs100  = round(coalesce(idx_climate_stress,         0) * 100, 1),
    ec100  = round(coalesce(idx_economic_capacity,      0) * 100, 1),
    oc100  = round(coalesce(idx_operational_complexity, 0) * 100, 1),
    gp100  = round(coalesce(idx_growth_potential,       0) * 100, 1),
    name   = paste0(county, ", ", state),
    has_em_int = as.integer(coalesce(has_emerald, FALSE)),
    across(c(est_total_meters, total_pop, median_income, median_year_built,
             n_utilities, avg_annual_permits, avg_sf_permits, avg_mf_permits,
             total_establishments, total_employment,
             est_residential_meters, est_commercial_meters, est_industrial_me
ters,
             est_emerald_pickups, n_iou, n_coop, n_muni, n_other),
           ~ as.integer(round(replace_na(.x, 0)))),
    across(c(demand_index_100, pop_growth_pct, hu_growth_pct,
             net_migration_rate, nri_eal_score, res_price_cents_kwh),
           ~ round(replace_na(.x, 0), 1)),
    demand_tier = coalesce(as.integer(demand_tier), 1L)
  )

# --- Build compact JSON keyed by 5-digit FIPS --------------------------
# Array positions:
#  0:score  1:tier  2:ms    3:ia    4:cs    5:ec    6:oc    7:gp
#  8:mt     9:pop   10:inc  11:yb   12:nu   13:name
# 14:permits 15:popgr 16:hugr 17:netmig 18:nri 19:price
# 20:resmt 21:commt 22:indmt  23:sfp  24:mfp
# 25:estabs  26:employment  27:hasEm  28:pickups
# 29:iou  30:coop  31:muni  32:other

CD_list <- setNames(
  lapply(seq_len(nrow(map_data)), function(i) {
    d <- map_data[i, ]
    list(
      d$demand_index_100, d$demand_tier,
      d$ms100, d$ia100, d$cs100, d$ec100, d$oc100, d$gp100,
      d$est_total_meters, d$total_pop, d$median_income,
      d$median_year_built, d$n_utilities, d$name,
      d$avg_annual_permits, d$pop_growth_pct, d$hu_growth_pct,
      d$net_migration_rate, d$nri_eal_score, d$res_price_cents_kwh,
      d$est_residential_meters, d$est_commercial_meters, d$est_industrial_met
ers,

      d$avg_sf_permits, d$avg_mf_permits, d$total_establishments, d$total_emp
loyment,
      d$has_em_int, d$est_emerald_pickups, d$n_iou, d$n_coop, d$n_muni, d$n_o
ther
    )
  }),
  map_data$map_key
)

cd_json   <- toJSON(CD_list, auto_unbox = TRUE)
# Phase 11B. Build County and Utility Tooltip Data
# --- Build per-county utility list for tooltip ----------------------------
# Start from meter-workbook utility names so housing-only utilities and any
# HIFLD-to-meter ID crosswalk cases keep human-readable labels in the map.
util_names <- meters_one %>%
  transmute(
    utility_number,
    meter_util_name = utility_name
  )

if (!is.null(hifld_sf)) {
  hifld_util_names <- hifld_sf %>%
    st_drop_geometry() %>%
    mutate(
      hifld_utility_id = as.character(ID),
      utility_number = coalesce(
        unname(hifld_id_to_meter_id[hifld_utility_id]),
        hifld_utility_id
      )
    ) %>%
    select(utility_number, hifld_util_name = NAME) %>%
    distinct()

  util_names <- util_names %>%
    left_join(hifld_util_names, by = "utility_number") %>%
    transmute(
      utility_number,
      util_name = coalesce(meter_util_name, hifld_util_name, paste("Utility",
utility_number))
    ) %>%
    distinct()
} else {
  util_names <- util_names %>%
    transmute(
      utility_number,
      util_name = coalesce(meter_util_name, paste("Utility", utility_number))
    ) %>%
    distinct()

}

county_fips_map <- map_data %>%
  select(county, state, map_key) %>%
  distinct()

cu_raw <- county_meter_allocation %>%
  left_join(county_fips_map, by = c("county", "state")) %>%
  filter(!is.na(map_key)) %>%
  left_join(util_names, by = "utility_number") %>%
  mutate(
    util_name = coalesce(util_name, paste("Utility", utility_number)),
    own_code  = coalesce(ownership_bucket, "Other"),
    mt = as.integer(round(allocated_total_meters)),
    pk = as.integer(round(coalesce(allocated_pickups_2024, 0)))
  ) %>%
  arrange(map_key, desc(allocated_total_meters))

cu_split <- split(cu_raw, cu_raw$map_key)
cu_named <- lapply(cu_split, function(df) {
  lapply(seq_len(nrow(df)), function(i)
    list(df$util_name[i], df$own_code[i], df$mt[i], df$pk[i], df$utility_numb
er[i])
  )
})
cat("County utility lists built:", length(cu_named), "counties\n")
cu_json <- toJSON(cu_named, auto_unbox = TRUE)

# --- Build per-utility territory totals for hover sub-tip -----------------
ut_raw <- county_meter_allocation %>%
  group_by(utility_number) %>%
  summarise(
    total_mt = as.integer(round(sum(allocated_total_meters, na.rm = TRUE))),
    total_pk = as.integer(round(sum(coalesce(allocated_pickups_2024, 0), na.r
m = TRUE))),
    n_co     = n_distinct(paste(county, state)),
    .groups  = "drop"
  )
ut_list <- setNames(
  lapply(seq_len(nrow(ut_raw)), function(i)
    list(mt = ut_raw$total_mt[i], pk = ut_raw$total_pk[i], nc = ut_raw$n_co[i
])),
  ut_raw$utility_number
)
ut_json <- toJSON(ut_list, auto_unbox = TRUE)
# Phase 11C. Load Static Map Assets

topo_json <- readLines("node_modules/us-atlas/counties-albers-10m.json", warn
= FALSE)
d3_js     <- paste(readLines("node_modules/d3/dist/d3.min.js", warn = FALSE),
collapse = "\n")
topo_js   <- paste(readLines("node_modules/topojson-client/dist/topojson-clie
nt.min.js",
                              warn = FALSE), collapse = "\n")
# Phase 11D. Assemble and Export the Self -Contained Map HTML
# --- Assemble HTML ------------------------------------------------------
html <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Emerald Demand Index</title>
<style>
@import url("https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;60
0;700&family=JetBrains+Mono:wght@400;500&display=swap");
*{margin:0;padding:0;box-sizing:border-box}
body{font-family:"DM Sans",sans-serif;background:#0a0f1a;color:#e0e4ec;min-he
ight:100vh;overflow-x:hidden}

/* Header */
.header{padding:10px 28px 8px;display:flex;justify-content:space-between;alig
n-items:flex-start;flex-wrap:wrap;gap:8px}
.header h1{font-size:20px;font-weight:700;color:#fff;letter-spacing:-0.5px}
.header .sub{font-size:12px;color:#7a8299;margin-top:3px}
.legend{display:flex;align-items:center;gap:6px;font-size:11px;color:#7a8299;
font-family:"JetBrains Mono",monospace}
.legend-bar{width:160px;height:10px;border-radius:5px;background:linear-gradi
ent(to right,#1a1f2e,#1b3a4b,#1a6b5a,#b8a038,#d4542e)}

/* Controls */
.controls{padding:0 28px 5px;display:flex;gap:10px;align-items:center;flex-wr
ap:wrap}
.cg{display:flex;align-items:center;gap:5px}
.cg label{font-size:11px;color:#7a8299;font-weight:500}
select{font-family:"DM Sans",sans-serif;background:#141929;color:#e0e4ec;bord
er:1px solid #2a3040;border-radius:5px;padding:5px 8px;font-size:11px;cursor:
pointer}
select:focus{outline:none;border-color:#4a7}
.btn{font-family:"DM Sans",sans-serif;background:#141929;color:#7a8299;border
:1px solid #2a3040;border-radius:5px;padding:5px 10px;font-size:11px;cursor:p
ointer;transition:all .15s}
.fbtn:hover{border-color:#4a7;color:#e0e4ec}.fbtn.on{background:#1a3a2e;borde
r-color:#4a7;color:#6ddbab}
.rbtn:hover{border-color:#c67b3a;color:#e0e4ec}.rbtn.on{background:#2a2218;bo
rder-color:#c67b3a;color:#f0a050}

.ebtn:hover{border-color:#7b4acf;color:#e0e4ec}.ebtn.on{background:#1e1a30;bo
rder-color:#7b4acf;color:#b08aee}

/* Stats bar */
.stats{padding:3px 28px 3px;display:flex;gap:20px;font-size:11px;color:#7a829
9;flex-wrap:wrap}
.sv{color:#e0e4ec;font-family:"JetBrains Mono",monospace;font-weight:600}

/* Map */
.map-wrap{position:relative;width:100%;padding:0 8px}
#map{width:100%;height:auto;max-height:calc(100vh - 130px);display:block}
.county{stroke:#0e1420;stroke-width:.2px;cursor:pointer;transition:opacity .1
5s}
.county:hover{stroke:#fff;stroke-width:1.5px;filter:brightness(1.3)}
.county.dim{opacity:.10}
.county.inR{stroke:#f0a050;stroke-width:.6px}
.county.emH{stroke:#b08aee;stroke-width:.7px}
.sb{fill:none;stroke:#2a3040;stroke-width:.7px;pointer-events:none}
.rc{fill:rgba(240,160,80,.05);stroke:#f0a050;stroke-width:1.5px;stroke-dashar
ray:6,4;pointer-events:none;opacity:0}
.rc.vis{opacity:1}

/* Tooltip */
.tip{position:fixed;pointer-events:none;background:#141929ee;border:1px solid
#2a3040;border-radius:10px;padding:14px 16px;font-size:11px;line-height:1.5;z
-index:1000;min-width:280px;max-width:340px;box-shadow:0 8px 32px rgba(0,0,0,
.6);opacity:0;transition:opacity .1s,border-color .2s;backdrop-filter:blur(8p
x)}
.tip.vis{opacity:1}
.tip.pinned{pointer-events:auto;border-color:#3d6b50;box-shadow:0 8px 32px rg
ba(0,0,0,.7),0 0 0 1px #3d6b5044;animation:tipPin .5s ease forwards}
@keyframes tipPin{0%{box-shadow:0 8px 32px rgba(0,0,0,.7),0 0 0 5px #6ddbab55
}100%{box-shadow:0 8px 32px rgba(0,0,0,.7),0 0 0 1px #3d6b5044}}
.tip.pinned .tcr:hover,.tip.pinned [data-xp]:hover{background:#ffffff0a;borde
r-radius:4px;cursor:default}
.tip-xp{position:fixed;pointer-events:none;background:#0d1624f0;border:1px so
lid #2a3040;border-radius:8px;padding:10px 13px;font-size:10px;line-height:1.
65;color:#a0a8bc;z-index:1100;width:220px;opacity:0;transition:opacity .1s;ba
ckdrop-filter:blur(8px)}
.tip-xp.vis{opacity:1}
.tn{font-size:14px;font-weight:700;color:#fff;margin-bottom:2px}
.te{font-size:10px;color:#b08aee;margin-bottom:4px}
.tsc{display:flex;gap:16px;margin-bottom:4px}
.tsl{font-size:9px;color:#7a8299;text-transform:uppercase;letter-spacing:.3px
}
.tsv{font-family:"JetBrains Mono",monospace;font-size:18px;font-weight:700}
.tt{font-size:10px;color:#7a8299;margin-bottom:6px;text-transform:uppercase;l
etter-spacing:.5px}
.td{height:1px;background:#2a3040;margin:6px 0}
.tsx{font-size:10px;color:#f0a050;text-transform:uppercase;letter-spacing:.5p

x;margin:5px 0 3px;font-weight:600}
.tr{display:flex;justify-content:space-between;padding:1.5px 0}
.tl{color:#7a8299;font-size:11px}
.tv{font-family:"JetBrains Mono",monospace;color:#e0e4ec;font-weight:500;font
-size:11px}
.tc{display:grid;grid-template-columns:1fr 1fr;gap:3px 14px}
.tcr{display:flex;justify-content:space-between;align-items:center;gap:6px}
.tbc{display:flex;align-items:center;gap:4px;flex:1}
.tbb{flex:1;height:3px;background:#1a1f2e;border-radius:2px;overflow:hidden}
.tbf{height:100%;border-radius:2px}

/* Radius panel */
.rp{position:fixed;bottom:20px;left:50%;transform:translateX(-50%);background
:#141929ee;border:1px solid #c67b3a44;border-radius:12px;padding:14px 20px;fo
nt-size:12px;z-index:999;box-shadow:0 8px 32px rgba(0,0,0,.5);backdrop-filter
:blur(8px);display:none;min-width:400px}
.rp.vis{display:flex;gap:20px;align-items:center;flex-wrap:wrap}
.rpt{font-size:11px;color:#f0a050;text-transform:uppercase;letter-spacing:.5p
x;font-weight:600;white-space:nowrap}
.rps{display:flex;gap:16px;flex-wrap:wrap}
.rpst{white-space:nowrap}
.rpv{font-family:"JetBrains Mono",monospace;color:#fff;font-weight:700;font-s
ize:13px}
.rpl{color:#7a8299;font-size:10px}

/* County search */
.srch-wrap{position:relative}
.srch{font-family:"DM Sans",sans-serif;background:#141929;color:#e0e4ec;borde
r:1px solid #2a3040;border-radius:5px;padding:5px 8px;font-size:11px;width:19
0px;outline:none}
.srch:focus{border-color:#4a90d9}
.srch-drop{position:absolute;top:calc(100% + 3px);left:0;background:#141929;b
order:1px solid #2a3040;border-radius:6px;min-width:230px;max-height:210px;ov
erflow-y:auto;z-index:2000;display:none;box-shadow:0 8px 24px rgba(0,0,0,.5)}
.srch-drop.vis{display:block}
.srch-item{padding:6px 10px;font-size:11px;cursor:pointer;color:#c0c8dc;borde
r-bottom:1px solid #1e2535}
.srch-item:last-child{border-bottom:none}
.srch-item:hover,.srch-item.ssel{background:#1a2a42;color:#fff}
.county.hl{stroke:#4a90d9!important;stroke-width:2.5px!important;filter:brigh
tness(1.5)!important}
</style>
</head>
<body>

<div class="header">
  <div>
    <h1>Emerald Demand Index</h1>
    <div class="sub">County-level meter pickup demand &middot; 6 components</
div>

  </div>
  <div class="legend"><span>0</span><div class="legend-bar"></div><span>100</
span></div>
</div>

<div class="controls">
  <div class="cg">
    <label>Tier:</label>
    <button class="btn fbtn on" data-t="all">All</button>
    <button class="btn fbtn" data-t="5">5</button>
    <button class="btn fbtn" data-t="4">4</button>
    <button class="btn fbtn" data-t="3">3</button>
    <button class="btn fbtn" data-t="2">2</button>
    <button class="btn fbtn" data-t="1">1</button>
  </div>
  <div class="cg">
    <label>Color:</label>
    <select id="colorBy">
      <option value="score">Demand Index</option>
      <option value="ms">Market Size</option>
      <option value="gp">Growth Potential</option>
      <option value="ec">Economic Capacity</option>
      <option value="oc">IOU Alignment</option>
      <option value="cs">Climate Stress</option>
      <option value="ia">Infrastructure Age</option>
      <option value="permits">Avg Permits (2022-2024)</option>
      <option value="popgrowth">Population Growth</option>
    </select>
  </div>
  <div class="cg">
    <label>Diameter:</label>
    <button class="btn rbtn on" data-r="0">Off</button>
    <button class="btn rbtn" data-r="100">200 mi</button>
    <button class="btn rbtn" data-r="200">400 mi</button>
    <button class="btn rbtn" data-r="300">600 mi</button>
  </div>
  <div class="cg">
    <button class="btn ebtn" id="emTog">Emerald Overlay</button>
  </div>
  <div class="cg">
    <label>Find:</label>
    <div class="srch-wrap">
      <input id="srchBox" class="srch" type="text" placeholder="County, State
..." autocomplete="off" spellcheck="false">
      <div id="srchDrop" class="srch-drop"></div>
    </div>
  </div>
</div>

<div class="stats" id="stats"></div>

<div class="map-wrap"><svg id="map" viewBox="0 0 975 610"></svg></div>
<div class="tip" id="tip"></div>
<div class="tip-xp" id="tipXp"></div>
<div class="rp" id="rp"></div>

<script>', d3_js, '</script>
<script>', topo_js, '</script>
<script>
const US=', topo_json, ';
const CD=', cd_json, ';
const CU=', cu_json, ';
const UT=', ut_json, ';

/* Index map */
const I={score:0,tier:1,ms:2,ia:3,cs:4,ec:5,oc:6,gp:7,mt:8,pop:9,inc:10,yb:11
,nu:12,name:13,
         permits:14,popgr:15,hugr:16,netmig:17,nri:18,price:19,resmt:20,commt
:21,indmt:22,
         sfp:23,mfp:24,estabs:25,employment:26,hasEm:27,pickups:28,iou:29,coo
p:30,muni:31,other:32};

const tierLbl={1:"Tier 1 - Low",2:"Tier 2 - Below Avg",3:"Tier 3 - Moderate",
4:"Tier 4 - High",5:"Tier 5 - Top"};
const compLbl={ms:"Market Size",ia:"Infra Age",cs:"Climate Stress",ec:"Econom
ic Capacity",oc:"IOU Alignment",gp:"Growth Potential"};

/* Color scale builder */
function mkScale(key){
  const cmap=["#1a1f2e","#1b3a4b","#1a6b5a","#b8a038","#d4542e"];
  let vals;
  if(key==="permits") vals=Object.values(CD).map(d=>Math.log1p(d[I.permits]))
;
  else if(key==="popgrowth") vals=Object.values(CD).map(d=>d[I.popgr]);
  else if(key==="nri") vals=Object.values(CD).map(d=>d[I.nri]);
  else if(key==="price") vals=Object.values(CD).map(d=>d[I.price]);
  else if(key==="score") vals=Object.values(CD).map(d=>d[I.score]);
  else vals=Object.values(CD).map(d=>d[I[key]]);
  vals=vals.filter(v=>v!=null&&isFinite(v));
  const lo=Math.min(...vals),hi=Math.max(...vals);
  return function(v){
    if(v==null||!isFinite(v))return"#0e1118";
    const t=Math.max(0,Math.min(1,(v-lo)/(hi-lo||1)));
    const idx=Math.min(Math.floor(t*(cmap.length-1)),cmap.length-2);
    return d3.interpolateRgb(cmap[idx],cmap[idx+1])(t*(cmap.length-1)-idx);
  };
}
function gv(d,key){
  if(key==="permits")return Math.log1p(d[I.permits]);
  if(key==="popgrowth")return d[I.popgr];
  if(key==="nri")return d[I.nri];

  if(key==="price")return d[I.price];
  if(key==="score")return d[I.score];
  return d[I[key]];
}

/* State */
let ck="score",cs=mkScale("score"),tier="all",rMi=0,showEm=false;
let tipFrozen=false,tipReady=false,readyTimer=null,lastMX=0,lastMY=0;
const fmt=d3.format(",");
const fK=v=>v>=1e6?(v/1e6).toFixed(1)+"M":v>=1e3?(v/1e3).toFixed(0)+"K":Strin
g(v);
const fP=v=>(v>=0?"+":"")+v.toFixed(1)+"%";

const tip=document.getElementById("tip");
const tipXp=document.getElementById("tipXp");
const rp=document.getElementById("rp");

/* Row explanation lookup (shown on hover when tooltip is pinned) */
const XP={
  ms:"Allocated meters, housing units, population, and non-residential meter
share. High = larger, more commercially mixed market.",
  ia:"ACS median year built. Older housing stock scores higher because it oft
en implies older electric infrastructure.",
  cs:"FEMA expected annual hazard loss plus heating and cooling degree days.
High = more climate and hazard stress.",
  ec:"Income, home value, business establishments, payroll, and residential e
lectricity price. High = stronger customer and business capacity.",
  oc:"Share of allocated meters served by investor-owned utilities. High = st
ronger fit for Emerald IOU-focused strategy.",
  gp:"Average annual permits (2022-2024), population growth (2020-2024), hous
ing growth (2019-2023), and 2024 net domestic migration. High = expanding mar
ket.",
  mt:"EIA Form 861 meters allocated using workbook counties, HIFLD overlap, a
nd overlap-adjusted housing for HIFLD-only backfill rows.",
  permits:"Average annual residential building permits, 2022-2024. SF = singl
e-family units; MF = multi-family units.",
  popgr:"Population change from 2020 to 2024 using Census population estimate
s.",
  hugr:"Housing unit change from 2019 to 2023 using ACS 5-year housing estima
tes.",
  netmig:"2024 net domestic migration per 1,000 residents. Positive = net in-
migration.",
  pop:"ACS 2019-2023 5-year total population.",
  estabs:"County Business Patterns establishments across all industries.",
  employment:"County Business Patterns employment across all industries.",
  utilities:"Unique utilities with allocated meter presence in the county, gr
ouped by ownership type.",
  inc:"ACS 2019-2023 5-year median household income.",
  price:"Average residential electricity price in cents per kWh (EIA).",
  nri:"FEMA National Risk Index expected annual loss in dollars, not a percen

tile score.",
  yb:"ACS median year built. Lower year = older housing stock.",
  pickups:"Estimated 2024 Emerald pickups allocated to counties by utility te
rritory share.",
};

function posTip(e){
  const r=tip.getBoundingClientRect();
  let x=e.clientX+18,y=e.clientY-8;
  if(x+r.width>window.innerWidth-8)x=e.clientX-r.width-18;
  if(y+r.height>window.innerHeight-8)y=window.innerHeight-r.height-8;
  if(y<8)y=8;
  tip.style.left=x+"px";tip.style.top=y+"px";
}

function enableReady(){
  tipReady=true;tip.classList.add("pinned");
  /* snap tooltip so cursor is flush against its left (or right) edge - zero
gap to enter */
  const r=tip.getBoundingClientRect();
  let x=lastMX,y=lastMY-8;
  if(x+r.width>window.innerWidth-8)x=lastMX-r.width;
  if(y+r.height>window.innerHeight-8)y=window.innerHeight-r.height-8;
  if(y<8)y=8;
  tip.style.left=x+"px";tip.style.top=y+"px";
}
function resetTip(){
  tipFrozen=false;tipReady=false;
  clearTimeout(readyTimer);
  tip.classList.remove("pinned","vis");
  tipXp.classList.remove("vis");
}

/* Mouse enters tooltip -> freeze it so user can read and hover rows */
tip.addEventListener("mouseenter",()=>{if(tipReady)tipFrozen=true;});
tip.addEventListener("mouseleave",()=>{tipXp.classList.remove("vis");resetTip
();});

/* Explanation sub-tip on row hover (only when frozen inside tooltip) */
tip.addEventListener("mouseover",e=>{
  if(!tipFrozen)return;
  const elUid=e.target.closest("[data-uid]");
  const elXp=e.target.closest("[data-xp]");
  let txt=null;
  if(elUid){
    const u=UT[elUid.dataset.uid];
    if(u){
      const pkLine=u.pk>0?`${fK(u.pk)} est. pickups 2024`:"no pickup data";
      txt=`Full territory: ${fK(u.mt)} total meters across ${u.nc} ${u.nc===1
?"county":"counties"} - ${pkLine}`;

    }
  } else if(elXp){
    txt=XP[elXp.dataset.xp];
  }
  if(!txt){tipXp.classList.remove("vis");return;}
  tipXp.textContent=txt;
  tipXp.classList.add("vis");
  const ref=(elUid||elXp).getBoundingClientRect(),tb=tip.getBoundingClientRec
t();
  let x=tb.right+8,y=ref.top;
  if(x+224>window.innerWidth-8)x=tb.left-232;
  if(y+120>window.innerHeight-8)y=window.innerHeight-128;
  if(y<8)y=8;
  tipXp.style.left=x+"px";tipXp.style.top=y+"px";
});

/* Tooltip */
function showTip(e,d){
  if(rMi>0)return;
  if(tipFrozen||tipReady)return; /* tooltip frozen - skip update */
  const dd=CD[d.id];if(!dd){tip.classList.remove("vis");return;}
  const bars=["ms","gp","ec","oc","cs","ia"].map(k=>{
    const v=dd[I[k]],c=v>66?"#d4542e":v>33?"#b8a038":"#1a6b5a";
    return`<div class="tcr" data-xp="${k}"><span class="tl">${compLbl[k]}</sp
an><div class="tbc"><div class="tbb"><div class="tbf" style="width:${v}%;back
ground:${c}"></div></div><span class="tv" style="font-size:10px;min-width:22p
x">${v}</span></div></div>`;
  }).join("");
  const em=dd[I.hasEm]?`<div class="te" data-xp="pickups">&#10022; Emerald Ac
tive &#8212; ${fmt(dd[I.pickups])} est. pickups</div>`:"";
  const sc=cs(dd[I.score]);
  tip.innerHTML=
    `<div class="tn">${dd[I.name]}</div>${em}`+
    `<div class="tsc"><div><div class="tsl">Demand Index</div><div class="tsv
" style="color:${sc}">${dd[I.score].toFixed(1)}</div></div></div>`+
    `<div class="tt">${tierLbl[dd[I.tier]]}</div>`+
    `<div class="td"></div><div class="tsx">Components</div><div class="tc">$
{bars}</div>`+
    `<div class="td"></div><div class="tsx">Growth Signals</div>`+
    `<div class="tr" data-xp="permits"><span class="tl">Avg Annual Permits</s
pan><span class="tv">${fmt(dd[I.permits])} (${fmt(dd[I.sfp])} SF / ${fmt(dd[I
.mfp])} MF)</span></div>`+
    `<div class="tr" data-xp="popgr"><span class="tl">Population Growth</span
><span class="tv">${fP(dd[I.popgr])}</span></div>`+
    `<div class="tr" data-xp="hugr"><span class="tl">Housing Growth</span><sp
an class="tv">${fP(dd[I.hugr])}</span></div>`+
    `<div class="tr" data-xp="netmig"><span class="tl">Net Migration</span><s
pan class="tv">${dd[I.netmig].toFixed(1)}/1k pop</span></div>`+
    `<div class="td"></div><div class="tsx">Market</div>`+
    `<div class="tr" data-xp="mt"><span class="tl">Estimated Meters</span><sp

an class="tv">${fK(dd[I.mt])} (${fK(dd[I.resmt])} R / ${fK(dd[I.commt])} C /
${fK(dd[I.indmt])} I)</span></div>`+
    `<div class="tr" data-xp="pop"><span class="tl">Population</span><span cl
ass="tv">${fmt(dd[I.pop])}</span></div>`+
    `<div class="tr" data-xp="estabs"><span class="tl">Establishments</span><
span class="tv">${fmt(dd[I.estabs])}</span></div>`+
    `<div class="tr" data-xp="employment"><span class="tl">Employment</span><
span class="tv">${fmt(dd[I.employment])}</span></div>`+
    `<div class="td"></div><div class="tsx">Local Context</div>`+
    `<div class="tr" data-xp="inc"><span class="tl">Median Income</span><span
class="tv">$${fmt(dd[I.inc])}</span></div>`+
    `<div class="tr" data-xp="price"><span class="tl">Residential Elec Price<
/span><span class="tv">${dd[I.price]}&#162;/kWh</span></div>`+
    `<div class="tr" data-xp="nri"><span class="tl">NRI EAL ($)</span><span c
lass="tv">$${fK(dd[I.nri])}</span></div>`+
    `<div class="tr" data-xp="utilities"><span class="tl">Utilities</span><sp
an class="tv">${dd[I.nu]} (${dd[I.iou]} IOU / ${dd[I.coop]} Co-op / ${dd[I.mu
ni]} Muni / ${dd[I.other]} Other)</span></div>`+
    `<div class="tr" data-xp="yb"><span class="tl">Median Year Built</span><s
pan class="tv">${dd[I.yb]||"&#8212;"}</span></div>`+
    (()=>{
      const cu=CU[d.id]||[];if(!cu.length)return"";
      const rows=cu.slice(0,8).map(u=>{
        const [nm,oc,mt,pk,uid]=u;
        const clr=ownCol[oc]||"#7a8299";
        const pkStr=pk>0?` &nbsp;<span style="color:#6ddbab">${fK(pk)} pkup</
span>`:"";
        return`<div class="tr" data-uid="${uid}"><span class="tl" style="colo
r:${clr};min-width:34px">${oc}</span><span class="tv" style="font-size:10px">
${nm} &mdash; ${fK(mt)} mt${pkStr}</span></div>`;
      }).join("");
      const more=cu.length>8?`<div class="tr"><span class="tl" style="color:#
7a8299">+${cu.length-8} more</span></div>`:"";
      return`<div class="td"></div><div class="tsx">Utilities (${cu.length})<
/div>${rows}${more}`;
    })();
  tip.classList.add("vis");
  if(!tipReady)posTip(e); /* freeze tooltip position once dwell timer fires *
/
  /* dwell timer: stop moving and enable pointer-events after 750ms still */
  const dx=e.clientX-lastMX,dy=e.clientY-lastMY;
  if(dx*dx+dy*dy>64){clearTimeout(readyTimer);readyTimer=setTimeout(enableRea
dy,750);lastMX=e.clientX;lastMY=e.clientY;}
}

/* D3 map */
const svg=d3.select("#map");
const path=d3.geoPath();
const counties=topojson.feature(US,US.objects.counties);
const states=topojson.mesh(US,US.objects.states,(a,b)=>a!==b);

const cp=svg.append("g").selectAll("path").data(counties.features).join("path
")
  .attr("class","county").attr("d",path)
  .attr("fill",d=>{const dd=CD[d.id];return dd?cs(dd[I.score]):"#0e1118";})
  .on("mousemove",(e,d)=>{if(rMi>0)showRad(d.id,e);else showTip(e,d);})
  .on("mouseleave",()=>{
    if(rMi===0){
      clearTimeout(readyTimer);
      if(!tipReady)tip.classList.remove("vis");
      /* if tipReady the tooltip has pointer-events:auto - let tip.mouseleave
handle cleanup */
    }else clearRad();
  });

svg.append("path").datum(states).attr("class","sb").attr("d",path);
const rc=svg.append("circle").attr("class","rc").attr("r",0);

/* Centroids + mile calibration */
const cen={};
counties.features.forEach(f=>{const c=path.centroid(f);if(c&&isFinite(c[0])&&
isFinite(c[1]))cen[f.id]=c;});
const pLA=cen["06037"],pCH=cen["17031"];
let PXM=.326;
if(pLA&&pCH){const dx=pLA[0]-pCH[0],dy=pLA[1]-pCH[1];PXM=Math.sqrt(dx*dx+dy*d
y)/1745;}

const ownCol={"IOU":"#c67b3a","Coop":"#4a9977","Muni":"#5b8abf","Other":"#7a8
299"};

/* Radius tool */
function showRad(fips,e){
  const ct=d3.pointer(e,svg.node());
  const rPx=rMi*PXM;
  rc.attr("cx",ct[0]).attr("cy",ct[1]).attr("r",rPx).classed("vis",true);
  const rPx2=rPx*rPx,inR=[];
  for(const[id,[cx,cy]]of Object.entries(cen)){const dx=cx-ct[0],dy=cy-ct[1];
if(dx*dx+dy*dy<=rPx2)inR.push(id);}
  const inS=new Set(inR);
  cp.classed("inR",d=>inS.has(d.id));
  const m=inR.filter(f=>CD[f]).map(f=>CD[f]);
  const n=m.length;if(n===0){rp.classList.remove("vis");return;}
  const avg=(m.reduce((s,d)=>s+d[I.score],0)/n).toFixed(1);
  const totM=m.reduce((s,d)=>s+(d[I.mt]||0),0);
  const totP=m.reduce((s,d)=>s+(d[I.pop]||0),0);
  const totPm=m.reduce((s,d)=>s+(d[I.permits]||0),0);
  const t45=m.filter(d=>d[I.tier]>=4).length;
  const avgG=(m.reduce((s,d)=>s+d[I.popgr],0)/n).toFixed(1);
  const emC=m.filter(d=>d[I.hasEm]).length;
  const totPk=m.reduce((s,d)=>s+(d[I.pickups]||0),0);

  const nm=CD[fips]?CD[fips][I.name]:fips;
  rp.innerHTML=
    `<div class="rpt">${rMi*2} mi dia from ${nm}</div>`+
    `<div class="rps">`+
    `<div class="rpst"><div class="rpv">${n}</div><div class="rpl">Counties</
div></div>`+
    `<div class="rpst"><div class="rpv">${avg}</div><div class="rpl">Avg Scor
e</div></div>`+
    `<div class="rpst"><div class="rpv">${fK(totM)}</div><div class="rpl">Met
ers</div></div>`+
    `<div class="rpst"><div class="rpv">${fK(totP)}</div><div class="rpl">Pop
ulation</div></div>`+
    `<div class="rpst"><div class="rpv">${fK(totPm)}</div><div class="rpl">Pe
rmits</div></div>`+
    `<div class="rpst"><div class="rpv">${avgG}%</div><div class="rpl">Avg Po
p Growth</div></div>`+
    `<div class="rpst"><div class="rpv">${t45}</div><div class="rpl">Tier 4-5
</div></div>`+
    `<div class="rpst"><div class="rpv">${emC}</div><div class="rpl">Emerald<
/div></div>`+
    `<div class="rpst"><div class="rpv" style="color:#6ddbab">${fK(totPk)}</d
iv><div class="rpl">Est. Emerald Pickups 2024</div></div>`+
    `</div>`;
  rp.classList.add("vis");
  tip.classList.remove("vis");
}
function clearRad(){rc.classed("vis",false);cp.classed("inR",false);rp.classL
ist.remove("vis");}

/* Recolor */
function recolor(){cs=mkScale(ck);cp.attr("fill",d=>{const dd=CD[d.id];if(!dd
)return"#0e1118";return cs(gv(dd,ck));});}

/* Filter + Emerald overlay */
function apply(){
  cp.classed("dim",d=>{const dd=CD[d.id];if(!dd)return true;if(tier!=="all"&&
dd[I.tier]!==parseInt(tier))return true;return false;});
  cp.classed("emH",d=>{if(!showEm)return false;const dd=CD[d.id];return dd&&d
d[I.hasEm];});
  updStats();
}
function updStats(){
  const v=Object.entries(CD).filter(([,d])=>tier==="all"||d[I.tier]===parseIn
t(tier));
  const n=v.length;
  const avg=(v.reduce((s,[,d])=>s+d[I.score],0)/n).toFixed(1);
  const mt=v.reduce((s,[,d])=>s+(d[I.mt]||0),0);
  const pop=v.reduce((s,[,d])=>s+(d[I.pop]||0),0);
  const pm=v.reduce((s,[,d])=>s+(d[I.permits]||0),0);
  const em=v.filter(([,d])=>d[I.hasEm]).length;

  document.getElementById("stats").innerHTML=
    `<div>Counties: <span class="sv">${fmt(n)}</span></div>`+
    `<div>Avg Score: <span class="sv">${avg}</span></div>`+
    `<div>Total Meters: <span class="sv">${fK(mt)}</span></div>`+
    `<div>Total Pop: <span class="sv">${fK(pop)}</span></div>`+
    `<div>Total Permits: <span class="sv">${fK(pm)}</span></div>`+
    `<div>Emerald: <span class="sv">${em}</span></div>`;
}

/* Event listeners */
document.getElementById("colorBy").addEventListener("change",e=>{ck=e.target.
value;recolor();});
document.querySelectorAll(".fbtn").forEach(b=>{b.addEventListener("click",()=
>{
  document.querySelectorAll(".fbtn").forEach(x=>x.classList.remove("on"));
  b.classList.add("on");tier=b.dataset.t;apply();});});
document.querySelectorAll(".rbtn").forEach(b=>{b.addEventListener("click",()=
>{
  document.querySelectorAll(".rbtn").forEach(x=>x.classList.remove("on"));
  b.classList.add("on");rMi=parseInt(b.dataset.r);
  if(rMi===0){clearRad();document.getElementById("map").style.cursor="default
";}
  else document.getElementById("map").style.cursor="crosshair";});});
document.getElementById("emTog").addEventListener("click",function(){
  showEm=!showEm;this.classList.toggle("on");apply();});

apply();

/* County search / lookup */
const nameIdx=Object.entries(CD).map(([fips,d])=>({fips,name:d[I.name]}));
const srchBox=document.getElementById("srchBox");
const srchDrop=document.getElementById("srchDrop");
let srchSelIdx=-1,hlFips=null;

function srchFilter(q){
  const ql=q.toLowerCase().trim();
  if(!ql)return[];
  return nameIdx.filter(x=>x.name.toLowerCase().includes(ql)).slice(0,12);
}
function srchRender(results){
  srchSelIdx=-1;
  if(!results.length){srchDrop.classList.remove("vis");return;}
  srchDrop.innerHTML=results.map((r,i)=>
    `<div class="srch-item" data-fips="${r.fips}" data-i="${i}">${r.name}</di
v>`
  ).join("");
  srchDrop.classList.add("vis");
  srchDrop.querySelectorAll(".srch-item").forEach(el=>{
    el.addEventListener("mousedown",e=>{
      e.preventDefault();

      srchPick(el.dataset.fips,el.textContent);
    });
  });
}
function srchPick(fips,name){
  srchBox.value=name;
  srchDrop.classList.remove("vis");
  /* remove previous highlight */
  if(hlFips)cp.filter(d=>d.id===hlFips).classed("hl",false);
  hlFips=fips;
  cp.filter(d=>d.id===fips).classed("hl",true);
  /* show tooltip pinned to county centroid */
  const c=cen[fips];
  if(!c)return;
  const svgEl=svg.node();
  const pt=svgEl.createSVGPoint();
  pt.x=c[0];pt.y=c[1];
  const sc=pt.matrixTransform(svgEl.getScreenCTM());
  showTip({clientX:sc.x,clientY:sc.y},{id:fips});
  tip.classList.add("vis");
}

srchBox.addEventListener("input",()=>srchRender(srchFilter(srchBox.value)));
srchBox.addEventListener("keydown",e=>{
  const items=srchDrop.querySelectorAll(".srch-item");
  if(e.key==="ArrowDown"){
    srchSelIdx=Math.min(srchSelIdx+1,items.length-1);
    items.forEach((el,i)=>el.classList.toggle("ssel",i===srchSelIdx));
    e.preventDefault();
  } else if(e.key==="ArrowUp"){
    srchSelIdx=Math.max(srchSelIdx-1,0);
    items.forEach((el,i)=>el.classList.toggle("ssel",i===srchSelIdx));
    e.preventDefault();
  } else if(e.key==="Enter"){
    if(srchSelIdx>=0){
      const el=items[srchSelIdx];
      srchPick(el.dataset.fips,el.textContent);
    } else if(items.length===1){
      srchPick(items[0].dataset.fips,items[0].textContent);
    }
  } else if(e.key==="Escape"){
    srchDrop.classList.remove("vis");
    srchBox.blur();
  }
});
srchBox.addEventListener("focus",()=>{
  if(srchBox.value)srchRender(srchFilter(srchBox.value));
});
document.addEventListener("click",e=>{
  if(!e.target.closest(".srch-wrap")){

    srchDrop.classList.remove("vis");
  }
});
</script>
</body>
</html>')

writeLines(html, OUTPUT_MAP_HTML)
cat("Map written:", OUTPUT_MAP_HTML, "\n")
cat("Map counties:", nrow(map_data), "\n")
cat("File size:", round(file.size(OUTPUT_MAP_HTML) / 1024), "KB\n")
