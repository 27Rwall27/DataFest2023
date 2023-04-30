# Libraries ---------------------------------------------------------------
library(tidyverse)

# Global parameters -------------------------------------------------------
merge_data  <- TRUE
update_data <- TRUE
load_data   <- TRUE
run_test    <- FALSE

# Merging Questions & QuestionPosts ---------------------------------------
if (merge_data) {
  # Load raw data
  questions <- read_csv("datafest/data/questions.csv")
  posts <- read_csv("datafest/data/questionposts.csv")
  clients <- read_csv("datafest/data/clients.csv")
  states <- read_csv("datafest/data/statesites.csv")

  # Merge question metrics/identifiers (questions) and content (questionposts)
  x <- inner_join(questions, posts, by = "QuestionUno", multiple = "first")
  x <- x |> filter(PostText != "Thank you")
  names(x)[names(x) == "StateAbbr.x"] <- "StateAbbr"
  x <- x |> select(-StateAbbr.y)
  x <- x |> select(-c(starts_with("Id")))
  x <- x |> select(-c(SubcategoryUno,  CategoryUno))

  x <- x |> group_by(StateAbbr) |> mutate("n_cases_state"         = n_distinct(QuestionUno),
                                          "state_lawyer_workload" = n_cases_state / n_distinct(TakenByAttorneyUno),
                                          "is_answered"           = ((TakenOnUtc == "NULL") & (TakenByAttorneyUno == "NULL")))

  # Merging [Q + QP] with Clients -------------------------------------------
  clients$Circuit <- ifelse(clients$StateName %in% c("Maine", "Massachusetts", "New Hampshire", "Puerto Rico", "Rhode Island"), 1,
                            ifelse(clients$StateName %in% c("Connecticut", "New York", "Vermont"), 2,
                                   ifelse(clients$StateName %in% c("Delaware", "New Jersey", "Pennsylvania", "Virgin Islands"), 3,
                                          ifelse(clients$StateName %in% c("Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia"), 4,
                                                 ifelse(clients$StateName %in% c("District of the Canal Zone", "Louisiana", "Mississippi", "Texas"), 5,
                                                        ifelse(clients$StateName %in% c("Kentucky", "Michigan", "Ohio", "Tennessee"), 6,
                                                               ifelse(clients$StateName %in% c("Illinois", "Indiana", "Wisconsin"), 7,
                                                                      ifelse(clients$StateName %in% c("Arkansas", "Iowa", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota"), 8,
                                                                             ifelse(clients$StateName %in% c("Alaska", "Arizona", "California", "Idaho", "Montana", "Nevada", "Oregon", "Washington", "Guam", "Hawaii"), 9,
                                                                                    ifelse(clients$StateName %in% c("Colorado", "Kansas", "New Mexico", "Oklahoma", "Utah", "Wyoming"), 10,
                                                                                           ifelse(clients$StateName %in% c("Alabama", "Florida", "Georgia"), 11, 9999)))))))))))




  y <- inner_join(x, clients, by = c("AskedByClientUno" ="ClientUno"), multiple = "first")
  names(y)[names(y) == "StateAbbr.x"] <- "StateAbbr"
  y <- y |> select(-StateAbbr.y)
  rm(x, merge_data, questions, posts, clients)

  # TEST --------------------------------------------------------------------
  # Testing workload on county level, annualized, etc.
  y$year <- y$AskedOnUtc |> year()
  y$month <- y$AskedOnUtc |> month()
  y$Category <- ifelse(y$Category %in% c("Education", "Juvenile", "Family and Children"), "family", y$Category)
  y$Category[y$Category == "Work, Employment and Unemployment"] <- "work"
  y$Category[y$Category == "Consumer Financial Questions"] <- "consumer"
  y$Category[y$Category == "Individual Rights"] <- "rights"
  y$Category[y$Category == "Income Maintenance"] <- "income"
  y$Category[y$Category == "Health and Disability"] <- "health"
  y$Category[y$Category == "Housing and Homelessness"] <- "housing"
  y$Category[y$Category == "Other"] <- "other"

  # Workload by state by year
  y <- y |> group_by(StateAbbr, year) |>
    mutate("n_cases_state_yr"  = n_distinct(QuestionUno),
           "state_workload_yr" = n_cases_state_yr / n_distinct(TakenByAttorneyUno))

  # Workload by county across entire program
  y <- y |>
    group_by(StateAbbr, County) |>
    mutate("county_n_cases_overall"     = n_distinct(QuestionUno),
           "county_n_attorneys_overall" = n_distinct(TakenByAttorneyUno),
           "county_workload_overall"    = county_n_cases_overall / county_n_attorneys_overall)

  # Workload by county by year
  y <- y |>
    group_by(StateAbbr, County, year) |>
    mutate("county_n_cases_yr"     = n_distinct(QuestionUno),
           "county_n_attorneys_yr" = n_distinct(TakenByAttorneyUno),
           "county_workload_yr"    = county_n_cases_yr / county_n_attorneys_yr)

  # Workload by county by category by year
  y <- y |>
    group_by(StateAbbr, County, year, Category) |>
    mutate("county_cat_n_cases_yr"    = n_distinct(QuestionUno),
           "county_cat_n_attorneys_y" = n_distinct(TakenByAttorneyUno),
           "county_cat_workload_yr"   = county_cat_n_cases_yr / county_cat_n_attorneys_y)

  # Workload by county by category by month
  y <- y |>
    group_by(StateAbbr, County, month, Category) |>
    mutate("county_cat_n_cases_m"     = n_distinct(QuestionUno),
           "county_cat_n_attorneys_m" = n_distinct(TakenByAttorneyUno),
           "county_cat_workload_m"    = county_cat_n_cases_m / county_cat_n_attorneys_m)

  # Length of time (years) county has been in program
  county_exp <- y |> group_by(StateAbbr, County) |> summarise("county_exp" = min(year))
  y <- left_join(y, county_exp, by = join_by("StateAbbr" == "StateAbbr", "County" == "County"))
  y$county_exp <- y$year - y$county_exp

  # Merging [Q + QP + C] with States ----------------------------------------
  z <- inner_join(y, states, by = "StateAbbr", multiple = "first")
  z <- z |> select(-c(starts_with("Id")))
  names(z)[names(z) == "CreatedUtc.x"] <- "CreatedUtc"
  z <- z |> select(-CreatedUtc.y)
  names(z)[names(z) == "StateName.x"] <- "StateName"
  z <- z |> select(-StateName.y)

  # Add new vars
  z$during_covid <- z$year %in% c(2020, 2021)
  z$text_nchar <- nchar(z$PostText)

  # Merge with Arman income vars --------------------------------------------
  # x <- read_csv("new_var_data.csv", col_select = "prop")

  # Export data
  # write_csv(z |> select(-PostText), "final_data.csv")

  # Clean workspace ---------------------------------------------------------
  rm(y, states, county_exp)
}


# Load data ---------------------------------------------------------------
if (update_data) {
  if (!update_data) {
    x <- read_csv("final_data.csv")
  } else {
    x <- z
    rm(z)
  }

  # Case urgency
  x$time_to_deadline_wks <- ifelse(test = (x$LegalDeadline == "NULL"),
                                   yes  = NA,
                                   no   = (lubridate::as_date(x$LegalDeadline) - lubridate::as_date(x$AskedOnUtc)) / 7)
  x$is_urgent <- (x$LegalDeadline != "NULL") & (x$time_to_deadline_wks > 0)
  x <- x |> select(-time_to_deadline_wks)

  rm_cols <- c("QuestionUno", "Subcategory", "TakenByAttorneyUno",
               "ClosedByAttorneyUno", "LegalDeadline", "CreatedUtc", "StateName",
               "IncomeMultiplier", "PerHouseholdMemberIncomeLimit", "BaseIncomeLimit")
  x <- x |> select(-c(all_of(rm_cols)))
  rm(rm_cols)


  # Paige code --------------------------------------------------------------
  x$Gender <- ifelse(x$Gender %in% c("Female,Male", "Female,Male,Non-Conforming,Other", "Female,Non-Conforming", "Female,Other", "Male,Non-Conforming", "Male,Other", "Non-Conforming", "Non-Conforming,Other", "Other"), "Other", ifelse(x$Gender == "NULL", "I'd rather not answer", x$Gender))
  x$Age <- ifelse(x$Age <= 25, "25_or_under", ifelse((x$Age > 25) & (x$Age < 50), "25_to_50", ifelse((x$Age > 50) & (x$Age < 65), "50-65", ifelse(x$Age >= 65, "65+", NA))))
  x$MaritalStatus <- ifelse(x$MaritalStatus == "NULL", "I'd rather not answer", x$MaritalStatus)
  x$Imprisoned <- ifelse(x$Imprisoned == "NULL", NA, x$Imprisoned)
  x$Veteran <- ifelse(x$Veteran == "NULL", NA, x$Veteran)

  # Merge with Arman pov data -----------------------------------------------
  pov <- read_csv("./data_archive/final_data_pov.csv", col_select = poverty)$poverty |> as.numeric()

  # Export new --------------------------------------------------------------
  x <- data.frame(x, "is_poor" = pov)
  x$is_answered <- !x$is_answered
  # write_csv(x, "final_data.csv")

  # Clean workspace ---------------------------------------------------------
  rm(pov)
}


# Load data ---------------------------------------------------------------
if (load_data) {
  if (!update_data) {
    x <- read_csv("final_data.csv")
  }

  x$has_balance_info <- ifelse(((x$SavingsBalance != "NULL") | (x$CheckingBalance != "NULL") | (x$InvestmentsBalance != "NULL")), 1, 0)
  x <- x |> select(-c(SavingsBalance, CheckingBalance, InvestmentsBalance))
  names(x)[names(x) == "year"] <- "asked_year"
  names(x)[names(x) == "month"] <- "asked_month"

  x$MaritalStatus <- ifelse(x$MaritalStatus %in% c("Divorced", "Divorced or Widowed", "Widowed"), "Divorced or Widowed", x$MaritalStatus)
  x$MaritalStatus <- ifelse(x$MaritalStatus %in% c("Married / remarried", "Married"), "Married", x$MaritalStatus)
  x$Veteran[is.na(x$Veteran)] <- "Unknown"
  x$Veteran <- ifelse(x$Veteran %in% c("Unknown", "No"), "No or Unknown", "Yes")


  x1 <- read_csv("./data_archive/final_datav5.csv")
  x1$EthnicIdentity[x1$EthnicIdentity %in% c("Slavic", "African", "Arab", "East Indian", "Native Hawaiian / Pacific Islander")] <- "Other"
  x1$EthnicIdentity[x1$EthnicIdentity %in% c("Hispanic", "Latino")] <- "Hispanic or Latino"

  model_input <- data.frame("state"                      = factor(x$StateAbbr),
                            "county"                     = factor(x$County),
                            "question_cat"               = factor(x$Category),
                            "is_answered"                = factor(as.numeric(x$is_answered)),
                            "state_workload_overall"     = x$state_lawyer_workload,
                            "state_workload_yr"          = x$state_workload_yr,
                            "client_ethnicity"           = factor(x1$EthnicIdentity),
                            "client_age"                 = factor(x$Age),
                            "client_gender"              = factor(x$Gender), # Note: Female as baseline
                            "is_married"                 = factor(x$MaritalStatus),
                            "is_veteran"                 = factor(x$Veteran),
                            "is_during_covid"            = factor(as.numeric(x$during_covid)),
                            "client_household_size"      = as.numeric(x$NumberInHousehold),
                            "client_income"              = as.numeric(x$AnnualIncome),
                            "circuit"                    = factor(as.numeric(x$Circuit)),
                            "asked_year"                 = factor(x$asked_year),
                            "asked_month"                = factor(x$asked_month),
                            "question_nchar"             = as.numeric(x$text_nchar),
                            "is_urgent"                  = factor(as.numeric(x$is_urgent)),
                            "is_poor"                    = factor(as.numeric(x$is_poor)),
                            "has_balance_info"           = factor(as.numeric(x$has_balance_info)),
                            "county_n_cases_overall"     = as.numeric(x$county_n_cases_overall),
                            "county_n_attorneys_overall" = as.numeric(x$county_n_attorneys_overall),
                            "county_workload_overall"    = as.numeric(x$county_workload_overall),
                            "county_n_cases_yr"          = as.numeric(x$county_n_cases_yr),
                            "county_n_attorneys_yr"      = as.numeric(x$county_n_attorneys_yr),
                            "county_workload_yr"         = as.numeric(x$county_workload_yr),
                            "county_cat_n_cases_yr"      = as.numeric(x$county_cat_n_cases_yr),
                            "county_cat_n_attorneys_yr"  = as.numeric(x$county_cat_n_attorneys_y),
                            "county_cat_workload_yr"     = as.numeric(x$county_cat_workload_yr),
                            "county_cat_n_cases_m"       = as.numeric(x$county_cat_n_cases_m),
                            "county_cat_n_attorneys_m"   = as.numeric(x$county_cat_n_attorneys_m),
                            "county_cat_workload_m"      = as.numeric(x$county_cat_workload_m)
  )

  model_input <- na.omit(model_input)
  rm(x, load_data, update_data, x1)

  # Add Arman income vars ---------------------------------------------------
  client_rel_income <- read_csv("new_var_data.csv", col_select = prop)
  model_input <- data.frame(model_input, "client_relative_income" = client_rel_income)
  write_csv(model_input, "model_input.csv")
  rm(client_rel_income)
}