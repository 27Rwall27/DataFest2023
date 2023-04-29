# DataFest2023

## Definition of cases_not_taken
- R code: x <- questions[(questions$TakenOnUtc == "NULL" & questions$TakenByAttorneyUno == "NULL"), ]

## Code for creating circuits

clients$Circuit <- ifelse(clients$StateName %in% c("Maine", "Massachusetts", "New Hampshire", "Puerto Rico", "Rhode Island"), "First Circuit", ifelse(clients$StateName %in% c("Connecticut", "New York", "Vermont"), "Second Circuit", ifelse(clients$StateName %in% c("Delaware", "New Jersey", "Pennsylvania", "Virgin Islands"), "Third Circuit", ifelse(clients$StateName %in% c("Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia"), "Fourth Circuit", ifelse(clients$StateName %in% c("District of the Canal Zone", "Louisiana", "Mississippi", "Texas"), "Fifth Circuit", ifelse(clients$StateName %in% c("Kentucky", "Michigan", "Ohio", "Tennessee"), "Sixth Circuit", ifelse(clients$StateName %in% c("Illinois", "Indiana", "Wisconsin"), "Seventh Circuit", ifelse(clients$StateName %in% c("Arkansas", "Iowa", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota"), "Eighth Circuit", ifelse(clients$StateName %in% c("Alaska", "Arizona", "California", "Idaho", "Montana",  "Nevada", "Oregon", "Washington", "Guam", "Hawaii"), "Ninth Circuit", ifelse(clients$StateName %in% c("Colorado", "Kansas", "New Mexico", "Oklahoma", "Utah", "Wyoming"), "Tenth Circuit", ifelse(clients$StateName %in% c("Alabama", "Florida", "Georgia"), "Eleventh Circuit", "ABA Federal")))))))))))

cases_not_taken$Circuit <- ifelse(cases_not_taken$StateAbbr %in% c("ME", "MA", "NH", "RI"), "First Circuit", ifelse(cases_not_taken$StateAbbr %in% c("CT", "NY", "VT"), "Second Circuit", ifelse(cases_not_taken$StateAbbr %in% c("DE", "NJ", "PA"), "Third Circuit", ifelse(cases_not_taken$StateAbbr %in% c("MD", "NC", "SC", "VA", "WV"), "Fourth Circuit", ifelse(cases_not_taken$StateAbbr %in% c("LA", "MS", "TX"), "Fifth Circuit", ifelse(cases_not_taken$StateAbbr %in% c("KY", "MI", "OH", "TN"), "Sixth Circuit", ifelse(cases_not_taken$StateAbbr %in% c("IL", "IN", "WI"), "Seventh Circuit", ifelse(cases_not_taken$StateAbbr %in% c("AR", "IA", "MN", "MO", "NE", "ND", "SD"), "Eighth Circuit", ifelse(cases_not_taken$StateAbbr %in% c("AK", "AZ", "CA", "ID", "MT", "NV", "OR", "WA", "HI"), "Ninth Circuit", ifelse(cases_not_taken$StateAbbr %in% c("CO", "KS", "NM", "OK", "UT", "WY"), "Tenth Circuit", ifelse(cases_not_taken$StateAbbr %in% c("AL", "FL", "GA"), "Eleventh Circuit", NA)))))))))))

## Code for cleaning the gender column

final_data$Gender <- ifelse(final_data$Gender %in% c("Female,Male", "Female,Male,Non-Conforming,Other", "Female,Non-Conforming", "Female,Other", "Male,Non-Conforming", "Male,Other", "Non-Conforming", "Non-Conforming,Other", "Other"), "Other", ifelse(final_data$Gender == "NULL", "I'd rather not answer", final_data$Gender))

## Code for cleaning the age column

final_data$Age <- ifelse(final_data$Age <= 25, "25_or_under", ifelse((final_data$Age > 25) & (final_data$Age < 50), "25_to_50", ifelse((final_data$Age > 50) & (final_data$Age < 65), "50-65", ifelse(final_data$Age >= 65, "65+", NA))))

## Cleaning the marital status column

final_data$MaritalStatus <- ifelse(final_data$MaritalStatus == "NULL", "I'd rather not answer", final_data$MaritalStatus)

## Cleaning the imprisoned column

final_data$Imprisoned <- ifelse(final_data$Imprisoned == "NULL", NA, final_data$Imprisoned)

## Cleaning the veteran column

final_data$Veteran <- ifelse(final_data$Veteran == "NULL", NA, final_data$Veteran)

## Best logistic regression model so far (accuracy: 0.7489328)

set.seed(1)

sample <- sample(c(TRUE, FALSE), nrow(final_data), replace = TRUE, prob = c(1 - 0.33, 0.33))

train <- final_data[sample, ]

test <- final_data[!sample, ]

model <- glm(formula = is_answered ~ text_nchar + during_covid + state_lawyer_workload + MaritalStatus + Age + Circuit + Category, family = binomial, data = train)

predictions <- predict(model, newdata = test, type = "response")

predictions <- ifelse(predictions > 0.5, "TRUE", "FALSE")

mean(predictions == test$is_answered, na.rm = TRUE)


## Code for adding the poverty variable 

final_data <- read.csv("final_data.csv")
final_data$AnnualIncome <- as.numeric(final_data$AnnualIncome)
final_data[is.na(final_data$AnnualIncome), ] <- 0
poverty <- rep(FALSE, nrow(final_data))
final_data_pov <- cbind(final_data, poverty)
final_data_pov[final_data_pov$AnnualIncome < 14891, ]$poverty <- TRUE
write.csv(final_data_pov, "final_data_pov.csv")


## Code for defining vars as cat or num vars
x <- read_csv("model_input.csv", show_col_types = FALSE)

x <- data.frame("state"                  = factor(x$state),
                "county"                 = factor(x$county),
                "question_cat"           = factor(x$question_cat),
                "is_answered"            = factor(as.numeric(x$is_answered)),
                "state_workload_overall" = as.numeric(x$state_workload_overall),
                "client_ethnicity"       = factor(x$client_ethnicity),
                "client_age"             = factor(x$client_age),
                "client_gender"          = factor(x$client_gender), # Note: Female as baseline
                "is_married"             = factor(x$is_married),
                "is_veteran"             = factor(x$is_veteran),
                "is_during_covid"        = factor(as.numeric(x$is_during_covid)),
                "client_household_size"  = as.numeric(x$client_household_size),
                "client_income"          = as.numeric(x$client_income),
                "circuit"                = factor(as.numeric(x$circuit)),
                "asked_year"             = factor(x$asked_year),
                "asked_month"            = factor(x$asked_month),
                "question_nchar"         = as.numeric(x$question_nchar),
                "is_urgent"              = factor(as.numeric(x$is_urgent)),
                "is_poor"                = factor(as.numeric(x$is_poor)),
                "has_balance_info"       = factor(as.numeric(x$has_balance_info)))



