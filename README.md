# DataFest2023


## Definition of cases_not_taken
- R code: x <- questions[(questions$TakenOnUtc == "NULL" & questions$TakenByAttorneyUno == "NULL"), ]

## Definition of X1_not_text
- Result of merging questions.csv and questionposts.csv files by unique QuetionsUno's in questionposts.csv
- R code: x <- inner_join(questions, posts, by = "QuestionUno", multiple = "first")

## Definition of X2_final_data
- Results of merging X1_not_text.csv with clients.csv and states.csv data
- Does not include text of questions (too large)

## Code for creating circuits

clients$Circuit <- ifelse(clients$StateName %in% c("Maine", "Massachusetts", "New Hampshire", "Puerto Rico", "Rhode Island"), "First Circuit", ifelse(clients$StateName %in% c("Connecticut", "New York", "Vermont"), "Second Circuit", ifelse(clients$StateName %in% c("Delaware", "New Jersey", "Pennsylvania", "Virgin Islands"), "Third Circuit", ifelse(clients$StateName %in% c("Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia"), "Fourth Circuit", ifelse(clients$StateName %in% c("District of the Canal Zone", "Louisiana", "Mississippi", "Texas"), "Fifth Circuit", ifelse(clients$StateName %in% c("Kentucky", "Michigan", "Ohio", "Tennessee"), "Sixth Circuit", ifelse(clients$StateName %in% c("Illinois", "Indiana", "Wisconsin"), "Seventh Circuit", ifelse(clients$StateName %in% c("Arkansas", "Iowa", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota"), "Eighth Circuit", ifelse(clients$StateName %in% c("Alaska", "Arizona", "California", "Idaho", "Montana",  "Nevada", "Oregon", "Washington", "Guam", "Hawaii"), "Ninth Circuit", ifelse(clients$StateName %in% c("Colorado", "Kansas", "New Mexico", "Oklahoma", "Utah", "Wyoming"), "Tenth Circuit", ifelse(clients$StateName %in% c("Alabama", "Florida", "Georgia"), "Eleventh Circuit", "ABA Federal")))))))))))

cases_not_taken$Circuit <- ifelse(cases_not_taken$StateAbbr %in% c("ME", "MA", "NH", "RI"), "First Circuit", ifelse(cases_not_taken$StateAbbr %in% c("CT", "NY", "VT"), "Second Circuit", ifelse(cases_not_taken$StateAbbr %in% c("DE", "NJ", "PA"), "Third Circuit", ifelse(cases_not_taken$StateAbbr %in% c("MD", "NC", "SC", "VA", "WV"), "Fourth Circuit", ifelse(cases_not_taken$StateAbbr %in% c("LA", "MS", "TX"), "Fifth Circuit", ifelse(cases_not_taken$StateAbbr %in% c("KY", "MI", "OH", "TN"), "Sixth Circuit", ifelse(cases_not_taken$StateAbbr %in% c("IL", "IN", "WI"), "Seventh Circuit", ifelse(cases_not_taken$StateAbbr %in% c("AR", "IA", "MN", "MO", "NE", "ND", "SD"), "Eighth Circuit", ifelse(cases_not_taken$StateAbbr %in% c("AK", "AZ", "CA", "ID", "MT", "NV", "OR", "WA", "HI"), "Ninth Circuit", ifelse(cases_not_taken$StateAbbr %in% c("CO", "KS", "NM", "OK", "UT", "WY"), "Tenth Circuit", ifelse(cases_not_taken$StateAbbr %in% c("AL", "FL", "GA"), "Eleventh Circuit", NA)))))))))))

## Code for cleaning the gender column

final_data$Gender <- ifelse(final_data$Gender %in% c("Female,Male", "Female,Male,Non-Conforming,Other", "Female,Non-Conforming", "Female,Other", "I'd rather not answer", "Male,Non-Conforming", "Male,Other", "Non-Conforming", "Non-Conforming,Other", "Other"), "Other", final_data$Gender)
