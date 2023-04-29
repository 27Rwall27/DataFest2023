# DataFest2023


## Definition of cases_not_taken
- R code: x <- questions[(questions$TakenOnUtc == "NULL" & questions$TakenByAttorneyUno == "NULL"), ]

## Code for the Clients column in clients.csv 

clients$Circuit <- ifelse(clients$StateName %in% c("Maine", "Massachusetts", "New Hampshire", "Puerto Rico", "Rhode Island"), "First Circuit", ifelse(clients$StateName %in% c("Connecticut", "New York", "Vermont"), "Second Circuit", ifelse(clients$StateName %in% c("Delaware", "New Jersey", "Pennsylvania", "Virgin Islands"), "Third Circuit", ifelse(clients$StateName %in% c("Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia"), "Fourth Circuit", ifelse(clients$StateName %in% c("District of the Canal Zone", "Louisiana", "Mississippi", "Texas"), "Fifth Circuit", ifelse(clients$StateName %in% c("Kentucky", "Michigan", "Ohio", "Tennessee"), "Sixth Circuit", ifelse(clients$StateName %in% c("Illinois", "Indiana", "Wisconsin"), "Seventh Circuit", ifelse(clients$StateName %in% c("Arkansas", "Iowa", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota"), "Eighth Circuit", ifelse(clients$StateName %in% c("Alaska", "Arizona", "California", "Idaho", "Montana",  "Nevada", "Oregon", "Washington", "Guam", "Hawaii"), "Ninth Circuit", ifelse(clients$StateName %in% c("Colorado", "Kansas", "New Mexico", "Oklahoma", "Utah", "Wyoming"), "Tenth Circuit", ifelse(clients$StateName %in% c("Alabama", "Florida", "Georgia"), "Eleventh Circuit", "ABA Federal")))))))))))
