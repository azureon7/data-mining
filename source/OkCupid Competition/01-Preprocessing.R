#=================
# 1. PREPROCESSING 
#=================

rm(list=ls())

# READ FUNCTIONS
source("source/OkCupid_Functions.R")

# READ CSV
train <- read.csv('./raw_datasets/101.csv', stringsAsFactors = F)
test <- read.csv('./raw_datasets/102.csv', stringsAsFactors = F)

# TOLGO VARIABILI INUTILI E PREPARO MYDATA (TRAIN E TEST UNITI)
train[,c(21:28)] <- test[,c(21:28)] <- NULL
test$Class <- NA
D <- rbind(train,test)

#------------------------
# 1.A FEATURE ENGINEERING
# offspring aggregata
D$offspring_A <- D$offspring 
D$offspring_A[which(D$offspring == "doesnt_have_kids_and_doesnt_want_any" | D$offspring == "doesnt_want_kids")] <- "doesnt_want"
D$offspring_A[which(D$offspring == "doesnt_have_kids_but_wants_them" | D$offspring == "doesnt_have_kids_but_might_want_them" | D$offspring == "might_want_kids" | D$offspring == "wants_kids" )] <- "wants"
D$offspring_A[which(D$offspring == "has_kids_and_wants_more" | D$offspring == "has_a_kid_and_wants_more" | D$offspring == "has_kids_and_might_want_more" | D$offspring == "has_a_kid_and_might_want_more")] <- "another"
D$offspring_A[which(D$offspring == "has_kids" | D$offspring == "has_kids_but_doesnt_want_more" | D$offspring == "has_a_kid_but_doesnt_want_more" | D$offspring == "has_a_kid")] <- "has_kids"

# pets aggregata
D$pets_A <- D$pets 
D$pets_A[which(grepl("dislikes", D$pets_A))] <- "no_animals"
D$pets_A[which(grepl("has", D$pets_A))] <- "has_animals"
D$pets_A[which(grepl("like", D$pets_A))] <- "likes_animals"

# atheist
D$atheist <- ifelse(D$religion == "atheism", "yes", "no")

# smoke
D$smoke <- ifelse(D$smokes == "yes", "yes", "altro")

# single
D$single <- ifelse(D$status == "single", "yes", "altro")

# male ricodifica
D$male <- ifelse(D$male == 1, "m", "f")

# town aggregata
D$town_A <- D$where_town
D$town_A[which(D$town_A == "palo_alto" | D$town_A == "mountain_view" | D$town_A == "san_mateo" | D$town_A == "redwood_city" | D$town_A == "belmont" | D$town_A == "stanford" | D$town_A == "menlo_park")] <- "SV"
D$town_A[which(D$town_A == "alameda" | D$town_A == "albany" | D$town_A == "pacifica" | D$town_A == "san_bruno" | D$town_A == "sausalito" | D$town_A == "south_san_francisco")] <- "high"
D$town_A[which(D$town_A == "daly_city" | D$town_A == "emeryville" | D$town_A == "berkeley" | D$town_A == "hayward" | D$town_A == "larkspur" | D$town_A == "novato" | D$town_A == "oakland" | D$town_A == "richmond" |D$town_A == "san_carlos" |D$town_A == "san_leandro" |D$town_A == "san_lorenzo"| D$town_A == "vallejo")] <- "medium"
D$town_A[which(D$town_A == "benicia" | D$town_A == "burlingame" | D$town_A == "castro_valley" | D$town_A == "el_cerrito" | D$town_A == "hercules" | D$town_A == "martinez" | D$town_A == "mill_valley"  | D$town_A == "other"  | D$town_A == "pleasant_hill"  | D$town_A == "san_pablo" | D$town_A == "san_rafael" | D$town_A == "walnut_creek" | D$town_A == "corte_madera" | D$town_A == "moraga" | D$town_A == "lafayette" | D$town_A == "millbrae" | D$town_A == "orinda" | D$town_A == "pinole" | D$town_A == "san_anselmo" | D$town_A == "fairfax"| D$town_A == "fremont"| D$town_A == "green_brae"| D$town_A == "half_moon_bay"| D$town_A == "el_sobrante"  )] <- "low"

# silicon valley
D$SV <- ifelse(D$town_A == "SV", "yes", "no")

# religion importance ricodificata
D$rel_imp <- D$religion_modifer
D$rel_imp[which(D$religion_modifer == "religion_mod_missing")] <- "missing"
D$rel_imp[which(D$religion_modifer == "and_laughing_about_it")] <- "0"
D$rel_imp[which(D$religion_modifer == "but_not_too_serious_about_it")] <- "1"
D$rel_imp[which(D$religion_modifer == "and_somewhat_serious_about_it")] <- "2"
D$rel_imp[which(D$religion_modifer == "and_very_serious_about_it")] <- "3"

# sign importance ricodificata
D$sign_imp <- D$sign_modifer
D$sign_imp[which(D$sign_modifer == "sign_mod_missing")] <- "missing"
D$sign_imp[which(D$sign_modifer == "but_it_doesnt_matter")] <- "0"
D$sign_imp[which(D$sign_modifer == "and_its_fun_to_think_about")] <- "1"
D$sign_imp[which(D$sign_modifer == "and_it_matters_a_lot")] <- "2"

# età aggregata
D$age_A <- D$age
D$age_A[which(D$age_A < 21)] <- "18-21"
D$age_A[which(D$age_A >= 40 & D$age_A <= 44)] <- "40-44"
D$age_A[which(D$age_A >= 45 & D$age_A <= 49)] <- "45-49"
D$age_A[which(D$age_A >= 50)] <- "50+"

D$age_dummy='no'
D$age_dummy[which(D$age_A >= 25 & D$age_A <= 40)] <- "yes"

# body type aggregata
D$body_type_A <- D$body_type
D$body_type_A[which(D$body_type_A == "curvy" | D$body_type_A == "full_figured"
                         | D$body_type_A == "overweight" | D$body_type_A == "used_up")] <- "over_size"
D$body_type_A[which(D$body_type_A == "athletic" | D$body_type_A == "jacked")] <- "athletic"
D$body_type_A[which(D$body_type_A == "body_type_missing" | D$body_type_A == "rather_not_say")] <- "body_type_missing"
D$body_type_A[which(D$body_type_A == "skinny" | D$body_type_A == "thin")] <- "thin"

# diet aggregata
D$diet_A <- D$diet
D$diet_A[which(grepl('vegan', D$diet_A))] <- "vegan"
D$diet_A[which(grepl('vegetarian', D$diet_A))] <- "vegetarian"
D$diet_A[which(grepl('kosher', D$diet_A) | grepl('mostly_halal', D$diet_A) | grepl('other', D$diet_A))] <- "other"
D$diet_A[which(grepl('other', D$diet_A))] <- "other"
D$diet_A[which(grepl('anything', D$diet_A))] <- "anything"

# drinks aggregata
D$drinks_A <- D$drinks
D$drinks_A[which(D$drinks_A == "desperately" | D$drinks_A == "very_often")] <- "very_often"

# education aggregata 
D$education_A <- D$education
D$education_A[which(D$education_A == "college_university" | D$education_A == "graduated_from_college_university")] <- "university"
D$education_A[which(grepl('dropped_out', D$education_A))] <- "dropped"
D$education_A[which(D$education == "dropped_out_of_college_university")] <- "dropped_university"
D$education_A[which(D$education_A == "graduated_from_high_school" | D$education_A == "graduated_from_law_school"
                         | D$education_A == "graduated_from_med_school" | D$education_A == "working_on_two_year_college"
                         | D$education_A == "working_on_college_university" | D$education_A == "two_year_college"
                         | D$education_A == "space_camp" | D$education_A == "law_school" 
                         | D$education_A == "high_school" )] <- "other"
D$education_A[which(D$education_A == "working_on_space_camp" | D$education_A == "working_on_med_school"
                         | D$education_A == "working_on_law_school" | D$education_A == "working_on_high_school"
                         | D$education_A == "graduated_from_two_year_college")] <- "work_other"
D$education_A[which(D$education_A == "graduated_from_masters_program" | D$education_A == "masters_program" | D$education_A == "graduated_from_space_camp")] <- "masters_program"
D$education_A[which(D$education_A == "graduated_from_ph_d_program" | D$education_A == "ph_d_program")] <- "phd_program"
D$education_A[which(D$education_A == "working_on_masters_program" | D$education_A == "working_on_ph_d_program")] <- "work_phd_mast"

# dummy dottorato
D$phdYN <- "no"
D$phdYN[which(D$education_A == "phd_program")] <- "yes"

# dummy education
D$educ_dummy1 <- "yes"
D$educ_dummy1[which(D$education_A!="ed_missing"&D$education_A!="other"&
                     D$education_A!="work_phd"&D$education_A!="work_phd_mast")] <- "no"
D$educ_dummy2 <- "no"
D$educ_dummy2[which(D$education_A=="masters_program"|D$education_A=="university"|
                      D$education_A=="work_other")] <- "yes"

# altezza
D$height_A <- D$height
D$height_A[which(D$height_A <= 61)] <- "61-"
D$height_A[which(D$height_A >= 75)] <- "75+"

# income 100.000 dummy
D$income100000 <- "no"
D$income100000[which(D$income == "inc100000")] <- "yes"

# variabili unioni delle dummy
D$UnionDummy <-  col.union(df = D[,c("tech","computer","SV")])

#---------------------
# 1.B VARIABILI FACTOR
# status,male,smokes,class,+tutte le 0/1 finali
var1 = get_positions(D,c('male','Class', colnames(D)[24:length(D)]))
D[,var1] = lapply(D[,var1], as.factor)
for(i in 24:83){
  levels(D[,i]) <- c("no", "yes")  
}
D$essay_link = as.factor(D$essay_link)
levels(D[,'essay_link']) <- c("no", "yes")  

#-----------------------------------------------
# 1.C CREO DATASET FINALE CON VARIABILI 'PRONTE'
var2=get_positions(D, c('essay_link', 'essay_length'))
var2=sort(c(var1,var2))
d = D[1:4000, var2]
d_test = D[4001:5000, var2]

# WRITE CSV
write.csv(d, './new_datasets/data.csv', row.names = F)
write.csv(d_test, './new_datasets/data_test.csv', row.names = F)
write_summary(d, './new_datasets/summary.csv')
