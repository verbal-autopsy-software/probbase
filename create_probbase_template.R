#------------------------------------------------------------------------------#
#
# create_probbase_template.R
#
# Identify symptoms added & dropped in the new WHO 2022 VA instrument.
# 
#------------------------------------------------------------------------------#

library(readxl)
library(dplyr)

#------------------------------------------------------------------------------#
# Download input files (probbase, mapping, and questionnaires)                 #
#------------------------------------------------------------------------------#

## probbase and mapping
probbase_url <- "https://github.com/verbal-autopsy-software/probbase/raw/main/probbase.xls"
download.file(probbase_url, destfile = "probbase.xls")
probbaseV5 <- read_excel("probbase.xls")
probbaseV5 <- as.matrix(probbaseV5)
probbaseV5[2, 3]

pva151_url <- "https://raw.githubusercontent.com/verbal-autopsy-software/pyCrossVA/master/pycrossva/resources/mapping_configuration_files/2016WHOv151_to_InterVA5.csv"
pva151_mapping_name <- "2016WHOv151_to_InterVA5.csv"
download.file(pva151_url, destfile = pva151_mapping_name)
pva151 <- read.csv(pva151_mapping_name)
pva151 <- lapply(pva151, function(x) if(is.character(x)){gsub("\n", " ", x)} else{x})
pva151 <- as.data.frame(pva151)
names(pva151)

## 2016 WHO VA Questionnaire v1.5.1
who2016_url <- "https://github.com/SwissTPH/WHO-VA/releases/download/1.5%2C1/WHO_VA_2016_V1.5.1.zip"
who2016_zip_name <- "WHO_VA_2016_V1.5.1.zip"
download.file(who2016_url, destfile = who2016_zip_name)
unzip(who2016_zip_name)
who2016_form_name <- "WHOVA2016_v1_5_1_XLS_form_for_ODK.xlsx"
who2016 <- read_excel(who2016_form_name, sheet = "survey")
who2016 <- lapply(who2016, function(x) if(is.character(x)){gsub("\n", " ", x)} else{x})
who2016 <- as.data.frame(who2016)
names(who2016)

table(who2016$type, useNA = "always")

exclude_type <- c("begin group", "end group", "start",
                  "today", "note", "text", "end", NA)

clean_who2016 <- subset(who2016, !(type %in% exclude_type))
dim(clean_who2016)

output_who2016 <- clean_who2016[, c("name", "label..English")]

# 2022 WHO VA Questionnaire v2022033001
who2022_url <- "https://github.com/SwissTPH/WHO-VA/releases/download/2022.1/WHOVA2022_XLS_form_for_ODK.xlsx"
who2022_form_name <- "WHOVA2022_XLS_form_for_ODK.xlsx"

download.file(who2022_url, destfile = who2022_form_name)
who2022 <- read_excel(who2022_form_name, sheet = "survey")
who2022 <- lapply(who2022, function(x) if(is.character(x)){gsub("\n", " ", x)} else{x})
who2022 <- as.data.frame(who2022)
names(who2022)

table(who2022$type, useNA = "always")

exclude_type <- c("audio", "audit", "begin group", "end", "end group",
                  "note", "start", "text", "today", "trigger")

clean_who2022 <- subset(who2022, !(type %in% exclude_type))
dim(clean_who2022)

output_who2022 <- clean_who2022[, c("name", "label..English..en.")]

#------------------------------------------------------------------------------#
# Build new probbase template (create new rows & columns)                      #
#------------------------------------------------------------------------------#

new_probbase <- probbaseV5

## add column to identify probbase rows that lost support in 2022 instrument
orig_col_names <- colnames(new_probbase)
orig_col_names
new_probbase <- cbind(new_probbase[, 1:3], "", new_probbase[, 4:ncol(new_probbase)])
colnames(new_probbase) <- c(orig_col_names[1:3],
                            "status_in_2022",
                            orig_col_names[4:length(orig_col_names)])

no_pb2020 <- !(tolower(pva151$Source.Column.ID) %in% tolower(output_who2022$name))
cbind(pva151[, c(1, 3)], no_pb2020)
table(new_probbase[, 1] %in% pva151[no_pb2020, 1])

dropped <- new_probbase[, 1] %in% pva151[no_pb2020, 1]
table(dropped)
length(dropped) == nrow(new_probbase)
colnames(new_probbase)
colnames(new_probbase)[4]
new_probbase[dropped, 4] <- "dropped"

## rename column who_2016 <- who_2022
colnames(new_probbase)[(colnames(new_probbase) == "who_2016")] <- "who_2022"

## add in COVID-19 column
index_col_covid <- which(colnames(new_probbase) == "b_0112")
n_rows_fill <- nrow(new_probbase) - 1
new_probbase <- cbind(new_probbase[, 1:index_col_covid],
                      c("01.13 Coronavirus disease (COVID-19)", rep("", n_rows_fill)),
                      new_probbase[, (index_col_covid + 1):ncol(new_probbase)])
colnames(new_probbase)[colnames(new_probbase) == ""] = "b_0113"

## Add new probbase rows for new 2022 instrument questions
table(output_who2022$name %in% output_who2016$name)
#### create new indic qdesc sdesc and who
new_pb_items <- output_who2022[!(output_who2022$name %in% output_who2016$name), ]
new_pb_items
new_pb_items <- new_pb_items %>%
    filter(grepl("^Id", name)) %>%
    filter(!grepl("check", name))
#### remove non-symptoms
## 1       Id10010a
## 2       Id10010b
## 3       Id10007a
## 4       Id10007b
new_pb_items[, "label..English..en."]
new_pb_items <- new_pb_items %>%
    slice(5:n())
colnames(new_probbase)
new_pb_indic <- sub("_1", "a", new_pb_items$name)
new_pb_indic <- sub("_", "", new_pb_indic)
new_pb_indic[nchar(new_pb_indic) == 7] <- paste0(new_pb_indic[nchar(new_pb_indic) == 7], "o")
new_pb_indic <- substr(new_pb_indic, nchar(new_pb_indic) - 3, nchar(new_pb_indic))
new_pb_indic <- paste0("i", new_pb_indic)

## new_pb_qdesc <- sub("^\\(.*\\) ", "", new_pb_items[, "label..English..en."])
new_pb_qdesc <- new_pb_items[, "label..English..en."]

new_pb_who <- paste0("W610",
                     substr(new_pb_indic, 2, 4),
                     "-",
                     substr(new_pb_indic, 5, 5))
new_rows <- cbind(new_pb_indic, trimws(new_pb_qdesc),
                  "", "added", # sdesc & status_in_2022
                  new_pb_who, 
                  matrix("", nrow=length(new_pb_indic), ncol = ncol(new_probbase) - 5))
colnames(new_rows) <- colnames(new_probbase)
dim(new_rows)
dim(new_probbase)
new_probbase <- rbind(new_probbase, new_rows)

## Convert new 2022 questions into probbase indicators

#### COVID
###### old: "(Id10483) Did s(h)e have a recent test for COVID-19?"
###### new: "(Id10483) Did s(h)e have a recent postive test result for COVID-19?"
###### remove: "(Id10484) What was the result?"
grep("\\(Id10483\\) Did s\\(h\\)e have a recent test for COVID\\-19\\?",
     new_probbase[, 2])
tmp_index <- grep("\\(Id10483\\) Did s\\(h\\)e have a recent test for COVID\\-19\\?",
                  new_probbase[, 2])
new_probbase[tmp_index, 2]
new_probbase[tmp_index, 2] <- "(Id10483) Did s(h)e have a recent positive test for COVID-19?"
grep("\\(Id10484\\) What was the result\\?", new_probbase[, 2])
tmp_index <- grep("\\(Id10484\\) What was the result\\?", new_probbase[, 2])
new_probbase[tmp_index, 2]
dim(new_probbase)
new_probbase <- new_probbase[-tmp_index, ]
dim(new_probbase)

#### old: "(Id10077_a) How long after the injury or accident did s/he die?"
#### new: "(Id10077_a) Did s(h)e die within 7 days of the injury or accident?"
grep("\\(Id10077_a\\) How long after the injury or accident did s/he die\\?",
     new_probbase[, 2])
tmp_index <- grep("\\(Id10077_a\\) How long after the injury or accident did s/he die\\?",
                  new_probbase[, 2])
new_probbase[tmp_index, 2]
new_probbase[tmp_index, 2] <- "(Id10077_a) Did s(h)e die within 7 days of the injury or accident?"
new_probbase[tmp_index, 2]

#### Smoke Tobacco
##### convert duration questions into binary inidicators
##### remove "How many (months/years)"
grep("\\(Id10413_a\\) For how long did s/he smoke tobacco\\?",
     new_probbase[, 2])
tmp_index <- grep("\\(Id10413_a\\) For how long did s/he smoke tobacco\\?",
     new_probbase[, 2])
new_probbase[tmp_index, 2] <- "(Id10413_a) Did s(h)e smoke tobacco for less than X years?"
new_probbase[tmp_index, 4] <- "create categories"
new_row_1 <- new_row_2 <- new_probbase[tmp_index,]
new_row_1[2] <- "(Id10413_b) Did s(h)e smoke tobacco for more than X years but less than Y years?"
new_row_1[4] <- "create categories"
new_row_2[2] <- "(Id10413_c) Did s(h)e smoke tobacco for at least Y years?"
new_row_2[4] <- "create categories"
dim(new_probbase)
new_probbase <- rbind(new_probbase[1:tmp_index,],
                      new_row_1, new_row_2,
                      new_probbase[(tmp_index + 1):nrow(new_probbase), ])
dim(new_probbase)

grep("\\(Id10413_d\\) How many \\(months/years\\)",
     new_probbase[, 2])
tmp_index <- grep("\\(Id10413_d\\) How many \\(months/years\\)",
                  new_probbase[, 2])
dim(new_probbase)
new_probbase <- new_probbase[-tmp_index, ]
dim(new_probbase)

#### Chew/Sniff Tobacco
##### convert duration questions into binary inidicators
##### remove "How many (months/years)"
grep("\\(Id10414_a\\) For how long did s/he chew and/or sniff  tobacco?",
     new_probbase[, 2])
tmp_index <- grep("\\(Id10414_a\\) For how long did s/he chew and/or sniff  tobacco?",
     new_probbase[, 2])
new_probbase[tmp_index, 2] <- "(Id10414_a) Did s(h)e chew and/or sniff tobacco for less than X years?"
new_probbase[tmp_index, 4] <- "create categories"
new_row_1 <- new_row_2 <- new_probbase[tmp_index,]
new_row_1[2] <- "(Id10414_b) Did s(h)e chew and/or sniff tobacco for more than X years but less than Y years?"
new_row_1[4] <- "create categories"
new_row_2[2] <- "(Id10414_c) Did s(h)e chew and/or sniff tobacco for at least Y years?"
new_row_2[4] <- "create categories"
dim(new_probbase)
new_probbase <- rbind(new_probbase[1:tmp_index,],
                      new_row_1, new_row_2,
                      new_probbase[(tmp_index + 1):nrow(new_probbase), ])
dim(new_probbase)

grep("\\(Id10414_d\\) How many \\(months/years\\)",
     new_probbase[, 2])
tmp_index <- grep("\\(Id10414_d\\) How many \\(months/years\\)",
                  new_probbase[, 2])
dim(new_probbase)
new_probbase <- new_probbase[-tmp_index, ]
dim(new_probbase)

#------------------------------------------------------------------------------#
# Identify age- & sex-specific indicators                                      #
#------------------------------------------------------------------------------#
orig_col_names <- colnames(new_probbase)
orig_col_names
dim(new_probbase)
new_probbase <- cbind(new_probbase[, 1:8],
                      "", "", # sex and age indicators
                      new_probbase[, 9:ncol(new_probbase)])
dim(new_probbase)
colnames(new_probbase) <- c(orig_col_names[1:8],
                            "sex", "age",
                            orig_col_names[9:length(orig_col_names)])

## sex
dont_ask_cols <- grep("dontask", colnames(new_probbase))
for (i in 1:nrow(new_probbase)) {
    male <- grep("i019bY", new_probbase[i, dont_ask_cols])
    female <- grep("i019aY", new_probbase[i, dont_ask_cols])
    if (length(male) == 1) {
        new_probbase[i, "sex"] <- "male"
    }
    if (length(female) == 1) {
        new_probbase[i, "sex"] <- "female"
        }
}
table(new_probbase[, "sex"])

## age
age_indic <- paste0("i022", letters[1:7], "Y")
age_indic
index_age_labels <- grep("i022[a-g]", new_probbase[, "indic"])
age_labels <- new_probbase[index_age_labels, "sdesc"]
#### neonates
tmp_index <- new_probbase[, "nnonly"] == "i022gY"
table(tmp_index, useNA="ifany")
new_probbase[tmp_index, "age"] <- "neonate"
for (i in 1:nrow(new_probbase)) {

    if (new_probbase[i, "age"] == "neonate") next
    
    d_ask_age <- grep("i022[a-g]", new_probbase[i, dont_ask_cols])
    if (length(d_ask_age) > 0) {
        tmp_row <- new_probbase[i, dont_ask_cols][d_ask_age]
        age_only <- age_labels[!(age_indic %in% tmp_row)]
        if (length(age_only) > 0){
            new_probbase[i, "age"] <- paste(age_only, collapse = " & ")
        }
    }
}


#------------------------------------------------------------------------------#
# Identify possible, impossible, irrelevant, and split symptom/cause pairs     #
#------------------------------------------------------------------------------#


## fill in values for cause/symptom pairs: (beware of SUBST==N!)
## (1) possible
## (2) impossible
## (3) irrelevant
## (4) split by sex
## (5) split by age
## (6) split by sex and age


#------------------------------------------------------------------------------#
# Write template to CSV                                                        #
#------------------------------------------------------------------------------#
write.csv(new_probbase, file = "template_probbase2020.csv", row.names=FALSE, na="")
