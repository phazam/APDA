####library(rdrop2)

#token<-drop_auth()
#saveRDS(token, "droptoken.rds")
####correct one ####token <- readRDS("droptoken.rds")
#####correct one ####apda <- drop_read_csv("query_result.csv", dtoken=token)
library(ggvis)
 apda <- read.delim("query_result.txt", header = TRUE, sep = "\t")
apda$AOS[apda$AOS == 1] <- "LEMM (Language, Epistemology, Mind, and Metaphysics)"
apda$AOS[apda$AOS == 2] <- "Value Theory"
apda$AOS[apda$AOS == 3] <- "History and Traditions"
apda$AOS[apda$AOS == 4] <- "Science, Logic, and Math"
apda$AOS[apda$AOS == 5] <- "Uknown"

apda$position[apda$position == 1] <- "Tenure-Track"
apda$position[apda$position == 2] <- "Other Permanent Position"
apda$position[apda$position == 3] <- "Other Permanent Position"
apda$position[apda$position == 4] <- "Other Permanent Position"
apda$position[apda$position == 5] <- "Other Permanent Position"
apda$position[apda$position == 11] <- "Fellowship/Postdoc"
apda$position[apda$position == 12] <- "Visiting Position"
apda$position[apda$position == 13] <- "Other Temporary Position"
apda$position[apda$position == 14] <- "Other Temporary Position"
apda$position[apda$position == 15] <- "Other Temporary Position"
apda$position[apda$position == 16] <- "Other Temporary Position"
apda$position[apda$position == 20] <- "Non-academic Position"
apda$position[apda$position == 21] <- "Not Placed or Unknown Placement"

apda$primaryAoS[apda$primaryAoS == 1] <- "Modern"
apda$primaryAoS[apda$primaryAoS == 2] <- "German"
apda$primaryAoS[apda$primaryAoS == 3] <- "Continental"
apda$primaryAoS[apda$primaryAoS == 4] <- "19th/20th"
apda$primaryAoS[apda$primaryAoS == 5] <- "Action"
apda$primaryAoS[apda$primaryAoS == 6] <- "Aesthetics"
apda$primaryAoS[apda$primaryAoS == 7] <- "American"
apda$primaryAoS[apda$primaryAoS == 8]<- "African"
apda$primaryAoS[apda$primaryAoS == 9] <- "Religion"
apda$primaryAoS[apda$primaryAoS == 10] <- "Analytic"
apda$primaryAoS[apda$primaryAoS == 11] <- "Ancient"
apda$primaryAoS[apda$primaryAoS == 12] <- "Applied Ethics"
apda$primaryAoS[apda$primaryAoS == 13] <- "Unknown"
apda$primaryAoS[apda$primaryAoS == 14] <- "Science"
apda$primaryAoS[apda$primaryAoS == 15] <- "Ethics"
apda$primaryAoS[apda$primaryAoS == 16] <- "Asian"
apda$primaryAoS[apda$primaryAoS == 17] <- "Epistemology"
apda$primaryAoS[apda$primaryAoS == 18] <- "Cognitive Science/Psychology      /Neuroscience/Linguistics"
apda$primaryAoS[apda$primaryAoS == 19] <- "Comparative"
apda$primaryAoS[apda$primaryAoS == 20] <- "Law"
apda$primaryAoS[apda$primaryAoS == 21] <- "Metaphysics"
apda$primaryAoS[apda$primaryAoS == 22] <- "Social/Political"
apda$primaryAoS[apda$primaryAoS == 23] <- "Gender/Race/Sexuality/Disability Studies"
apda$primaryAoS[apda$primaryAoS == 24] <- "Logic"
apda$primaryAoS[apda$primaryAoS == 25] <- "Decision Theory"
apda$primaryAoS[apda$primaryAoS == 26] <- "Economics"
apda$primaryAoS[apda$primaryAoS == 27] <- "Education"
apda$primaryAoS[apda$primaryAoS == 28] <- "Biology"
apda$primaryAoS[apda$primaryAoS == 29] <- "Metaphilosophy"
apda$primaryAoS[apda$primaryAoS == 30] <- "Mind"
apda$primaryAoS[apda$primaryAoS == 31] <- "History"
apda$primaryAoS[apda$primaryAoS == 32] <- "Physics"
apda$primaryAoS[apda$primaryAoS == 33] <- "Medieval/Renaissance"
apda$primaryAoS[apda$primaryAoS == 34] <- "Language"
apda$primaryAoS[apda$primaryAoS == 35] <- "Math"
apda$primaryAoS[apda$primaryAoS == 36] <- "Meta-Ethics"
apda$primaryAoS[apda$primaryAoS == 37] <- "Technology"
apda$primaryAoS[apda$primaryAoS == 38] <- "Unknown"
apda$primaryAoS[apda$primaryAoS == 39] <- "Unknown"
apda$primaryAoS[apda$primaryAoS == 40] <- "Unknown"
apda$primaryAoS[apda$primaryAoS == 41] <- "Unknown"


all <- data.frame(apda)


axis_vars <- c(
#  "Placement Type" = "position",
  "Year" = "graduation_year"
 )#,"University" = "university_name")
#Phdfrom_names <- list(unique("university_name"))



aos_lev <- c("LEMM (Language, Epistemology, Mind, and Metaphysics)",
              "Value Theory", "History and Traditions","Science, Logic, and Math")


legend_vars <- c(
  "Gender" = "has_gender",
  "Most Recent Placement" = "position"
)

primaryAoSlevels <- c("Modern",
"German",
"Continental",
"19th/20th",
"Action",
"Aesthetics",
"American",
"African",
"Religion",
"Analytic",
"Ancient",
"Applied Ethics",
"Unknown",
"Science",
"Ethics",
"Asian",
"Epistemology",
"Cognitive Science/Psychology/Neuroscience/Linguistics",
"Comparative",
"Law",
"Metaphysics",
"Social/Political",
"Gender/Race/Sexuality/Disability Studies",
"Logic",
"Decision Theory",
"Economics",
"Education",
"Biology",
"Metaphilosophy",
"Mind",
"History",
"Physics",
"Medieval/Renaissance",
"Language",
"Math",
"Meta-Ethics",
"Technology",
"Value")


