library(readr)
library(dplyr)
Awards <- read_csv("Processed_Award_Data_Subsetted_07082025.csv")
# Update the Project Title for specific Award IDs
Awards$`Project Title`[Awards$`Award #` == "24-0613-A0002"] <- "Wyoming-Sourced Rare Earth Elements as Neutron Poisons"
Awards$`Project Title`[Awards$`Award #` == "24-0430-A0003-1"] <- "Critical Minerals Leadership Academy (CMLA)"


# Convert project titles to lowercase for case-insensitive search
Awards$`Project Title` <- tolower(Awards$`Project Title`)

# Define keywords based on Dr. Chitnis's request:
# "research and related activities on minerals (surveying, analysis, mining, processing, etc.)"
# Each keyword below supports that scope, with reasoning.

keywords <- c(
  "trona",
  "uranium",
  "geosciences",
  "deposit",
  "post-mining",
  "pro-mining",
  "ree",             # Rare earth elements
  "rees",            # Plural form
  "mineral",         # Core subject: naturally occurring inorganic substances
  "minerals",        # Plural form — covers more title variations
  "mining",          # Directly refers to mineral extraction — central to request
  "mine",            # General term — matches titles like "mine safety" or "mine reclamation"
  "mines",           # Plural — captures broader project references
  "ore",             # Mineral-bearing rock — common in extraction/processing titles
  "ores",            # Plural form
  "tailings",        # Mining waste — often subject of environmental impact studies
  "post-mining",     # Refers to land/environmental post-mining
  "mine waste",      # Includes environmental remediation or waste handling
  "rare earth",      # Critical materials often covered in policy/strategic research
  "rare earths",     # Plural form
  "stratigraphy",    # Rock layer study — helps locate mineral-bearing formations
  "sedimentology"   # Focuses on sediments where minerals may concentrate
)

# Add word boundaries to each keyword
keywords_with_boundaries <- paste0("\\b", keywords, "\\b")

# Create regex pattern with OR operator
pattern <- paste(keywords_with_boundaries, collapse = "|")

# Filter rows by full word match in 'Project Title'
filtered_awards <- Awards[grepl(pattern, Awards$`Project Title`, ignore.case = TRUE), ]

filtered_awards <- filtered_awards%>%
  filter(`Award #` !="25-0067-A0001")

# View filtered results
dim(filtered_awards)
#View(filtered_awards)

write.csv(filtered_awards, "Mineral_Related_Grants_RoamWyo_07092025.csv", row.names = FALSE)


# Must be included:

# 24-0613-A0002   # Wyoming-Sourced Rare Earth Elements as Neutron Poisons - Update the Project title
# 24-0430-A0003-1 # Critical Minerals Leadership Academy (CMLA) - Update the Project title
# 24-0002-A0001 # REEs Recycling for REMs Production by Hydrogen Plasma Reduction of REOs/Salts	





library(readxl)
Project_Information_Wyocloud <- read_excel("Project Information_Results.xlsx")
# Filter rows by full word match in `Project Name`
filtered_awards_wyocloud <- Project_Information_Wyocloud[grepl(pattern, Project_Information_Wyocloud$`Project Name`, ignore.case = TRUE), ]

# Joining selected rows (Wyocloud and RoamWyo)
filtered_awards$`Project Number` <- gsub("-", "", filtered_awards$`Award #`)



# Perform anti join by 'Project Number'
unmatched_awards <- anti_join(filtered_awards_wyocloud, filtered_awards, by = "Project Number")

unmatched_awards <- unmatched_awards%>%
  filter(`Award Status` == "ACTIVE")


# Saving the Awards which have the keywords:
write.csv(unmatched_awards, "Mineral_Related_Grants_Not_in_RoamWyo_But_in_Wyocloud_07092025.csv", row.names = FALSE)












# Optional: export
# write.csv(filtered_awards, "Mineral_Related_Grants.csv", row.names = FALSE)


### Filtering the proposals with the keywords:


Proposal <- read_csv("Processed_Proposal_Data_Subsetted_070825.csv")  
Proposal$`full proposal title`
# Filter rows by full word match in `Project Name`
filtered_Proposal_Roamwy <- Proposal[grepl(pattern, Proposal$`full proposal title`, ignore.case = TRUE), ]

filtered_Proposal_Roamwy <- filtered_Proposal_Roamwy %>%
  filter(Status %in% c("Submitted to Sponsor","Under Consideration"))

# Saving the proposals which are submitted to sponsor or under consideration (having the keywords)
write.csv(filtered_Proposal_Roamwy, "Mineral_Related_Proposals_07092025.csv", row.names = FALSE)
