library(readr)
library(dplyr)
Awards <- read_csv("Processed_Award_Data_Subsetted_07082025.csv")


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
  "geological",
  "geology",
  "post-mining",
  "pro-mining",
  "REE",             # Rare earth elements
  "mineral",         # Core subject: naturally occurring inorganic substances
  "minerals",        # Plural form — covers more title variations
  "mining",          # Directly refers to mineral extraction — central to request
  "mine",            # General term — matches titles like "mine safety" or "mine reclamation"
  "mines",           # Plural — captures broader project references
  "ore",             # Mineral-bearing rock — common in extraction/processing titles
  "ores",            # Plural form
  "geology",         # Scientific field supporting mineral discovery
  "geological",      # Adjective form, e.g., "geological mapping"
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
View(filtered_awards)



# Optional: export
# write.csv(filtered_awards, "Mineral_Related_Grants.csv", row.names = FALSE)


### Filtering the proposals with the keywords:


Proposal <- read_csv("Processed_Proposal_Data_Subsetted_070825.csv")  

# # Filter rows by full word match in any of the three title fields
# filtered_proposals <- Proposal[
#   grepl(pattern, Proposal$`full proposal title`, ignore.case = TRUE) |
#     grepl(pattern, Proposal$`Project Title`, ignore.case = TRUE) |
#     grepl(pattern, Proposal$`proposal opportunity name`, ignore.case = TRUE),
# ]
# 
# filtered_proposals_funded <- filtered_proposals %>%
#   filter(Status == "Funded")
# View(filtered_proposals_funded)
# 
# # Create Award # by replacing 'P' with 'A' in Proposal #
# filtered_proposals_funded$`Award #` <- sub("P", "A", filtered_proposals_funded$`Proposal #`)
# 
# intersect(unique(Awards$`Award #`),unique(filtered_proposals_funded$`Award #`))
# 
# Awards_need setdiff(unique(filtered_proposals_funded$`Award #`),unique(Awards$`Award #`))
