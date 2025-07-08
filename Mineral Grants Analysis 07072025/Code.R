library(readr)
Awards <- read_csv("Processed_Award_Data_Subsetted.csv")



# Convert project titles to lowercase for case-insensitive search
Awards$`Project Title` <- tolower(Awards$`Project Title`)

# Define keywords based on Dr. Chitnis's request:
# "research and related activities on minerals (surveying, analysis, mining, processing, etc.)"
# Each keyword below supports that scope, with reasoning.

keywords <- c(
  "mineral",         # Core subject: naturally occurring inorganic substances
  "minerals",        # Plural form — covers more title variations
  "mining",          # Directly refers to mineral extraction — central to request
  "mine",            # General term — matches titles like "mine safety" or "mine reclamation"
  "mines",           # Plural — captures broader project references
  "ore",             # Mineral-bearing rock — common in extraction/processing titles
  "ores",            # Plural form
  "geology",         # Scientific field supporting mineral discovery
  "geological",      # Adjective form, e.g., "geological mapping"
  "drilling",        # Technique used in mineral exploration
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

# View filtered results
View(filtered_awards)

# Optional: export
# write.csv(filtered_awards, "Mineral_Related_Grants.csv", row.names = FALSE)