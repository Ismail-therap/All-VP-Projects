# Load Data:
# Load Data Paths
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/0_Data_Path_Configuration.R")
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/1_Proposal_data_load_and_clean_from_Cayuse.R")
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/2_Financial_data_load_and_clean_from_Wyocloud.R")
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/3_PI_Joining_and_FSU_funding_from_Wyocloud.R")


# After running above 3 services, we are getting mainly 2 Mother data sets.

# Mother data 1: award_non_award
# Mother data 2: pi_joining_date_and_funding


# =====================================================
# STEP 2: Preparing PI Data for Filtering
# =====================================================

# 📌 List of Project Managers
project_managers <- c(
  "Ahn, Juhyeon", "Aikens, Ellen", "Armstrong, Melanie", "Bailey, Ryan", 
  "Barrile, Gabriel", "Bedford, Nicole", "Beiermann, Clint", "Bell, Jennifer", 
  "Bernhardt, Natasha", "Bhattarai, Surendra", "Biasi, Joseph", 
  "Bock, Jarrod", "Borgholthaus, Cameron", "Brower, Alexandra", "Burgess, Matthew", 
  "Burton, Peter", "Davidson, Curt", "Davis, Robert", "De Mello Tavares Lima, Paulo", 
  "Demir, Aysegul", "Dittoe, Dana", "Dyett, Jordan", "Eidin, Emil", "Elokely, Khaled", 
  "Encinger, Amy", "Evans, Alecia", "Field, Sean", "Forzan, Maria", "French, Alexander", 
  "Gansauer, Grete", "Genoese, Francesca", "Germain, Sara", "Gilbert, Lauren", 
  "Grogan, Kelly", "Groot, Koen", "Grover, Abhay", "Gulick, Eleanor", "Harrington, Erin", 
  "Hawes, Jason", "Hayes, Lauren", "Holmes Fay, Irina", "Hunt, Tiffany", "Imhof, Michael", 
  "Iqbal, Hasan", "Irisarri, Jorge", "James, Alexander", "Joyce, Meridith", "Kane, Sarah", 
  "Koger, Benjamin", "Krause, Jennifer", "Kuper, Julie", "Leonard, Bryan", "Lewis, Madeline", 
  "Li, Xuesong", "Macy, Marisa", "Maia Sabino, Roberta", "Majeed, Yaqoob", "Mann, Allison", 
  "Matthews, Samuel", "McClure, Kenneth", "Moe, Maxwell", "Nowell, Stella", "Pascual, David", 
  "Payne, Anna", "Perry, Anne", "Petrovic, Alexander", "Phalen, Hannah", "Phillips, Brittney", 
  "Pinello, Jennifer", "Ragan, Izabela", "Rau, Daniel", "Saito, Masanori", "Sanders, Miriam", 
  "Saxena, Ankit", "Schlomer, Matthew", "Shane-Nichols, Amy", 
  "Sheshappanavar, Shivanand Venkanna", "Shukla, Rammohan", "Song, Yu", "Srednik, Mariela", 
  "Suyama, Takashi", "Tatum, Garrett", "Taylor, Dane", "Taylor, Nicolina", 
  "Tedmon-Jones, Tyler", "Tsai, Yu-Tsung", "Tuft, Alexander", "Vanderstichel, Raphael", 
  "Walker, Ian", "Woodward, Richard", "Wor, Magdalena", "Yang, Xuhao", "Zhang, Ling",
  "Nakka David, Sheba Rani","Elshehabi, Tawfik","Nguyen, Duong","McFarlin, Jamie","Zhou, Zejian"
  
  
)

# 📌 Format Names as "First Name Last Name"
formatted_names <- unname(sapply(project_managers, function(name) {
  parts <- strsplit(name, ", ")[[1]]
  paste(parts[2], parts[1])
}))

# 📌 Create Name Patterns for Filtering
first_names <- sapply(strsplit(formatted_names, " "), function(x) x[1])
other_part <- sapply(strsplit(formatted_names, " "), function(x) x[2])
name_patterns <- paste0(first_names, ".*", other_part)


# ===================================================================
# STEP 2: Cleaning Joining date and FSU data based on spot checking
# ===================================================================

# After spot checking found unnecessary PI and also found someone very important missing. So, that's why deleting and adding 2 PI's in this part. 

# Load PI Joining Date and Funding Data
pi_joining_date_and_funding <- pi_joining_date_and_funding %>%
  filter(`PI` != "Williams-Woodward, Jean")   # Remove unwanted PI




# ============================================
# STEP 3: Filtering Joining Date and FSU Data
# =============================================

# 📌 Filter Joining Date and FSU Data
filtered_pi_joining_funding <- pi_joining_date_and_funding %>%
  filter(str_detect(`PI`, paste(other_part, collapse = "|")))

# 📌 Rename and Rearrange Columns
filtered_pi_joining_funding <- filtered_pi_joining_funding %>%
  rename(
    `Total Start Up Funds` = `Total_FSU`,
    `Joining Date` = `Assignment Start Date`
  ) %>%
  select(`PI`, `Joining Date`,`Joining FY`, `Total Start Up Funds`)

filtered_pi_joining_funding <- filtered_pi_joining_funding %>%
  mutate(`Total Start Up Funds` = format(`Total Start Up Funds`, scientific = FALSE))

# 📌 Remove Unnecessary Entry
filtered_pi_joining_funding <- filtered_pi_joining_funding %>%
  filter(`PI` != "Pru, James")

# 📌 Correct PI Name Format
filtered_pi_joining_funding <- filtered_pi_joining_funding %>%
  mutate(`PI` = str_replace(`PI`, "^(.*),\\s*(.*)$", "\\2 \\1"))




# ==============================
# STEP 4: Filtering Award Data
# ===============================

# 📌 Filter Award Data Using PI Names
filtered_award_non_award <- award_non_award %>%
  mutate(
    Matched_Pattern = sapply(PI, function(x) {
      matched_pattern <- name_patterns[which(str_detect(x, name_patterns))]
      if (length(matched_pattern) > 0) matched_pattern else NA
    })
  ) %>%
  filter(!is.na(Matched_Pattern))  # Retain only matched entries


filtered_award_non_award <- filtered_award_non_award %>%
  mutate(`PI` = str_replace(Matched_Pattern, "(.*)\\..*\\.(.*)", "\\2 \\1"))



# Convert "Juhyeon.*Ahn" to "Ahn Juhyeon"
filtered_award_non_award <- filtered_award_non_award %>%
  mutate(`PI` = str_replace(Matched_Pattern, "^(.*?)\\..*\\.(.*)$", "\\2 \\1"))


# 📌 Correct Name Format
filtered_award_non_award <- filtered_award_non_award %>%
  mutate(`PI` = str_replace_all(Matched_Pattern, "(.*)\\..*\\.(.*)", "\\2 \\1")) %>%
  mutate(`PI` = str_replace_all(`PI`, "\\*", "")) %>%
  mutate(`PI` = str_replace_all(`PI`, "\\.", " ")) 


# =====================================================
# STEP 5: Joining Filtered Award Data with FSU
# =====================================================

# 📌 Name Corrections for Specific PIs
filtered_pi_joining_funding <- filtered_pi_joining_funding %>%
  mutate(`PI` = case_when(
    `PI` == "Shivanand Venkanna Sheshappanavar" ~ "Shivanand Venkanna",
    `PI` == "Roberta Maia Sabino" ~ "Roberta Maia",
    `PI` == "Paulo De Mello Tavares Lima" ~ "Paulo De",
    `PI` == "Irina Holmes Fay" ~ "Irina Holmes",
    `PI` == "Sheba Rani Nakka David" ~ "Sheba Rani",
    TRUE ~ `PI`
  ))


