# Load Data:
# Load Data Paths
# Load Data:
# Load Data Paths
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/0_Data_Path_Configuration.R")
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/1_Proposal_data_load_and_clean_from_Cayuse.R")
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/2_Financial_data_load_and_clean_from_Wyocloud.R")
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/3_Jami_List_Hire_Date_FSU.R")



# After running above 3 services, we are getting mainly 2 Mother data sets.

# Mother data 1: award_non_award
# Mother data 2: pi_joining_date_and_funding
# By PI we can join this two data.

# Fixing PI names after manual checking:

pi_joining_date_and_funding$PI <- ifelse(
  pi_joining_date_and_funding$PI == "Anne Perry",
  "Abby Anne Perry",
  pi_joining_date_and_funding$PI
)




pi_joining_date_and_funding$PI <- ifelse(
  pi_joining_date_and_funding$PI == "Irina Fay",
  "Irina Holmes Fay",
  pi_joining_date_and_funding$PI
)



award_non_award$PI <- ifelse(
  award_non_award$PI == "Irina Fay",
  "Irina Holmes Fay",
  award_non_award$PI
)









results <- summarize_pi_funding_by_FY("FY2020", pi_joining_date_and_funding, award_non_award)
results <- summarize_pi_funding_by_FY("FY2021", pi_joining_date_and_funding, award_non_award)
results <- summarize_pi_funding_by_FY("FY2022", pi_joining_date_and_funding, award_non_award)
results <- summarize_pi_funding_by_FY("FY2023", pi_joining_date_and_funding, award_non_award)
results <- summarize_pi_funding_by_FY("FY2024", pi_joining_date_and_funding, award_non_award)
results <- summarize_pi_funding_by_FY("FY2025", pi_joining_date_and_funding, award_non_award)



