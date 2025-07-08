# 📌 Load Required Libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# 📌 Function to Plot Relative Responses for a Given Question
# 📌 Load Required Libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# 📌 Final Correct Function
# 📌 Load Required Libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# 📌 Updated Final Correct Function
plot_likert_response <- function(data, variable) {
  
  # Step 1: Prepare summary
  overall_summary <- data %>%
    group_by(`Candidate Name`, .data[[variable]]) %>%
    summarise(Count = n(), .groups = "drop")
  
  # Rename the second column to "Rating"
  names(overall_summary)[2] <- "Rating"
  
  # Step 2: Remove NA ratings
  overall_summary <- overall_summary %>%
    filter(!is.na(Rating))
  
  # Step 3: Calculate total responses per Candidate
  total_responses <- overall_summary %>%
    group_by(`Candidate Name`) %>%
    summarise(Total = sum(Count), .groups = "drop")
  
  # Step 4: Merge and calculate proportion
  overall_summary <- overall_summary %>%
    left_join(total_responses, by = "Candidate Name") %>%
    mutate(Proportion = Count / Total)
  
  # Step 5: Set the correct Likert order
  likert_levels <- c(
    "Strongly disagree", 
    "Somewhat disagree", 
    "Neither agree nor disagree", 
    "Somewhat agree", 
    "Strongly agree"
  )
  
  overall_summary$Rating <- factor(overall_summary$Rating, levels = likert_levels)
  
  # Step 6: Plot
  p <- ggplot(overall_summary, aes(x = Rating, y = Proportion, fill = `Candidate Name`)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.4), width = 0.4) +
    geom_text(aes(label = percent(Proportion, accuracy = 1)),
              position = position_dodge(width = 0.4),
              vjust = -0.3,
              size = 4,
              fontface = "bold") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = paste0("Relative Ratings for: ", variable),
      subtitle = "Proportion of Responses by Candidate (normalized to total responses)",
      x = "Response Category",
      y = "Percentage of Responses",
      fill = "Candidate"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
      plot.title = element_text(face = "bold", size = 16),
      axis.title = element_text(size = 14)
    )
  
  return(p)
}

plot_likert_response(Organized_Data, "The candidate outlined a clear and compelling vision for the future of WIHR.")

plot_likert_response(Organized_Data, "The candidate demonstrated an understanding of the local and national Humanities landscape.")

plot_likert_response(Organized_Data, "The candidate answered questions effectively")




############################################################
library(dplyr)

# 📌 Step 2: Define Likert recoding
likert_recode <- function(x) {
  recode(x,
         "Strongly agree" = 5,
         "Somewhat agree" = 4,
         "Neither agree nor disagree" = 3,
         "Somewhat disagree" = 2,
         "Strongly disagree" = 1)
}

# 📌 Step 3: Recode all 6 Likert columns
likert_questions <- c(
  "The candidate outlined a clear and compelling vision for the future of WIHR.",
  "The candidate demonstrated an understanding of the local and national Humanities landscape.",
  "The candidate was prepared and organized.",
  "The presentation was concise and well-structured.",
  "The candidate offered concrete examples and strategies for WIHR.",
  "The candidate answered questions effectively"
)

Organized_Data <- Organized_Data %>%
  mutate(across(all_of(likert_questions), ~likert_recode(as.character(.))))



#####################################################

# 📌 Load necessary library
library(dplyr)
library(tidyr)

# 📌 Step 1: Wide-format table showing Median Scores for each Candidate by Question
median_table <- median_summary %>%
  select(`Candidate Name`, Short_Question, Median_Score) %>%
  pivot_wider(
    names_from = `Candidate Name`,
    values_from = Median_Score
  )

# 📌 Step 2: View the table
print(median_table)

##################################################

# 📌 Load Required Libraries
library(dplyr)
library(ggplot2)

# 📌 Step 1: Define the original Likert columns
likert_questions <- c(
  "The candidate outlined a clear and compelling vision for the future of WIHR.",
  "The candidate demonstrated an understanding of the local and national Humanities landscape.",
  "The candidate was prepared and organized.",
  "The presentation was concise and well-structured.",
  "The candidate offered concrete examples and strategies for WIHR.",
  "The candidate answered questions effectively"
)

# 📌 Step 2: Create a mapping for short names
question_short_names <- c(
  "The candidate outlined a clear and compelling vision for the future of WIHR." = "clear and compelling vision",
  "The candidate demonstrated an understanding of the local and national Humanities landscape." = "local and national humanities landscape",
  "The candidate was prepared and organized." = "prepared and organized",
  "The presentation was concise and well-structured." = "concise and well-structured",
  "The candidate offered concrete examples and strategies for WIHR." = "concrete examples and strategies",
  "The candidate answered questions effectively" = "answered questions effectively"
)

# 📌 Step 3: Reshape the data into long format
survey_long <- Organized_Data %>%
  select(`Candidate Name`, all_of(likert_questions)) %>%
  pivot_longer(
    cols = all_of(likert_questions),
    names_to = "Question",
    values_to = "Response"
  )

# 📌 Step 4: Calculate the median response per Candidate per Question
median_summary <- survey_long %>%
  group_by(`Candidate Name`, Question) %>%
  summarise(Median_Score = median(as.numeric(Response), na.rm = TRUE), .groups = "drop") %>%
  mutate(Short_Question = recode(Question, !!!question_short_names))


# 📌 Step 5: Correct way to Calculate Number of Responses per Candidate
n_responses <- Organized_Data %>%
  group_by(`Candidate Name`) %>%
  summarise(N = n(), .groups = "drop")  # Just count rows!

# 📌 Step 6: Create a subtitle with N info
n_info <- paste0(n_responses$`Candidate Name`, " (N=", n_responses$N, ")", collapse = "; ")


ggplot(median_summary, aes(x = Short_Question, y = Median_Score, color = `Candidate Name`, group = `Candidate Name`)) +
  geom_point(size = 4) +
  geom_line(size = 1) +
  ylim(1, 5) +
  labs(
    title = "Median Likert Scale Scores by Candidate",
    subtitle = paste("Across Key Evaluation Questions |", n_info),
    x = "Evaluation Questions",
    y = "Median Score (1 = Strongly Disagree, 5 = Strongly Agree)",
    color = "Candidate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.8,face = "bold",size = 12),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )


#########################################################################
# 📌 Load Required Libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# 📌 Step 1: Define Likert Columns
likert_questions <- c(
  "The candidate outlined a clear and compelling vision for the future of WIHR.",
  "The candidate demonstrated an understanding of the local and national Humanities landscape.",
  "The candidate was prepared and organized.",
  "The presentation was concise and well-structured.",
  "The candidate offered concrete examples and strategies for WIHR.",
  "The candidate answered questions effectively"
)

# 📌 Step 2: Create survey_long_full
survey_long_full <- Organized_Data %>%
  select(`Candidate Name`, `Collected from`, all_of(likert_questions)) %>%
  pivot_longer(
    cols = all_of(likert_questions),
    names_to = "Question",
    values_to = "Response"
  )

# 📌 Step 3: Calculate Median by Candidate Name + Collected From + Question
median_summary_collected <- survey_long_full %>%
  group_by(`Candidate Name`, `Collected from`, Question) %>%
  summarise(Median_Score = median(as.numeric(Response), na.rm = TRUE), .groups = "drop") %>%
  mutate(Short_Question = recode(Question,
                                 "The candidate outlined a clear and compelling vision for the future of WIHR." = "clear and compelling vision",
                                 "The candidate demonstrated an understanding of the local and national Humanities landscape." = "local and national humanities landscape",
                                 "The candidate was prepared and organized." = "prepared and organized",
                                 "The presentation was concise and well-structured." = "concise and well-structured",
                                 "The candidate offered concrete examples and strategies for WIHR." = "concrete examples and strategies",
                                 "The candidate answered questions effectively" = "answered questions effectively"
  ))

# 📌 Step 4: Calculate Number of Responses per Candidate Name
n_responses_collected <- Organized_Data %>%
  group_by(`Candidate Name`) %>%
  summarise(N = n(), .groups = "drop")

# 📌 Step 1: Table of counts by Candidate and Collected from
n_table_collected <- Organized_Data %>%
  group_by(`Candidate Name`, `Collected from`) %>%
  summarise(N = n(), .groups = "drop")

# 📌 Step 2: Create subtitle string separately for Meeting and Public link
n_meeting <- n_table_collected %>%
  filter(`Collected from` == "Meeting") %>%
  mutate(info = paste0(`Candidate Name`, " (N=", N, ")")) %>%
  pull(info) %>%
  paste(collapse = ", ")

n_public <- n_table_collected %>%
  filter(`Collected from` == "Public link") %>%
  mutate(info = paste0(`Candidate Name`, " (N=", N, ")")) %>%
  pull(info) %>%
  paste(collapse = ", ")

# 📌 Step 3: Final subtitle text
subtitle_text <- paste0("Meeting: ", n_meeting, " | Public link: ", n_public)

# 📌 Step 4: Now Plot
ggplot(median_summary_collected, aes(x = Short_Question, y = Median_Score, color = `Candidate Name`, group = `Candidate Name`)) +
  geom_point(size = 4) +
  geom_line(size = 1) +
  ylim(1, 5) +
  facet_wrap(~ `Collected from`) +
  labs(
    title = "Median Likert Scale Scores by Candidate",
    subtitle = subtitle_text,
    x = "Evaluation Questions",
    y = "Median Score (1 = Strongly Disagree, 5 = Strongly Agree)",
    color = "Candidate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.7,face = "bold",size = 12),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )


############################################################################



# 📌 Load Required Libraries
library(dplyr)
library(ggplot2)

# 📌 Step 1: Prepare Data
overall_summary <- Organized_Data %>%
  group_by(`Candidate Name`, `Overall, I would rate this presentation as:`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  rename(Rating = `Overall, I would rate this presentation as:`)

# 📌 Step 2: Calculate total responses per Candidate
total_responses <- overall_summary %>%
  group_by(`Candidate Name`) %>%
  summarise(Total = sum(Count), .groups = "drop")

# 📌 Step 3: Merge total and calculate proportion
overall_summary <- overall_summary %>%
  left_join(total_responses, by = "Candidate Name") %>%
  mutate(Proportion = Count / Total)

# 📌 Step 4: Make Rating an ordered factor
overall_summary$Rating <- factor(overall_summary$Rating, 
                                 levels = c("Excellent", "Very Good", "Good", "Average", "Below Average"))

# 📌 Step 5: Set dodge width
dodge_width <- 0.4

# 📌 Step 6: Plot Relative (Proportional) Bar Chart
ggplot(overall_summary, aes(x = Rating, y = Proportion, fill = `Candidate Name`)) +
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = 0.4) +
  geom_text(aes(label = scales::percent(Proportion, accuracy = 1)),
            position = position_dodge(width = dodge_width),
            vjust = -0.3,
            size = 4,
            fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Relative Presentation Ratings by Candidate",
    subtitle = "Proportion of Responses by Candidate (normalized to total responses)",
    x = "Rating",
    y = "Percentage of Responses",
    fill = "Candidate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 14)
  )


##############

# 📌 Load Required Libraries
library(dplyr)
library(ggplot2)
library(scales)

# 📌 Step 1: Prepare Data
recommend_summary <- Organized_Data %>%
  group_by(`Candidate Name`, `Would you recommend this candidate`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  rename(Response = `Would you recommend this candidate`)

# 📌 Step 2: Calculate total responses per Candidate
total_responses_recommend <- recommend_summary %>%
  group_by(`Candidate Name`) %>%
  summarise(Total = sum(Count), .groups = "drop")

# 📌 Step 3: Merge and calculate proportion
recommend_summary <- recommend_summary %>%
  left_join(total_responses_recommend, by = "Candidate Name") %>%
  mutate(Proportion = Count / Total)

# 📌 Step 4: Optional - Set Order for Responses
response_levels <- c("No", "With reservation", "Yes")
recommend_summary$Response <- factor(recommend_summary$Response, levels = response_levels)

# 📌 Step 5: Plot
ggplot(recommend_summary, aes(x = Response, y = Proportion, fill = `Candidate Name`)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.4), width = 0.4) +
  geom_text(aes(label = percent(Proportion, accuracy = 1)),
            position = position_dodge(width = 0.4),
            vjust = -0.3,
            size = 4,
            fontface = "bold") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Recommendation of Candidate by Participants",
    subtitle = "Proportion of Responses (Normalized to Total Responses)",
    x = "Recommendation Response",
    y = "Percentage of Responses",
    fill = "Candidate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 14)
  )



#### WORD CLOUD




# 📌 Function to Generate Word Cloud by Candidate Name
# 📌 Load Required Libraries
library(dplyr)
library(tm)
library(wordcloud)
library(RColorBrewer)

# 📌 Final Correct Function: Word Cloud by Candidate Name with Title
generate_wordcloud_by_candidate <- function(data, column_name, candidate_name, min_freq = 1, max_words = 100) {
  
  # Step 1: Filter Data for Selected Candidate
  text_data <- data %>%
    filter(`Candidate Name` == candidate_name) %>%
    pull(column_name)
  
  # Check if there is any text available
  if (length(text_data) == 0 || all(is.na(text_data))) {
    message(paste0("No text data available for Candidate: ", candidate_name))
    return(NULL)
  }
  
  # Step 2: Clean and Prepare Text
  text_corpus <- Corpus(VectorSource(text_data))
  
  text_corpus <- text_corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("english"))
  
  # Step 3: Create Term-Document Matrix
  tdm <- TermDocumentMatrix(text_corpus)
  m <- as.matrix(tdm)
  word_freqs <- sort(rowSums(m), decreasing = TRUE)
  
  # Step 4: Create Word Data Frame
  word_data <- data.frame(word = names(word_freqs), freq = word_freqs)
  
  # Step 5: Generate Word Cloud
  set.seed(1234)
  wordcloud(words = word_data$word,
            freq = word_data$freq,
            min.freq = min_freq,
            max.words = max_words,
            random.order = FALSE,
            rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"))
  
  # 📌 Step 6: Add Candidate Name as Title
  title(main = paste("Word Cloud for", candidate_name), font.main = 2)
}


generate_wordcloud_by_candidate(Organized_Data, 
                                "Additional Comments", 
                                candidate_name = "Morris")


generate_wordcloud_by_candidate(Organized_Data, 
                                "Additional Comments", 
                                candidate_name = "Sailor")

