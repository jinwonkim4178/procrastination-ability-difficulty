################################################
########## Load Packages and datasets ########## 
################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)


## NOTE: We merge enrollment records with LMS data using the university's data warehouse (Step 1)
lms_enroll_df <- read_csv("./data/lms_enroll_dataset.csv")
student_info <-
  c("mellon_id", "birth_year", "birth_month", "female", "urm", 
    "major_name_1", "major_name_2", "major_name_3", "major_name_4", "major_minor")
course_info <-
  c("course_id", "course_code", "term_code", "term_desc")

## Calculate Procrastination Index
processed_df <-
  lms_enroll_df %>% 
  filter(complete.cases(assignment_due_at, assignment_points_possible)) %>% 
  filter(submission_state == "graded")

graded_students <- 
  processed_df %>% 
  distinct(course_id, term_desc, student_id) %>% 
  count(course_id, term_desc) %>% 
  rename(num_stu = n)

rank_assignment <- 
  processed_df %>% 
  mutate(late_hours = as.numeric(difftime(submitted_at, assignment_due_at, units = "hours"))) %>% 
  group_by(student_id, course_id, term_desc) %>% 
  summarise(avg_late_hours = mean(late_hours, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(course_id, term_desc) %>% 
  mutate(rank = rank(avg_late_hours, ties.method = "first")) %>% 
  ungroup()

initial_df <- 
  processed_df %>% 
  left_join(graded_students, by = c("course_id", "term_desc")) %>% 
  left_join(rank_assignment, by = c("course_id", "student_id", "term_desc")) %>% 
  mutate(procra_rank = rank / num_stu) %>% 
  select(all_of(student_info), all_of(course_info), final_grade, procra_rank) %>% 
  distinct()

## Creating Major Combinations
get_major_combo <- function(row) {
  majors <- unique(na.omit(c(row["major1"], row["major2"], row["major3"], row["major4"], row["major_minor"])))
  majors <- sort(majors)
  combo <- paste(majors, collapse = "_")
  return(combo)
}

initial_df$major_combo <- apply(initial_df, 1, get_major_combo)

## Step 2 (Exclude courses graded Pass/Fail and enrollments without final letter grades: 
step2_df <- 
initial_df %>% 
  mutate(grade = case_when(
    final_grade == 'A+' ~ 4.0, final_grade == 'A' ~ 4.0, final_grade == 'A-' ~ 3.7,
    final_grade == 'B+' ~ 3.3, final_grade == 'B' ~ 3.0, final_grade == 'B-' ~ 2.7,
    final_grade == 'C+' ~ 2.3, final_grade == 'C' ~ 2.0, final_grade == 'C-' ~ 1.7,
    final_grade == 'D+' ~ 1.3, final_grade == 'D' ~ 1.0, final_grade == 'D-' ~ 0.7,
    final_grade == 'F' ~ 0.0, final_grade == 'P' ~ 4.1, final_grade == 'NP' ~ -0.1,
    final_grade == 'NR' ~ -2.0, final_grade == 'W' ~ -3.0, final_grade == 'I' ~ -4.0,
    final_grade == 'IP' ~ -5.0, final_grade == 'UR' ~ -6.0
  )) %>% 
  mutate(time = case_when(term_desc == "Fall 2019" ~ "FS19",
                          term_desc == "Winter 2020" ~ "WS20",
                          term_desc == "Spring 2020" ~ "SS20",
                          term_desc == "Fall 2020" ~ "FS20",
                          term_desc == "Winter 2021" ~ "WS21",
                          term_desc == "Spring 2021" ~ "SS21",
                          term_desc == "Fall 2021" ~ "FS21",
                          term_desc == "Winter 2022" ~ "WS22",
                          term_desc == "Spring 2022" ~ "SS22",
                          term_desc == "Fall 2022" ~ "FS22",
                          term_desc == "Winter 2023" ~ "WS23",
                          term_desc == "Spring 2023" ~ "SS23",
                          term_desc == "Fall 2023" ~ "FS23",
                          term_desc == "Winter 2024" ~ "WS24",
                          term_desc == "Spring 2024" ~ "SS24")) %>% 
  distinct(student_id, course_id, time, final_grade, grade, major1, major2, major3, major4, major_minor, major_combo) %>% 
  filter(!final_grade %in% c('P', 'NP', 'NR', 'W', 'I', 'IP', 'UR'))


## Step 3: 
### 1) Restrict our sample to majors (or combinations) with >= 20 distinct courses and >= 100 total enrollments
### 2) Restrict our sample to majors with the highest enrollment per course (top 10%)
distinct_major_df <-
  initial_df %>% 
  distinct(student_id, term_desc, course_id, major1, major2, major3, major4, major_minor)

distinct_major_df$major_combo <- apply(distinct_major_df, 1, get_major_combo)

sample_majors <- 
  distinct_major_df %>% 
  count(course_id, major_combo, name = 'count') %>% 
  group_by(major_combo) %>% 
  summarise(avg_count = mean(count, na.rm = T),
            n_courses = n_distinct(canvas_course_id),
            total_counts = sum(count)) %>% 
  filter(n_courses >= 20, total_counts >= 100) %>% 
  arrange(-avg_count) %>% 
  slice_max(order_by = avg_count, prop = 0.1) %>% pull(major_combo)

step3_df <-
  step2_df %>% 
  filter(major_combo %in% sample_majors)

## Step 4: Remove cases where IRT estimations failed
### Combining IRT results
join_irt <- function(df, base_path) {
  majors <- unique(info$major_combo)
  
  out <- lapply(majors, function(mj) {
    message("=== processing major: ", mj, " ===")
    folder <- file.path(base_path, mj)
    
    student_ability <- read_csv(file.path(folder, "student_ability.csv"), show_col_types = FALSE)
    
    course_difficulty <- read_csv(file.path(folder, "course_difficulty.csv"), show_col_types = FALSE)
    
    df %>%
      filter(major_combo == mj) %>%
      left_join(student_ability, by = "student_id") %>%
      left_join(course_difficulty,   by = "course_id")
  })
  
  bind_rows(out)
}

base_path <- "./cleaned_data/IRT_results"
irt_combined <- join_irt(step3_df, base_path)
step4_df <- irt_combined %>% 
  filter(!is.na(abilities), !is.na(diff))


## Step 5: Remove majors showing evidence of multidimensionality
step5_df <- step4_df %>% 
  filter(!major_combo %in% c('PUBLIC_HEALTH_SCIENCES', 'COMPUTER_SCIENCE_AND_ENGINEERING',
                             'MATHEMATICS_QUANTITATIVE_ECONOMICS')) %>% 
  write_csv("./cleaned_data/final_analysis_dataset.csv")





























## Test
test_combination <- 
  distinct_major_df %>% 
  count(canvas_course_id, major_combo, name = 'count') %>% 
  group_by(major_combo) %>% 
  summarise(avg_count = mean(count, na.rm = T),
            n_courses = n_distinct(canvas_course_id),
            total_counts = sum(count)) %>% 
  filter(n_courses >= 20, total_counts >= 100) %>% 
  arrange(-avg_count) %>% 
  slice_max(order_by = avg_count, prop = 0.1) %>% pull(major_combo)

test_combination

test_combination  

major_processed_df$major_combo <- apply(major_processed_df, 1, get_major_combo)


major_delete_duplicates2 <- 
  major_processed_df %>% 
  #  filter(major_combo %in% test_combination) %>% 
  mutate(grade = case_when(
    final_grade == 'A+' ~ 4.0, final_grade == 'A' ~ 4.0, final_grade == 'A-' ~ 3.7,
    final_grade == 'B+' ~ 3.3, final_grade == 'B' ~ 3.0, final_grade == 'B-' ~ 2.7,
    final_grade == 'C+' ~ 2.3, final_grade == 'C' ~ 2.0, final_grade == 'C-' ~ 1.7,
    final_grade == 'D+' ~ 1.3, final_grade == 'D' ~ 1.0, final_grade == 'D-' ~ 0.7,
    final_grade == 'F' ~ 0.0, final_grade == 'P' ~ 4.1, final_grade == 'NP' ~ -0.1,
    final_grade == 'NR' ~ -2.0, final_grade == 'W' ~ -3.0, final_grade == 'I' ~ -4.0,
    final_grade == 'IP' ~ -5.0, final_grade == 'UR' ~ -6.0
  )) %>% 
  mutate(time = case_when(term_desc == "Fall 2019" ~ "FS19",
                          term_desc == "Winter 2020" ~ "WS20",
                          term_desc == "Spring 2020" ~ "SS20",
                          term_desc == "Fall 2020" ~ "FS20",
                          term_desc == "Winter 2021" ~ "WS21",
                          term_desc == "Spring 2021" ~ "SS21",
                          term_desc == "Fall 2021" ~ "FS21",
                          term_desc == "Winter 2022" ~ "WS22",
                          term_desc == "Spring 2022" ~ "SS22",
                          term_desc == "Fall 2022" ~ "FS22",
                          term_desc == "Winter 2023" ~ "WS23",
                          term_desc == "Spring 2023" ~ "SS23",
                          term_desc == "Fall 2023" ~ "FS23",
                          term_desc == "Winter 2024" ~ "WS24",
                          term_desc == "Spring 2024" ~ "SS24")) %>% 
  distinct(mellon_id, canvas_course_id, time, final_grade, grade, major1, major2, major3, major4, major_minor, major_combo) %>% 
  filter(!final_grade %in% c('P', 'NP', 'NR', 'W', 'I', 'IP', 'UR')) %>% 
  # filter(grade != -2, grade != 4.1, grade != -0.1, grade != -3.0, grade != -5.0, grade!= -6.0, grade != -4.0) %>% #Should delete NR
  group_by(mellon_id, canvas_course_id, time) %>% 
  summarise(
    grade = mean(grade, na.rm=T),
    major1 = first(major1),
    major2 = first(major2),
    major3 = first(major3),
    major4 = first(major4),
    major_minor = first(major_minor),
    major_combo = first(major_combo),
    .groups = "drop"
  )

major_delete_duplicates %>% 
  filter(major1 == 'CRIMINOLOGY, LAW AND SOCIETY') %>% 
  count(grade)

major_delete_duplicates2 %>% 
  filter(major1 == 'POLITICAL SCIENCE') %>% 
  count(grade)

major_delete_duplicates2 %>% 
  filter(major1 == "BIOLOGICAL SCIENCES") %>% 
  filter(is.na(major2), is.na(major3), is.na(major4), is.na(major_minor))



selected_df <-  
  df %>% 
  mutate(mellon_id = paste0("stu_", mellon_id),
         canvas_course_id = paste0("course_", canvas_course_id)) %>%
  distinct(mellon_id, canvas_course_id, birth_year, birth_month, female, urm, int_student, black, asian, hispanic,
           low_income, low_income_desc, first_generation, sat_total_score, act_total_score, uc_total_score,
           term_code, term_desc, num_stu, avg_assignment_scores, sum_assignment_scores, avg_late_hours,
           rank, procra_rank)

major_delete_duplicates2 %>% 
  left_join(selected_df, by = c("mellon_id", "canvas_course_id")) %>% 
  write_csv("./cleaned_data/final_dataset.csv")






long_major_df <- major_delete_duplicates %>% 
  pivot_longer(cols = c(major1, major2, major3, major4, major_minor), names_to = "major_type", values_to = "major") %>% 
  filter(!is.na(major)) %>% 
  mutate(major = gsub("[^A-Za-z0-9]", "_", major)) %>% 
  mutate(grade = as.numeric(grade)) %>% 
  distinct(mellon_id, canvas_course_id, time, grade, major)

long_major_df %>% 
  distinct(grade) %>% 
  view()

major_delete_duplicates %>% 
  summarise(median = median(grade))
## Median of Course grades: 3.7


major_delete_duplicates %>% 
  distinct(grade) %>% 
  view()
