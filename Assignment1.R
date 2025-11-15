# Task 1

q1_path <- "Data/Q1"
q1_files <- list.files(q1_path, pattern = "^Q1_.*\\.txt$", full.names = TRUE)

course_codes <- sub("^Q1_(.*)\\.txt$", "\\1", basename(q1_files))

student_lists <- lapply(q1_files, function(f) readLines(f, warn = FALSE))
names(student_lists) <- course_codes

enrollment_counts <- sapply(student_lists, length)
max_n <- max(enrollment_counts)
min_n <- min(enrollment_counts)

courses_max <- names(enrollment_counts[enrollment_counts == max_n])
courses_min <- names(enrollment_counts[enrollment_counts == min_n])

cat("[1a] Courses with highest number of students:",
    paste(courses_max, collapse = ", "), "with", max_n, "students\n")
cat("[1a] Course(s) with lowest number of students :",
    paste(courses_min, collapse = ", "), "with", min_n, "students\n")

all_students <- unlist(student_lists)
distinct_students <- unique(all_students)
cat("[1b] Total number of distinct students:", length(distinct_students), "\n")

find_courses_by_student <- function(student_name) {
  courses <- names(student_lists)[sapply(student_lists,
                                         function(x) student_name %in% x)]
  if (length(courses) == 0) {
    cat("[1c] Student", student_name,
        "is not registered for any course.\n")
  } else {
    cat("[1c] Student", student_name,
        "is registered for:", paste(courses, collapse = ", "), "\n")
  }
}

find_courses_by_student("NAME001")

cds512 <- student_lists[["CDS512"]]
cds521 <- student_lists[["CDS521"]]

both_512_521 <- intersect(cds512, cds521)
cat("[1d] Students in CDS512 AND CDS521:", paste(both_512_521, collapse = ", "), "\n")

only_512 <- setdiff(cds512, cds521)
cat("[1e] Students in CDS512 but NOT CDS521:", paste(only_512, collapse = ", "), "\n")

only_521 <- setdiff(cds521, cds512)
cat("[1f] Students in CDS521 but NOT CDS512:", paste(only_521, collapse = ", "), "\n")

in_512_or_521 <- union(cds512, cds521)
not_512_nor_521 <- setdiff(distinct_students, in_512_or_521)
cat("[1g] Students NOT in CDS512 and NOT in CDS521:", paste(not_512_nor_521, collapse = ", "), "\n")

student_course_counts <- table(all_students)
students_3_courses <- names(student_course_counts[student_course_counts == 3])

students_3_courses_results <- if(length(students_3_courses) > 0) students_3_courses else "None"

cat("[1h] Students registered for exactly three courses:", paste(students_3_courses_results, collapse = ", "), "\n")



# Task 2

q2_path <- "Data/Q2"
q2_files <- list.files(q2_path, pattern = "^Q2_Part_.*\\.txt$", full.names = TRUE)

q2_texts <- sapply(q2_files, function(f) {
  paste(readLines(f, warn = FALSE), collapse = " ")
})
full_text <- paste(q2_texts, collapse = " ")

count_word <- function(word, text) {
  pattern <- paste0("\\b", word, "\\b")
  matches <- gregexpr(pattern, tolower(text), ignore.case = TRUE)
  if (matches[[1]][1] == -1) {
    return(0)
  } else {
    return(length(matches[[1]]))
  }
}

words_target <- c("analytics", "insight", "of")
counts <- sapply(words_target, count_word, text = full_text)

cat("[2a] Total occurrences across all 10 files:", paste(names(counts), counts, sep = ": ", collapse = ", "), "\n")


clean_text <- tolower(full_text)
clean_text <- gsub("[^a-z]+", " ", clean_text)
word_vec <- unlist(strsplit(clean_text, "\\s+"))
word_vec <- word_vec[word_vec != ""]

word_freq <- table(word_vec)
word_freq_sorted <- sort(word_freq, decreasing = TRUE)
top_10 <- head(word_freq_sorted, 10)

cat("[2b] Top 10 most frequent words:", paste(names(top_10), top_10, sep = ": ", collapse = ", "), "\n")


                  
