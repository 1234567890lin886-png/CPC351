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


 task3
points <- data.frame(
  id = 1:10,
  x  = c( 60, 180,  80, 140,  20, 100, 200, 140,  40, 100),
  y  = c(200, 200, 180, 180, 160, 160, 160, 140, 120, 120)
)

num_points <- nrow(points)
print(points)

euclidean_distance <- function(p1, p2) {
  sqrt((p1$x - p2$x)^2 + (p1$y - p2$y)^2)
}

base_dist <- matrix(0, nrow = num_points, ncol = num_points)

for (i in 1:num_points) {
  for (j in 1:num_points) {
    if (i != j) {
      base_dist[i, j] <- euclidean_distance(points[i, ], points[j, ])
    }
  }
}

cat("\nBase distance matrix (Euclidean):\n")
print(round(base_dist, 2))


set.seed(351)  

n <- num_points
BIG <- 1e9    
traffic_factor <- matrix(runif(n * n, min = 0.8, max = 1.4), nrow = n)



weather_factor <- matrix(runif(n * n, min = 0.9, max = 1.3), nrow = n)



blocked <- matrix(FALSE, nrow = n, ncol = n)


blocked[2, 7] <- FALSE; blocked[7, 2] <- FALSE
blocked[3, 8] <- FALSE; blocked[8, 3] <- TRUE


cost_sel  <- base_dist
cost_eval <- base_dist * traffic_factor * weather_factor


cost_sel[blocked]  <- BIG
cost_eval[blocked] <- BIG



cat("\nCost matrix for evaluation (with traffic + weather + closures):\n")
print(round(cost_eval, 2))

find_component <- function(node, components) {
  for (i in seq_along(components)) {
    if (node %in% components[[i]]) {
      return(i)
    }
  }
  return(NULL)
}

cheapest_link_tour_components <- function(cost_sel, cost_eval) {
  n <- nrow(cost_sel)
  
  edges <- data.frame(from = integer(), to = integer(), cost = double())
  
  if (n > 1) {
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        w <- cost_sel[i, j]
        if (w < 1e8) {  # 排除封路的 BIG 边
          edges <- rbind(edges, data.frame(from = i, to = j, cost = w))
        }
      }
    }
  }
  
  edges <- edges[order(edges$cost), ]
  
  tour_edges <- data.frame(from = integer(), to = integer(), cost = double())
  degrees    <- rep(0, n)
  components <- as.list(1:n) 
  
  for (k in 1:nrow(edges)) {
    edge <- edges[k, ]
    u    <- edge$from
    v    <- edge$to
    
    if (degrees[u] >= 2 || degrees[v] >= 2) next
    
    comp_u <- find_component(u, components)
    comp_v <- find_component(v, components)
    
    if (!is.null(comp_u) && !is.null(comp_v) && comp_u != comp_v) {
      tour_edges <- rbind(tour_edges, edge)
      degrees[u] <- degrees[u] + 1
      degrees[v] <- degrees[v] + 1
      
      merged <- c(components[[comp_u]], components[[comp_v]])
      components <- components[-c(comp_u, comp_v)]
      components <- append(components, list(merged))
    }

    if (nrow(tour_edges) == n - 1) break
  }
  
  endpoints <- which(degrees == 1)
  if (length(endpoints) != 2) {
    warning("Endpoints not equal to 2, something may be wrong in construction.")
  } else {
    u <- endpoints[1]
    v <- endpoints[2]
    final_cost_sel  <- cost_sel[u, v]
    final_cost_eval <- cost_eval[u, v]
    
    if (final_cost_sel >= 1e8) {
      warning("Final edge is blocked or extremely expensive; cannot close tour properly.")
    } else {
      final_edge <- data.frame(from = u, to = v, cost = final_cost_sel)
      tour_edges <- rbind(tour_edges, final_edge)
      degrees[u] <- degrees[u] + 1
      degrees[v] <- degrees[v] + 1
    }
  }
  
  start_node    <- tour_edges$from[1]
  current_node  <- tour_edges$to[1]
  tour_path     <- c(start_node, current_node)
  remaining_edges <- tour_edges[-1, ]
  
  while (length(tour_path) < n) {
    found_next <- FALSE
    for (i in 1:nrow(remaining_edges)) {
      e_from <- remaining_edges$from[i]
      e_to   <- remaining_edges$to[i]
      
      if (e_from == current_node && !(e_to %in% tour_path)) {
        current_node <- e_to
        tour_path    <- c(tour_path, current_node)
        remaining_edges <- remaining_edges[-i, ]
        found_next <- TRUE
        break
      } else if (e_to == current_node && !(e_from %in% tour_path)) {
        current_node <- e_from
        tour_path    <- c(tour_path, current_node)
        remaining_edges <- remaining_edges[-i, ]
        found_next <- TRUE
        break
      }
    }
    if (!found_next) break 
  }
  
  tour_path <- c(tour_path, tour_path[1])
  
  total_cost <- 0
  for (k in 1:(length(tour_path) - 1)) {
    i <- tour_path[k]
    j <- tour_path[k + 1]
    total_cost <- total_cost + cost_eval[i, j]
  }
  
  list(
    tour_path      = tour_path,       
    tour_edges_sel = tour_edges,     
    total_cost     = total_cost       
  )
}


result <- cheapest_link_tour_components(cost_sel, cost_eval)

cat("\n--- Cheapest Link Algorithm Results ---\n")
cat("Final Tour Path (IDs):", paste(result$tour_path, collapse = " -> "), "\n")
cat("Total Tour Distance (Cost with traffic + weather + closures):",
    result$total_cost, "\n")

cat("\nSelected edges (using base distance for selection):\n")
print(result$tour_edges_sel)

tour_points <- result$tour_path
path_x <- points$x[tour_points]
path_y <- points$y[tour_points]

plot(points$x, points$y,
     pch = 19, col = "blue", cex = 1.5,
     xlab = "X", ylab = "Y",
     main = "Hamiltonian Tour (Cheapest Link with Traffic/Weather/Road Closures)")
text(points$x, points$y, labels = points$id, pos = 3)

lines(path_x, path_y, col = "red", lwd = 2)

points(path_x[1], path_y[1], pch = 21, bg = "yellow", cex = 2)

arrows(path_x[-length(path_x)], path_y[-length(path_y)],
       path_x[-1],             path_y[-1],
       length = 0.1, col = "red")
                                         
