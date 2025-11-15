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



#task 3
points <- data.frame(
  id = 1:10,
  x  = c( 60, 180,  80, 140,  20, 100, 200, 140,  40, 100),
  y  = c(200, 200, 180, 180, 160, 160, 160, 140, 120, 120)
)

points


compute_base_distance <- function(pts) {
  n <- nrow(pts)
  d <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        dx <- pts$x[i] - pts$x[j]
        dy <- pts$y[i] - pts$y[j]
        d[i, j] <- sqrt(dx^2 + dy^2)  # Euclidean distance
      }
    }
  }
  d
}

base_dist <- compute_base_distance(points)
base_dist



set.seed(351)  

n <- nrow(points)


traffic_factor <- matrix(runif(n * n, min = 0.8, max = 1.4), nrow = n)
traffic_factor <- (traffic_factor + t(traffic_factor)) / 2  # 保持对称


weather_factor <- matrix(runif(n * n, min = 0.9, max = 1.3), nrow = n)
weather_factor <- (weather_factor + t(weather_factor)) / 2



blocked <- matrix(FALSE, nrow = n, ncol = n)


blocked[2, 7] <- FALSE; blocked[7, 2] <- FALSE
blocked[3, 8] <- FALSE; blocked[8, 3] <- TRUE


BIG <- 1e9  

cost_mat <- base_dist * traffic_factor * weather_factor
cost_mat[blocked] <- BIG


cost_mat


path_exists <- function(u, v, adj) {
  n <- length(adj)
  visited <- rep(FALSE, n)
  stack <- c(u)
  visited[u] <- TRUE
  
  while (length(stack) > 0) {
    cur <- tail(stack, 1)
    stack <- head(stack, -1)
    
    if (cur == v) return(TRUE)
    
    for (nb in adj[[cur]]) {
      if (!visited[nb]) {
        visited[nb] <- TRUE
        stack <- c(stack, nb)
      }
    }
  }
  FALSE
}

cheapest_link_tour <- function(cost_mat) {
  n <- nrow(cost_mat)
  
  
  edges <- data.frame(i = integer(0), j = integer(0), w = numeric(0))
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      w <- cost_mat[i, j]
      if (w < 1e8) {  
        edges <- rbind(edges, data.frame(i = i, j = j, w = w))
      }
    }
  }
  edges <- edges[order(edges$w), ]  
  

  
  deg  <- integer(n)          
  adj  <- vector("list", n)   
  sel  <- data.frame(i = integer(0), j = integer(0)) 
  
  for (k in 1:nrow(edges)) {
    i <- edges$i[k]
    j <- edges$j[k]
    
 
    if (deg[i] == 2 || deg[j] == 2) next
    
    if (nrow(sel) < n - 1) {
      
      if (path_exists(i, j, adj)) next
      
     
      sel  <- rbind(sel, c(i, j))
      adj[[i]] <- c(adj[[i]], j)
      adj[[j]] <- c(adj[[j]], i)
      deg[i] <- deg[i] + 1
      deg[j] <- deg[j] + 1
      
    } else {
     
      if (deg[i] == 1 && deg[j] == 1) {
        sel  <- rbind(sel, c(i, j))
        adj[[i]] <- c(adj[[i]], j)
        adj[[j]] <- c(adj[[j]], i)
        deg[i] <- deg[i] + 1
        deg[j] <- deg[j] + 1
        break
      }
    }
  }
 
  start <- 1
  tour  <- c(start)
  prev  <- NA
  cur   <- start
  
  repeat {
    nbrs <- adj[[cur]]
    
   
    if (is.na(prev)) {
      next_candidates <- nbrs
    } else {
      next_candidates <- nbrs[nbrs != prev]
    }
    
    if (length(next_candidates) == 0) break
    nxt  <- next_candidates[1]
    
    prev <- cur
    cur  <- nxt
    if (cur == start) break
    tour <- c(tour, cur)
  }
  
 
  tour <- c(tour, start)
  

  total_dist <- 0
  for (t in 1:(length(tour) - 1)) {
    total_dist <- total_dist + cost_mat[tour[t], tour[t + 1]]
  }
  
  list(
    tour           = tour,          
    total_distance = total_dist,    
    selected_edges = sel
  )
}



result <- cheapest_link_tour(cost_mat)


tour_points <- result$tour  


path_x <- points$x[tour_points]
path_y <- points$y[tour_points]


plot(points$x, points$y,
     pch = 19,
     col = "blue",
     cex = 1.5,
     xlab = "X",
     ylab = "Y",
     )


text(points$x, points$y, labels = points$id, pos = 3)

lines(path_x, path_y, col = "red", lwd = 2)

points(path_x[1], path_y[1], pch = 21, bg = "yellow", cex = 2)

library(graphics)
arrows(path_x[-length(path_x)], path_y[-length(path_y)],
       path_x[-1],             path_y[-1],
       length = 0.1, col = "red")

cat("Hamiltonian tour (order of points):\n")
print(result$tour)

cat("\nTotal distance/cost of round trip (with traffic, weather & closures):\n")
print(result$total_distance)

cat("\nSelected edges (i, j):\n")
print(result$selected_edges)
                                         
