input <- readLines("Day 1/input.txt")


data_matrix <- do.call(rbind, strsplit(input, "\\s+"))
left_column <- sort(as.numeric(data_matrix[, 1]))
right_column <- sort(as.numeric(data_matrix[, 2]))

get_total_distance <- function(left_column, right_column) {
  distance <- 0

  for (i in seq_along(left_column)) {
    distance <- distance + abs(left_column[i] - right_column[i])
  }
  return(distance)
}

get_similarity_score <- function(left_column, right_column) {
  similarity_score <- 0
  for (i in seq_along(left_column)) {
    appearances <- sum(right_column == left_column[i])
    similarity_score <- similarity_score + left_column[i] * appearances
  }
  return(similarity_score)
}

print(paste("part 1 distance:", get_total_distance(left_column, right_column)))
print(paste("part 2 score:", get_similarity_score(left_column, right_column)))
