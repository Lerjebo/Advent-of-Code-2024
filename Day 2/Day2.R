input <- readLines("Day 2/input.txt")
data_matrix <- lapply(strsplit(input, "\\s+"), function(x) as.integer(x))


safe_check_count <- function(data_matrix) {
  count <- 0

  for (row in seq_len(length(data_matrix))) {
    current_row <- data_matrix[[row]]

    differences <- diff(current_row)

    is_increasing <- all(differences > 0) && all(differences <= 3)
    is_decreasing <- all(differences < 0) && all(differences >= -3)

    if (is_increasing || is_decreasing) {
      count <- count + 1
    }
  }
  return(count)
}

safe_check_count_one_fault <- function(data_matrix) {
  count <- 0
  for (row in seq_len(length(data_matrix))) {
    current_row <- data_matrix[[row]]

    differences <- diff(current_row)

    is_increasing <- all(differences > 0) && all(differences <= 3)
    is_decreasing <- all(differences < 0) && all(differences >= -3)

    if (!is_increasing && !is_decreasing) {
      for (element in seq_len(length(current_row))) {
        new_row <- current_row[-element]
        differences <- diff(new_row)
        is_increasing <- all(differences > 0) && all(differences <= 3)
        is_decreasing <- all(differences < 0) && all(differences >= -3)
        if (is_increasing || is_decreasing) {
          break
        }
      }
    }
    if (is_increasing || is_decreasing) {
      count <- count + 1
    }
  }
  return(count)
}

print(paste("part 1: ", safe_check_count(data_matrix)))
print(paste("part 2: ", safe_check_count_one_fault(data_matrix)))
