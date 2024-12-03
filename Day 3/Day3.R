input <- paste(readLines("Day 3/input.txt"), collapse = "")

extract_mul_numbers <- function(text) {
  total_numbers <- 0


  pattern <- "mul\\((\\d+),(\\d+)\\)"
  matches <- gregexpr(pattern, text, perl = TRUE)
  numbers <- regmatches(text, matches)[[1]]
  numbers <- gsub("mul\\(|\\)", "", numbers)
  number_pairs <- strsplit(numbers, ",")
  total_numbers <- sum(sapply(number_pairs, function(x) {
    as.integer(x[1]) * as.integer(x[2])
  }))
  return(total_numbers)
}

extract_instructions <- function(text) {
  total_numbers <- 0

  remove_instructions_pattern <- "don't\\(\\).*?do\\(\\)"
  cleaned_text <- gsub(remove_instructions_pattern, "", text, perl = TRUE)

  pattern <- "mul\\((\\d+),(\\d+)\\)"
  matches <- gregexpr(pattern, cleaned_text, perl = TRUE)
  numbers <- regmatches(cleaned_text, matches)[[1]]
  numbers <- gsub("mul\\(|\\)", "", numbers)
  number_pairs <- strsplit(numbers, ",")
  total_numbers <- sum(sapply(number_pairs, function(x) {
    as.integer(x[1]) * as.integer(x[2])
  }))
  return(total_numbers)
}

print(paste("Part 1: ", extract_mul_numbers(input)))
print(paste("Part 2: ", extract_instructions(input)))
