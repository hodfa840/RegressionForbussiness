play_game <- function() {
  # Initialize boxes with one containing 2$, the other one is empty
  boxes <- c(2, 0)
  
  # Pick a box at random
  #pick <- sample(boxes, 1)
  pick = c(0)
  # Initialize round count
  round <- 1
  
  # Keep playing until non-empty box is picked
  while (pick == 0) {
    round <- round+1
    # Re-arrange boxes randomly
    boxes <- sample(boxes)
    # Double the value of the non-empty box
    boxes[boxes != 0] <- boxes[boxes != 0] * 2
    # Pick a box at random
    pick <- sample(boxes, 1)
  }
  
  # Return the number of rounds played and the value of the box picked in a dataframe
  return(data.frame(round = round, value = pick))
}

game_simulation <- function(n) {
  # Run the game n times
  results <- replicate(n, play_game(), simplify = FALSE)
  
  # convert the results to dataframe
  results_df <- data.frame(round = unlist(lapply(results, "[[", "round")), 
                           value = unlist(lapply(results, "[[", "value")),
                           stringsAsFactors = FALSE,
                           col.names = c("rounds", "value"))
  return(results_df)
}
set.seed(1234)
results_df <- game_simulation(100000)


# Calculate the median and average of of the values
median_values <- median(results_df$value, na.rm = TRUE)
average_values <- mean(results_df$value,na.rm = TRUE)

# Print the median of the values
print(paste("Median of values:", median_values))

# Print the median and average of the rounds
print(paste("Median of rounds:", median_rounds))
print(paste("Average of rounds:", average_rounds))

dev.new()
#barplot

# Set the layout of the plots on the page
par(mfrow = c(1, 2))

# Create the first bar plot
barplot(table(sort(results_df$round)), 
        main = "Frequency of Rounds",
        xlab = "Round",
        ylab = "Frequency",
        col = "blue",
        las = 2)

# Create the second bar plot
barplot(sort(table(results_df$value),decreasing = TRUE),
        main = "Frequency of Values",
        xlab = "Value",
        ylab = "Frequency",
        col = "red",
        las = 2)




# Calculate summary statistics for the rounds column
rounds_summary <- summary(results_df$round)

# Print the summary statistics for the rounds column
print(rounds_summary)

# Calculate summary statistics for the value column
value_summary <- summary(results_df$value)

# Print the summary statistics for the value column
print(value_summary)

quantile(results_df$round)
quantile(results_df$value)


