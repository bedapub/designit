samples <- data.frame(sampId = 1:3, sampName = letters[1:3])
samples

bc <- BatchContainer$new(dimensions = c("row" = 3, "column" = 2))
bc

set.seed(42)
# assigns samples randomly
bc <- assign_random(bc, samples)
bc$get_samples()

# assigns samples in order
bc <- assign_in_order(bc)
bc$get_samples()
