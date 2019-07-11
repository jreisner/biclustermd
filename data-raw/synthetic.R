set.seed(29)
A <- matrix(1:6, nrow = 2, ncol = 3)
B <- matrix(5, nrow = 3, ncol = 4)
synthetic <- kronecker(A, B)
rownames(synthetic) <- paste0("R", 1:6)
colnames(synthetic) <- paste0("C", 1:12)

synthetic[sample(1:length(synthetic), 0.5 * length(synthetic))] <- NA

shuffled_rows <- sample(1:nrow(synthetic), nrow(synthetic))
shuffled_cols <- sample(1:ncol(synthetic), ncol(synthetic))

synthetic <- synthetic[shuffled_rows, shuffled_cols]
