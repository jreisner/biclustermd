context("row names")

test_that(
  "row.names.biclustermd() returns all rows and row names", {
    
    sbc <- biclustermd(synthetic)
    expect_equal(nrow(row.names(sbc)), nrow(synthetic))
    expect_equal(ncol(row.names(sbc)), 2)
    expect_equal(all(row.names(synthetic) %in% row.names(sbc)$name), TRUE)
    
  }
)