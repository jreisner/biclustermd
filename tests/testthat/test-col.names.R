context("column names")

test_that(
  "col.names() returns all columns and column names", {
    
    sbc <- biclustermd(synthetic)
    expect_equal(nrow(col.names(sbc)), ncol(synthetic))
    expect_equal(ncol(col.names(sbc)), 2)
    expect_equal(all(colnames(synthetic) %in% col.names(sbc)$name), TRUE)
    
  }
)