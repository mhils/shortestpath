context("randomGraph")

test_that("randomGraph generates random graphs", {
  a <- randomGraph()
  for(i in 1:100){
    b <- randomGraph()
    if(edge_attr(a)$weight != edge_attr(b)$weight){
      expect_true(TRUE)
      break
    }
  }
})
