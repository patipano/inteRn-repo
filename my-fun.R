convertVectorToTriangle <- function(vect, dimrow, dimcol) {
    n <- length(vect)
    vrow <- sapply(1:dimrow, function(x) {
      rep(x, dimcol - x + 1)
    })
    vcol <- sapply(1:dimrow, function(x) {
      1:(dimcol - x + 1)
    })
    df <- data.frame(row = unlist(vrow), col = unlist(vcol), val = vect)
    return(acast(df, row ~ col, value.var = "val"))
}


convertVectorToTriangle(1:5, 2, 3)
acast(cvtt, row ~ col, value.var = "val")
