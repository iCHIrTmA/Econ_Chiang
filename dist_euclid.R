dist_euclid <-
function(point_a, point_b){
    diffs <- (point_a - point_b);
    sqr.diffs <- diffs^2
    sum.diffs <- sum(sqr.diffs);
    sqrt(sum.diffs)}
