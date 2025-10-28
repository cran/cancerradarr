#' Generate all the possible combinations of slices in a chopped vector
#'
#' @param vect.size int, the size of the vector
#'
#' @return a matrix containing all the possible slices to chope a vector per line
#' @export
#'
#' @examples
#' chop_vector(3)
#'
chop_vector <-
  function(vect.size = 3) {
    formal.comb.list <- list()
    cur.valid.comb <- dplyr::tibble(Var1 = 1)
    if (vect.size > 1) {
      for (i in 2:vect.size) {
        new.comb <- dplyr::tibble(x = 1:i)
        names(new.comb) <- paste0('Var', i)
        all.comb <- tidyr::expand_grid(cur.valid.comb, new.comb) |> as.matrix()
        cur.valid.comb.id <- apply(all.comb, 1, function(.x) {
          .x.diff <- diff(.x)
          all(.x.diff <= 1 & .x.diff >= 0)
        })
        cur.valid.comb <- all.comb[cur.valid.comb.id, ] |> dplyr::as_tibble()
      }
    }
    cur.valid.comb |> as.matrix()
  }
