# 
# 
# ## version 1 ----
# ageg.in.list <- 
#   c(
#     '00_04', '05_09', '10_14', '15_19', '20_24', '25_29', '30_34', 
#     '35_39', '40_44', '45_49', '50_54', '55_59', '60_64', '65_69', 
#     '70_74', '75_79', '80_84', '85'
#   )
#   
# dat.tmp <- 
#   dat.in.can |>
#   filter(cob_iso3 == 'RWA', can == 'cx', ageg %in% ageg.in.list)
# 
# ncan.min <- 5
# 
# i.first <- 0
# i.cum <- 0
# ageg.out.list <- list()
# 
# for(i in seq_along(ageg.in.list)) {
#   i.count <- dat.tmp |> filter(ageg == ageg.in.list[i]) |> pull('ncan')
#   ## detect the first non null count
#   if(i.first == 0 & i.count > 0) i.first <- i
#   if(i.first > 0){
#     if(i.cum == 0) i.start <- i
#     i.cum <- i.cum + i.count
#     if(i.cum >= ncan.min) {
#       i.stop <- i
#       ageg.out.list <- c(ageg.out.list, list(c(ageg.in.list[i.start:i.stop])))
#       i.cum <- 0
#     }
#   } else {
#     ageg.out.list <- c(ageg.out.list, list(c(ageg.in.list[i])))
#   }
#   ## detect the last non null count
#   if(i.count > 0) i.last <- i
# }
# 
# ## add the potential missing cancer in latest categories
# i.max <- length(ageg.in.list)
# ageg.cur.last <- ageg.out.list |> tail(1) |> unlist() |> tail(1)
# i.cur.last <- which(ageg.in.list == ageg.cur.last)
# if(i.last > i.cur.last){
#   ageg.out.list[[length(ageg.out.list)]] <- c(ageg.out.list[[length(ageg.out.list)]], ageg.in.list[(i.cur.last + 1):i.last])
# }
# if(i.last < i.max) {
#   for(i in (i.last + 1):i.max){
#     ageg.out.list <- c(ageg.out.list, list(c(ageg.in.list[i])))
#   }
# }
# 
# ## compute the aggregated age group names
# ageg.out.list.names <- 
#   map_chr(
#     ageg.out.list, 
#     ~ {
#       a.start <- .x |> head(1) |> str_remove('_.*')
#       a.stop <- .x |> tail(1) |> str_remove('.*_')
#       if(a.start != a.stop){
#         paste0(a.start, '_', a.stop)
#       } else {
#         a.start
#       }
#     }
#   )
# names(ageg.out.list) <- ageg.out.list.names
# 
# 
# ## version 2 ----
# i.first <- i.last <- 0
# for(i in seq_along(ageg.in.list)) {
#   i.count <- dat.tmp |> filter(ageg == ageg.in.list[i]) |> pull('ncan')
#   ## detect the first non null count
#   if(i.first == 0 & i.count > 0) i.first <- i
#   ## detect the last non null count
#   if(i.count > 0) i.last <- i
# }
# 
# ## generate all combinations of age
# # chop_vector <- 
# #   function(vect.size = 3) {
# #     formal.comb.list <- list()
# #     for(i in 1:vect.size){
# #       formal.comb.list <- c(formal.comb.list, list(1:i))
# #     }
# #     
# #     all.comb <- expand.grid(formal.comb.list) |>  as.matrix() 
# #     valid.comb.id <- apply(all.comb, 1, function(.x){ .x.diff <- diff(.x); all( .x.diff <= 1 & .x.diff >= 0)})  
# #     valid.comb <- all.comb[valid.comb.id, ]
# #     valid.comb
# #   }
# 
# # chop_vector <- 
# #   function(vect.size = 3) {
# #     formal.comb.list <- list()
# #     cur.valid.comb <- dplyr::tibble(Var1 = 1)
# #     if(vect.size > 1){
# #       for(i in 2:vect.size){
# #         new.comb <- dplyr::tibble(x = 1:i)
# #         names(new.comb) <- paste0('Var', i)
# #         all.comb <- tidyr::expand_grid(cur.valid.comb, new.comb) |>  as.matrix() 
# #         cur.valid.comb.id <- apply(all.comb, 1, function(.x){ .x.diff <- diff(.x); all( .x.diff <= 1 & .x.diff >= 0)})  
# #         cur.valid.comb <- all.comb[cur.valid.comb.id, ] |> dplyr::as_tibble()
# #       }
# #     } 
# #     cur.valid.comb
# #   }
# 
# 
# # dat.tmp.02 <- dat.tmp[i.first: i.last, ]
# # vect.lenght <- i.last - i.first + 1
# # chop.vector <- chop_vector(vect.lenght) |> as.matrix()
# # 
# # dat.tmp.03 <- dat.tmp.02 |> pull('ncan')
# # 
# # split(dat.tmp.03, chop.vector)
# # valid.aggr <-
# #   apply(
# #     chop.vector, 
# #     1, 
# #     function(.x) {
# #       sum(sapply(split(dat.tmp.03, .x), function(.xx) sum(.xx) >= ncan.min))
# #     }
# #   )
# # 
# # selected.ageg.aggr.id <- c(1:(i.first - 1), chop.vector[which.max(valid.aggr), , drop = TRUE] + i.first - 1, (i.last + 1):length(ageg.in.list)) 
# # 
# # selected.ageg.aggr <- split(ageg.in.list, selected.ageg.aggr.id)
# # 
# # ageg.out.list.names <- 
# #   map_chr(
# #     selected.ageg.aggr, 
# #     ~ {
# #       a.start <- .x |> head(1) |> str_remove('_.*')
# #       a.stop <- .x |> tail(1) |> str_remove('.*_')
# #       if(a.start != a.stop){
# #         paste0(a.start, '_', a.stop)
# #       } else {
# #         a.start
# #       }
# #     }
# #   )
# # names(selected.ageg.aggr) <- ageg.out.list.names
# # 
# # ageg.aggr.vect <- purrr::map_dbl(selected.ageg.aggr, length)
# # ageg.aggr.vect.02 <- purrr::map2(names(ageg.aggr.vect), ageg.aggr.vect, ~ rep(.x, .y)) |> unlist()
# # 
# # dat.tmp.04 <- 
# #   dat.tmp |> 
# #   mutate(ageg.aggr = ageg.aggr.vect.02) |> 
# #   group_by(ageg.aggr) |> 
# #   summarise(ncan = sum(ncan))
# # 
# # aggregate(dat.tmp |> pull('ncan'), by = selected.aggr, sum)
# 
# 
