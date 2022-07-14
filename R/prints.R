
print_infos <- function(infos){
  res <- lapply(infos, function(l){
    c(setNames(unlist(l[which(!names(l) %in% c("wgt", "anzahl", "fund_info"))]),names(l[which(!names(l) %in% c("wgt", "anzahl", "fund_info"))])), l$fund_info) %>% 
      round(.,6)
  })
  cbind("date"=names(res), bind_rows(res))
}



