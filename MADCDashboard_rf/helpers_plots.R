## Helper functions for plots

# # # # # 
## Define helper functions ----

pie_graph_fast <- function(data, 
                           condx, 
                           dx = NULL,
                           combn_vctr,
                           combn_vctr_rgx) {
  # print(combn_vctr)
  par(mar = rep(0, 4))
  combn_vctr[1] <- "remaining"
  select_condx_combn_cnt <- c()
  select_condx_combn_rows <- list()
  
  if (!is.null(dx)) {
    data_cp <- data[data$uds_dx_der == dx, ]
  } else {
    data_cp <- data
  }
  dx_sum <- nrow(data_cp)
  
  for (i in length(combn_vctr_rgx):1) {
    select_condx_combn_rows[[i]] <-
      stringr::str_detect(string = data_cp$condx_combn_name,
                          pattern = combn_vctr_rgx[i])
    select_condx_combn_cnt[i] <- sum(select_condx_combn_rows[[i]])
    data_cp <- data_cp[!select_condx_combn_rows[[i]], ]
  }
  
  # print("rows")
  # print(select_condx_combn_rows)
  # print("count")
  # print(select_condx_combn_cnt)
  # print("----")
  # print(select_condx_combn_cnt[as.logical(select_condx_combn_cnt)])
  
  pie(x = select_condx_combn_cnt[as.logical(select_condx_combn_cnt)],
      labels = 
        sprintf("%.2f", 
                select_condx_combn_cnt[as.logical(select_condx_combn_cnt)] / 
                dx_sum),
      col = rainbow(
        n = length(
          select_condx_combn_cnt[as.logical(select_condx_combn_cnt)]), s = 0.7),
      clockwise = TRUE,
      init.angle = 180)
  legend("topleft",
         legend = 
           paste0(combn_vctr[as.logical(select_condx_combn_cnt)], " n=",
                  select_condx_combn_cnt[as.logical(select_condx_combn_cnt)]),
         fill = rainbow(
           n = length(
             select_condx_combn_cnt[as.logical(select_condx_combn_cnt)]), s = 0.7),
         cex = 1, bty = "n"
  )
}









