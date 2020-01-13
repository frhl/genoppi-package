


#Gradient for user upload color
separate_to_groups_for_color <- function(vp_data, color_data, color_theme, type){
  # color_data <- color_data[complete.cases(color_data),]
  # color_data <- as.numeric(color_data$score)
  if(type == "cont"){
    color_data$score_rounded <- round(as.numeric(color_data$score), 1)
    col <- c(color = colorRampPalette(brewer.pal(11, color_theme))(((max(color_data$score_rounded)-min(color_data$score_rounded))*10 + 1)))
    unique_col <- data.frame(cbind("unique_score"=seq(min(color_data$score_rounded), max(color_data$score_rounded), by=0.1), col))
    color_d <- merge(unique_col, color_data, by.x = "unique_score", by.y = "score_rounded")
  } else {
    unique_score <- unique(sort(color_data$score))
    col <- c(color = colorRampPalette(brewer.pal(3, color_theme))(length(unique_score)))
    unique_col <- data.frame(cbind(unique_score, col))
    color_d <- merge(unique_col, color_data, by.x = "unique_score", by.y = "score")
  }
  vp_data$score <- color_data$score[match(vp_data$gene, color_data$gene)]
  vp_data$score[is.na(vp_data$score)] <- "empty"
  yes_exist <- subset(vp_data, score != "empty")
  no_exist <- subset(vp_data, score == "empty")

  yes_exist$col <- color_d$col[match(yes_exist$gene, color_d$gene)]
  df1 <- yes_exist

  list(df1=df1, no_exist=no_exist)
}

#Discrete for user upload color
# separate_to_groups_for_color_discrete <- function(vp_data, color_data, color_theme, type){
#   unique_score <- unique(sort(color_data$score))
#   col <- c(color = colorRampPalette(brewer.pal(3, color_theme))(length(unique_score)))
#   unique_col <- data.frame(cbind(unique_score, col))
#   color_d <- merge(unique_col, color_data, by.x = "unique_score", by.y = "score")
#
#   vp_data$score <- color_data$score[match(vp_data$gene, color_data$gene)]
#   vp_data$score[is.na(vp_data$score)] <- "empty"
#   yes_exist <- subset(vp_data, score != "empty")
#   no_exist <- subset(vp_data, score == "empty")
#
#   yes_exist$col <- color_d$col[match(yes_exist$gene, color_d$gene)]
#   df1 <- yes_exist
#
#   list(df1=df1, no_exist=no_exist)
# }

#Marker colors based on FDR
separate_to_groups_for_color_integrated <- function(vp_data, threshold, col1, col2){
  below_thresh <- subset(vp_data, FDR < threshold)
  above_thresh <- subset(vp_data, FDR >= threshold)
  a_col <- colorRampPalette(c(col1, col1))(nrow(below_thresh))
  below_thresh <- cbind(below_thresh, rev(a_col))
  names(below_thresh)[names(below_thresh) == 'rev(a_col)'] <- 'col'
  b_col <- colorRampPalette(c(col2, col2))(nrow(above_thresh))
  above_thresh <- cbind(above_thresh, b_col)
  names(above_thresh)[names(above_thresh) == 'b_col'] <- 'col'
  data <- rbind(below_thresh, above_thresh)
}
# separate_to_groups_for_color_integrated <- function(vp_data, threshold){
#   below_thresh <- subset(vp_data, FDR < threshold)
#   above_thresh <- subset(vp_data, FDR >= threshold)
#   below_thresh$col <- "sig"
#   above_thresh$col <- "insig"
#   # a_col <- colorRampPalette(c(col1, col1))(nrow(below_thresh))
#   # below_thresh <- cbind(below_thresh, rev(a_col))
#   # names(below_thresh)[names(below_thresh) == 'rev(a_col)'] <- 'col'
#   # b_col <- colorRampPalette(c(col2, col2))(nrow(above_thresh))
#   # above_thresh <- cbind(above_thresh, b_col)
#   # names(above_thresh)[names(above_thresh) == 'b_col'] <- 'col'
#   data <- rbind(below_thresh, above_thresh)
# }

#Blue and green separate, specifically in case plot opacity is low
# separate_to_groups_for_color_integrated_bar <- function(vp_data, threshold){
#   below_thresh <- subset(vp_data, FDR <= threshold)
#   above_thresh <- subset(vp_data, FDR > threshold)
#   #
#   a_col <- colorRampPalette(c('seagreen3', 'seagreen3'))(nrow(below_thresh))
#   below_thresh <- cbind(below_thresh, rev(a_col))
#   names(below_thresh)[names(below_thresh) == 'rev(a_col)'] <- 'col'
#   b_col <- colorRampPalette(c("royalblue2", "royalblue2"))(nrow(above_thresh))
#   above_thresh <- cbind(above_thresh, b_col)
#   names(above_thresh)[names(above_thresh) == 'b_col'] <- 'col'
#   data <- rbind(below_thresh, above_thresh)
# }

#Two gray tones
separate_to_groups_for_cbf_integrated <- function(vp_data, threshold){
  below_thresh <- subset(vp_data, FDR < threshold)
  above_thresh <- subset(vp_data, FDR >= threshold)
  a_col <- colorRampPalette(c('#525252', '#525252'))(nrow(below_thresh))
  below_thresh <- cbind(below_thresh, rev(a_col))
  names(below_thresh)[names(below_thresh) == 'rev(a_col)'] <- 'col'
  b_col <- colorRampPalette(c("grey76", "grey76"))(nrow(above_thresh))
  above_thresh <- cbind(above_thresh, b_col)
  names(above_thresh)[names(above_thresh) == 'b_col'] <- 'col'
  data <- rbind(below_thresh, above_thresh)
}

#Two gray tones, specifically in case plot opacity is low
# separate_to_groups_for_cbf_integrated_bar <- function(vp_data, threshold){
#   below_thresh <- subset(vp_data, FDR <= threshold)
#   above_thresh <- subset(vp_data, FDR > threshold)
#   a_col <- colorRampPalette(c('#969696', '#969696'))(nrow(below_thresh))
#   below_thresh <- cbind(below_thresh, rev(a_col))
#   names(below_thresh)[names(below_thresh) == 'rev(a_col)'] <- 'col'
#   b_col <- colorRampPalette(c("#d9d9d9", "#d9d9d9"))(nrow(above_thresh))
#   above_thresh <- cbind(above_thresh, b_col)
#   names(above_thresh)[names(above_thresh) == 'b_col'] <- 'col'
#   data <- rbind(below_thresh, above_thresh)
# }

separate_to_groups_for_exac_bar <- function(vp_data){
  grp1 <- subset(vp_data, FDR <= 0.33)
  grp2 <- subset(vp_data, 0.33 < FDR & FDR <= 0.66)
  grp3 <- subset(vp_data, 0.66 < FDR & FDR <= 1)
  a_col <- colorRampPalette(c('#66c2a5', '#66c2a5'))(nrow(grp1))
  grp1 <- cbind(grp1, rev(a_col))
  names(grp1)[names(grp1) == 'rev(a_col)'] <- 'col'
  b_col <- colorRampPalette(c("#fc8d62", "#fc8d62"))(nrow(grp2))
  grp2 <- cbind(grp2, b_col)
  names(grp2)[names(grp2) == 'b_col'] <- 'col'
  c_col <- colorRampPalette(c("#8da0cb", "#8da0cb"))(nrow(grp3))
  grp3 <- cbind(grp3, c_col)
  names(grp3)[names(grp3) == 'c_col'] <- 'col'
  data <- rbind(grp1, grp2, grp3)
}
