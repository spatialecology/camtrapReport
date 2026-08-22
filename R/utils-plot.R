.basic_corrplot <- function(x, main = "Species Co-occurrence") {
  x[upper.tri(x, diag = TRUE)] <- NA
  
  .colors <- grDevices::colorRampPalette(c("red", "white", "blue"))(100)
  
  graphics::image(
    seq_len(ncol(x)),
    seq_len(nrow(x)),
    t(x[rev(seq_len(nrow(x))), , drop = FALSE]),
    col = .colors,
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = main
  )
  
  labels <- colnames(x)
  n <- length(labels)
  
  graphics::text(
    x = 1:n,
    y = graphics::par("usr")[3] - 0.5,
    labels = labels,
    srt = 45,
    adj = 1,
    xpd = TRUE
  )
  
  graphics::text(
    x = graphics::par("usr")[1] - 0.5,
    y = 1:n,
    labels = rev(labels),
    srt = 45,
    adj = 1,
    xpd = TRUE
  )
  
  graphics::legend(
    x = n / 1.2,
    y = n,
    legend = round(seq(-1, 1, length.out = 10), 2),
    fill = grDevices::colorRampPalette(c("red", "white", "blue"))(10),
    border = NA,
    bty = "n",
    y.intersp = 1,
    cex = 0.8
  )
}

#--------



#--------
