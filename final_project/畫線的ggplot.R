decisionplot_ggplot <- function(model, data, class = NULL, predict_type = "class",
                                resolution = 100, showgrid = TRUE, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  cn <- colnames(data)
  
  k <- length(unique(cl))
  
  data$pch <- data$col <- as.integer(cl) + 1L
  gg <- ggplot(aes_string(cn[1], cn[2]), data = data) + 
    geom_point(aes_string(col = 'as.factor(col)', shape = 'as.factor(col)'), size = 3)
  
  # make grid
  r <- sapply(data[, 1:2], range, na.rm = TRUE)
  xs <- seq(r[1, 1], r[2, 1], length.out = resolution)
  ys <- seq(r[1, 2], r[2, 2], length.out = resolution)
  
  g <- cbind(rep(xs, each = resolution), 
             rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  g$col <- g$pch <- as.integer(as.factor(p)) + 1L
  
  if(showgrid) 
    gg <- gg + geom_point(aes_string(x = cn[1], y = cn[2], col = 'as.factor(col)'), data = g, shape = 20, size = 0.005)
  
   gg + geom_contour(aes_string(x = cn[1], y = cn[2], z = 'col'), data = g, inherit.aes = TRUE,col="BLACK",binwidth = 1)
}

