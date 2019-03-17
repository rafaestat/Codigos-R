

grafico <- function(data, y, pred){
  is.L <- is.logical(y)
  y <- as.logical(y)
  predicts <- if(is.data.frame(pred)) pred else data[, pred, drop = FALSE]
  e <- lapply(predicts, function(x) if(is.logical(x)) x == y else (x >= 0.5) == y)
  
  evl <- data.frame(row.names = names(predicts))
  evl$Accuracy <- unlist(lapply(e, function(x) mean(x)))
  evl <- evl[order(evl$Accuracy, decreasing = TRUE), , drop = FALSE]
  
  cf <- lapply(predicts, function(x) {
    if (!is.logical(x)) {
      x <- x >= 0.5
    }
    if (is.L) {
      Actual <- y
      Predict <- x
    } else {
      Actual <- as.integer(y)
      Predict <- as.integer(x)
    }
    xtabs(~ Predict + Actual)
  })
  
  charts <- list()
  g <- lapply(names(predicts), function(x) {
    od <- order(predicts[, x], decreasing = TRUE)
    data.frame(p = (1:length(y)) / length(y) * 100, gain = cumsum(y[od]) / sum(y) * 100, model = x)
  })
  gdf <- do.call(rbind, g)
  charts$Gains <- lattice::xyplot(x = gain ~ p, data = gdf, type = "l", auto.key = list(space = "right"), groups = model,
                                  panel = function(...) {lattice::panel.segments(x0 = 0, y0 = 0, x1 = 100, y1 = 100, col = "gray"); lattice::panel.xyplot(...)},
                                  xlim = extendrange(c(0, 100)), ylim = extendrange(c(0, 100)), xlab = "Top Percentile (%)", ylab = "Coverage of Responses (%)", main = "Gains Chart")
  g <- lapply(names(predicts), function(x) {
    od <- order(predicts[, x], decreasing = TRUE)
    data.frame(p = (1:length(y)) / length(y) * 100, response = cumsum(y[od]) / 1:length(y) * 100, cs = cumsum(y[od]), model = x)
  })
  gdf <- do.call(rbind, g)
  base <- mean(y) * 100
  charts$Response <- lattice::xyplot(x = response ~ p, data = gdf, type = "l", auto.key = list(space = "right"), groups = model,
                                     panel = function(...) {lattice::panel.segments(x0 = 0, y0 = base, x1 = 100, y1 = base, col = "gray"); lattice::panel.xyplot(...)},
                                     xlim = extendrange(c(0, 100)), xlab = "Top Percentile (%)", ylab = "Cumulative Response Rate (%)", main = "Response Chart")
  
  list(Evaluation = evl, ConfusionMatrices = cf, Charts = charts)
}

load("~/Projetos_R/mlr_CAGED/amostra.RData")

grafico(data = train, 
        y = with(train, Sexo), 
        pred = c("Município", 
                 "CBO.2002.Ocupação", 
                 "CNAE.2.0.Subclas", 
                 "Grau.Instrução", 
                 "Idade", 
                 "Raça.Cor", 
                 "Salário.Mensal", 
                 "Saldo.Mov", 
                 "UF"))
