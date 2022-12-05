library(tercen)
library(tercenApi)
library(dplyr, warn.conflicts = FALSE)
library(base64enc)
library(png)
library(ggplot2)
library(uuid)
library(svglite)
library(tim)
library(grDevices)
library(RColorBrewer)
library(jsonlite)
library(stringr)

getValues <- function(ctx){
  values <- list()
  
  data <- ctx %>% select(.y, .ri, .ci)
  if(ctx$hasXAxis) data$.x <- select(ctx, .x)[[".x"]]
  
  if(length(ctx$colors)) data <- data %>% dplyr::bind_cols(ctx$select(ctx$colors))
  if(length(ctx$labels)) data <- data %>% dplyr::bind_cols(ctx$select(ctx$labels))
  
  rnames <- ctx$rselect() 
  rnames$.ri <- seq_len(nrow(rnames)) - 1
  data <- left_join(data, rnames, by = ".ri", suffix = c("", ".YYY")) %>%
    select(-ends_with(".YYY"))
  
  cnames <- ctx$cselect()
  cnames$.ci <- seq_len(nrow(cnames)) - 1
  data <- left_join(data, cnames, by = ".ci", suffix = c("", ".YYY")) %>%
    select(-ends_with(".YYY"))
  
  return(data)
}

ctx <- tercenCtx()

mimetype = ctx$op.value("format", type = as.character, default = "image/png")

input.par <- list(
  plot.width   = ctx$op.value("plot.width", type = as.double, default = 750),
  plot.height  = ctx$op.value("plot.height", type = as.double, default = 750),
  jitter       = ctx$op.value("jitter", type = as.logical, default = TRUE),
  average.type = ctx$op.value("average.type", type = as.character, default = "Mean"),
  dot.size     = ctx$op.value("dot.size", type = as.double, default = 0.5),
  error.type   = ctx$op.value("error.type", type = as.character, default = "Standard Deviation"),
  bar.position = ctx$op.value("bar.position", type = as.character, default = "dodge"),
  bar.width    = ctx$op.value("bar.width", type = as.double, default = 0.25),
  dodge.width  = ctx$op.value("dodge.width", type = as.double, default = 1.1),
  jitter.width = ctx$op.value("jitter.width", type = as.double, default = 0.05),
  xlab         = ctx$op.value("xlab", type = as.character, default = ""),
  ylab         = ctx$op.value("ylab", type = as.character, default = ""),
  title        = ctx$op.value("title", type = as.character, default = ""),
  subtitle     = ctx$op.value("subtitle", type = as.character, default = ""),
  caption      = ctx$op.value("caption", type = as.character, default = ""),
  theme        = ctx$op.value("theme", type = as.character, default = "light"),
  color.palette= ctx$op.value("color.palette", type = as.character, default = "Set1"),
  wrap.1d      = ctx$op.value("wrap.1d", type = as.logical, default = TRUE)
)

df <- getValues(ctx)

if(length(ctx$labels) == 0) {
  var_lab <- ".y"
} else {
  var_lab <- ctx$labels[[1]] 
}

if(input.par$average.type == "Mean") {
  df_agg <- df %>%
    group_by_at(vars(-.y)) %>%
    summarise(mn = mean(.y, na.rm = TRUE),
              n = n(),
              custom_error = mean(!!sym(var_lab), na.rm = TRUE),
              stdv = sd(.y, na.rm = TRUE))
}
if(input.par$average.type == "Median") {
  df_agg <- df %>%
    group_by_at(vars(-.y)) %>%
    summarise(mn = median(.y, na.rm = TRUE),
              n = n(),
              custom_error = mean(!!sym(var_lab), na.rm = TRUE),
              stdv = sd(.y, na.rm = TRUE))
}

# Compute SE
df_agg <- df_agg %>% mutate(se = stdv / sqrt(n))

fill.col <- NULL
if(length(ctx$colors) > 0) {
  fill.col <- unlist(ctx$colors)
  if(length(ctx$colors) > 1) {
    df_agg$Grouping_factors <- factor(apply(df_agg[, fill.col], 1, paste0, collapse = " - "))
    df$Grouping_factors <- factor(apply(df[, fill.col], 1, paste0, collapse = " - "))
    fill.col <- "Grouping_factors"
  }
}

if(!ctx$hasXAxis) {
  df_agg$.x <- ""
  df$.x <- ""
} 

th <- get(paste0("theme_", input.par$theme))
theme_set(th())

clean_colname <- paste0("`", fill.col, "`")

### Core plot with x and y axes
pos <- switch (
  input.par$bar.position,
  "dodge" = position_dodge(width = input.par$dodge.width),
  "stack" = "stack",
  "fill" = "fill"
)

plt <- ggplot(df_agg, aes_string(x = ".x", y = "mn", fill = clean_colname)) +
  geom_bar(position = pos, stat = "identity") +
  labs(
    title = input.par$title,
    subtitle = input.par$subtitle,
    caption = input.par$caption,
    x = input.par$xlab,
    y = input.par$ylab,
    fill = "Legend"
  )

### Add jitter
if(input.par$bar.position == "dodge") {

  if(input.par$jitter) {
    if(is.null(fill.col)) {
      plt <- plt + geom_jitter(
        data = df,
        aes_string(x = ".x", y = ".y", fill = clean_colname),
        size = input.par$dot.size
      )
    } else {
      plt <- plt + geom_jitter(
        data = df,
        aes_string(x = ".x", y = ".y", fill = clean_colname),
        size = input.par$dot.size,
        position = position_jitterdodge(jitter.width = input.par$jitter.width, dodge.width = input.par$dodge.width)
      )
    }
  }
}

### Add SD bars
if(input.par$bar.position == "dodge") {
  if(input.par$error.type == "Standard Deviation") {
    plt <- plt + geom_errorbar(
      aes(ymin = mn - stdv, ymax = mn + stdv),
      width = input.par$bar.width,
      position = position_dodge(width = input.par$dodge.width)
    )
  } else if(input.par$error.type == "Custom") {
    plt <- plt + geom_errorbar(
      aes(ymin = mn - custom_error, ymax = mn + custom_error),
      width = input.par$bar.width,
      position = position_dodge(width = input.par$dodge.width)
    )
  } else if(input.par$error.type == "95% Confidence Interval") {
    plt <- plt + geom_errorbar(
      aes(ymin = mn - 1.96 * se, ymax = mn + 1.96 * se),
      width = input.par$bar.width,
      position = position_dodge(width = input.par$dodge.width)
    )
  }
}

### Color palette
n_groups <- length(unique(df_agg[[fill.col]]))
pal <- suppressWarnings(
  colorRampPalette(brewer.pal(name = input.par$color.palette, n = n_groups))(n_groups)
)
plt <- plt + scale_fill_manual(values = pal)


### Facets based on rows and columns
cnames <- unlist(ctx$cnames)
if(ctx$cnames[[1]] == "") cnames <- "."
rnames <- unlist(ctx$rnames)
if(ctx$rnames[[1]] == "") rnames <- "."

rnames <- paste0("`", rnames, "`")
cnames <- paste0("`", cnames, "`")

if(any(c(rnames, cnames) %in% ".") & input.par$wrap.1d) {
  plt <- plt + facet_wrap(
    as.formula(paste(
      "~",
      paste(rnames, collapse = "+"),
      "+",
      paste(cnames, collapse = "+")
    ))
  )
} else {
  plt <- plt + facet_grid(
    as.formula(paste(
      paste(rnames, collapse = "+"),
      "~",
      paste(cnames, collapse = "+")
    ))
  )
}

if (mimetype == 'image/png'){
  tmp <- tempfile(fileext = ".png")
  png(tmp, width = input.par$plot.width, height = input.par$plot.height, unit = "px")
  plot(plt)
  dev.off()
} else if (mimetype == 'image/svg+xml'){
  tmp <- tempfile(fileext = ".svg")
  # SVG sizes are in inches, not pixels
  res <- 144
  svglite(tmp, width = input.par$plot.width/res, height = input.par$plot.height/res)
  # ggplot(plt) 
  plot(plt)
  dev.off()
} else {
  stop("Bad mimetype")
}

df_out <- tim::png_to_df(tmp, filename = "barplot.png")
if(mimetype == 'image/svg+xml') {
  df_out$mimetype <- 'image/svg+xml'
  df_out$filename <- "barplot.svg"
} 

df_out %>%
  ctx$addNamespace() %>%
  as_relation() %>%
  as_join_operator(list(), list()) %>%
  save_relation(ctx)
