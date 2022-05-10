library(tercen)
library(tercenApi)
library(dplyr, warn.conflicts = FALSE)
library(base64enc)
library(png)
library(ggplot2)
library(uuid)
library(svglite)

getValues <- function(ctx){
  values <- list()
  
  data <- ctx %>% select(.y, .ri, .ci)
  if(ctx$hasXAxis) data$.x <- select(ctx, .x)[[".x"]]
  
  if(length(ctx$colors)) data <- data %>% dplyr::bind_cols(ctx$select(ctx$colors))
  
  rnames <- ctx$rselect() 
  rnames$.ri <- seq_len(nrow(rnames)) - 1
  data <- left_join(data, rnames, by = ".ri")
  
  cnames <- ctx$cselect()
  cnames$.ci <- seq_len(nrow(cnames)) - 1
  data <- left_join(data, cnames, by = ".ci")
  
  return(data)
}

ctx <- tercenCtx()

mimetype = ctx$op.value("format", type = as.character, default = "image/svg+xml")

input.par <- list(
  plot.width   = ctx$op.value("plot.width", type = as.double, default = 750),
  plot.height  = ctx$op.value("plot.height", type = as.double, default = 750),
  jitter       = ctx$op.value("jitter", type = as.logical, default = FALSE),
  average.type = ctx$op.value("average.type", type = as.character, default = "Mean"),
  dot.size     = ctx$op.value("dot.size", type = as.double, default = 0.5),
  error.type   = ctx$op.value("error.type", type = as.character, default = "Standard Deviation"),
  bar.width    = ctx$op.value("bar.width", type = as.double, default = 0.25),
  xlab         = ctx$op.value("xlab", type = as.character, default = ""),
  ylab         = ctx$op.value("ylab", type = as.character, default = ""),
  title        = ctx$op.value("title", type = as.character, default = ""),
  subtitle     = ctx$op.value("subtitle", type = as.character, default = ""),
  caption      = ctx$op.value("caption", type = as.character, default = "")
)


df <- getValues(ctx)

if(input.par$average.type == "Mean") {
  df_agg <- df %>%
    group_by_at(vars(-.y)) %>%
    summarise(mn = mean(.y, na.rm = TRUE),
              n = n(),
              stdv = sd(.y, na.rm = TRUE))
}
if(input.par$average.type == "Median") {
  df_agg <- df %>%
    group_by_at(vars(-.y)) %>%
    summarise(mn = median(.y, na.rm = TRUE),
              n = n(),
              stdv = sd(.y, na.rm = TRUE))
}

fill.col <- NULL
if(length(ctx$colors) > 0) {
  fill.col <- unlist(ctx$colors)
} 

if(!ctx$hasXAxis) {
  df_agg$.x <- ""
  df$.x <- ""
} 

theme_set(theme_light())

### Core plot with x and y axes
plt <- ggplot(df_agg, aes_string(x = ".x", y = "mn", fill = fill.col)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "identity") +
  labs(
    title = input.par$title,
    subtitle = input.par$subtitle,
    caption = input.par$caption,
    x = input.par$xlab,
    y = input.par$ylab,
    fill = "Legend"
  )

### Add jitter
if(input.par$jitter) {
  plt <- plt + geom_jitter(
    data = df,
    aes_string(x = ".x", y = ".y", fill = fill.col),
    size = input.par$dot.size,
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.9)
  )
}

### Add SD bars
if(input.par$error.type == "Standard Deviation") {
  plt <- plt + geom_errorbar(
    aes(ymin = mn - stdv, ymax = mn + stdv),
    width = input.par$bar.width,
    position = position_dodge(width = 0.9)
  )
}

### Facets based on rows and columns
cnames <- unlist(ctx$cnames)
if(ctx$cnames[[1]] == "") cnames <- "."
rnames <- unlist(ctx$rnames)
if(ctx$rnames[[1]] == "") rnames <- "."

plt <- plt + facet_grid(
  as.formula(paste(
    paste(rnames, collapse = "+"),
    "~",
    paste(cnames, collapse = "+")
  ))
)

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

output_string <- base64enc::base64encode(
  readBin(tmp, "raw", file.info(tmp)[1, "size"]),
  "txt"
)

#checksum <- as.vector(tools::md5sum(tmp))

output_md <- base64enc::base64encode(charToRaw("# Bar plot\n\n This is a bar plot generated in Tercen."),"txt")

tibble::tibble(
  filename = c("markdown", "svg"),
  mimetype = c("text/markdown", mimetype),
  .content = c(output_md,output_string)
) %>%
  ctx$addNamespace() %>%
  as_relation() %>%
  as_join_operator(list(), list()) %>%
  save_relation(ctx)
