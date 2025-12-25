library(ggplot2)
library(ggwordcloud)
library(dplyr)
library(cowplot)
library(grid)
# 1. maybe someday I will add some wordcloud
########
terms <- c("")
create_wordcloud_data <- function(terms) {
  df <- data.frame(
    word = terms,
    freq = 80
  )
}
wordcloud_data <- create_wordcloud_data(terms)
#####

# 2. create the cloud by col
########
create_vertical_wordcloud <- function(data) {
  # 计算竖排位置
  n <- nrow(data)
  x_pos <- rep(11, n)
  y_pos <- seq(17.5, 6, length.out = n)
  data$x <- x_pos
  data$y <- y_pos
  # make the cloud more colorful
  p <- ggplot(data, aes(x = x, y = y, label = word, size = freq, color = freq)) +
    geom_text(
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      alpha = 0.9, 
      family = "serif"
    ) +
    scale_size_area(max_size = 6, guide = "none") +
    # may my words have colorful forms
    scale_color_gradientn(
      colors = c("#FF0000", "#FFD700", "#32CD32", "#FFFFFF"),
      guide = "none"
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.margin = margin(0, 0, 0, 0),
      legend.position = "none"
    ) +
    # limit the area
    xlim(5, 17) +
    ylim(5, 18)
  return(p)
}
#####

# 3. creare the tree
########
create_christmas_tree <- function() {
  Tree.X <- c(10, 11, 12, 10, 11, 12, 10, 11, 12, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 7, 8, 9, 10, 11, 12, 13, 14, 15, 7, 8, 9, 10, 11, 12, 13, 14, 15, 8, 9, 10, 11, 12, 13, 14, 8, 9, 10, 11, 12, 13, 14, 9, 10, 11, 12, 13, 10, 11, 12, 10, 11, 12, 11, 6, 7, 8, 6, 7, 8, 3, 4, 3, 4, 14, 15, 16, 14, 15, 16, 18, 19, 18, 19)
  Tree.Y <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 13, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, 16, 16, 16, 17, 17, 17, 18, 1, 1, 1, 2, 2, 2, 1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 2, 2)
  # some gifts and a tree
  Tree.Colour <- c(
    rep("#143306", 148),
    rep("blueviolet", 6),
    rep("firebrick2", 10),
    rep("blueviolet", 4)
  )
  ChristmasTree <- data.frame(Tree.X, Tree.Y, Tree.Colour)
  tree <- ggplot() +
    geom_tile(data = ChristmasTree, aes(x = Tree.X, y = Tree.Y, fill = Tree.Colour)) +
    scale_fill_identity() +
    theme_void() +
    labs(x = "", y = "") +
    theme(
      plot.background = element_rect(fill = "black", color = NA),
      plot.margin = margin(20, 20, 20, 20),
      panel.background = element_rect(fill = "black", color = NA)
    )
  # light it
  Desired.Lights <- 50
  Lights <- data.frame(
    Lights.X = c(round(runif(round(Desired.Lights * 0.35), 4, 18), 0),
                 round(runif(round(Desired.Lights * 0.20), 5, 17), 0),
                 round(runif(round(Desired.Lights * 0.17), 6, 16), 0),
                 round(runif(round(Desired.Lights * 0.13), 7, 15), 0),
                 round(runif(round(Desired.Lights * 0.10), 8, 14), 0),
                 round(runif(round(Desired.Lights * 0.05), 10, 12), 0)),
    Lights.Y = c(round(runif(round(Desired.Lights * 0.35), 4, 6), 0),
                 round(runif(round(Desired.Lights * 0.20), 7, 8), 0),
                 round(runif(round(Desired.Lights * 0.17), 9, 10), 0),
                 round(runif(round(Desired.Lights * 0.13), 11, 12), 0),
                 round(runif(round(Desired.Lights * 0.10), 13, 14), 0),
                 round(runif(round(Desired.Lights * 0.05), 15, 17), 0))
  )
  tree <- tree +
    geom_point(data = Lights, aes(x = Lights.X, y = Lights.Y),
               colour = "lightgoldenrodyellow", shape = 16, size = 2, alpha = 0.7)
  # add some bells  
  Baubles <- data.frame(
  Bauble.X = c(6, 9, 15, 17, 5, 13, 16, 7, 10, 14, 7, 9, 11,14, 8, 14, 9, 12, 11, 12, 14, 11, 17, 10),
  Bauble.Y = c(4, 5, 4, 4, 5, 5, 5, 6, 6, 6, 8, 8, 8, 8, 10,10, 11, 11, 12, 13, 10, 16, 7, 14),
  Bauble.Colour = factor(c(1, 2, 2, 3, 2, 3, 1, 3, 1, 1, 1, 2, 1, 2,3, 3, 2, 1, 3, 2, 1, 3, 3, 1)),
  Bauble.Size = c(1, 3, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1, 3, 3, 3,2, 3, 1, 1, 2, 2, 3, 3, 2)
  )  
  tree <- tree +
    geom_point(data = Baubles, aes(x = Bauble.X, y = Bauble.Y,colour = Bauble.Colour, size = Bauble.Size),
               shape = 16) +
    scale_colour_manual(values = c("firebrick2", "gold", "dodgerblue3")) +
    scale_size_area(max_size = 8) +
    theme(legend.position = "none")
  # for gifts  
  tree <- tree +
  geom_segment(aes(x = 2.5, xend = 4.5, y = 1.5, yend = 1.5),
               colour = "blueviolet", linewidth = 2) +
    geom_segment(aes(x = 5.5, xend = 8.5, y = 1.5, yend = 1.5),
                 colour = "dodgerblue3", linewidth = 2) +
    geom_segment(aes(x = 13.5, xend = 16.5, y = 1.5, yend = 1.5),
                 colour = "blueviolet", linewidth = 2) +
    geom_segment(aes(x = 17.5, xend = 19.5, y = 1.5, yend = 1.5),
                 colour = "dodgerblue3", linewidth = 2) +
    geom_segment(aes(x = 3.5, xend = 3.5, y = 0.5, yend = 2.5),
                 colour = "blueviolet", linewidth = 2) +
    geom_segment(aes(x = 7.0, xend = 7.0, y = 0.5, yend = 2.5),
                 colour = "dodgerblue3", linewidth = 2) +
    geom_segment(aes(x = 15.0, xend = 15.0, y = 0.5, yend = 2.5),
                 colour = "blueviolet", linewidth = 2) +
    geom_segment(aes(x = 18.5, xend = 18.5, y = 0.5, yend = 2.5),
                 colour = "dodgerblue3", linewidth = 2)
  # a star
  tree <- tree +
    annotate("text", x = 11, y = 19.5, label = "★",
             size = 15, color = "gold", family = "serif")
  # Merry Christmas!
  tree <- tree +
    annotate("text", x = 11, y = 21.5,
             label = "Merry Christmas!",
             color = "white", size = 12, fontface = "bold",family = "Arial Rounded MT Bold")
  return(tree)
}
#####

# 4. mix it
########
create_combined_plot <- function() {
  tree_plot <- create_christmas_tree()
  wc_plot <- create_vertical_wordcloud(wordcloud_data)
  combined <- ggdraw() +
    draw_plot(tree_plot, x = 0, y = 0, width = 1, height = 1) +
    draw_plot(wc_plot,
              x = 0.2,
              y = 0.1,
              width = 0.65,
              height = 0.65)
  return(combined)
}
#####

# 5. create the whole picture
combined_plot <- create_combined_plot()
print(combined_plot)
ggsave("bioinfo_christmas_tree.png",
       combined_plot,
       width = 12,
       height = 14,
       dpi = 300,
       bg = "black")
