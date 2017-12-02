############################################################
#                                                          #
#                 Plot-type images for UI                  #
#            (default output: 1200x1200 pixels)            #
#                                                          #
############################################################
# Load packages
library(ggplot2)

# Histogram
histogram <- ggplot(data = faithful) +
    aes(x = eruptions) +
    geom_histogram(fill = '#0072B2') +
    labs(title = 'Histogram',
         x = 'Eruption time (minutes)',
         y = 'Count') +
    theme_bw(base_size = 14) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(colour = '#000000',
                                   size = 0.4))

# ggsave(filename = './www/histogram_200x200.png',
#        plot = histogram,
#        width = 4,
#        height = 4,
#        dpi = 50)
#
# ggsave(filename = './www/histogram_400x400.png',
#        plot = histogram,
#        width = 4,
#        height = 4,
#        dpi = 100)
#
# ggsave(filename = './www/histogram_800x800.png',
#        plot = histogram,
#        width = 4,
#        height = 4,
#        dpi = 200)

ggsave(filename = './www/histogram_1200x1200.png',
       plot = histogram,
       width = 4,
       height = 4,
       dpi = 300)

# Scatterplot
scatter_plot <- ggplot(data = iris) +
    aes(x = Petal.Length,
        y = Petal.Width,
        colour = Species) +
    geom_point(size = 2) +
    labs(title = 'Scatter plot',
         x = 'Petal length (cm)',
         y = 'Petal width (cm)') +
    scale_color_brewer(type = 'qual', palette = 'Dark2') +
    theme_bw(base_size = 14) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(colour = '#000000',
                                   size = 0.4))

# ggsave(filename = './www/scatterplot_200x200.png',
#        plot = scatter_plot,
#        width = 4,
#        height = 4,
#        dpi = 50)
# 
# ggsave(filename = './www/scatterplot_400x400.png',
#        plot = scatter_plot,
#        width = 4,
#        height = 4,
#        dpi = 100)
# 
# ggsave(filename = './www/scatterplot_800x800.png',
#        plot = scatter_plot,
#        width = 4,
#        height = 4,
#        dpi = 200)

ggsave(filename = './www/scatterplot_1200x1200.png',
       plot = scatter_plot,
       width = 4,
       height = 4,
       dpi = 300)

# Boxplot
box_plot <- ggplot(data = mtcars) +
    aes(x = factor(cyl),
        y = mpg,
        fill = cyl) +
    geom_boxplot(colour = '#000000') +
    labs(title = 'Box-and-whisker plot',
         x = 'Number of engine cylinders',
         y = 'Miles per gallon of gasoline') +
    scale_fill_gradient(low = '#0072B2', high = '#56B4E9') +
    theme_bw(base_size = 14) +
    theme(legend.position = 'none',
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(colour = '#000000',
                                   size = 0.4))

# ggsave(filename = './www/boxplot_200x200.png',
#        plot = box_plot,
#        width = 4,
#        height = 4,
#        dpi = 50)
# 
# ggsave(filename = './www/boxplot_400x400.png',
#        plot = box_plot,
#        width = 4,
#        height = 4,
#        dpi = 100)
# 
# ggsave(filename = './www/boxplot_800x800.png',
#        plot = box_plot,
#        width = 4,
#        height = 4,
#        dpi = 200)

ggsave(filename = './www/boxplot_1200x1200.png',
       plot = box_plot,
       width = 4,
       height = 4,
       dpi = 300)