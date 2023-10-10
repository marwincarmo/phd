fig1 <- ggplot(
  data = iris
  , aes(y = Species, x = value, fill = Species)
) + 
  geom_boxplot() +                       
  facet_grid(attribute~.) + 
  scale_x_continuous(name = "attribute") +
  scale_y_discrete(name = "Species") +
  theme_classic() + 
  theme(legend.position = "none")

fig1

# figure saving settings
units <- "in"  
fig_w <- 3.2
fig_h <- fig_w
dpi <- 600
device <- "tiff" 

ggsave("figures/fig1.tiff",
       plot = fig1,
       device = device,
       width = fig_w,
       height = fig_h,
       units = units,
       dpi = dpi)
