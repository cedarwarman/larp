# Here I'll take a look at Lian's lap6c germination counts

library(tidyverse)

###### Manipulating the data ######
# Importing data
pollen_germ <- read.table(file = './data/larp6c_germination_counts.tsv', header = T, sep = '\t')

# Making the rep number a factor
pollen_germ$Rep <- as.factor(pollen_germ$Rep)

# Taking out non-complete cases
pollen_germ <- pollen_germ[complete.cases(pollen_germ), ]

# For now, removing the unknowns. They're assigned somewhat arbitrarily, and 
# might skew the mean
pollen_germ <- pollen_germ %>% filter(Category != "unknown")

# Grouping by first the rep, then the letter
pollen_germ <- pollen_germ %>% group_by(Rep, Letter)

# Calculating the percentages of each category at each letter of each rep
pollen_germ <- pollen_germ %>% mutate(percent = 100 * (Value / sum(Value)))

# Adding in a factor for the category, larp6c vs WT.
pollen_germ$group <- NA

pollen_germ$group <- rep(c('WT', 'larp6c', 'WT', 'larp6c', 'WT', 'larp6c', #rep1
                           'larp6c', 'WT', 'larp6c', 'WT', 'larp6c', 'WT', #rep2
                           'larp6c', 'WT', 'larp6c', 'WT', 'larp6c', 'WT', #rep3,
                           'WT', 'larp6c', 'WT', 'larp6c', 'WT', 'larp6c'), #rep4
                           each = 3)


###### Germination boxplot ######

# Rearranging the factors
pollen_germ$Category <- factor(pollen_germ$Category,
                                   levels = c("germinated", "ungerminated", "burst"))
pollen_germ$group <- factor(pollen_germ$group,
                                levels = c("WT", "larp6c"))
color_vector <- c("#019E73", "#2558A8", "#9F5C82")

# Germination plot
ggplot(pollen_germ, aes(x = group, y = percent, fill = Category)) +
  geom_boxplot(size = 1, outlier.size = 1, position = position_dodge(0.85), color = 'black') +
  scale_fill_manual(values = color_vector,
                    breaks = c("germinated", "ungerminated", "burst"),
                    labels = c("Germinated", "Ungerminated", "Burst")) +
  scale_x_discrete(labels = c("WT", "larp6c homo")) +
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                     labels = seq(0, 100, by = 20),
                     limits = c(0, 100)) +
  labs(title = "In vitro pollen germination", y = "Percent") +
  theme_bw() +
  theme(axis.title = element_text(size = 28, face = 'bold'),
        axis.text = element_text(size = 22, face = 'bold', color = 'black'),
        axis.text.x = element_text(size = 26, face = 'bold', color = 'black'),
        plot.title = element_text(size = 32, face = 'bold', margin = margin(0, 0, 10, 0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        panel.border = element_blank(),
        axis.line = element_line(size = 1, color = 'black'),
        axis.ticks = element_line(size = 1, color = 'black'),
        axis.ticks.length = unit(8, 'pt'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        panel.grid = element_blank(),
        legend.title = element_text(size = 24, face = 'bold'),
        legend.text = element_text(size = 20, face = 'bold'),
        legend.position = 'right')

ggsave(filename = './plots/germination.png',
       device = 'png',
       width = 8,
       height = 6,
       dpi = 400,
       units = 'in')

# Rearranging the factors
pollen_germ$Category <- factor(pollen_germ$Category,
                               levels = c("ungerminated", "germinated", "burst"))
pollen_germ$group <- factor(pollen_germ$group,
                            levels = c("WT", "larp6c"))
color_vector <- c("#157E96", "#C49354")

# Germination plot to match Lian's plot
ggplot(pollen_germ, aes(x = Category, y = percent, fill = group)) +
  geom_boxplot(size = 1, outlier.size = 1, position = position_dodge(0.85), color = 'black') +
  scale_fill_manual(values = color_vector,
                    breaks = c("WT", "larp6c"),
                    labels = c("WT", "larp6c")) +
  scale_x_discrete(labels = c("Ungerminated", "Germinated", "Burst")) +
  scale_y_continuous(breaks = seq(0, 80, by = 20),
                     labels = seq(0, 80, by = 20),
                     limits = c(0, 80)) +
  labs(title = "In vitro pollen germination", y = "Percent") +
  theme_bw() +
  theme(axis.title = element_text(size = 28, face = 'bold'),
        axis.text = element_text(size = 22, face = 'bold', color = 'black'),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 20, face = 'bold', color = 'black'),
        plot.title = element_text(size = 32, face = 'bold', margin = margin(0, 0, 10, 0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        panel.border = element_blank(),
        axis.line = element_line(size = 1, color = 'black'),
        axis.ticks = element_line(size = 1, color = 'black'),
        axis.ticks.length = unit(8, 'pt'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 20, face = 'bold'),
        legend.position = 'right')
  # Add facet wrap by trial by uncommenting the following three lines: 
  #       strip.background = element_blank(),
  #       strip.text = element_text(size = 28, face = 'bold')) +
  # facet_wrap(~Rep)

ggsave(filename = './plots/germination_lian.png',
       device = 'png',
       width = 8,
       height = 8,
       dpi = 400,
       units = 'in')


# Germination boxplot for paper -------------------------------------------
# Grabbing the data
john_germ <- read.csv(file = './data/larp6c_germination_data_from_john.csv')
colnames(john_germ) <- c("germinated", 
                         "ungerminated", 
                         "burst", 
                         "unknown", 
                         "rep", 
                         "genotype", 
                         "time", 
                         "plant_letter", 
                         "order")

# Tidying structure
john_germ <- pivot_longer(john_germ, 
                          germinated:unknown, 
                          names_to = "category", 
                          values_to = "count")
john_germ$time <- sub('^.', '', john_germ$time)
john_germ <- john_germ %>% unite(grouped_name, genotype:time, sep = '_', remove = FALSE)

# Getting percentages
john_germ <- john_germ %>% filter(category != "unknown")
john_germ <- john_germ %>% group_by(time, rep, order)
john_germ <- john_germ %>% mutate(percent = 100 * (count / sum(count)))

# Making the plot (boxplot faceted)
john_germ$category <- factor(john_germ$category,
                               levels = c("germinated", "ungerminated", "burst"))
john_germ$grouped_name <- factor(john_germ$grouped_name,
                            levels = c("WT_15", "WT_30", 
                                       "larp6c_15", "larp6c_30"))
john_germ$genotype <- factor(john_germ$genotype, levels = c("WT", "larp6c"))
# color_vector <- c("#7DFFA0", "#97FFFF", "#FFB8F0")
# color_vector <- c("#A5FDAF", "#BCBCBC", "#FFB8EB") # watermelon
color_vector <- c("#44AA99", "#636363", "#d6d6d6")

ggplot(john_germ, aes(x = time, y = percent, fill = category)) +
  geom_boxplot(size = 1, outlier.size = 1, position = position_dodge(0.85), color = 'black') +
  scale_fill_manual(values = color_vector,
                    breaks = c("germinated", "ungerminated", "burst"),
                    labels = c("Germinated", "Ungerminated", "Burst")) +
  scale_x_discrete(labels = c("15 min", "30 min", "15 min", "30 min")) +
  scale_y_continuous(breaks = seq(0, 70, by = 10),
                     labels = seq(0, 70, by = 10),
                     limits = c(0, 72)) +
  labs(title = "In vitro pollen germination", y = "Percent") +
  theme_bw() +
  theme(axis.title = element_text(size = 26, face = 'bold'),
        axis.text = element_text(size = 22, face = 'bold', color = 'black'),
        axis.text.x = element_text(size = 20, face = 'bold', color = 'black'),
        # plot.title = element_text(size = 32, face = 'bold', margin = margin(0, 0, 10, 0)),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        panel.border = element_blank(),
        axis.line = element_line(size = 1, color = 'black'),
        axis.ticks = element_line(size = 1, color = 'black'),
        axis.ticks.length = unit(8, 'pt'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 20, face = 'bold'),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 26, face = 'bold')) +
  facet_wrap(~ genotype, strip.position = "bottom")

ggsave(filename = './plots/germination_boxplot.png',
       device = 'png',
       width = 7,
       height = 7.5,
       dpi = 400,
       units = 'in')

# Jitter plot
color_vector <- c("#009899", "#009928", "#990077")
ggplot(john_germ, aes(x = time, y = percent, color = category)) +
  geom_jitter(position = position_jitterdodge(0.5),
              shape = 16,
              size = 3,
              stroke = 1) +
  scale_color_manual(values = color_vector,
                    breaks = c("ungerminated", "germinated", "burst"),
                    labels = c("Ungerminated", "Germinated", "Burst")) +
  stat_summary(fun = mean,
               geom = "point",
               shape=95,
               size=15,
               # color = "black",
               aes(fill = category),
               position = position_dodge(0.8),
               show.legend = FALSE) +
  scale_x_discrete(labels = c("15 min", "30 min", "15 min", "30 min")) +
  scale_y_continuous(breaks = seq(0, 70, by = 10),
                     labels = seq(0, 70, by = 10),
                     limits = c(0, 72)) +
  labs(title = "In vitro pollen germination", y = "Percent") +
  theme_bw() +
  theme(axis.title = element_text(size = 28, face = 'bold'),
        axis.text = element_text(size = 22, face = 'bold', color = 'black'),
        axis.text.x = element_text(size = 20, face = 'bold', color = 'black'),
        plot.title = element_text(size = 32, face = 'bold', margin = margin(0, 0, 10, 0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        panel.border = element_blank(),
        axis.line = element_line(size = 1, color = 'black'),
        axis.ticks = element_line(size = 1, color = 'black'),
        axis.ticks.length = unit(8, 'pt'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 20, face = 'bold'),
        legend.position = 'right',
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 28, face = 'bold')) +
  facet_wrap(~ genotype, strip.position = "bottom")

# Line plot
color_vector <- c("#009899", "#009928", "#990077")



###### Stats ######
# Going to do the Pearson's chi-squared test for homogeneity (here adding everything together)
stats_df <- pollen_germ %>% group_by(group, Category) %>%
  summarize(n = sum(Value))

# # Uncomment this section to do the reps individually
# stats_df <- pollen_germ %>% 
#   ungroup() %>% 
#   filter(Rep == 1) %>%
#   group_by(group, Category) %>%
#   summarize(n = sum(Value))


# Making the contingency table
contingency_table <- matrix(c(stats_df$n[stats_df$group == "larp6c" & stats_df$Category == "burst"],
                              stats_df$n[stats_df$group == "WT" & stats_df$Category == "burst"],
                              stats_df$n[stats_df$group == "larp6c" & stats_df$Category == "germinated"],
                              stats_df$n[stats_df$group == "WT" & stats_df$Category == "germinated"],
                              stats_df$n[stats_df$group == "larp6c" & stats_df$Category == "ungerminated"],
                              stats_df$n[stats_df$group == "WT" & stats_df$Category == "ungerminated"]),
                              ncol = 3)
colnames(contingency_table) <- c("burst", "germinated", "ungerminated")
rownames(contingency_table) <- c("larp6c", "wt")

contingency_table

# Running the test
chisq.test(contingency_table)
