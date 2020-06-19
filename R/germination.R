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

# 1900x1500
ggplot(pollen_germ, aes(x = group, y = percent, fill = Category)) +
  geom_boxplot(size = 2) +
  scale_fill_manual(values = color_vector,
                    breaks = c("germinated", "ungerminated", "burst"),
                    labels = c("Germinated", "Ungerminated", "Burst")) +
  scale_x_discrete(labels = c("WT", "larp6c homo")) +
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                     labels = seq(0, 100, by = 20),
                     limits = c(0, 100)) +
  labs(title = "In vitro pollen germination", y = "Percent") +
  theme_bw() +
  theme(axis.title = element_text(size = 52, face = "bold"),
        axis.text = element_text(size = 56, face = "bold", color = "black"),
        axis.text.x = element_text(margin = margin(5, 0, 0, 0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
        plot.title = element_text(size = 72, face = "bold", margin = margin(0, 0, 40, 0)), 
        axis.line = element_line(size = 2, color = "black"),
        axis.ticks = element_line(size = 2, color = "black"),
        axis.ticks.length = unit(10, "pt"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 44, face = "bold")) #+
  facet_wrap(~Rep)


###### Stats ######
# Going to do the Pearson's chi-squared test for homogeneity
stats_df <- pollen_germ %>% group_by(group, Category) %>%
  summarize(n = sum(Value))

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