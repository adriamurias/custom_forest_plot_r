# Packages
library(readxl)
library(tidyverse)
library(meta)

# Import Data
or_df <- read_excel("material/OR for forest plot_1.xlsx")

#------------------------------------------------------------------------------
# Data Wrangling
#------------------------------------------------------------------------------

or_df$`OR (95% CI)`<-
  gsub("[()]", "", or_df$`OR (95% CI)`) #remove "()" from OR column
or_df$`OR (95% CI)`<-
  gsub("[-]", "", or_df$`OR (95% CI)`)  #remove "-" from OR column

or_df <- data.frame(
  or_df[-4],
  #split OR column by space delimiter
  str_split_fixed(or_df$`OR (95% CI)`, " ", 3),
  #add index column
  index=1:nrow(or_df)
)
names(or_df)[4:6] <- c("OR", "OR_lower", "OR_upper") #new col names

# Set column type for numeric variables
or_df[c("OR", "OR_lower", "OR_upper")] <- sapply(
  or_df[c("OR", "OR_lower", "OR_upper")],
  as.double)

#------------------------------------------------------------------------------
# Create forest plot
#------------------------------------------------------------------------------

plot_or_forest_1<-
  ggplot(data=or_df,
         aes(y=index, x=OR, xmin=OR_lower, xmax=OR_upper)) +
  geom_point(shape=22, fill="#0c2c84", color="#0c2c84") + 
  geom_errorbarh(height=.1, color="#0c2c84") +
  scale_y_continuous(breaks=1:nrow(or_df),
                     labels=or_df$Subgroup,
                     trans = "reverse") +
  scale_x_continuous(breaks = seq(0,18,by = 1),
                     limits = c(0,18)) +
  labs(title='OR (95% CI)', x='', y = '') +
  geom_vline(xintercept=1, color='black', linetype='dashed') +
  theme_classic()

plot_or_forest_2<-
  ggplot(data=or_df,
         aes(y=index, x=OR, xmin=OR_lower, xmax=OR_upper)) +
  geom_point(shape=22, fill="#0c2c84", color="#0c2c84") + 
  geom_errorbarh(height=.1, color="#0c2c84") +
  scale_y_continuous(breaks=1:nrow(or_df),
                     labels=or_df$Subgroup,
                     trans = "reverse") +
  scale_x_continuous(breaks = seq(0,7,by = 1)) +
  coord_cartesian(xlim = c(0,7)) +
  labs(title='OR (95% CI)', x='', y = '') +
  geom_vline(xintercept=1, color='black', linetype='dashed') +
  geom_point(aes(x=7.305,y = 10.985),
             pch = -9658, color = "#0c2c84", size=2.2) +
  theme_classic()

plot_or_forest_3<-
  ggplot(data=or_df,
         aes(y=index, x=OR, xmin=OR_lower, xmax=OR_upper)) +
  geom_point(shape=22, fill="#0c2c84", color="#0c2c84") + 
  geom_errorbarh(height=.1, color="#0c2c84") +
  scale_y_continuous(breaks=1:nrow(or_df),
                     labels=or_df$Subgroup,
                     trans = "reverse") +
  scale_x_continuous(breaks = seq(0,7,by = 1)) +
  coord_cartesian(xlim = c(0,7)) +
  geom_vline(xintercept=1, color='black') +
  geom_point(aes(x=7.305,y = 10.985),
             pch = -9658, color = "#0c2c84", size=2.2) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.length.x=unit(-0.16, "cm"))

ggsave("figures/plot_or_forest_1.png", plot_or_forest_1, width = 8, height = 6)
ggsave("figures/plot_or_forest_2.png", plot_or_forest_2, width = 8, height = 6)
ggsave("figures/plot_or_forest_3.png", plot_or_forest_3, width = 8, height = 6)

