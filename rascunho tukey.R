# Projetando uma funcao para criar graficos com o teste de Tukey


# library
library(multcomp)
library(multcompView)
library(purrr)
library(dplyr)
library(magrittr)

# Create data
data <- map2_dfr(c("A", "B", "C", "D", "E"),
        list(2:5, 6:10, 1:7, 3:10, 10:20),
        ~ tibble(treatment = rep(.x, 20)) %>% 
          bind_cols(value = sample(.y, 20, replace = T))) %>% 
  as.data.frame()


# What is the effect of the treatment on the value ?

ANOVA <- lm(data$value ~ data$treatment ) %>% aov

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'data$treatment', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown" )


# I need to group the treatments that are not different each other together.
generate_label_df <- function(TUKEY, variable){
  require(multcompView)
  
  # Extract labels and factor levels from Tukey post-hoc
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

# Apply the function on my dataset
LABELS=generate_label_df(TUKEY , "data$treatment") %>% 
  bind_cols(q = data %>% group_by(treatment) %>% summarise(q = quantile(value, probs = 1)) %>% .[,2])

# Plot results:
data %>% 
  ggplot(aes(x= treatment, y=value))+
  geom_boxplot()+
  geom_jitter(alpha=0.3)+
  theme_bw()+
  geom_text(data = LABELS, aes(x= treatment, y=q, label=Letters, color = Letters), vjust=-0.5, hjust=-1)




