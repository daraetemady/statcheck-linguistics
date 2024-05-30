library(statcheck)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(broom)

# All pdfs
files <- list.files("Journals", recursive = TRUE, pattern = "\\.pdf")
length(files)
files
files <- as.data.frame(files) %>%
  mutate(year = substring(files, 5, 8)) %>%
  filter(year != 2024) %>%
  mutate(journal = substring(files, 13, 15)) %>%
  group_by(journal) %>%
  summarise(n.art = length(unique(files)))
files
sum(files$n.art)


# Read pdfs with statcheck, checks for one tailed tests
#stat <- checkPDFdir("Journals", OneTailedTxt = TRUE)

# Write statcheck data to csv
#write.csv(stat, "C:/Users/darae/Documents/ILN2200/statcheck-linguistics/statcheck_data.csv", row.names = FALSE)

data <- read.csv("statcheck_data.csv")

# Data table
data.table <- data %>% 
  mutate(year = strtoi(substring(source, 1, 4)),
         journal = substring(source, 9, 11)) %>%
  drop_na(reported_p) %>%
  drop_na(computed_p) %>%
  filter(year != 2024)

# Plot reported_p to computed_p
plot.computed.reported <- ggplot(data = errors,
       aes(x = reported_p, y = computed_p)) +
  geom_point(alpha = 0.3) +
  geom_vline(color = "red", xintercept = 0.05) +
  geom_hline(color = "red", yintercept = 0.05) +
  geom_abline(color = "red", slope = 1, intercept = 0.04) + 
  geom_abline(color = "red", slope = 1, intercept = -0.04) +
  labs(x = "Reported P-value", y = "Computed P-value") +
  scale_x_continuous(breaks = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5,
                                0.6, 0.7, 0.8, 0.9, 1.0)) +
  scale_y_continuous(breaks = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5,
                                0.6, 0.7, 0.8, 0.9, 1.0))

plot.computed.reported

# All errors
errors <- filter(data.table, error == TRUE) %>%
  mutate(relative_error = if_else(computed_p == 0, 
                                  0, 
                                  abs(computed_p-reported_p)/abs(computed_p)),
         absolute_error = abs(computed_p-reported_p))
nrow(errors)
errors[which(errors$reported_p == 0.05), ]

#Total number of errors
length(which(errors$rounding_error == TRUE))

# Zero computed_p
nrow(filter(data.table, computed_p == 0))
# Zero computed_p & error
nrow(filter(data.table, computed_p == 0 & error == TRUE))

# Zero errors
pzero <- filter(errors, reported_p == 0) %>%
  mutate(over = if_else(computed_p >= 0.001, TRUE, FALSE))
nrow(filter(pzero, computed_p == 0))

nrow(pzero[which(pzero$computed_p == 0),])

journal.pzero <- pzero %>%
  group_by(journal) %>%
  summarise(
    pzero = n(),
    over = length(which(computed_p >= 0.001)),
    under = length(which(computed_p < 0.001))
  )

year.pzero <- pzero %>%
  group_by(journal, year) %>%
  summarise(
    pzero = n(),
    over = length(which(computed_p >= 0.001)),
    under = length(which(computed_p < 0.001))
  )

# Relative error
max(errors$relative_error)
median(errors$relative_error)
errors[order(errors$relative_error),]


# Relative error plot
ggplot(data = filter(errors, computed_p != 0 & p_comp == "<"), aes(x = reported_p, y = relative_error, color = if_else(computed_p >= 0.05, TRUE, FALSE))) +
  geom_point(alpha = 0.6) +
  geom_vline(color = "red", xintercept = 0.05) +
  scale_x_continuous(breaks = c(0.05, 0.25, 0.5,
                              0.75, 1.0)) +
  scale_y_continuous(transform = "log10")+ #, breaks = c(1e+1, 1e+50, 1e+100, 1e+150, 1e+200, 1e+250)) +
  labs(x = "reported p < x", y = "Relative Error", color = "The computed\n p-value is ...") +
  scale_color_manual(values = c("purple", "yellow"), labels = c("less than .05", "more than .05"))

ggplot(data = filter(errors, computed_p != 0), aes(x = reported_p, y = relative_error)) +
  geom_point(alpha = 0.5) +
  geom_vline(color = "red", xintercept = 0.05) +
  scale_x_continuous(breaks = c(0.05, 0.25, 0.5,
                                0.75, 1.0)) +
  scale_y_continuous(transform = "log10", breaks = c(1e+1, 1e+50, 1e+100, 1e+150, 1e+200, 1e+250)) +
  labs(x = "p < X", y = "Relative Error", color = "Reported P-value")

# All decision errors
gross.error.data <- data.table[data.table$decision_error == TRUE, ] %>%
  mutate(over = if_else(computed_p >= 0.05, 1, 0))

filter(gross.error.data, over == 0)
gross.error.data
# reported p under 0.05
nrow(filter(gross.error.data, over == 0))
# reported p over 0.05
nrow(filter(gross.error.data, over == 1))
filter(gross.error.data, over == 1)

long.gross.error.data <- gross.error.data %>%
  count(journal, over) %>%
  group_by(journal) %>%
  mutate(prop = n / sum(n),
         over = if_else(over == 1, TRUE, FALSE))
gross.error.data
sum(filter(long.gross.error.data, over == FALSE)$n)

ggplot(long.gross.error.data, aes(x = journal, y =prop, fill = over)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  theme_light() +
  labs(x = "Journal", y = "Proportion", fill = "Gross errors that\n were wrongly\n reported as ...") +
  geom_text(aes(label = round(prop, 2), 
                color = "red"), 
            position = position_dodge(0.9),
            size = 4,
            fontface = "bold",
            vjust = -0.4) +
  guides(color = FALSE) 
  #scale_fill_manual(values = c("black", "grey"), labels = c("over .05", "under .05"))

plot.gross.error

year.gross <- data.table %>%
  group_by(year) %>%
  summarise(n.nhst = n(),
            sig = length(which(computed_p >= 0.05 & decision_error == TRUE)),
            insig = length(which(computed_p < 0.05 & decision_error == TRUE)),
            prop.sig = 100*sig/length(which(computed_p >= 0.05)),
            prop.insig = 100*insig/length(which(computed_p < 0.05)),
            n.sig = length(which(reported_p < 0.05)),
            n.insig = length(which(reported_p >= 0.05)))

year.gross

# prop.sig lm
summary(lm(prop.sig ~ year, data = year.gross))
# prop.insig lm
summary(lm(prop.insig ~ year, data = year.gross))

ggplot() +
  geom_line(data = year.gross, linetype = "dashed", aes(x = year, y = prop.sig)) +
  geom_line(data = year.gross, aes(x = year, y = prop.insig)) +
  geom_point(data = year.gross, shape = 1, aes(x = year, y = prop.sig, size = n.sig)) +
  geom_point(data = year.gross, aes(x = year, y = prop.insig, size = n.insig)) +
  scale_shape_manual(labels = "ok") +
  scale_x_continuous(breaks = c(2000, 
                                2004, 
                                2008, 
                                2012,
                                2016,
                                2020, 2023)) +
  labs(x = "Year", y = "Percentage") +
  guides(size = FALSE) +
  geom_text() +
  annotate("text", label = "closed circles, insignificant: beta = 0.04, Rsquared = .52", x = 2012, y = 16) + 
  annotate("text", label = "open circles, significant: beta = -0.38, Rsquared = .39", x = 2012.4, y = 15)

# Article table
data.articles <- data.table %>%
  group_by(source) %>%
  summarise(n.nhst = n(), 
            errors = length(which(error == TRUE)),
            gross.errors = length(which(decision_error == TRUE))) %>%
  mutate(year = strtoi(substring(source, 1, 4)),
         journal = substring(source, 9, 11),
         prop.errors = errors/n.nhst,
         prop.gross.errors = gross.errors/n.nhst,
         erroneous = if_else(errors > 0, 1, 0),
         gross.erroneous = if_else(gross.errors > 0, 1, 0),
         norm.squared = prop.errors * errors)
filter(data.articles, norm.squared == max(norm.squared))

propxcount <- data.articles$n.nhst * data.articles$prop.errors
max(propxcount)

# Data by year
years <- data.table %>%
  group_by(year) %>%
  summarise(nhst = n(),
            errors = length(which(error == TRUE)),
            gross.errors = length(which(decision_error == TRUE)),
            prop.errors = errors/nhst,
            prop.gross.errors = gross.errors/nhst)

ggplot(data = years, aes(x = year, y = gross.errors)) +
  geom_jitter()

# Summary table per journal per year
year.summary <- data.table %>%
  group_by(journal, year) %>%
  summarise(n.nhst = n(),
            art.w.nhst = length(unique(source)),
            errors = length(which(error == TRUE)),
            gross.errors = length(which(decision_error == TRUE)),
            prop.errors = errors/n.nhst,
            prop.gross.errors = gross.errors/n.nhst)

# LMs for seeing how variables change over the years
nhst.overtheyears <- summary(lm(nhst ~ year, data = years))
errors.overtheyears <- summary(lm(errors ~ year, data = years))
gross.errors.overtheyears <- summary(lm(gross.errors ~ year, data = years))
prop.errors.overtheyears <- summary(lm(prop.errors ~ year, data = years))
prop.gross.errors.overtheyears <- summary(lm(prop.gross.errors ~ year, data = years))

nhst.overtheyears
errors.overtheyears
gross.errors.overtheyears
prop.errors.overtheyears
prop.gross.errors.overtheyears

# Producing a lm per journal using prop.errors
lmerrors <- function() {
  lm.info <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(lm.info) <- c("journal", "coefficient", "pvalue", "rsquared")
  
  for (i in unique(year.summary$journal)) {
    
    t <- year.summary[which(year.summary$journal == as.character(i)), ]
    tlm <- lm(prop.errors ~ year, data = t)
    
    lm.info <- rbind(lm.info, list(i, summary(tlm)$coefficients[2], summary(tlm)$coefficients[4], summary(tlm)$r.squared))
    
  }
  print(lm.info)
}
lmerrors()

# Producing a lm per journal using prop.gross.errors
lmgrosserrors <- function() {
  lm.info <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(lm.info) <- c("journal", "coefficient", "pvalue", "rsquared")
  
  for (i in unique(year.summary$journal)) {
    
    t <- year.summary[which(year.summary$journal == as.character(i)), ]
    tlm <- lm(prop.gross.errors ~ year, data = t)
    
    lm.info <- rbind(lm.info, list(i, summary(tlm)$coefficients[2], summary(tlm)$coefficients[4], summary(tlm)$r.squared))
    
  }
  print(lm.info)
}
lmgrosserrors()

nrow(data.articles[which(data.articles$erroneous == 1), ])/nrow(data.articles)
nrow(data.articles[which(data.articles$gross.erroneous == 1), ])/nrow(data.articles)
mean(data.articles$prop.errors[which(data.articles$prop.errors > 0)])
mean(data.articles$prop.gross.errors[which(data.articles$prop.gross.errors > 0)])


data.table

journal.summary <- data.articles %>%
  group_by(journal) %>%
  summarise(art.w.nhst = n(),
            n.nhst = sum(n.nhst),
            mean.nhst = n.nhst/art.w.nhst,
            errors = sum(errors),
            gross.errors = sum(gross.errors),
            prop.errors = errors/n.nhst,
            prop.gross.errors = gross.errors/n.nhst,
            erroneous = sum(erroneous),
            gross.erroneous = sum(gross.erroneous),
            prop.erroneous = erroneous/art.w.nhst,
            prop.gross.erroneous = gross.erroneous/art.w.nhst)

mean(journal.summary$art.w.nhst)
mean(journal.summary$n.nhst)
mean(journal.summary$errors)
mean(journal.summary$gross.errors)


# lm for total prop.errors ~ year
lm(prop.errors ~ year, year.summary)


# Producing a lm per journal using prop.errors
lmerrors <- function() {
  lm.info <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(lm.info) <- c("journal", "coefficient", "pvalue", "rsquared")
  
  for (i in unique(year.summary$journal)) {
    
    t <- year.summary[which(year.summary$journal == as.character(i)), ]
    tlm <- lm(prop.errors ~ year, data = t)
    
    lm.info <- rbind(lm.info, list(i, summary(tlm)$coefficients[2], summary(tlm)$coefficients[4], summary(tlm)$r.squared))

    }
  print(lm.info)
}
lmerrors()

# Producing a lm per journal using prop.gross.errors
lmgrosserrors <- function() {
  lm.info <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(lm.info) <- c("journal", "coefficient", "pvalue", "rsquared")
  
  for (i in unique(year.summary$journal)) {
    
    t <- year.summary[which(year.summary$journal == as.character(i)), ]
    tlm <- lm(prop.gross.errors ~ year, data = t)
    
    lm.info <- rbind(lm.info, list(i, summary(tlm)$coefficients[2], summary(tlm)$coefficients[4], summary(tlm)$r.squared))
    
  }
  print(lm.info)
}
lmgrosserrors()

plot.articles.prop.errors <- ggplot(data.articles,
       aes(x = year, y = prop.errors, size = n.nhst)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.2) +
  geom_smooth(method = "gam", size = 1, color = "red") + 
  facet_wrap(~journal, ncol = 2) + 
  theme_light() +
  labs(x = "Year", y = "Proportion of Errors") +
  scale_size_continuous(name="Number of\n NHST") +
  #guides(color = FALSE) +
  scale_x_continuous(breaks = c(2000, 
                                2004, 
                                2008, 
                                2012,
                                2016,
                                2020))

plot.articles.prop.errors

plot.articles.prop.gross.errors <- ggplot(data.articles,
                                    aes(x = year, y = prop.gross.errors, size = n.nhst)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.2) +
  geom_smooth(method = "gam", size = 1, color = "red") + 
  facet_wrap(~journal, ncol = 2) + 
  theme_light() +
  labs(x = "Year", y = "Proportion of Gross Errors") +
  scale_size_continuous(name="Number of\n NHST") +
  #guides(color = FALSE) +
  scale_x_continuous(breaks = c(2000, 
                                2004, 
                                2008, 
                                2012,
                                2016,
                                2020))

plot.articles.prop.gross.errors

ggplot(data = years, aes(x = year)) +
  geom_line(aes(y = prop.errors))

long.journal.summary <- pivot_longer(data = journal.summary,
                                     cols = c(prop.erroneous, prop.gross.erroneous),
                                     names_to = "type",
                                     values_to = "prop")

bar.articles.erroneous <- ggplot(data = long.journal.summary, aes(x = journal, y = prop, fill = type)) +
  geom_bar(stat = "identity", position =position_dodge(0.9)) +
  scale_fill_manual(values = c("prop.erroneous" = "grey", "prop.gross.erroneous" = "black"),
                    name = "Error Type",
                    labels = c("Errors", "Gross Errors")) +
  theme_light() +
  labs(x = "Journal", y = "Proportion") +
  geom_text(aes(label = round(prop, 2), 
                color = "red"), 
            position = position_dodge(0.9),
            size = 4,
            fontface = "bold",
            vjust = -0.4) +
  guides(color = FALSE)
  
bar.articles.erroneous

