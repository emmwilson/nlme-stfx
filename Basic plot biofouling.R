pacman::p_load(
  tidyverse,
  readxl,
  ggplot2
)

std <- function(x) {
  sd(x)/sqrt(length(x))
} 

UV_Housing_Data <- read_excel("~/Desktop/UV-Housing Data.xlsx", sheet = "Pivot Table") 
names(UV_Housing_Data)<-make.names(names(UV_Housing_Data),unique = TRUE)

UV_Housing_Data <- UV_Housing_Data %>% pivot_wider(names_from = Label, values_from = Count.of.Label)

UV_Housing_Data <- UV_Housing_Data %>% mutate(Total = BOVO + BSC + Bug + CIO + MEMB + Barn + LEUC)

UV_Housing_Data <- UV_Housing_Data %>% mutate(Percent.cover = Total/49*100)

UV_Housing_Data_avg <- UV_Housing_Data %>%
  group_by(Days.After.Treatment, Treatment) %>%
  summarize(se = std(Percent.cover), avg_PC = mean(Percent.cover))

basic_plot <- function(y) {
ggplot(data = y, aes(Days.After.Treatment, avg_PC, ymin = avg_PC - se, ymax = avg_PC + se, colour = Treatment, position = "dodge")) +
  geom_point()+
  geom_line() +
  geom_errorbar(width = 0.2)
}

basic_plot(UV_Housing_Data_avg)

#Plot of each species
UV_Housing_Data_CIO <- UV_Housing_Data %>%
  group_by(Days.After.Treatment, Treatment) %>%
  summarize(se = std(CIO), avg_CIO = mean(CIO))

basic_plot_CIO <- function(y) {
  ggplot(data = y, aes(Days.After.Treatment, avg_CIO, ymin = avg_CIO - se, ymax = avg_CIO + se, colour = Treatment)) +
    geom_point() +
    geom_line() +
    geom_errorbar()
}
basic_plot_CIO(UV_Housing_Data_CIO)

UV_Housing_Data_Barn <- UV_Housing_Data %>%
  group_by(Days.After.Treatment, Treatment) %>%
  summarize(se = std(Barn), avg_Barn = mean(Barn))

basic_plot_Barn <- function(y) {
  ggplot(data = y, aes(Days.After.Treatment, avg_Barn, ymin = avg_Barn - se, ymax = avg_Barn + se, colour = Treatment)) +
    geom_point() +
    geom_line() +
    geom_errorbar()
}
basic_plot_Barn(UV_Housing_Data_Barn)

UV_Housing_Data_BOVO <- UV_Housing_Data %>%
  group_by(Days.After.Treatment, Treatment) %>%
  summarize(se = std(BOVO), avg_BOVO = mean(BOVO))

basic_plot_BOVO <- function(y) {
  ggplot(data = y, aes(Days.After.Treatment, avg_BOVO, ymin = avg_BOVO - se, ymax = avg_BOVO + se, colour = Treatment)) +
    geom_point() +
    geom_line() +
    geom_errorbar()
}
basic_plot_BOVO(UV_Housing_Data_BOVO)

UV_Housing_Data_BSC <- UV_Housing_Data %>%
  group_by(Days.After.Treatment, Treatment) %>%
  summarize(se = std(BSC), avg_BSC = mean(BSC))

basic_plot_BSC <- function(y) {
  ggplot(data = y, aes(Days.After.Treatment, avg_BSC, ymin = avg_BSC - se, ymax = avg_BSC + se, colour = Treatment)) +
    geom_point() +
    geom_line() +
    geom_errorbar()
}
basic_plot_BSC(UV_Housing_Data_BSC)

UV_Housing_Data_Bug <- UV_Housing_Data %>%
  group_by(Days.After.Treatment, Treatment) %>%
  summarize(se = std(Bug), avg_Bug = mean(Bug))

basic_plot_Bug <- function(y) {
  ggplot(data = y, aes(Days.After.Treatment, avg_Bug, ymin = avg_Bug - se, ymax = avg_Bug + se, colour = Treatment)) +
    geom_point() +
    geom_line() +
    geom_errorbar()
}
basic_plot_Bug(UV_Housing_Data_Bug)

UV_Housing_Data_MEMB <- UV_Housing_Data %>%
  group_by(Days.After.Treatment, Treatment) %>%
  summarize(se = std(MEMB), avg_MEMB = mean(MEMB))

basic_plot_MEMB <- function(y) {
  ggplot(data = y, aes(Days.After.Treatment, avg_MEMB, ymin = avg_MEMB - se, ymax = avg_MEMB + se, colour = Treatment)) +
    geom_point() +
    geom_line() +
    geom_errorbar()
}
basic_plot_MEMB(UV_Housing_Data_MEMB)

UV_Housing_Data_LEUC <- UV_Housing_Data %>%
  group_by(Days.After.Treatment, Treatment) %>%
  summarize(se = std(LEUC), avg_LEUC = mean(LEUC))

basic_plot_LEUC <- function(y) {
  ggplot(data = y, aes(Days.After.Treatment, avg_LEUC, ymin = avg_LEUC - se, ymax = avg_LEUC + se, colour = Treatment)) +
    geom_point() +
    geom_line() +
    geom_errorbar()
}
basic_plot_LEUC(UV_Housing_Data_LEUC)
