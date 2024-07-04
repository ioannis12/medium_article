library(openxlsx)
library(dplyr)
library(knitr)
library(ggplot2)

options(digits=7)

link <- 'https://www.football-data.co.uk/mmz4281/2324/G1.csv'
link <- 'https://www.football-data.co.uk/mmz4281/2223/G1.csv'
link <- 'https://www.football-data.co.uk/mmz4281/2122/G1.csv'
link <- 'https://www.football-data.co.uk/mmz4281/2021/G1.csv'

df <- read.csv(link)

df1 <- df[,c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "AvgH", "AvgD", "AvgA", "Avg.2.5",
            "Avg.2.5.1")]

df1 <- df1%>%rename('Team1' = 'HomeTeam',
                    'Team2' = 'AwayTeam', 
                    'Score1' = 'FTHG',
                    'Score2' = 'FTAG', 
                    'Avg1' = 'AvgH',
                    'Avg2' = 'AvgA',
                    'Avg.2.5.over' = 'Avg.2.5',
                    'Avg.2.5.under' = 'Avg.2.5.1')

df1$Home <- 1

df1$FTR <- ifelse(df1$FTR == 'H', 'Team1', ifelse(df1$FTR == 'A', 'Team2', 'Draw'))

df2 <- df[,c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "AvgH", "AvgD", "AvgA", "Avg.2.5",
             "Avg.2.5.1")]

df2 <- df2%>%rename('Team2' = 'HomeTeam',
                    'Team1' = 'AwayTeam', 
                    'Score2' = 'FTHG',
                    'Score1' = 'FTAG', 
                    'Avg2' = 'AvgH',
                    'Avg1' = 'AvgA',
                    'Avg.2.5.over' = 'Avg.2.5',
                    'Avg.2.5.under' = 'Avg.2.5.1')

df2$Home <- 0

df2$FTR <- ifelse(df2$FTR == 'H', 'Team2', ifelse(df2$FTR == 'A', 'Team1', 'Draw'))

df3 <- rbind(df1, df2)

df3 <- df3[with(df3, order(Date, Avg.2.5.over)),]

rm(df1, df2, link)

df3$Favorite <- ifelse(df3$Avg1 < df3$Avg2, 1, ifelse(df3$Avg1 > df3$Avg2, 0, 0.5))

df3$TotalGoals <- df3$Score1 + df3$Score2


df3$PL_F <- ifelse(df3$Favorite == 1 & df3$FTR == 'Team1', df3$Avg1 -1 , ifelse(df3$Favorite == 1 & df3$FTR != 'Team1', -1 , 0))

sum(df3$PL_F, na.rm = T)

df3$PL_U <- ifelse(df3$Favorite == 0 & df3$FTR == 'Team1', df3$Avg1 -1, ifelse(df3$Favorite == 0 & df3$FTR != 'Team1', -1, 0))

sum(df3$PL_U, na.rm = T)

df3$PL_H <- ifelse(df3$Home == 1 & df3$FTR == 'Team1', df3$Avg1 - 1 , ifelse(df3$Home == 1 & df3$FTR != 'Team1', -1, 0))

sum(df3$PL_H, na.rm = T)

df3$PL_A <- ifelse(df3$Home == 0 & df3$FTR == 'Team1', df3$Avg1 -1, ifelse(df3$Home == 0 & df3$FTR != 'Team1', -1, 0))

sum(df3$PL_A, na.rm = T)

df3$PL_D <- ifelse(df3$FTR == 'Draw' & df3$Home == 1, df3$AvgD - 1, ifelse(df3$Home == 1 & df3$FTR != 'Draw', -1, 0))

sum(df3$PL_D)

df3$PL_opp <- ifelse(df3$Home == 0 & df3$FTR == 'Team2', df3$Avg2 - 1 , ifelse(df3$Home == 0 & df3$FTR != 'Team2', -1, 0))

sum(df3$PL_opp)
# df3$PL_D <- ifelse(df3$Home == 0 & df3$FTR == 'Draw', df3$AvgD -1, ifelse(df3$Home == 0 & df3$FTR != 'Draw', -1, 0))

df3$PL_over <- ifelse(df3$Home == 1 & df3$TotalGoals > 2.5, df3$Avg.2.5.over - 1, ifelse(df3$Home == 1 & df3$TotalGoals < 2.5, - 1, 0))

sum(df3$PL_over)

df3$PL_under <- ifelse(df3$Home == 1 & df3$TotalGoals < 2.5, df3$Avg.2.5.under - 1, ifelse(df3$Home == 1 & df3$TotalGoals > 2.5, - 1, 0))

sum(df3$PL_under, na.rm = T)

df3$points <- ifelse(df3$FTR == 'Team1', 3, ifelse(df3$FTR == 'Draw', 1, 0))

table(df$FTR)

df3%>%
  group_by( Home)%>%
  summarise(sum = sum(PL_F, na.rm = T), n())

df3%>%
  group_by(Home)%>%
  summarise(sum = sum(PL_U, na.rm = T), n())

df3%>%
  group_by(Home)%>%
  summarise(sum = sum(PL_D, na.rm = T), n())


df3%>%
  group_by( Favorite)%>%
  summarise(sum_fav = sum(PL_D), n())

teams1 <- df3%>%
  group_by(Team1)%>%
#  mutate(PL = PL_H + PL_A + PL_D)
  summarise(PL_A = sum(PL_A), n())

colnames(teams1) <- c('Team', 'Profit/Loss', '# games')

teams2 <- df3%>%
  group_by(Team1)%>%
  #  mutate(PL = PL_H + PL_A + PL_D)
  summarise(PL_H = sum(PL_H), n())

teams3 <- df3%>%
  group_by(Team1)%>%
  #  mutate(PL = PL_H + PL_A + PL_D)
  summarise(PL = sum(PL_A + PL_H), PL_H = sum(PL_H), PL_A = sum(PL_A), 
            PL_D = sum(PL_D), PL_F = sum(PL_F), PL_U = sum(PL_U))%>%
  arrange(desc(PL))

colnames(teams3) <- c('Teams', 'Home & Away', 'Home', 'Away', 'Draws', 'Favorites', 'Underdogs')


rbind(teams3, c('SUM', colSums(teams3[,c(2:7)]),''))


df3%>%
  group_by(FTR)%>%
  summarise(PL = sum(PL_over, na.rm = T), n())

plot(cumsum(df3$PL_over), type = 'h', ylab = 'cumulative return', xlab = 'betting on over 2.5 goals', xaxt='n')

hist(df3$Avg.2.5.over)

df3%>%
  group_by(Team1)%>%
  summarise(sum(PL_over))

df3%>%
  group_by(Team1)%>%
  summarise(mean(TotalGoals))

mean(df3$TotalGoals)

table(df3$Team1, df3$FTR)

df3%>%
  group_by(FTR)%>%
  summarise(PL = sum(PL_over), n())


df3%>%
  group_by(Team1)%>%
  summarise(count(FTR), n())

res <- df3%>%
  group_by(Team1)%>%
  summarise(games = n(), Points = sum(points), goals_for = sum(Score1), goals_against = sum(Score2), 
            goals_diff = goals_for - goals_against)%>%
  arrange(desc(Points))#%>%
#  kable

df3%>%
  count(FTR, Team1)

df3 <- df3 %>%
  group_by(Team1) %>%
  mutate(goals_vor = lag(cumsum(Score1), 1, default = 0), 
         goals_against = lag(cumsum(Score2), 1, default = 0),
         goals_diff = goals_vor - goals_against) %>%
  ungroup()


##

# install.packages("gridExtra")
# install.packages("png")
library(flextable)

df <- data.frame(Teams = row.names(df), unclass(table(df3$Team1, df3$FTR)))

names(res) <- c('Teams', 'Games', 'Points', 'Goals for', 'Goals against', 'Goal difference')


ft <- flextable(teams3)

# Customize the table: you can adjust the font size and reduce the row height
ft <- ft %>%
  fontsize(size = 8) %>%   # Set font size
  padding(padding = 1, part = "all") %>%  # Adjust padding
  autofit()  # Automatically adjust the column widths

save_as_image(ft, path = "table_image.png")

ggplot(df3, aes(x = Date, y = cumsum(PL_over))) +
  geom_line(color = "blue", size = 1) +    # Add a line
  geom_point(color = "red", size = 2) +    # Add points
  labs(
    title = "Cumulative Sum of PL_over Over Time",
    x = "Date",
    y = "Cumulative Sum"
  ) +
  theme_minimal(base_size = 15) +          # Use a minimal theme with larger base size
  theme(
    plot.title = element_text(hjust = 0.5), # Center the plot title
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    panel.grid.major = element_line(color = "grey80"), # Major grid lines
    panel.grid.minor = element_line(color = "grey90")  # Minor grid lines
  )
