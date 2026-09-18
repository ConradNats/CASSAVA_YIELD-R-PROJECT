# R Learning Roadmap Used in This Project

## Step 1: Variables

In R, a variable stores something:

```r
x <- 10
name <- "Conrad"
```

`<-` means "put this value into this variable."

## Step 2: Data frames

A data frame is similar to an Excel table:

```r
df <- read_excel("data/raw/Cassava_Yield_Data.xlsx",
                 sheet = "Cassava Data")
```

## Step 3: Selecting a column

```r
df$ferT
df$TotalWeightperhectare
```

## Step 4: Functions

Functions perform actions:

```r
mean(df$TotalWeightperhectare)
summary(df)
```

## Step 5: Filtering

With dplyr:

```r
df %>%
  filter(tillage == "conv")
```

## Step 6: Grouping

```r
df %>%
  group_by(ferT) %>%
  summarise(mean_weight = mean(TotalWeightperhectare))
```

## Step 7: Graphs

ggplot2 follows the idea:

```r
ggplot(data, aes(x = variable, y = variable)) +
  geom_point()
```

## Step 8: Statistical tests

Correlation:

```r
cor.test(x, y)
```

Two groups:

```r
t.test(outcome ~ group, data = df)
```

Two categorical variables:

```r
chisq.test(table(df$group1, df$group2))
```

More than two groups:

```r
aov(outcome ~ group, data = df)
```

## The most important beginner habit

Run the analysis in small sections. After every command, ask:

1. What did R return?
2. What does the result mean?
3. Why did I use this function?
4. What would change if I changed the variable?
