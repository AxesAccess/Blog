# let's merge data with europe to see if some countries are missing

`%ni%` <- Negate(`%in%`)

# set working directory
setwd("/mnt/Projects/Blog/posts/20240927-euro-tech-money")

# merge data with fix-countries
fix.countries <- read_csv("data/fix-countries.csv")
data <- data |>
  left_join(fix.countries, by = c("Country" = "Country")) |>
  mutate(Country = ifelse(is.na(Fix), Country, Fix)) |>
  select(-Fix) |>
  arrange(Country)


data <- data |> mutate(Country = trimws(Country))

data |>
  filter(str_to_upper(data$Country) %ni% unique(str_to_upper(world.cities$country.etc))) |>
  select(Country) |>
  distinct() |>
  arrange(Country) |>
  print()


data <- data |> mutate(Job.Title = trimws(Job.Title))

titles <- unique(data$Job.Title) |> data.frame()

titles$Category <- "?"

colnames(titles) <- c("Job.Title", "Category")

titles |> write.csv("data/job-titles.csv", row.names = FALSE)

titles <- titles |> select(-Category)

categories <- read.csv("data/job-categories.csv") |> mutate(Job.Title = str_to_lower(Job.Title)) |> unique()

titles <- titles |>
  mutate(Job.Title = str_to_lower(Job.Title)) |>
  unique()

colnames(titles) <- c("Job.Title")

titles |>
  left_join(categories, by = c("Job.Title" = "Job.Title")) |>
  filter(is.na(Category)) |>
  distinct() |>
  write.csv("data/job-titles.csv", row.names = FALSE)

titles |>
  group_by(Category) |>
  summarise(n = n()) |>
  arrange(desc(n))

#

data$Country <- reorder(data$Country, data$Pre.Tax.TC, FUN = mean)

data |> inner_join(cities, by = c("City" = "name")) |>
  arrange(mean(Pre.Tax.TC)) |>
  ggplot(aes(x = factor(country.etc), y = Pre.Tax.TC)) +
  geom_boxplot() +
  stat_summary(
    fun.y = "mean",
    geom = "point",
    shape = 23,
    size = 3,
    fill = "white"
  ) +
  coord_flip()

#

by_country <- by_country |>
  arrange(desc(resp_count)) |>
  head(20)

by_country$Country <- reorder(by_country$Country, by_country$mean_salary)

by_country |>
  ggplot(aes(x = mean_salary, y = Country)) +
  geom_point() +
  geom_errorbarh(
    aes(xmin = mean_salary - 1.96 * se_salary, xmax = mean_salary + 1.96 * se_salary),
    height = 0
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "mean_salary", y = "")


```{r salaries-by-country}
by_country <- data |>
  inner_join(cities, by = c("City" = "name")) |>
  group_by(country.etc) |>
  summarise(
    median_salary = median(Pre.Tax.TC, na.rm = TRUE),
    resp_count = n(),
    mean_salary = mean(Pre.Tax.TC, na.rm = TRUE),
    sd_salary = sd(Pre.Tax.TC, na.rm = TRUE),
    se_salary = sd_salary / sqrt(resp_count)
  )

by_country <- rename(by_country, Country = country.etc)

by_country |>
  arrange(desc(resp_count)) |>
  head(20) |>
  kable()
```


df <- data.frame(x = 1:10 , y = 1:10)
p <- ggplot(df, aes(x, y))
p + geom_line(linetype = 2)
p + geom_line(linetype = "dotdash")

# An example with hex strings; the string "33" specifies three units on followed
# by three off and "3313" specifies three units on followed by three off followed
# by one on and finally three off.
p + geom_line(linetype = "3313")

# Mapping line type from a grouping variable
ggplot(economics_long, aes(date, value01)) +
  geom_line(aes(linetype = variable))

# Linewidth examples
ggplot(economics, aes(date, unemploy)) +
  geom_line(linewidth = 2, lineend = "round")
ggplot(economics, aes(date, unemploy)) +
  geom_line(aes(linewidth = uempmed), lineend = "round")

# Size examples
p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point(size = 4)
p + geom_point(aes(size = qsec))
p + geom_point(size = 2.5) +
  geom_hline(yintercept = 25, size = 3.5)

# Shape examples
p + geom_point()
p + geom_point(shape = 5)
p + geom_point(shape = "k", size = 3)
p + geom_point(shape = ".")
p + geom_point(shape = NA)
p + geom_point(aes(shape = factor(cyl)))

# A look at all 25 symbols
df2 <- data.frame(x = 1:5 , y = 1:25, z = 1:25)
p <- ggplot(df2, aes(x, y))
p + geom_point(aes(shape = z), size = 4) +
  scale_shape_identity()
# While all symbols have a foreground colour, symbols 19-25 also take a
# background colour (fill)
p + geom_point(aes(shape = z), size = 4, colour = "Red") +
  scale_shape_identity()
p + geom_point(aes(shape = z), size = 4, colour = "Red", fill = "Black") +
  scale_shape_identity()



```{r salaries-by-country-boxplot, fig.width=6, fig.height=10}
library(scales)

data |>
  inner_join(cities, by = c("City" = "name")) |>
  select(country.etc, Pre.Tax.TC) |>
  ggplot(aes(
    x = reorder(factor(country.etc), Pre.Tax.TC, median),
    y = Pre.Tax.TC
  )) +
  geom_boxplot() +
  stat_summary(
    fun.y = "mean",
    geom = "point",
    shape = 23,
    size = 3,
    fill = "white"
  ) +
  theme_minimal() +
  coord_flip() +
  labs(
    x = "",
    y = ""
  ) +
  scale_y_continuous(labels = scales::comma)
```


