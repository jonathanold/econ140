library(tidyverse)
library(plotly)
library(moderndive)

install.packages('reticulate')
reticulate::install_miniconda()
reticulate::conda_install('r-reticulate', 'python-kaleido')
reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
reticulate::use_miniconda('r-reticulate')


# Make up data- please ignore! ---------------------------------------------------------

set.seed(123456789)
n = 300
data = data.frame(seq(1, n))
data$n = data$seq.1..3000.
data$seq.1..3000.=NULL
data$male = 0
data$male[data$n>=1613]=1
data$n = NULL

# Individual effect
data$individual = rnorm(n,0,1)
data$ability = rnorm(n,0,1)

sampleDist = function(n) { 
  sample(x = c(8,9,10,11,12,13,14,15,16,9,10,11,12,13,14,10,11,15,12,17,17,10,12,11,13,15,15,14, 12, 12, 16, 11, 10, 10, 7, 10, 12, 12, 15, 15, 8, 9, 10, 12), n, replace = T) 
}

# Make education variable
data$education = sampleDist(n)
data$education = round(data$education+data$ability*3.5,digits=1)
data$education[data$education<=6] = 6
data$education[data$education>=20] = 20


# Gender
data$gender = 2*data$ability + rnorm(n,0,1)
data$gender[data$gender>0]=1
data$gender[data$gender<=0]=0
data$female = data$gender

# Important:
# Wages: We create a wage variable and then evaluate our regression based on that.
# Monthly wages are a function of an individual effect, ability, and gender.
data$wage_monthly = 2000 + 100*data$individual + 400*data$ability - 500*data$female




## Set up graph
# Define 3D scatterplot points --------------------------------------------
# Get coordinates of points for 3D scatterplot
x_values <- data$education %>% 
  round(3)
y_values <- data$ability %>% 
  round(3)
x2_values <- data$female 

z_values <- data$wage_monthly %>% 
  round(3)


# Define regression plane -------------------------------------------------
# Construct x and y grid elements
x_grid <- seq(from = min(x_values), to = max(x_values), length = 50)
y_grid <- seq(from = min(y_values), to = max(y_values), length = 50)
x2_grid <- seq(from = -0.5, to = 1.5, length = 50)

# Construct z grid by computing
# 1) fitted beta coefficients
# 2) fitted values of outer product of x_grid and y_grid
# 3) extracting z_grid (matrix needs to be of specific dimensions)
beta_hat <- data %>% 
  lm(wage_monthly ~ education + ability, data = .) %>% 
  coef()
fitted_values <- crossing(y_grid, x_grid) %>% 
  mutate(z_grid = beta_hat[1] + beta_hat[2]*x_grid + beta_hat[3]*y_grid)
z_grid <- fitted_values %>% 
  pull(z_grid) %>%
  matrix(nrow = length(x_grid)) %>%
  t()



beta2_hat <- data %>% 
  lm(wage_monthly ~ ability + female, data = .) %>% 
  coef()
fitted2_values <- crossing(y_grid, x2_grid) %>% 
  mutate(z2_grid = beta2_hat[1] + beta2_hat[2]*y_grid + beta2_hat[3]*x2_grid)

z2_grid <- fitted2_values %>% 
  pull(z2_grid) %>%
  matrix(nrow = length(x2_grid)) %>%
  t()



# Define text element for each point in plane
text_grid <- fitted_values %>% 
  pull(z_grid) %>%
  round(3) %>% 
  as.character() %>% 
  paste("Monthly wages: ", ., sep = "") %>% 
  matrix(nrow = length(x_grid)) %>%
  t()

text2_grid <- fitted2_values %>% 
  pull(z2_grid) %>%
  round(3) %>% 
  as.character() %>% 
  paste("Monthly wages: ", ., sep = "") %>% 
  matrix(nrow = length(x2_grid)) %>%
  t()




# Plot using plotly -------------------------------------------------------
fig = plot_ly() %>%
  # 3D scatterplot:
  add_markers(
    x = x_values,
    y = y_values,
    z = z_values,
    marker = list(size = 5),
    hoverinfo = 'text',
    text = ~paste(
      "Monthly wages: ", z_values, "<br>",
      "Ability: ", y_values, "<br>",
      "Education: ", x_values 
    )
  )  %>%
  # Axes labels and title:
  layout(
    title = list(text="3D scatterplot and regression plane", size=24),
    scene = list(
      zaxis = list(title = "y: Monthly wages"),
      yaxis = list(title = "x2: Ability to earn money"),
      xaxis = list(title = "x1: Education")
    )
  ) %>%
  # Regression plane:
  add_surface(
    x = x_grid,
    y = y_grid,
    z = z_grid,
    hoverinfo = 'text',
    text = text_grid)

fig






####### Second example (not discussed in the course): Gender wage gap
  
  # Plot 2 using plotly -------------------------------------------------------
  plot_ly() %>%
    # 3D scatterplot:
    add_markers(
      x = x2_values,
      y = y_values,
      z = z_values,
      marker = list(size = 5),
      hoverinfo = 'text',
      text = ~paste(
        "Monthly wages: ", z_values, "<br>",
        "Female: ", x2_values, "<br>",
        "Education: ", x_values 
      )
    ) %>%
    # Axes labels and title:
    layout(
      title = "3D scatterplot and regression plane",
      scene = list(
        zaxis = list(title = "y: Monthly wages"),
        yaxis = list(title = "x2: Ability"),
        xaxis = list(title = "x1: Female")
      )
    ) %>%
  # Regression plane:
  add_surface(
    x = x2_grid,
    y = y_grid,
    z = z2_grid,
    hoverinfo = 'text',
    text = text2_grid
  ) 
  
  
  
  
  
