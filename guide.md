# What this app contains

This app consist in two differentiated parts in their corresponding tab:

- The representation of the variables by the classification label (Income higher or lower than US$ 50K).
- The attempt to predict the belonging to one class or the other based on your selection.
- A report generator.

# How this app works

In *The Variables* tab, you can find two checkboxes to select the desired data that will form the corresponding density histograms and barplots.
Underneath the interface, a `reactive` element catches the options selected and filters the information based on it. `ggplot` does the rest of the job!

In the *Predict your Income Class* tab, the intention was to allow the user to generate its own predictions. The set of slider, radio buttons and pickers allow to tune all variables. Then, those inputs are used to generate a `data.frame`. A simple logistic regression model is generated in the server, and `predict()` uses those two elements to create a value with the probability of earning less than 50K. That probability is converted to a label (A = Less than 50K, or B), which is intended to be used as a trigger for the revealing of a `conditionalPanel()` with a custom message and picture for each result. This last thing did not work, for some reason. That is why the result is shown as a `textOutput` element.

There is also a report generator in other tab: it will return a HTML file with your variable charts (as Plotly interactive objects!) in one section and the values to predict plus the prediction itself in the other. You just have to click the button!

# Bugs

Fortunately, the app seems bug-free as it is today! Just one thing that is a bit annoying is that the color of the plots will always be red if only one label is selected, istead of always keeping the same color for each class. For sure there is a way around this, but I did not find it.

# References

 - [Shiny on RStudio](https://shiny.rstudio.com/)
 - [Shiny tutorial also used](https://mastering-shiny.org/)
 - [Shiny Function reference version 1.2.0](https://shiny.rstudio.com/reference/shiny/1.2.0/)
 - [UCI Repository - Main Page](https://archive.ics.uci.edu/ml/datasets/Adult)
 - [Adult Income Dataset - Direct Download](https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data)
