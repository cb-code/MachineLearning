
# chb2132 - APAN5355 Fall 2020 - HW0 Due Sept 21, 6pm
# Hw0 | chb2132
# ML | APAN 5335

# setting seed for reproducible results
set.seed(1984)

# loading in initial libraries, will load in other libraries as needed
library(tidyverse)

# 1. R basics / warm up (6 points)
# (1 points) Write a function called roll_die() that returns an integer between 1 and 6, distributed uniformly.

roll_die <- function(){
    die <- 1:6;
    N <- 1;
    
    roll <- sample(die, size = N, replace = TRUE);
    return(roll);
}

roll_die();

# (2 points) Write a function roll_loaded_die(weighted_value, loaded_weight) that simulates the roll of a loaded die, where loaded_weight is the probability mass that is placed on on the specified weighted value. Your function should validate that weighted_value \(\in \{1,2,3,4,5,6\}\) and that loaded_weight doesn’t violate any rules about probabilities.

roll_loaded_die <- function(weighted_value, loaded_weight){
    
    valid_val <- between(weighted_value, 1, 6);
    valid_weight <- between(loaded_weight, 1, 100);
    
    die <- 1:6;
    weights <- rep(1, 6);
    
    # checking to see if input parameters are valid   
    if(valid_val && valid_weight){
    
        # assigning remaining distribution to unchosen values in the weighting vect.
        unloaded_weight <- (100.00000 - loaded_weight)/5;
        
        # assigning the designated value the designated weight
        weights <- rep(unloaded_weight, 6);
        weights[weighted_value] <- loaded_weight;
        
        wroll <- sample(die, size = 1, replace = TRUE, prob = weights);
        
        return(wroll);
    }
    
    # if incorrect parameters are passed, will return and provide parameter guide text
    else{
        return("Please enter a valid weighted value (1-6) and loaded_weight (1-100)");
    }
}

roll_loaded_die(); 

# (1 points) Generate a sample of 10000 rolls for each of your fair and loaded dice, using the functions that you defined above (choose any loading and weighted value you wish).

# replicating 10000 rolls of each function with arbitrary parameters for loaded_rolls

fair_rolls <- replicate(10000, roll_die());

loaded_rolls <- replicate(10000, roll_loaded_die(3,14));

# (2 points) Plot two histograms showing the distributions of the samples from your fair and loaded dice rolls. (Either show them on the same plot, or stacked one above the other so that they are easy to compare).

# loading in library used for plotting
library(ggplot2)

fair <- data.frame(as.numeric(fair_rolls))
loaded <- data.frame(as.numeric(loaded_rolls))

# creating data frame with fair/loaded value for dice id/attribute
fair$dice <- 'fair_rolls'
loaded$dice <- 'loaded_rolls'

# setting column names for the combined dataframe for plotting
colname_labels <- c("roll", "dice")

colnames(fair) <- colname_labels
colnames(loaded) <- colname_labels

# combining fair and loaded die rolls into one data frame to plot
co_trolls <- rbind(fair, loaded)

ggplot(co_trolls, aes(roll, fill = dice)) + 
   geom_histogram(alpha = 0.25)

# 2 (10 points). Recall the example discussed in class of using Bayes’ Rule in the interpretation of a diagnostic test, such as the ones now being used to test for COVID antibodies.
# (5 points) Write a function that calculates the posterior probability of a patient having COVID, given \(P(Cov)\), the prior (the prevalence in the population), the test sensitivity (true positive rate) \(P(Positive | Cov)\), and the false positive rate (1 - specificity), \(P(Pos | \neg Cov)\).

# Setup of problem, provided probabilties, equations, and some calculations to derive implemented function formula

# P_A <- P(Covid) (Prior, Given)
# P_Na <- 1 - P_A (1 - P(Covid) = P(-Covid) (Calulated)

# P_BgA <- P(Positive|Covid) (Sensitivity, Given)
# P_BgNa <- P(Positive|-Covid) (False Positive Rate, Given)

# P_B <- P(Positive) = (P(Positive|Covid) * P(Covid)) + (P(Positive|-Covid) * P(-Covid)) (Calculated)

# P_AgB <- P(Covid|Positive) (Posterior Probability, To Calculate)

# P(Covid|Positive) <- (P(Positive|Covid) * P(Covid)) / P(B))
# P(Covid|Positive) <- (P(Positive|Covid) * P(Covid)) / ((P(Postitive|Covid)*P(Covid)) + (P(Positive|-Covid) * P(-Covid)))

bayes_post_prob <- function(P_A, P_BgA, P_BgNa){
    P_B <- (P_BgA * P_A) + (P_BgNa * (1 - P_A));
    P_AgB <- (P_BgA * P_A) / P_B;
    
    return(P_AgB);
}

# (5 points) Using your function, generate a figure that illustrates how the posterior probability of having COVID given a positive test changes as a function of the sensitivity (between 50% and 100%), specificity (between 50% and 100%), and the prior (between 0.01% and 5%). (Try to come up with a good graphical representation that allows you to vary the three variables; you might make a sequence of plots and/or use multiple lines on the plot for different values, and/or plot in 3D)

# setting values for each probability based on given parameters and goal of equal length vectors
pa <- seq(.01, 5, 1)
pba <- seq(50, 100, 12.5)
pbna <- seq(50, 100, 12.5)

# plug 3 vars into function for output var, generate data frame of all four var values
pp <- bayes_post_prob(pa, pba, pbna);
ppvs <- data.frame(pa, pba, pbna, pp);

plot(ppvs);

# 3. Iris Dataset (4 points)
# The \(iris\) dataset is a popular toy dataset used to illustrate machine learning algorithms, and is provided with R.
# (1 points) How many observations are in the iris dataframe? What are the variables?

# viewing iris dataset, specific description provided below
str(iris)
nrows(iris)
colnames(iris)

# There are 150 observations of 5 different variables included in the iris dataset that comes with R. The variables names are as follows:
# [1] Sepal.Length [2] Sepal.Width [3] Petal.Length [4] Petal.Width [5] Species

# (1 points) Convert the dot characters in the column names of this dataframe to underscores. (Do it in a functional way that would work even if there were hundreds of column names, rather than just manually reassigning the names)

# substituting . chars with _ chars for the column name for all of the iris dataset columns
colnames(iris) <- gsub(".", "_", colnames(iris), fixed = TRUE)

# (2 points) Using the graphics package of your choice, generate a grid of scatterplots comparing each variable against the others, and color the points by species. Briefly describe what you observe.

# combining all vars into a single vector, grouped by/coloured by species, to be plot altogether into a single scatter plotS
iris_melt <- reshape2::melt(iris, id.var = 'Species')
    
ggplot(iris_melt, aes(x = variable, y = value, colour = as.factor(Species))) + 
    geom_point() + xlab('Variable') + ggtitle('Iris Variables Coloured By Species')
