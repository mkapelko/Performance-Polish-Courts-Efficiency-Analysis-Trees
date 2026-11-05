###Programming Convexified EAT in Polish courts of justice

###PACKAGES

install.packages("eat")

install.packages("readxl")

library(readxl)
library(eat)


###We assume we have a dataset in Excel format (data_final_m1.xlsx) which for model m1 consists of three inputs and three outputs

datos_m1<- read_excel("data_final_m1.xlsx", sheet = 1)
                       

###We save the seed for reproducibility of the results

set.seed(120)

###FINDING THE HYPERPARAMETER OF THE BEST EAT MODEL


x <- c(1,2,3)

y <- c(4,5,6)

x_ordenado <- sort(x)
y_ordenado <- sort(y)

x <- x_ordenado 
y <- y_ordenado

x
y

n <- nrow(datos_m1) # Observations in the dataset
selected <- sample(1:n, n * 0.8) # Training indexes
training <- datos_m1[selected, ] # Training set
test <- datos_m1[- selected, ] # Test set
grid_best_EAT <- bestEAT(training = training,
                             test = test,
                             x, 
                             y,
                             numStop = c(5,10,15,20),
                             fold = 5)

res_numStop <- grid_best_EAT$numStop[1]


###WE DEVELOP THE BEST MODEL


model_m1 <- eat::EAT(data = datos_m1, x, y,numStop = res_numStop, fold=5) 

resumen_model <- summary(model_m1)

sink("results_m1.txt")
summary(model_m1)
sink()  

###WE DRAW THE EFFICIENCY ANALYSIS TREE 

png("tree_graph_m1.png")
plotEAT(object = model_m1)
dev.off()

###EFFICIENCY SCORES OF THE BCC (VRS) MODEL (OUTPUT-ORIENTED) USING CEAT


scores_EAT_m1 <- efficiencyCEAT(data = datos_m1,
                                 x, 
                                 y,
                                 object = model_m1, 
                                 scores_model = "BCC.OUT",
                                 digits = 3,
                                 DEA = TRUE,
                                 print.table = TRUE,
                                 na.rm = TRUE)

sink("scores_m1.txt")
scores_EAT_m1
sink()  

#Export results to xlsx format

install.packages("writexl")
library(writexl)
write_xlsx(scores_CAT_m1,"CEATBCC.xlsx")


###CREATION OF VIRTUAL POINTS

columnas <- c(x, y)

columnas <- sort(columnas)

# Create new data frame with selected columns
df <- datos_m1[, columnas]

# Extract vectors "a" and predictions "y" from the model
a_vectors <- model_m1[["model"]][["a"]]
a_vectors

y_predictions <- model_m1[["model"]][["y"]]
y_predictions

filas_ampl <- data.frame(cbind(a_vectors,y_predictions))

#We export virtual points to xlsx format

library(writexl)
write_xlsx(filas_ampl,"virtual_m1.xlsx")