#Exercise 1 - Sparse matrices 
##Write an R function that:
##1a. Reads a dense matrix from a file.
##1b. Converts the dense matrix into a sparse matrix.
##1c. Computes the sparsity percentage (percentage of zero elements).
##1d. Saves the sparse matrix to disk, allowing the output file name to be specified.
##1e. Returns the number of non-zero elements and the sparsity percentage.
setwd("~/bioinformaticexam_030926")
install.packages("Matrix")
library(Matrix)
exam_function<- function(imput_matrix, output_file){
  #1a
  dense_matrix<-readMM(imput_matrix)
  #1b
  sparse_matrix<-Matrix(dense_matrix, sparse=TRUE)
  #1c
  sparse_percent<-sum(sparse_matrix==0)/length(sparse_matrix)*100
  #1d
  writeMM(sparse_matrix, file=output_file)
  print(sparse_matrix) 
  cat("Sparsity percentage is", sparse_percent, "while numbers different form zero are", nnzero(sparse_matrix), "\n")
}
test_result<-exam_function("matrix.mtx", "test_results.mtx")
#I save the function in another file Rscript, in the same folder, called exam_function

#Exercise 2 - Docker and Front-End 
#Using the function developed in the previous exercise:
#2a. Create a Dockerfile containing R.
#2b. Place the developed function inside the container.
#2c. Install the required libraries.
#2d. Write a frontend function that runs the container, passing parameters from the command line.
#2e. Share a local folder using a Docker volume.
#2f. Execute the function inside the container.
#2g. Retrieve the generated output file and print the sparsity percentage to screen.
setwd("~/bioinformaticexam_030926")
args<-commandArgs(trailingOnly = TRUE)
source("exam_function.R")
results<-exam_function(args[1], args[2])
#i save this code in a Rscript called esegui.R
#i create a textfile in the same wd called Dockerfile, in which i put:
FROM rocker/r-ver:4.3.0
RUN R -e "install.packages('Matrix', repos='https://cloud.r-project.org/')"
COPY exam_function.R /app/exam_function.R
COPY esegui.R /app/esegui.R
WORKDIR /app
#then i open the Terminal and wrote:
docker build -t matrix_exam .
docker run -v "%cd%":/app/dati exam Rscript esegui.R dati/filtered_feature_bc_matrix/matrix.mtx dati/test_results.mtx

#Exercise 3 - R package 
##Using the functions developed in the previous exercises:
##3a. Create an R package.
##3b. Add the functions to the package.
##3c. Document the functions using roxygen2.
##3d. Generate the documentation.
##3e. Build the package.
##3f. Run the package check (R CMD check).
##3g. Publish the project on GitHub together with a README.
setwd("~/bioinformaticexam_030926")
#install devtools and roxygen2
install.packages(c("devtools", "roxygen2", "usethis", "Rtools"))
library(devtools)
devtools::create("toolbox")#to create a folder toolbox in my current wd
#move exam_function.R to the new folder toolbox/R
#set the wd to toolbox
devtools::document()
usethis::use_package("Matrix")#add the necessity to have also the Matrix package installed and loaded
#write line for roxygen2 above the function using #'
setwd("~/bioinformaticexam_030926/toolbox")#set the wd into toolbox folder
devtools::install()
devtools::check()
#create github package
#https://github.com/rebeccapasteris500-wq/toolbox.git #link of my toolbox
#oper the terminal on the correct wd, in toolbox and digit
#git init
#git add .
#git commit -m "first commit" or whatever
#git branch -M main
#git remote add origin https://github.com/rebeccapasteris500-wq/toolbox.git
#git push -u origin main

#Exercise 4 - Fixing ggplot2 code 
#The following code contains 3 errors. Identify and fix them.
library(ggplot2)
plotDistribution <- function(df){
  p <- ggplot(df, aes(x = value, y = group)) +
    geom_boxplot()
  ggsave(plot.pdf)
  return(df)
}
#CORRECT CODE
library(ggplot2)
plotDistribution<-function(df){
  p<-ggplot(df,aes(x=group,y=value)) + #invert the axes
    geom_boxplot()
  ggsave("plot.pdf",plot=p) #""required and also the indication of the plot
  return(p)#return the plot not the function
}

#Exercise 5 - Debugging an R function (4 points)
#The following code contains errors. Identify and fix them.
sparsityInfo <- function(mat){
  total <- length(mat)
  nonzero <- sum(mat = 0)
  cat("Non-zero elements:", nonzero, "\n")
  cat("Sparsity:", sparsity, "\n")
}
runSparsityInfo <- function(file){
  data <- read.csv(file)
  SparsityInfo(data)
}
#CORRECT CODE
sparsityInfo<-function(mat){
  total<-length(mat)
  nonzero<-sum(mat!=0) #necessity to add ! 
  zero<-sum(mat==0) #needed to define sparsity
  sparsity<-(zero/total)*100 #necessity to define the sparsity variable
  cat("Non-zero elements:",nonzero,"\n")
  cat("Sparsity:",sparsity,"\n")
}
runSparsityInfo<-function(file){
  data<-read.csv(file)
  sparsityInfo(data) #no capital letter
}