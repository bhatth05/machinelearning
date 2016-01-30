# Set working directory
setwd("C:/Users/Administrator/Documents")

# Load packages
require(knitr)
require(markdown)

# Create .md, .html, and .pdf files
knit("MachineLearning.Rmd")
markdownToHTML('MachineLearning.md', 'MachineLearning.html', options=c("use_xhml"))
system("pandoc -s MachineLearning.html -o MachineLearning.pdf")
