---
title: "Ejemplo"
date: 2020-06-08T08:06:25+06:00
description: Introduction to Sample Post
menu:
  sidebar:
    name: Ejemplo
    identifier: ejemplo
    weight: 50
tags: ["Basic", "Multi-lingual"]
categories: ["Basic"]
---

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

We know $se=\frac{\sigma}{\sqrt{n}}$. Solving
$4\frac{\sigma}{\sqrt{n}}=1 \Rightarrow n=16\sigma^2$ therefore if
*σ* = 15% yields 16(15<sup>2</sup>) = 3, 600 years.

The Standard Error is
$SE_i=\sqrt{var(m_i)} = \sqrt{\frac{1}{i-1}\sum\_{j=1}^i(y_j-m_i)^2}$

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
