# FOMC-NLP

This repository contains files used in "Macroeconomic Effects of Federal Reserve's Unconventional Monetary Policy: A Computational Linguistics Approach" (2020).

### Paper Abstract
I propose a new approach to estimating the effects of Federal Reserve’s unconventional monetary policy after the Great Recession on the economy. During the 2009-2015 U.S. zero lower bound (ZLB) period, the Federal Reserve resorted to unconventional policies like forward guidance and large-scale asset purchases. I quantify these policies by creating an unconventional monetary policy index using computational linguistics and machine learning on the FOMC meeting. This index provides information on how much of the discussion during each meeting was pertaining to the large-scale asset purchase program and forward guidance. A positive shock to this measure can be interpreted as more discussion about this topics in particular meeting. Using such shocks as instruments in a in a factor-augmented vector auto-regression framework, I find that the Fed’s unconventional policy had its desired effects of stimulating the economy, by increasing output and reducing the unemployment rate. I also find significant, albeit less, effects on the financial markets.

Paper available at https://sites.google.com/view/prithachaudhuri/research

### Description of files
* fomc_minutes_2.Rmd: this is the main file, describing the text analysis and LDA process on FOMC meeting minutes. This is followed by the econometric analysis using a FAVAR model.
* bayesian_var.R: this script describes the function to implement Bayesian VAR using Gibbs sampler to estimate the FAVAR model. It's called in main file. 
* get_favar_data.R: this script describes how I obtained the dataset for the FAVAR analysis from FRED and Quandl using their API. 
* data: this folder contains three csv files. fomc1993_2019.csv is used to web scrape the FOMC meeting minutes (description in main file). fredmetrics.csv and quandlmetrics.csv contain codes to extract data from FRED and Quandl websites using API keys (description in get_favar_data.R).
