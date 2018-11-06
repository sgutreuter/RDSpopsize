# RDSpopsize

A very small R package implementing two-source (mark and recapture) population size estimation 
(PSE) where the "recapture" sampling was embedded in an RDS survey.  Because the "multiplier 
method" used in epidemiology is identical to two-source PSE, this code is applicable for sampling
performed under that alias.

This package was created for provisional use.  There remain substantial methodological gaps
in PSE applications in epidemiology.  For example, it is easy to envision multiple-source PSE
wherein one of the follow-up sampling efforts is embedded in an RDS survey, but others are 
not.  Additionally, there is need for estimators bridging multiple RDS surveys.

Installing the package from R

library(devtools);
install_github("sgutreuter/RDSpopsize")
