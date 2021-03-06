% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{sim_wrightfisher}
\alias{sim_wrightfisher}
\title{Simulate from simple Wright-Fisher model}
\usage{
sim_wrightfisher(N, L, alleles, mu, m_matrix, t_out, output_format = 3)
}
\arguments{
\item{N}{number of individuals per deme - currently must be the same for all
demes.}

\item{L}{number of loci.}

\item{alleles}{number of alleles. Can be single number for all loci, or
vector of length \code{L}.}

\item{mu}{mutation rate. Assumes finite-alleles model, with equal chance of
mutating from any allele to any other.}

\item{m_matrix}{migration matrix specifying the per-generation probability of
an individual migrating from any deme (in rows) to any other deme (in
columns).}

\item{t_out}{vector of times at which results will be output.}

\item{output_format}{choose the output format. 1 = counts, 2 = list of
genotypes over demes, 3 = matrix of genotypes over demes.}
}
\description{
Simulate Wright-Fisher evolution. The model used here is
  currently very basic and makes a number of simplifying assumptions, but may
  be extended in future to add flexibility.
}
\details{
Currently assumes haploid population and independent loci (no
  linkage disequilibrium). Initialises from symmetric Dirichlet(1) allele
  frequencies at every locus. Due to the way migration is currently
  implemented, \code{N} must be the same for all demes.
}
