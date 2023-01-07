
#include <Rcpp.h>

//------------------------------------------------
// Simulate from simple Wright-Fisher model
// [[Rcpp::export]]
Rcpp::List sim_wrightfisher_cpp(Rcpp::List args, Rcpp::List args_functions, Rcpp::List args_progress);

//------------------------------------------------
// Simulate from lattice model assuming two alleles
// [[Rcpp::export]]
Rcpp::List sim_lattice_biallelic_cpp(Rcpp::List args, Rcpp::List args_functions, Rcpp::List args_progress);

//------------------------------------------------
// apply box blur to matrix
// [[Rcpp::export]]
Rcpp::NumericMatrix box_blur_cpp(Rcpp::NumericMatrix m, int d);
