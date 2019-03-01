
#include <Rcpp.h>

//------------------------------------------------
// Example function
Rcpp::List dummy1_cpp(Rcpp::List args);

//------------------------------------------------
// Simulate from simple Wright-Fisher model
Rcpp::List sim_wrightfisher_cpp(Rcpp::List args, Rcpp::List args_functions, Rcpp::List args_progress);
