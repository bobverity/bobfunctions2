
#include "main.h"
#include "misc_v3.h"
#include "probability_v1.h"

using namespace std;

//------------------------------------------------
// Dummy function to test Rcpp working as expected
// [[Rcpp::export]]
Rcpp::List dummy1_cpp(Rcpp::List args) {
  
  // print message to console
  Rcpp::Rcout << "running C++ dummy1_cpp function\n";
  
  // get inputs from Rcpp format to base C++ format
  vector<double> x = Rcpp::as<vector<double>>(args("x"));
  
  // square values
  for (int i=0; i<int(x.size()); i++) {
    x[i] *= x[i];
  }
  
  // return as Rcpp list
  Rcpp::List ret = Rcpp::List::create(Rcpp::Named("x_squared") = x);
  return ret;
}

//------------------------------------------------
// Simulate from simple Wright-Fisher model
// [[Rcpp::export]]
Rcpp::List sim_wrightfisher_cpp(Rcpp::List args, Rcpp::List args_functions, Rcpp::List args_progress) {
  
  // TODO - this could be made considerably faster by applying migration to the
  // raw probabilities, then simply doing multinomial draws from these
  // probabilities, rather than the stochastic migration model currently
  // implemented
  
  // get inputs from Rcpp format to base C++ format
  vector<int> N = rcpp_to_vector_int(args("N"));
  int K = N.size();
  int L = rcpp_to_int(args("L"));
  vector<int> alleles = rcpp_to_vector_int(args("alleles"));
  double mu = rcpp_to_double(args("mu"));
  vector<vector<double>> m_matrix = rcpp_to_matrix_double(args("m_matrix"));
  vector<int> t_out = rcpp_to_vector_int(args("t_out"));
  int n_t_out = t_out.size();
  Rcpp::Function update_progress = args_functions["update_progress"];
  
  // objects for storing results
  vector<vector<vector<vector<int>>>> pop_store(n_t_out);
  
  // initialise populations
  vector<vector<vector<int>>> pop(K);
  for (int k=0; k<K; ++k) {
    pop[k] = vector<vector<int>>(L);
    for (int l=0; l<L; ++l) {
      vector<double> p = rdirichlet1(1.0, alleles[l]);
      pop[k][l] = rmultinom1(N[k], p);
    }
  }
  
  // migration
  int n_pair = 0.5*K*(K-1);
  vector<vector<int>> m_pair(n_pair, vector<int>(2));
  int i2 = 0;
  for (int k1=0; k1<(K-1); ++k1) {
    for (int k2=(k1+1); k2<K; ++k2) {
      m_pair[i2][0] = k1;
      m_pair[i2][1] = k2;
      i2++;
    }
  }
  vector<int> pair_vec = seq_int(0,n_pair-1);
  
  // loop through generations
  int t_out_next = 0;
  for (int t=0; t<max(t_out); ++t) {
    
    // report progress
    update_progress(args_progress, "pb", t, max(t_out)-1);
    
    // loop through demes and loci
    for (int k=0; k<K; ++k) {
      for (int l=0; l<L; ++l) {
        
        // apply drift by drawing from multinomial distribution with probabilities
        // given by previous generation frequencies
        int N_sum_old = N[k];
        int N_sum_new = N[k];
        for (int i=0; i<alleles[l]; ++i) {
          if (pop[k][l][i] == N_sum_old) {
            pop[k][l][i] = N_sum_new;
            break;
          }
          double p_i = pop[k][l][i]/double(N_sum_old);
          N_sum_old -= pop[k][l][i];
          pop[k][l][i] = rbinom1(N_sum_new, p_i);
          N_sum_new -= pop[k][l][i];
        }
        
        // apply mutation
        if (mu > 0) {
          int N_mut = 0;
          for (int i=0; i<alleles[l]; ++i) {
            int N_mu_i = rbinom1(pop[k][l][i], mu);
            pop[k][l][i] -= N_mu_i;
            N_mut += N_mu_i;
          }
          if (N_mut > 0) {
            for (int i=0; i<alleles[l]; ++i) {
              int N_mu_i = rbinom1(N_mut, (i+1)/double(alleles[l]));
              pop[k][l][i] += N_mu_i;
              N_mut -= N_mu_i;
            }
          }
        }
        
      }  // end loop through loci
    }  // end loop through demes
    
    
    // apply pairwise migration in random order
    reshuffle(pair_vec);
    for (int i=0; i<n_pair; ++i) {
      
      // choose random donor and recipient demes
      int k1 = m_pair[pair_vec[i]][0];
      int k2 = m_pair[pair_vec[i]][1];
      int n = rbinom1(N[0], m_matrix[k1][k2]);
      if (n == 0) {
        continue;
      }
      
      // loop through loci
      for (int l=0; l<L; ++l) {
        
        // n1 and n2 are the remaining number of migrants that need to be
        // assigned at this locus
        int n1 = n;
        int n2 = n;
        
        // N1 and N2 are the remaining total number of individuals at this locus
        int N1 = N[0];
        int N2 = N[0];
        
        // loop through alleles
        for (int j=0; j<alleles[l]; ++j) {
          
          // tmp1 gene copies to be removed from deme1
          int tmp1 = rbinom1(n1, pop[k1][l][j]/double(N1));
          if (pop[k1][l][j] == N1) {
            tmp1 = n1;
          }
          N1 -= pop[k1][l][j];
          
          // tmp2 gene copies to be added to deme2
          int tmp2 = rbinom1(n2, pop[k2][l][j]/double(N2));
          if (pop[k2][l][j] == N2) {
            tmp2 = n2;
          }
          N2 -= pop[k2][l][j];
          
          // update values
          pop[k1][l][j] += -tmp1 + tmp2;
          pop[k2][l][j] += tmp1 - tmp2;
          n1 -= tmp1;
          n2 -= tmp2;
        }
      }
    }
    
    // store result
    if ((t+1) == t_out[t_out_next]) {
      pop_store[t_out_next] = pop;
      if (t_out_next < (n_t_out-1)) {
        t_out_next++;
      }
    }
    
  }
  
  // return as Rcpp list
  Rcpp::List ret = Rcpp::List::create(Rcpp::Named("pop") = pop_store);
  return ret;
}
