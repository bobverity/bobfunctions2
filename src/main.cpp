
#include "main.h"
#include "misc_v12.h"
#include "probability_v16.h"

using namespace std;

//------------------------------------------------
// Simulate from simple Wright-Fisher model
Rcpp::List sim_wrightfisher_cpp(Rcpp::List args, Rcpp::List args_functions, Rcpp::List args_progress) {
  
  // start timer
  chrono::high_resolution_clock::time_point t1 = chrono::high_resolution_clock::now();
  
  // get inputs from Rcpp format to base C++ format
  int N = rcpp_to_int(args("N"));
  int K = rcpp_to_int(args("K"));
  int L = rcpp_to_int(args("L"));
  vector<int> alleles = rcpp_to_vector_int(args("alleles"));
  double mu = rcpp_to_double(args("mu"));
  vector<vector<double>> mig_mat = rcpp_to_matrix_double(args("mig_mat"));
  vector<int> t_out = rcpp_to_vector_int(args("t_out"));
  int n_t_out = t_out.size();
  int initial_method = rcpp_to_int(args("initial_method"));
  vector<vector<double>> initial_params = rcpp_to_matrix_double(args("initial_params"));
  bool silent = rcpp_to_bool(args["silent"]);
  Rcpp::Function update_progress = args_functions["update_progress"];
  
  // objects for storing results
  vector<vector<vector<vector<int>>>> allele_counts_store(n_t_out);
  
  // initialise allele counts in each deme based on input method
  vector<vector<vector<int>>> allele_counts(K);
  if (initial_method == 1) {
    double theta = 2*N*mu;
    for (int k = 0; k < K; ++k) {
      allele_counts[k] = vector<vector<int>>(L);
      for (int l = 0; l < L; ++l) {
        vector<double> p = rdirichlet1(theta / (double)alleles[l], alleles[l]);
        allele_counts[k][l] = rmultinom1(N, p);
      }
    }
  } else if (initial_method == 2) {
    for (int k = 0; k < K; ++k) {
      allele_counts[k] = vector<vector<int>>(L);
      if (k == 0) {
        for (int l = 0; l < L; ++l) {
          vector<double> p = rdirichlet2(initial_params[l]);
          allele_counts[k][l] = rmultinom1(N, p);
        }
      } else {
        allele_counts[k] = allele_counts[0];
      }
    }
  }
  
  
  // create list of all pairwise demes that have positive migration rates
  vector<pair<pair<int, int>, double>> mig_list;
  for (int k1 = 0; k1 < (K - 1); ++k1) {
    for (int k2 = (k1 + 1); k2 < K; ++k2) {
      if (mig_mat[k1][k2] > 0) {
        pair<int, int> deme_pair = {k1, k2};
        mig_list.push_back(make_pair(deme_pair, mig_mat[k1][k2]));
      }
    }
  }
  
  // option to store initial values
  int t_out_next = 0;
  if (t_out[t_out_next] == 0) {
    allele_counts_store[t_out_next] = allele_counts;
    t_out_next++;
  }
  
  // loop through generations
  for (int t = 1; t < (max(t_out) + 1); ++t) {
    
    // report progress
    if (!silent) {
      update_progress(args_progress, "pb", t, max(t_out));
    }
    
    // apply migration
    for (int j = 0; j < mig_list.size(); ++j) {
      
      // draw number of migrants
      int n_mig = rbinom1(N, mig_list[j].second);
      
      // apply migration
      if (n_mig > 0) {
        pair<int, int> deme_pair = mig_list[j].first;
        int k1 = deme_pair.first;
        int k2 = deme_pair.second;
        
        for (int l = 0; l < L; ++l) {
          int tmp1 = n_mig;
          int tmp2 = n_mig;
          int tmp3 = N;
          int tmp4 = N;
          for (int i = 0; i < alleles[l]; ++i) {
           
            // subtract migrants from deme1
            int n_mig_i1 = rhyper1(allele_counts[k1][l][i], tmp3 - allele_counts[k1][l][i], tmp1);
            tmp1 -= n_mig_i1;
            tmp3 -= allele_counts[k1][l][i];
            allele_counts[k1][l][i] -= n_mig_i1;
            
            // subtract migrants from deme2
            int n_mig_i2 = rhyper1(allele_counts[k2][l][i], tmp4 - allele_counts[k2][l][i], tmp2);
            tmp2 -= n_mig_i2;
            tmp4 -= allele_counts[k2][l][i];
            allele_counts[k2][l][i] -= n_mig_i2;
            
            // add migrants
            allele_counts[k1][l][i] += n_mig_i2;
            allele_counts[k2][l][i] += n_mig_i1;
            
          }
        }
        
      }
    }
    
    // loop through demes and loci
    for (int k = 0; k < K; ++k) {
      for (int l = 0; l < L; ++l) {
        
        // apply drift by drawing from multinomial distribution with probabilities
        // given by previous generation frequencies
        int draws_remaining = N;
        int N_remaining = N;
        for (int i = 0; i < (alleles[l] - 1); ++i) {
          int count_new = rbinom1(draws_remaining, allele_counts[k][l][i] / (double)N_remaining);
          N_remaining -= allele_counts[k][l][i];
          draws_remaining -= count_new;
          allele_counts[k][l][i] = count_new;
          
          if (N_remaining == 0) {
            break;
          }
        }
        allele_counts[k][l][alleles[l] - 1] = draws_remaining;
        
        // apply mutation
        if (mu > 0) {
          int n_mut = rbinom1(N, mu);
          if (n_mut > 0) {
            
            // subtract mutants from existing counts
            int draws_remaining = n_mut;
            int N_remaining = N;
            for (int i = 0; i < (alleles[l] - 1); ++i) {
              int n_mut_i = rbinom1(draws_remaining, allele_counts[k][l][i] / (double)N_remaining);
              N_remaining -= allele_counts[k][l][i];
              draws_remaining -= n_mut_i;
              allele_counts[k][l][i] -= n_mut_i;
              
              if (N_remaining == 0) {
                break;
              }
            }
            allele_counts[k][l][alleles[l] - 1] -= draws_remaining;
            
            // add new mutants to allele counts
            draws_remaining = n_mut;
            for (int i = 0; i < (alleles[l] - 1); ++i) {
              int n_mut_i = rbinom1(draws_remaining, 1.0 / double(alleles[l] - i));
              draws_remaining -= n_mut_i;
              allele_counts[k][l][i] += n_mut_i;
            }
            allele_counts[k][l][alleles[l] - 1] += draws_remaining;
            
          }
        }
        
      }  // end loop through loci
    }  // end loop through demes
    
    // store result
    if (t == t_out[t_out_next]) {
      allele_counts_store[t_out_next] = allele_counts;
      if (t_out_next < (n_t_out - 1)) {
        t_out_next++;
      }
    }
    
  }  // end simulation loop
  
  // end timer
  chrono::high_resolution_clock::time_point t2 = chrono::high_resolution_clock::now();
  chrono::duration<double> time_span = chrono::duration_cast< chrono::duration<double> >(t2 - t1);
  print("simulation completed in", time_span.count(), "seconds\n");
  
  // return as Rcpp list
  Rcpp::List ret = Rcpp::List::create(Rcpp::Named("allele_counts") = allele_counts_store);
  return ret;
}

//------------------------------------------------
// Simulate from lattice model assuming two alleles
Rcpp::List sim_lattice_biallelic_cpp(Rcpp::List args, Rcpp::List args_functions, Rcpp::List args_progress) {
  
  // start timer
  chrono::high_resolution_clock::time_point t1 = chrono::high_resolution_clock::now();
  
  // get inputs from Rcpp format to base C++ format
  int demes_x = rcpp_to_int(args("demes_x"));
  int demes_y = rcpp_to_int(args("demes_y"));
  int N = rcpp_to_int(args("N"));
  double N_inv = 1.0 / double(N);
  double mu = rcpp_to_double(args("mu"));
  double m = rcpp_to_double(args("m"));
  vector<int> t_out = rcpp_to_vector_int(args("t_out"));
  int n_t_out = t_out.size();
  
  double p_init = rcpp_to_double(args("p_init"));
  bool torus = rcpp_to_bool(args["torus"]);
  bool silent = rcpp_to_bool(args["silent"]);
  
  Rcpp::Function update_progress = args_functions["update_progress"];
  
  
  // objects for storing results
  Rcpp::List ret;
  
  // create two identical matrices for storing allele frequencies at all points
  // in a grid. We will step forward in time by alternating between these
  // matrices
  vector<vector<double>> p_mat1(demes_y, vector<double>(demes_x, p_init));
  vector<vector<double>> p_mat2(demes_y, vector<double>(demes_x));
  
  // option to store output at time 0
  int output_index = 0;
  int target_mat = 0;
  if (t_out[0] == 0) {
    ret.push_back(p_mat1);
    output_index++;
  }
  
  // loop through generations
  for (int t = 1; t < (max(t_out) + 1); ++t) {
    
    // report progress
    if (!silent) {
      update_progress(args_progress, "pb", t, max(t_out));
    }
    
    // update matrices
    if (target_mat == 0) {
      
      for (int i = 0; i < demes_y; ++i) {
        for (int j = 0; j < demes_x; ++j) {
          
          // deal with edges
          int up = i - 1;
          int down = i + 1;
          int left = j - 1;
          int right = j + 1;
          if (i == 0) {
            up = (torus) ? demes_y - 1 : i;
          }
          if (i == (demes_y - 1)) {
            down = (torus) ? 0 : i;
          }
          if (j == 0) {
            left = (torus) ? demes_x - 1 : j;
          }
          if (j == (demes_x - 1)) {
            right = (torus) ? 0 : j;
          }
          
          // update allele frequency
          double p_mig =  0.5*m*p_mat1[up][j] + 0.5*m*p_mat1[down][j] +
            0.5*m*p_mat1[i][left] + 0.5*m*p_mat1[i][right] + p_mat1[i][j]*(1 - 2*m);
          double p_mut = p_mig*(1 - mu*0.5) + (1 - p_mig)*mu*0.5;
          p_mat2[i][j] = rbinom1(N, p_mut) * N_inv;
        }
      }
      
    } else {
      
      for (int i = 0; i < demes_y; ++i) {
        for (int j = 0; j < demes_x; ++j) {
          
          // deal with edges
          int up = i - 1;
          int down = i + 1;
          int left = j - 1;
          int right = j + 1;
          if (i == 0) {
            up = (torus) ? demes_y - 1 : i;
          }
          if (i == (demes_y - 1)) {
            down = (torus) ? 0 : i;
          }
          if (j == 0) {
            left = (torus) ? demes_x - 1 : j;
          }
          if (j == (demes_x - 1)) {
            right = (torus) ? 0 : j;
          }
          
          // update allele frequency
          double p_mig =  0.5*m*p_mat2[up][j] + 0.5*m*p_mat2[down][j] +
            0.5*m*p_mat2[i][left] + 0.5*m*p_mat2[i][right] + p_mat2[i][j]*(1 - 2*m);
          double p_mut = p_mig*(1 - mu*0.5) + (1 - p_mig)*mu*0.5;
          p_mat1[i][j] = rbinom1(N, p_mut) * N_inv;
        }
      }
      
    }
    
    // save output
    if (t == t_out[output_index]) {
      if (target_mat == 0) {
        ret.push_back(p_mat1);
      } else {
        ret.push_back(p_mat2);
      }
      
      if (output_index < (n_t_out - 1)) {
        output_index++;
      }
    }
    
    // alternate target matrix
    target_mat = 1 - target_mat;
  }
  
  // end timer
  chrono::high_resolution_clock::time_point t2 = chrono::high_resolution_clock::now();
  chrono::duration<double> time_span = chrono::duration_cast< chrono::duration<double> >(t2 - t1);
  print("simulation completed in", time_span.count(), "seconds\n");
  
  // return Rcpp list
  return ret;
}

//------------------------------------------------
// apply box blur to matrix
Rcpp::NumericMatrix box_blur_cpp(Rcpp::NumericMatrix m, int d) {
  
  // initialise return matrix
  Rcpp::NumericMatrix ret(m.nrow(), m.ncol());
  int nr = ret.nrow();
  int nc = ret.ncol();
  
  // apply box blur to ret matrix
  for (int i = 0; i < nr; ++i) {
    for (int j = 0; j < nc; ++j) {
      
      // get limits of box
      int i_min = (i-d > 1) ? i-d : 1;
      int i_max = (i+d < nr) ? i+d : nr;
      int j_min = (j-d > 1) ? j-d : 1;
      int j_max = (j+d < nc) ? j+d : nc;
      
      // sum values within box
      double v = 0;
      for (int a = i_min; a < i_max; ++a) {
        for (int b = j_min; b < j_max; ++b) {
          v += m(a,b);
        }
      }
      
      // store mean value
      v /= double((i_max - i_min)*(j_max - j_min));
      ret(i,j) = v;
    }
  }
  
  return ret;
}
