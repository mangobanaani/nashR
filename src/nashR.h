#ifndef NASHR_H
#define NASHR_H

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP c_lemke_howson(SEXP payoff_A, SEXP payoff_B, SEXP n_rows, SEXP n_cols, SEXP init_label);
SEXP c_support_enumeration(SEXP payoff_A, SEXP payoff_B, SEXP n_rows, SEXP n_cols);
SEXP c_homotopy_nash(SEXP payoffs_r, SEXP n_players_r, SEXP n_strats_r, SEXP prior_r);

#endif
