#ifndef ECONTK_H
#define ECONTK_H

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP c_lemke_howson(SEXP payoff_A, SEXP payoff_B, SEXP n_rows, SEXP n_cols, SEXP init_label);
SEXP c_support_enumeration(SEXP payoff_A, SEXP payoff_B, SEXP n_rows, SEXP n_cols);

#endif
