#include "econtk.h"

static const R_CallMethodDef CallEntries[] = {
    {"c_lemke_howson",         (DL_FUNC) &c_lemke_howson,         5},
    {"c_support_enumeration",  (DL_FUNC) &c_support_enumeration,  4},
    {NULL, NULL, 0}
};

void R_init_econtk(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
