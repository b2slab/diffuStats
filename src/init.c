#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP diffuStats_convertSparse(SEXP);
extern SEXP diffuStats_ParallelHeatrank(SEXP, SEXP, SEXP);
extern SEXP diffuStats_serialHeatrank(SEXP, SEXP, SEXP, SEXP);
extern SEXP diffuStats_sparsify2(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"diffuStats_convertSparse",    (DL_FUNC) &diffuStats_convertSparse,    1},
    {"diffuStats_ParallelHeatrank", (DL_FUNC) &diffuStats_ParallelHeatrank, 3},
    {"diffuStats_serialHeatrank",   (DL_FUNC) &diffuStats_serialHeatrank,   4},
    {"diffuStats_sparsify2",        (DL_FUNC) &diffuStats_sparsify2,        3},
    {NULL, NULL, 0}
};

void R_init_diffuStats(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
