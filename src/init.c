#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP diffusion_convertSparse(SEXP);
extern SEXP diffusion_ParallelHeatrank(SEXP, SEXP, SEXP);
extern SEXP diffusion_serialHeatrank(SEXP, SEXP, SEXP, SEXP);
extern SEXP diffusion_sparsify2(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"diffusion_convertSparse",    (DL_FUNC) &diffusion_convertSparse,    1},
    {"diffusion_ParallelHeatrank", (DL_FUNC) &diffusion_ParallelHeatrank, 3},
    {"diffusion_serialHeatrank",   (DL_FUNC) &diffusion_serialHeatrank,   4},
    {"diffusion_sparsify2",        (DL_FUNC) &diffusion_sparsify2,        3},
    {NULL, NULL, 0}
};

void R_init_diffusion(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
