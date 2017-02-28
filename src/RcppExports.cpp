// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// convertSparse
arma::sp_mat convertSparse(S4 mat);
RcppExport SEXP diffusion_convertSparse(SEXP matSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< S4 >::type mat(matSEXP);
    rcpp_result_gen = Rcpp::wrap(convertSparse(mat));
    return rcpp_result_gen;
END_RCPP
}
// sparsify2
arma::sp_mat sparsify2(const arma::mat& perm, int nrow, int header);
RcppExport SEXP diffusion_sparsify2(SEXP permSEXP, SEXP nrowSEXP, SEXP headerSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type perm(permSEXP);
    Rcpp::traits::input_parameter< int >::type nrow(nrowSEXP);
    Rcpp::traits::input_parameter< int >::type header(headerSEXP);
    rcpp_result_gen = Rcpp::wrap(sparsify2(perm, nrow, header));
    return rcpp_result_gen;
END_RCPP
}
// serialHeatrank
arma::vec serialHeatrank(const arma::mat& R, const arma::sp_mat& perm, const arma::sp_mat& G, int ind);
RcppExport SEXP diffusion_serialHeatrank(SEXP RSEXP, SEXP permSEXP, SEXP GSEXP, SEXP indSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type R(RSEXP);
    Rcpp::traits::input_parameter< const arma::sp_mat& >::type perm(permSEXP);
    Rcpp::traits::input_parameter< const arma::sp_mat& >::type G(GSEXP);
    Rcpp::traits::input_parameter< int >::type ind(indSEXP);
    rcpp_result_gen = Rcpp::wrap(serialHeatrank(R, perm, G, ind));
    return rcpp_result_gen;
END_RCPP
}
// ParallelHeatrank
arma::mat ParallelHeatrank(const arma::mat& R, const arma::mat& perm, const S4& G);
RcppExport SEXP diffusion_ParallelHeatrank(SEXP RSEXP, SEXP permSEXP, SEXP GSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type R(RSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type perm(permSEXP);
    Rcpp::traits::input_parameter< const S4& >::type G(GSEXP);
    rcpp_result_gen = Rcpp::wrap(ParallelHeatrank(R, perm, G));
    return rcpp_result_gen;
END_RCPP
}
