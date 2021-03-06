#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp; using namespace arma;

//' S4 sparse matrix to arma::sp_mat
//'
//' Convert an S4 sparse matrix from the \code{\link[Matrix]{Matrix}} package
//' to an arma \code{sp_mat}.
//'
//' @source \url{http://gallery.rcpp.org/articles/armadillo-sparse-matrix/}
//'
//' @param mat S4 sparse matrix from the \code{\link[Matrix]{Matrix}}
//'
//' @return an arma::sp_mat object
// [[Rcpp::export]]
arma::sp_mat convertSparse(S4 mat) {// slight improvement with two non-nested loops

  // obtain dim, i, p. x from S4 object
  IntegerVector dims = mat.slot("Dim");
  arma::urowvec i = Rcpp::as<arma::urowvec>(mat.slot("i"));
  arma::urowvec p = Rcpp::as<arma::urowvec>(mat.slot("p"));
  arma::vec x     = Rcpp::as<arma::vec>(mat.slot("x"));
  
  int nrow = dims[0], ncol = dims[1];
  
  // use Armadillo sparse matrix constructor
  arma::sp_mat res(i, p, x, nrow, ncol);

  // Rcout << "SpMat res:\n" << res << std::endl;
  return(res);
}

//' Sparsify arma::mat into arma::sp_mat
//'
//' Return permutations as a numeric sparse matrix
//' (can be binary or continuous)
//'
//' @param perm dense matrix with the permutations
//' @param nrow number of rows for the sparse matrix
//' @param G sparse column matrix
//'
//' @return an arma::sp_mat object
// [[Rcpp::export]]
arma::sp_mat sparsify2(const arma::mat& perm, int nrow, const arma::sp_mat& G) {
  unsigned int ncol = perm.n_cols;
  unsigned int header = G.n_nonzero;

  // Indices of non-null elements
  arma::vec inds = arma::vectorise(perm.head_rows(header)) - 1;

  // Positions of column breaks in the vectorised permutations
  arma::urowvec pos(ncol + 1);
  for (int i = 0; i < pos.size(); ++i) {
    pos(i) = i*header;
  }
  // For compatibility with sparse Matrix in R -- REMOVED, 
  // now numeric values are not symmetrical
  // Repeat the values that are being permuted in a matrix, then
  // assign it by column and melt it by column when calling sp_mat
  // This does what's it's supposed to do (checked)
  arma::colvec Gnonzero = nonzeros(G);
  arma::mat Gdatarep(header, ncol);
  Gdatarep.each_col() = Gnonzero; 

  // Sparse matrix with the permutations
  arma::sp_mat permSp = arma::sp_mat(
    conv_to<uvec>::from(inds),
    pos,
    arma::vectorise(Gdatarep),
    nrow,
    ncol
  );
  return permSp;
}


//' Compute heatrank for a single case
//'
//' The heatrank incorporates the correction \code{(r + 1)/(p + 1)}
//' instead of \code{r/p}
//'
//' @param R dense matrix with the diffusion kernel
//' @param perm sparse matrix with the permutations
//' @param G sparse matrix with the heat sources
//' @param ind index of the G column for current source
//'
//' @return an arma::vec with node heatranks
// [[Rcpp::export]]
arma::vec serialHeatrank(
    const arma::mat& R,
    const arma::sp_mat& perm,
    const arma::sp_mat& G,
    int ind) {
  int m = R.n_rows ;
  int n = perm.n_cols ;
  
  arma::vec Tf = R * G.col(ind) ;
  arma::vec ans(m, fill::zeros) ;
  
  for (int i = 0; i < n; i++) {
    ans = ans + (( R * perm.col(i)) > Tf) ;
  }
  return((ans + 1)/(n + 1)) ;
}

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;

// ' ParallelHeatrank structure for parallel computation
// '
// ' C++ struct using the RcppParallel backend in order to compute the
// ' heatranks for large inputs and permutation numbers
// '
// ' @slot R dense kernel matrix
// ' @slot perm dense permutation matrix
// ' @slot G sparse heat sources
// ' @slot n_bkgd number of nodes in the permutation background
// ' (cannot be guessed from perm as it only contains the indices)
// ' @slot n_nodes number of total nodes in the network
// '
// ' @rdname parallelHeatrank
struct parallelHeatrank : public Worker
{
  // source vectors
  arma::mat R;
  arma::mat perm;
  arma::sp_mat G;
  int n_bkgd, n_nodes;

  // destination matrix
  arma::mat output;

  // constructors
  parallelHeatrank(
    const arma::mat& R,
    const arma::mat& perm,
    const arma::sp_mat& G) : R(R), perm(perm), G(G), n_bkgd(R.n_cols),
    n_nodes(R.n_rows), output(n_nodes, G.n_cols, fill::zeros) {
  }

  // process just the elements of the range I've been asked to
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {
      sp_mat permSp = sparsify2(perm, n_bkgd, G.col(i));
      output.col(i) = serialHeatrank(R, permSp, G, i);
    }
  }
};

//' Compute heatrank in parallel
//'
//' \code{ParallelHeatrank} is a wrapper that computes heatranks for (possibly)
//' different backgrounds and for multiple inputs at once. It will
//' reuse the permutations, which have to be passed to the function.
//' The input must be binary for this implementation, so numeric values for
//' each node are not supported.
//'
//' @param R dense matrix with the diffusion kernel
//' @param perm dense matrix with the permutations (indices in columns).
//' This has to ensure that enough indices are sampled, i.e. at least as
//' great as the largest list in the input (largest \code{colSums} in G)
//' @param G S4 sparse matrix with the heat sources
//'
//' @return a matrix with the same amount of rows that \code{R}
//' and columns in \code{G}, containing the heatrank scores. These scores
//' are corrected using \code{(r + 1)/(p + 1)}
//' instead of \code{r/p}. The smaller the score, the
//' warmer the node.
//'
// [[Rcpp::export]]
arma::mat ParallelHeatrank(const arma::mat& R,
                           const arma::mat& perm,
                           const S4& G) {

  arma::sp_mat Gs = convertSparse(G);

  // declare the instance that takes a pointer to the vector data
  parallelHeatrank PH(R, perm, Gs);

  // call paralleReduce to start the work
  parallelFor(0, Gs.n_cols, PH);

  // return the computed product
  return PH.output;
}
