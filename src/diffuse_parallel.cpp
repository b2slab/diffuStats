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

  IntegerVector dims = mat.slot("Dim");
  arma::urowvec i = Rcpp::as<arma::urowvec>(mat.slot("i"));
  arma::urowvec p = Rcpp::as<arma::urowvec>(mat.slot("p"));
  arma::vec x     = Rcpp::as<arma::vec>(mat.slot("x"));

  int nrow = dims[0], ncol = dims[1];
  arma::sp_mat res(nrow, ncol);

  // create space for values, and copy
  arma::access::rw(res.values) = arma::memory::acquire_chunked<double>(x.size() + 1);
  arma::arrayops::copy(arma::access::rwp(res.values), x.begin(), x.size() + 1);

  // create space for row_indices, and copy
  arma::access::rw(res.row_indices) = arma::memory::acquire_chunked<arma::uword>(i.size() + 1);
  arma::arrayops::copy(arma::access::rwp(res.row_indices), i.begin(), i.size() + 1);

  // create space for col_ptrs, and copy
  arma::access::rw(res.col_ptrs) = arma::memory::acquire<arma::uword>(p.size() + 2);
  arma::arrayops::copy(arma::access::rwp(res.col_ptrs), p.begin(), p.size() + 1);

  // important: set the sentinel as well
  arma::access::rwp(res.col_ptrs)[p.size()+1] = std::numeric_limits<arma::uword>::max();

  // set the number of non-zero elements
  arma::access::rw(res.n_nonzero) = x.size();

  // Rcout << "SpMat res:\n" << res << std::endl;
  return(res);
}

// hello world functions
//
// // [[Rcpp::export]]
// int intNew(int n) {
//   return int(n);
// }
//
//
// // [[Rcpp::export]]
// arma::vec vecOnes(int n) {
//   return ones<vec>(n);
// }

//' Sparsify arma::mat into arma::sp_mat
//'
//' Return permutations as a 1-0 sparse matrix
//'
//' @param perm dense matrix with the permutations
//' @param nrow number of rows for the sparse matrix
//' @param header Number of rows required from \code{perm} (will depend
//' on the size of the input list)
//'
//' @return an arma::sp_mat object
// [[Rcpp::export]]
arma::sp_mat sparsify2(const arma::mat& perm, int nrow, int header) {
  unsigned int ncol = perm.n_cols;
  // arma::vec inds = arma::vectorise(perm);

  arma::vec inds = arma::vectorise(perm.head_rows(header)) - 1;

  arma::urowvec pos(ncol + 1);
  for (int i = 0; i < pos.size(); ++i) {
    pos(i) = i*header;
  }
  // For compatibility with sparse Matrix in R
  // Rows must be sorted
  for (int i = 1; i < pos.size(); ++i) {
    std::sort(inds.begin() + pos(i - 1), inds.begin() + pos(i));
  }

  arma::vec ones = arma::vec(ncol*header, fill::ones);
  arma::sp_mat permSp = arma::sp_mat(
    conv_to<uvec>::from(inds),
    pos,
    ones,
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
  // cout << "Rows: " << m << endl;
  // cout << "Number of perms: " << n << endl;
  arma::vec Tf = R * G.col(ind) ;
  // cout << "Sum of generation: " << sum(G.col(ind)) << endl;
  arma::vec ans(m, fill::zeros) ;
  // cout << "Sum of perms: ";
  for (int i = 0; i < n; i++) {
    // cout << sum(perm.col(i)) << " ";
    // cout << perm.col(i) << endl;
    ans = ans + (( R * perm.col(i)) > Tf) ;
  }
  // cout << endl;
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
    // cout << "n_bkgd: " << n_bkgd << endl;
    // cout << "n_nodes: " << n_nodes << endl;
    // cout << "Number of scores sets: " << G.n_cols << endl;
    // cout << "Number of permutations: " << perm.n_cols << endl;
  }

  // process just the elements of the range I've been asked to
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {
      unsigned int sumGi = nonzeros(G.col(i)).size();
      // cout << "n_input: " << sumGi << endl;
      sp_mat permSp = sparsify2(perm, n_bkgd, sumGi);
      // cout << "PermSp: " << permSp.n_rows << "x" << permSp.n_cols << endl;
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

  // arma::sp_mat Ps = convertSparse(perm) ;
  arma::sp_mat Gs = convertSparse(G) ;

  // declare the instance that takes a pointer to the vector data
  parallelHeatrank PH(R, perm, Gs);

  // call paralleReduce to start the work
  parallelFor(0, Gs.n_cols, PH);

  // return the computed product
  return PH.output;
}
