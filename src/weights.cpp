//[[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>
#include <Eigen/Dense>


//[[Rcpp::export(.sparse_weights)]]
Rcpp::List sparse_weights(const Eigen::MatrixXd& X,
                          const Eigen::MatrixXi& indices,
                          const Eigen::MatrixXd& distances,
                          const double phi,
                          const int k,
                          const bool sym_circ,
                          const bool scale)
{
    // Preliminaries
    int n = int(X.cols());

    // Array of keys and values, 2*(k+2)*n is a loose upper bound on the number
    // of nonzero weights, trimming happens later
    Eigen::ArrayXXi keys(2, 2 * (k + 2) * n);
    Eigen::ArrayXd values(2 * (k + 2) * n);

    // Fill keys
    int key_count = 0;
    for (int i = 0; i < indices.cols(); i++) {
        for (int j = 0; j < indices.rows(); j++) {
            if (i != indices(j, i)) {
                keys(0, key_count) = i;
                keys(1, key_count) = indices(j, i);
                keys(0, key_count + 1) = indices(j, i);
                keys(1, key_count + 1) = i;

                values(key_count) = distances(j, i);
                values(key_count + 1) = distances(j, i);

                key_count += 2;
            }
        }
    }

    // Apply symmetric circulant
    if (sym_circ) {
        for (int i = 0; i < n; i++) {
            int j = (i + 1) % n;
            double d_ij = (X.col(i) - X.col(j)).norm();

            keys(0, key_count) = i;
            keys(1, key_count) = j;
            keys(0, key_count + 1) = j;
            keys(1, key_count + 1) = i;

            values(key_count) = d_ij;
            values(key_count + 1) = d_ij;

            key_count += 2;
        }
    }

    // Trim unused key/value pairs
    keys.conservativeResize(2, key_count);
    values.conservativeResize(key_count);

    // Compute mean squared distance
    double msd = 0;
    if (scale) {
        for (int j = 0; j < n; j++) {
            for (int i = j + 1; i < n; i++) {
                msd += (X.col(j) - X.col(i)).squaredNorm();
            }
        }
        msd /= (n * (n - 1) / 2);
    }

    // Compute weights
    values = values.square();
    if (scale) {
        values /= msd;
    }
    values = Eigen::exp(-phi * values);

    // Return result as a list with all relevant variables
    Rcpp::List res = Rcpp::List::create(
        Rcpp::Named("keys") = keys,
        Rcpp::Named("values") = values,
        Rcpp::Named("msd") = msd
    );

    return res;
}
