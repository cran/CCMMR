#include <Rcpp.h>
#include <RcppEigen.h>
#include <Eigen/Eigen>
#include <iostream>
#include <algorithm>

//[[Rcpp::depends(RcppEigen)]]


// Convert a set of keys (row, col) and values into a sparse matrix, requires
// the input to be already column major
Eigen::SparseMatrix<double>
sparse_from_dok(const Eigen::Matrix<int, 2, Eigen::Dynamic>& M_idx,
                const Eigen::VectorXd& M_val, const int n_rows,
                const int n_cols)
{
    Eigen::SparseMatrix<double> result(n_rows, n_cols);

    int nnz = int(M_val.size());
    int count = 1;
    int max_count = 1;

    // Count maximum number of nonzero values per column
    for (int i = 1; i < nnz; i++) {
        if (M_idx(1, i) == M_idx(1, i - 1)) {
            count++;
        } else {
            max_count = std::max(count, max_count);
            count = 1;
        }
    }

    // Reserve memory for the sparse matrix
    result.reserve(Eigen::VectorXi::Constant(n_cols, max_count));

    // Insert the elements
    for (int i = 0; i < nnz; i++) {
        result.insert(M_idx(0, i), M_idx(1, i)) = M_val(i);
    }

    // Compress
    result.makeCompressed();

    return result;
}


struct CCMMConstants {
    Eigen::MatrixXd X;
    double eps_conv;
    double eps_fusions;
    double kappa_eps = 0.5;
    double kappa_pen = 1.0;
    int burn_in;
    int max_iter;

    CCMMConstants(const Eigen::MatrixXd& X,
                  const Eigen::SparseMatrix<double>& W,
                  const double eps_conv, const double eps_fusions,
                  const int burn_in, const int max_iter, const bool scale) :
                  X(X), eps_conv(eps_conv), eps_fusions(eps_fusions),
                  burn_in(burn_in), max_iter(max_iter)
    {
        // Scaling constants for the loss function
        if (scale) {
            double norm_X = X.norm();

            kappa_eps = 1 / (2 * norm_X * norm_X);
            kappa_pen = 1 / (norm_X * W.sum());
        }
    }
};


struct CCMMVariables {
    // Variables used in the minimization
    Eigen::MatrixXd M;
    Eigen::MatrixXd XU;
    Eigen::SparseMatrix<double> U;
    Eigen::SparseMatrix<double> UWU;
    Eigen::ArrayXd cluster_sizes;

    // Variables to construct the merge table
    Eigen::ArrayXi observation_labels;
    Eigen::ArrayXXi merge_table;
    Eigen::ArrayXd merge_height;
    int merge_table_index = 0;

    // Additional information
    double loss = 0;
    int n_iterations = 0;


    CCMMVariables(const Eigen::MatrixXd& X,
                  const Eigen::SparseMatrix<double>& W) : M(X), XU(X), UWU(W)
    {
        int n = int(M.cols());

        // Cluster membership matrix
        U = Eigen::SparseMatrix<double>(n, n); U.setIdentity();

        // Array with cluster sizes
        cluster_sizes = Eigen::ArrayXd::Ones(n);

        // Initialize observation labels as -1, ..., -n
        observation_labels = Eigen::ArrayXi(n);
        for (int i = 0; i < n; i++) {
            observation_labels(i) = -i - 1;
        }

        // Initialize the merge table
        merge_table = Eigen::ArrayXXi(2, n - 1);

        // Initialize merge height vector
        merge_height = Eigen::ArrayXd(n - 1);
    }


    double loss_fusions(const CCMMConstants& constants, double lambda) const
    {
        // TODO: Profile later with and without .noalias()
        Eigen::MatrixXd temp;
        temp.noalias() = constants.X - M * U.transpose();

        // Paper equivalent: kappa_eps * ||X - UM||^2
        double result = constants.kappa_eps * temp.squaredNorm();

        // Initialize sum for penalty term
        double penalty = 0.0;

        // Compute the penalty term
        for (int j = 0; j < UWU.outerSize(); j++) {
            for (Eigen::SparseMatrix<double>::InnerIterator it(UWU, j); it; ++it) {
                int i = int(it.row());

                if (i < j) {
                    penalty += it.value() * (M.col(i) - M.col(j)).norm();
                }
            }
        }

        return result + lambda * constants.kappa_pen * penalty;
    }


    void update(const double kappa_eps, const double kappa_pen,
                const double lambda, const int burn_in, const int iter)
    {
        // Due to Eigen following colmajor conventions, this function computes
        // the transpose of the update that is shown in the paper.
        // Number of variables (p) and current number of clusters (c)
        int p = int(M.rows());
        int c = int(M.cols());

        // Initialize M_update
        Eigen::MatrixXd M_update = Eigen::MatrixXd::Zero(p, c);

        // Paper equivalent: diagonal of U^T U + gamma * D0
        Eigen::ArrayXd diagonal = Eigen::ArrayXd::Zero(c);

        // Precompute lambda * kappa_pen / (2 * kappa_eps)
        double gamma = lambda * kappa_pen / (2 * kappa_eps);

        // Paper equivalent: gamma * (D0 - C0) * M0. Can also be seen as
        // gamma * abs(C) * M0 as D0 is twice the diagonal of C and all
        // off-diagonal elements of C are negative
        for (int j = 0; j < UWU.outerSize(); j++) {
            for (Eigen::SparseMatrix<double>::InnerIterator it(UWU, j); it; ++it) {
                int i = int(it.row());

                if (i > j) {
                    double w_ij = it.value();

                    // Compute gamma * UWU_ij / ||m_i - m_j||
                    double temp1 = (M.col(i) - M.col(j)).norm();
                    temp1 = gamma * w_ij / std::max(temp1, 1e-6);

                    for (int row = 0; row < p; row++) {
                        double temp2 = temp1 * (M(row, i) + M(row, j));

                        M_update(row, i) += temp2;
                        M_update(row, j) += temp2;
                    }

                    diagonal(i) += temp1;
                    diagonal(j) += temp1;
                }
            }
        }

        // Paper equivalent: add U^t * X to the update
        M_update += XU;

        // Finish the diagonal matrix and multiply the update with its inverse
        diagonal = 2 * diagonal + cluster_sizes;

        for (int i = 0; i < c; i++) {
            M_update.col(i) /= diagonal(i);
        }

        // Apply step-doubling
        if (iter > burn_in) {
            M_update = 2 * M_update - M;
        }

        // Set new M
        M = M_update;
    }


    Eigen::SparseMatrix<double> fusion_candidates(const double eps_fusions)
    {
        // Preliminaries
        int n = int(M.cols());
        int cluster = 1;
        double eps_fus_sq = eps_fusions * eps_fusions;
        Eigen::ArrayXi cluster_membership = Eigen::ArrayXi::Zero(n);
        // Eigen::ArrayXd distances = 2 * eps_fus_sq * Eigen::ArrayXd::Ones(n);

        // Find fusion candidates
        for (int j = 0; j < UWU.outerSize(); j++) {
            if (cluster_membership(j) == 0) {
                cluster_membership(j) = cluster;
                cluster++;

                for (Eigen::SparseMatrix<double>::InnerIterator it(UWU, j); it; ++it) {
                    int i = int(it.row());

                    /* Alternative way of finding fusion candidates, allows
                     * "stealing" of a row of M if a later evaluated distance
                     * is found to be smaller. Positive effect appears to be
                     * negligible, requires further testing
                     if (j != i) {
                     double d_ij = (M.col(i) - M.col(j)).squaredNorm();

                     if ((d_ij <= eps_fus_sq) && (d_ij < distances(i))) {
                     cluster_membership(i) = cluster_membership(j);
                     distances(i) = d_ij;
                     }
                     }*/

                    // Same approach as CCMMR
                    if (i > j) {
                        double d_ij = (M.col(i) - M.col(j)).squaredNorm();

                        if (d_ij <= eps_fus_sq) {
                            cluster_membership(i) = cluster_membership(j);
                        }
                    }
                }
            }
        }

        // Make a set of triplets from which to fill the new membership matrix
        // of the form (i, j, value)
        std::vector<Eigen::Triplet<int>> elements(n);
        for (int i = 0; i < n; i++) {
            elements[i] = Eigen::Triplet<int>(i, cluster_membership(i) - 1, 1);
        }

        // Construct the new membership matrix
        Eigen::SparseMatrix<double> result(n, cluster - 1);
        result.setFromTriplets(elements.begin(), elements.end());
		result.makeCompressed();

        return result;
    }


    bool fuse(const double eps_fusions, const double lambda)
    {
        auto U_new = fusion_candidates(eps_fusions);

        if (U_new.rows() > U_new.cols()) {
            // Simple updates of variables
            UWU = U_new.transpose() * UWU * U_new;
            XU = XU * U_new;

            // New cluster sizes and new M
            Eigen::ArrayXd cluster_sizes_new = Eigen::ArrayXd::Zero(U_new.cols());
            Eigen::MatrixXd M_new = Eigen::MatrixXd::Zero(M.rows(), U_new.cols());

            // Array to hold the number of clusters merged into a new cluster
            // for the purpose of constructing new merge table entries
            Eigen::ArrayXi merge_cluster_sizes = Eigen::ArrayXi::Zero(U_new.cols());

            for (int j = 0; j < U_new.outerSize(); j++) {
                for (Eigen::SparseMatrix<double>::InnerIterator it(U_new, j); it; ++it) {
                    int i = int(it.row());

                    // Set new cluster size
                    cluster_sizes_new(j) += cluster_sizes(i);

                    // Set new columns of M
                    M_new.col(j) += M.col(i) * cluster_sizes(i);

                    // Count the number of clusters from the previous
                    // clustering that make up each new cluster
                    merge_cluster_sizes(j) += 1;
                }
            }

            // Compute weighted average for columns of M
            for (int i = 0; i < M_new.cols(); i++) {
                M_new.col(i) /= cluster_sizes_new(i);
            }

            // Adding entries to the merge table
            // Indices of clusters that are formed from more than one cluster
            // based on U_new
            // TODO: Make loop stop after we know that we had all clusters that are made up of other clusters
            for (int U_new_col = 0; U_new_col < merge_cluster_sizes.size(); U_new_col++) {
                if (merge_cluster_sizes(U_new_col) > 1) {
                    // Find out which previous clusters make up the new cluster
                    // Get the first actual observation for each cluster
                    Eigen::ArrayXi components(merge_cluster_sizes(U_new_col));

                    int component_i = 0;
                    for (Eigen::SparseMatrix<double>::InnerIterator it(U_new, U_new_col); it; ++it) {
                        // U_col is the column in U which stores which actual
                        // observations form cluster U_col_new
                        int U_col = int(it.row());

                        // Get the index of the first nonzero element of column
                        // U_col. This is (based on index) the first actual
                        // observation that is part of (old) cluster U_col
                        Eigen::SparseMatrix<double>::InnerIterator it_U(U, U_col);
                        components(component_i) = int(it_U.row());

                        // Increment component index
                        component_i++;
                    }

                    for (component_i = 1; component_i < components.size(); component_i++) {
                        // Set the merge table entries
                        merge_table(0, merge_table_index) = observation_labels(components(component_i - 1));
                        merge_table(1, merge_table_index) = observation_labels(components(component_i));

                        // Record the height at which the merge occurred
                        merge_height(merge_table_index) = lambda;

                        merge_table_index++;

                        // Update the labels that are used to make the entries
                        observation_labels(components(component_i - 1)) = merge_table_index;
                        observation_labels(components(component_i)) = merge_table_index;
                    }

                    // Update all the labels, some are still pointing to an old
                    // entry of the merge table
                    for (component_i = 0; component_i < components.size(); component_i++) {
                        observation_labels(components(component_i)) = merge_table_index;
                    }
                }
            }

            // Update U
            U = U * U_new;

            // Set M and cluster_sizes to their updates
            M = M_new;
            cluster_sizes = cluster_sizes_new;
        }

        return U_new.rows() > U_new.cols();
    }


    void minimize(const CCMMConstants& constants, const double lambda)
    {
        // Preliminaries
        int iter = 0;
        double loss_1 = loss_fusions(constants, lambda);
        double loss_0 = (2 + constants.eps_conv) * loss_1;

        while ((fabs(loss_0 - loss_1) / loss_1 > constants.eps_conv) &&
               (iter < constants.max_iter) && lambda > 0) {
            // Compute update for M
            update(constants.kappa_eps, constants.kappa_pen, lambda,
                   constants.burn_in, iter);

            // Boolean to store whether fusions occurred
            bool clusters_fused = false;

            // Keep fusing while there are eligible fusions
            while (fuse(constants.eps_fusions, lambda)) {
                clusters_fused = true;
            }

            // Update loss values, if cluster fusions occurred, set the
            // previous loss to a value such that at least one more minimizing
            // iteration is performed
            if (clusters_fused) {
                loss_1 = loss_fusions(constants, lambda);
                loss_0 = (2 + constants.eps_conv) * loss_1;
            } else {
                loss_0 = loss_1;
                loss_1 = loss_fusions(constants, lambda);
            }

            // Check for user interrupt
            Rcpp::checkUserInterrupt();

            iter++;
        }

        // Minimization result
        n_iterations = iter;
        loss = loss_1;
    }


    int num_clusters()
    {
        int result = int(M.cols());

        return result;
    }
};


struct CCMMResults {
    // Clusterpath and info variables
    Eigen::ArrayXXd clusterpath;
    Eigen::ArrayXXd info_d;
    Eigen::ArrayXXi info_i;
    bool save_clusterpath;
    int info_index;

    // Merge table variables
    Eigen::ArrayXXi merge;
    Eigen::ArrayXd height;
    int merge_index;

    CCMMResults(const int n_obs, const int n_vars, const int n_lambdas,
                const bool save_clusterpath) : save_clusterpath(save_clusterpath)
    {
        merge = Eigen::ArrayXXi(2, n_obs - 1);
        height = Eigen::ArrayXd(n_obs - 1);
        info_d = Eigen::ArrayXXd(2, n_lambdas);
        info_i = Eigen::ArrayXXi(2, n_lambdas);
        merge_index = 0;
        info_index = 0;

        if (save_clusterpath) {
            clusterpath = Eigen::ArrayXXd(n_vars, n_obs * n_lambdas);
        }
    }

    void add_results(const CCMMVariables& variables, const double lambda)
    {
        if (save_clusterpath) {
            int start_idx = info_index * int(variables.U.rows());

            for (int j = 0; j < variables.U.outerSize(); j++) {
                for (Eigen::SparseMatrix<double>::InnerIterator it(variables.U, j); it; ++it) {
                    int i = int(it.row());
                    clusterpath.col(start_idx + i) = variables.M.col(j);
                }
            }
        }

        // Add entries to the info array
        info_d(0, info_index) = lambda;
        info_d(1, info_index) = variables.loss;
        info_i(0, info_index) = variables.n_iterations;
        info_i(1, info_index) = int(variables.U.cols());

        info_index++;

        // Add entries to the merge table
        for (int i = merge_index; i < variables.merge_table_index; i++) {
            merge(0, i) = variables.merge_table(0, i);
            merge(1, i) = variables.merge_table(1, i);
            height(i) = variables.merge_height(i);
        }

        merge_index = variables.merge_table_index;
    }

    void finalize()
    {
        merge.conservativeResize(2, merge_index);
        height.conservativeResize(merge_index);
    }
};


//[[Rcpp::export(.convex_clusterpath)]]
Rcpp::List
convex_clusterpath(const Eigen::MatrixXd& X,
                   const Eigen::MatrixXi& W_idx,
                   const Eigen::VectorXd& W_val,
                   const Eigen::VectorXd& lambdas,
                   const double eps_conv,
                   const double eps_fusions,
                   const bool scale,
                   const bool save_clusterpath,
                   const int burnin_iter,
                   const int max_iter_conv)
{
    // Number of observations to clusters
    int n_obs = int(X.cols());
    int n_vars = int(X.rows());
    int n_lambdas = int(lambdas.size());

    // Sparse weight matrix
    Eigen::SparseMatrix<double> W = sparse_from_dok(W_idx, W_val, n_obs, n_obs);

    // Cluster membership matrix
    Eigen::SparseMatrix<double> U(n_obs, n_obs); U.setIdentity();

    // Initialize CCMM structs
    CCMMVariables variables(X, W);
    CCMMConstants constants(X, W, eps_conv, eps_fusions, burnin_iter,
                            max_iter_conv, scale);
    CCMMResults results(n_obs, n_vars, n_lambdas, save_clusterpath);

    // Minimize the convex clustering loss function for each lambda
    for (int i = 0; i < n_lambdas; i++) {
        variables.minimize(constants, lambdas(i));
        results.add_results(variables, lambdas(i));
    }

    // Do some cleaning up on the variables
    results.finalize();

    // Return result as a list with all relevant variables
    Rcpp::List res = Rcpp::List::create(
        Rcpp::Named("clusterpath") = results.clusterpath,
        Rcpp::Named("merge") = results.merge,
        Rcpp::Named("height") = results.height,
        Rcpp::Named("info_i") = results.info_i,
        Rcpp::Named("info_d") = results.info_d
    );

    return res;
}


//[[Rcpp::export(.convex_clustering)]]
Rcpp::List
convex_clustering(const Eigen::MatrixXd& X,
                  const Eigen::MatrixXi& W_idx,
                  const Eigen::VectorXd& W_val,
                  const double eps_conv,
                  const double eps_fusions,
                  const bool scale,
                  const bool save_clusterpath,
                  const int burnin_iter,
                  const int max_iter_conv,
                  const int target_low,
                  const int target_high,
                  const int max_iter_phase_1,
                  const int max_iter_phase_2,
                  const int verbose,
                  const double lambda_init,
                  const double factor)
{
    // Number of observations to clusters
    int n_obs = int(X.cols());
    int n_vars = int(X.rows());

    // Sparse weight matrix
    Eigen::SparseMatrix<double> W = sparse_from_dok(W_idx, W_val, n_obs, n_obs);

    // Cluster membership matrix
    Eigen::SparseMatrix<double> U(n_obs, n_obs); U.setIdentity();

    // Initialize CCMM structs
    CCMMVariables variables(X, W);
    CCMMConstants constants(X, W, eps_conv, eps_fusions, burnin_iter,
                            max_iter_conv, scale);
    CCMMResults results(n_obs, n_vars, target_high - target_low + 1, save_clusterpath);

    // Counters to keep track of the number of minimizations
    int phase_1_instances_solved = 0;
    int phase_2_instances_solved = 0;

    // Variables for lambda
    double lambda = lambda_init / (1 + factor) - 1e-8;
    double lambda_lb = lambda;
    double lambda_ub = lambda;
    double lambda_target = lambda;

    // Set current target for number of clusters
    int current_target = std::min(n_obs - 1, target_high);

    // Minimize loss for lambda = 0
    variables.minimize(constants, 0);

    // Create variables struct to store the result if the target has been
    // found
    CCMMVariables variables_target = variables;

    // Create variables struct to have a warm start to return to while
    // continuously increasing lambda when looking for the current target
    CCMMVariables variables_lb = variables;

    // Count the number of targets that were found
    int targets_found = 0;

    // If the number of observations is part of the target interval, add the
    // solution for lambda = 0
    if (target_high == n_obs) {
        if (verbose > 0) {
            Rcpp::Rcout << "Searching for " << n_obs << " clusters\n";
            Rcpp::Rcout << "    lambda = 0 | number of clusters: " << n_obs << "\n";
        }

        results.add_results(variables_target, 0);

        // Increment the counter for the number of targets found
        targets_found++;
    }

    // Settings for printing information
    Rcpp::Rcout << std::fixed;
    Rcpp::Rcout.precision(5);

    while (current_target >= target_low) {
        if (verbose > 0) {
            Rcpp::Rcout << "Searching for " << current_target << " clusters\n";
            Rcpp::Rcout << "Phase 1: acquiring lower bound for lambda\n";
        }

        // Set warm start
        variables = variables_target;
        lambda = (lambda_target + 1e-8) * (1 + factor);
        lambda_lb = lambda_target + 1e-8;

        // Booleans to store how phase 1 ends
        bool target_found = false;      // Target was found
        bool target_sandwiched = false; // Values above and below target found

        // Counter for the number of iterations
        int iter = 0;

        while (iter < max_iter_phase_1 && lambda < 1e30) {
            // Minimize the loss
            variables.minimize(constants, lambda);
            phase_1_instances_solved++;

            if (verbose > 0) {
                Rcpp::Rcout << "    lambda = " << lambda;
                Rcpp::Rcout << " | number of clusters: " << variables.num_clusters();
                Rcpp::Rcout << '\n';
            }

            // Check each case
            if (variables.num_clusters() > current_target) {
                // Store result as a warm start for the next minimization
                variables_lb = variables;
                lambda_lb = lambda;

                // Increase lambda
                lambda *= 1 + factor;
            } else if (variables.num_clusters() == current_target) {
                // Store solution for which target was attained and keep the
                // corresponding lambda as the upper bound for this number of
                // clusters
                variables_target = variables;
                lambda_target = lambda;
                lambda_ub = lambda;

                // Store that the target was found and break the while loop
                target_found = true;
                break;
            } else {
                // Store upper bound for lambda
                lambda_ub = lambda;

                // Store that the target was sandwiched and break the loop
                target_sandwiched = true;
                break;
            }

            // Increment the counter for the number of phase 1 iterations
            iter++;
        }

        // If the target was found or sandwiched, refine lambda. In case the
        // target was found, perform fewer refinement iterations
        if (target_found || target_sandwiched) {
            if (verbose > 0) {
                Rcpp::Rcout << "Phase 2: refining lambda\n";
            }

            iter = target_found * max_iter_phase_2 / 2;

            while (iter < max_iter_phase_2 && lambda_ub - lambda_lb > 1e-6) {
                // New guess for lambda
                lambda = 0.5 * (lambda_lb + lambda_ub);

                // Minimize the loss
                variables.minimize(constants, lambda);
                phase_2_instances_solved++;

                if (verbose > 0) {
                    Rcpp::Rcout << "    lambda = " << lambda;
                    Rcpp::Rcout << " | number of clusters: " << variables.num_clusters();
                    Rcpp::Rcout << '\n';
                }

                if (variables.num_clusters() > current_target) {
                    // Store result as a warm start for the next minimization
                    // and set new lower bound for lambda
                    variables_lb = variables;
                    lambda_lb = lambda;
                } else if (variables.num_clusters() == current_target) {
                    // Store solution for which target was attained and keep
                    // the corresponding lambda as the upper bound for this
                    // number of clusters
                    variables_target = variables;
                    lambda_target = lambda;
                    lambda_ub = lambda;

                    // Store that the target was found
                    target_found = true;

                    // Reset variables to warm start
                    variables = variables_lb;
                } else {
                    // Set new upper bound for lambda
                    lambda_ub = lambda;

                    // Reset variables to warm start
                    variables = variables_lb;
                }

                // Increment the counter for the number of phase 2 iterations
                iter++;
            }

            if (target_found) {
                // Add the result
                results.add_results(variables_target, lambda_target);

                // Increment the counter for the number of targets found
                targets_found++;
            } else {
                // Start the next search from the lower bound of this search
                variables_target = variables_lb;
                lambda_target = lambda_lb;
            }
        } else {
            break;
        }

        current_target--;
    }

    // Do some cleaning up on the variables
    results.finalize();

    // Return result as a list with all relevant variables
    Rcpp::List res = Rcpp::List::create(
        Rcpp::Named("clusterpath") = results.clusterpath,
        Rcpp::Named("merge") = results.merge,
        Rcpp::Named("height") = results.height,
        Rcpp::Named("info_i") = results.info_i,
        Rcpp::Named("info_d") = results.info_d,
        Rcpp::Named("phase_1_instances") = phase_1_instances_solved,
        Rcpp::Named("phase_2_instances") = phase_2_instances_solved,
        Rcpp::Named("targets_found") = targets_found
    );

    return res;
}
