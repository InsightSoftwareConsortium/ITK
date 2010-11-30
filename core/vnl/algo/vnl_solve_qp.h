// This is core/vnl/algo/vnl_solve_qp.h
#ifndef vnl_solve_qp_h_
#define vnl_solve_qp_h_

//:
// \file
// \brief Functions to solve various forms of constrained quadratic programming
// \author  Tim Cootes

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

//: Solve quadratic programming problem with linear constraints
//  Minimise F(x)=0.5x'Hx + g'x  subject to Ax=b
//  \param H Hessian of F(x) - must be symmetric
//  \retval True if successful
bool vnl_solve_qp_with_equality_constraints(const vnl_matrix<double>& H,
                                            const vnl_vector<double>& g,
                                            const vnl_matrix<double>& A,
                                            const vnl_vector<double>& b,
                                            vnl_vector<double>& x);

//: Solve quadratic programming problem with constraint sum(x)=0
//  Minimise F(x)=0.5x'Hx + g'x  subject to sum(x)=0
//  Special case of quadratic programming  (Equality constraint: x.1=0)
//  \param H Hessian of F(x) - must be symmetric
//  \retval True if successful
bool vnl_solve_qp_zero_sum(const vnl_matrix<double>& H,
                           const vnl_vector<double>& g,
                           vnl_vector<double>& x);

//: Find non-negative solution to a constrained quadratic programming problem
//  Minimise F(x)=0.5x'Hx + g'x  subject to Ax=b and x(i)>=0 for all i
//
//  Uses a variant of the active set strategy to solve the problem.
//  This performs a sequence of unconstrained solutions.  If the inequality
//  constraints are violated, the most violated x(i) is set to zero
//  and a slightly smaller problem is solved.
//  \param H Hessian of F(x) - must be symmetric
//  \param x On input, it must satisfy all constraints (Ax=b, x(i)>=0)
//  \param con_tol Tolerance for testing constraints:   |Ax-b|^2<con_tol
//  \param verbose When true, output error messages to cerr if failed
//  \retval True if successful
bool vnl_solve_qp_with_non_neg_constraints(const vnl_matrix<double>& H,
                                           const vnl_vector<double>& g,
                                           const vnl_matrix<double>& A,
                                           const vnl_vector<double>& b,
                                           vnl_vector<double>& x,
                                           double con_tol = 1e-8,
                                           bool verbose=true);

//: Find non-negative solution to a constrained quadratic programming problem
//  Minimise F(x)=0.5x'Hx + g'x  subject to sum(x)=1 and x(i)>=0 for all i
//
//  Uses a variant of the active set strategy to solve the problem.
//  This performs a sequence of unconstrained solutions.  If the inequality
//  constraints are violated, the most violated x(i) is set to zero
//  and a slightly smaller problem is solved.
//  \param H Hessian of F(x) - must be symmetric
//  \param x On input, it must satisfy all constraints (sum(x)=1, x(i)>=0)
//  \param verbose When true, output error messages to cerr if failed
//  \retval True if successful
bool vnl_solve_qp_non_neg_sum_one(const vnl_matrix<double>& H,
                                  const vnl_vector<double>& g,
                                  vnl_vector<double>& x,
                                  bool verbose=true);

#endif // vnl_solve_qp_h_

