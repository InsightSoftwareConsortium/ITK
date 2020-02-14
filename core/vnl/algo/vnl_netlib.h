// This is core/vnl/algo/vnl_netlib.h
#ifndef vnl_netlib_h_
#define vnl_netlib_h_
//:
// \file
// \brief Declare in a central place the list of symbols from netlib
// \author fsm
//
// Declare in a central place the list of symbols from netlib referenced from vnl-algo.
// This list was auto-generated, so it is exhaustive as of 16 March 2000 (10pm).
//
// Note: the declarations are initially entered as "int f()", which
// will conflict with the actual prototype. If you get a conflict,
// enter the correct prototype in here.
//
// \verbatim
// Modifications
//  dac (Manchester) 28/03/2001: tidied up documentation
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
// \endverbatim

#include <v3p_netlib.h>

// xSVDC
#define vnl_netlib_svd_proto(T) \
T *x, long int const *ldx, long int const *m, long int const *n, \
T *sv, \
T *errors, \
T *u, long int const *ldu, \
T *v, long int const *ldv, \
T *work, \
long int const *job, long int *info
#define vnl_netlib_svd_params \
x, ldx, m, n, sv, errors, u, ldu, v, ldv, work, job, info

// xQRDC
#define vnl_netlib_qrdc_proto(T) \
T *x, \
long int const* ldx, \
long int const* n, \
long int const* p, \
T* qraux, \
long int *jpvt, \
T *work, \
long int const* job
#define vnl_netlib_qrdc_params \
x, ldx, n, p, qraux, jpvt, work, job

// xQRSL
#define vnl_netlib_qrsl_proto(T) \
T const *x, \
long int *ldx, \
long int *n, \
long int *k, \
T const *qraux, \
T const *y, \
T *qy, \
T *qty, \
T *b, \
T *rsd, \
T *xb, \
long int *job, \
long int *info
#define vnl_netlib_qrsl_params \
x, ldx, n, k, qraux, y, qy, qty, b, rsd, xb, job, info

#endif // vnl_netlib_h_
