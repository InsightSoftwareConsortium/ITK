// This is vxl/vnl/algo/vnl_netlib.h
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

#include <netlib.h>

// xSVDC
#define vnl_netlib_svd_proto(T) \
T *x, int const *ldx, int const *m, int const *n, \
T *sv, \
T *errors, \
T *u, int const *ldu, \
T *v, int const *ldv, \
T *work, \
int const *job, int *info
#define vnl_netlib_svd_params \
x, ldx, m, n, sv, errors, u, ldu, v, ldv, work, job, info

// xQRDC
#define vnl_netlib_qrdc_proto(T) \
T *x, \
int const* ldx, \
int const* n, \
int const* p, \
T* qraux, \
int *jpvt, \
T *work, \
int const* job
#define vnl_netlib_qrdc_params \
x, ldx, n, p, qraux, jpvt, work, job

// xQRSL
#define vnl_netlib_qrsl_proto(T) \
T const *x, \
int *ldx, \
int *n, \
int *k, \
T const *qraux, \
T const *y, \
T *qy, \
T *qty, \
T *b, \
T *rsd, \
T *xb, \
int *job, \
int *info
#define vnl_netlib_qrsl_params \
x, ldx, n, k, qraux, y, qy, qty, b, rsd, xb, job, info

#if 0 // old interface

struct vnl_netlib
{
  // To see why we do this, consider what would happen with:
  //  #define complex       vcl_complex<float>
  //  #define doublecomplex vcl_complex<double>
  // when vcl_complex is a #define for complex.
  typedef int                 integer;
  typedef float               real;
  typedef double              doublereal;
  typedef vcl_complex<float>  complex;
  typedef vcl_complex<double> doublecomplex;
};

extern "C" {
#define integer       vnl_netlib::integer
#define real          vnl_netlib::real
#define doublereal    vnl_netlib::doublereal
#define complex       vnl_netlib::complex
#define doublecomplex vnl_netlib::doublecomplex
  // complex eigensystem
  int zgeev_(char const *jobvl,
             char const *jobvr,
             integer *n,
             doublecomplex *a,
             integer *lda,
             doublecomplex *w,
             doublecomplex *vl,
             integer *ldvl,
             doublecomplex *vr,
             integer *ldvr,
             doublecomplex *work,
             integer *lwork,
             doublereal *rwork,
             integer *info);

  // linpack xSVDC() routines
  int ssvdc_(vnl_netlib_svd_proto(real));
  int dsvdc_(vnl_netlib_svd_proto(doublereal));
  int csvdc_(vnl_netlib_svd_proto(complex));
  int zsvdc_(vnl_netlib_svd_proto(doublecomplex));

  // linpack xQRDC QR routines
  int sqrdc_(vnl_netlib_qrdc_proto(real));
  int dqrdc_(vnl_netlib_qrdc_proto(doublereal));
  int cqrdc_(vnl_netlib_qrdc_proto(complex));
  int zqrdc_(vnl_netlib_qrdc_proto(doublecomplex));

  // solve A x = b using QR ?
  int sqrsl_(vnl_netlib_qrsl_proto(real));
  int dqrsl_(vnl_netlib_qrsl_proto(doublereal));
  int cqrsl_(vnl_netlib_qrsl_proto(complex));
  int zqrsl_(vnl_netlib_qrsl_proto(doublecomplex));

  // real eigensystem
  int rg_(int const* nm,
          int const* n,
          double const* a,
          doublereal* wr,
          doublereal* wi,
          int const* matz,
          doublereal* z,
          int* iv1,
          doublereal* fv1,
          int* ierr);

  // temperton fft routines
  int gpfa_ (real  *a, real  *b, real  const *triggs,
             int const *inc, int const *jump, int const *n,
             int const *lot, int const *isign, int const *, int *);
  int setgpfa_ (real  *triggs, const int *, const int *, int *);
  int dgpfa_(doublereal *a, doublereal *b, doublereal const *triggs,
             int const *inc, int const *jump, int const *n,
             int const *lot, int const *isign, int const *, int *);
  int dsetgpfa_(doublereal *triggs, const int *, const int *, int *);

  // symmetric eigensystem
  int rs_(int const * nm, int const * n,
          doublereal const *a, doublereal *w,
          int const * matz, doublereal const *z,
          doublereal const *fv1, doublereal const *fv2,
          int * ierr);

  // generalized eigensystem
  int rsg_ (int const * nm, int const * n, doublereal const *a, doublereal const *b,
            doublereal *w, int const * matz, doublereal *z, doublereal *fv1, doublereal *fv2,
            int *ierr);

  // cholesky
  int dpofa_(doublereal *m, const int* lda, const int* n, int* info);
  int dposl_(const doublereal *a, const int* lda, const int* n, doublereal *b);
  int dpoco_(doublereal *a, const int* lda, const int* n,
             doublereal* rcond, doublereal *z, int *info);
  int dpodi_(const doublereal *a, const int* lda, const int* n,
             doublereal* det, const int* job);

  // roots of real polynomial
  void rpoly_(const doublereal* op, int* degree, doublereal *zeror,
              doublereal *zeroi, int *fail);

  //
  void dchscdf_();
  int lbfgs_();
  int dnlaso_();
  int cg_();

  // lmdif() is used by vnl_levenberg_marquardt
  int lmdif_(int fcn(int* m,      // I    Number of residuals
                     int* n,      // I    Number of unknowns
                     doublereal const* x, // I    Solution vector, size n
                     doublereal* fx,      // O    Residual vector f(x)
                     int* iflag   // IO   0 ==> print, -1 ==> terminate
                     ),
             int *m,              // I     Number of residuals, must be > #unknowns
             int *n,              // I     Number of unknowns
             doublereal *x,       // IO    Solution vector, size n
             doublereal *fvec,    // W m   Storage for residual vector
             doublereal *ftol,    // I     Termination tolerance on F (sum of squared residuals)
             doublereal *xtol,    // I     Termination tolerance on X (solution vector)
             doublereal *gtol,    // I     Termination tolerance on Grad(F)' * F = 0
             int    *maxfev,      // I     Termination maximum number of iterations.
             doublereal *epsfcn,  // I     Step length for FD Jacobian
             doublereal *diag,    // I     Multiplicative scale factors for variables
             int    *mode,        // I     1 => Compute diag, 2 => user has set diag
             doublereal *factor,  // I     Initial step bound.  Set to 100.
             int    *nprint,      // I     +ive => print every nprint iters.
             int    *info,        // O     See switch (info) below
             int    *nfev,        // O     Number of function evaluations
             doublereal *fjac,    // O m*n Upper n*n is P'J'JP = R'R
             int    *ldfjac,      // I     Leading dimension of fdjac -- set to m
             int    *ipvt,        // O n   Permutation indices P
             doublereal *qtf,     // O n   Q'*f(x)
             doublereal *wa1,     // W n
             doublereal *wa2,     // W n
             doublereal *wa3,     // W n
             doublereal *wa4,     // W m
             doublereal *errors); // O 2   Start/end RMS errors

  // lmder() is used by vnl_levenberg_marquardt
  int lmder1_(int fcn(int* m,          // I    Number of residuals
                      int* n,          // I    Number of unknowns
                      doublereal const* x, // I    Solution vector, size n
                      doublereal* fx,      // O    Residual vector f(x), size m
                      doublereal* fJ,      // O    m * n Jacobian f(x)
                      int*,
                      int* iflag        // I    1 -> calc fx, 2 -> calc fjac
                                        // O    0 ==> print, -1 ==> terminate
                      ),
              int const* m,             // I    Number of residuals
              int const* n,             // I    Number of unknowns
              doublereal*    x,         // I    Solution vector, size n
              doublereal*    fvec,      // O    Residual vector f(x), size m
              doublereal*    fjac,      // O    m * n Jacobian f(x)
              int const* ldfjac,        // I    LD of fjac
              doublereal const* tol,    // I    x/ftol
              int* info,                // O
              int* ipvt,                // O length n
              doublereal * wa,          // I work, length lwa
              const int* lwa);          // I > 5*n+m
#undef integer
#undef real
#undef doublereal
#undef complex
#undef doublecomplex
};

#endif // 0

#endif // vnl_netlib_h_
