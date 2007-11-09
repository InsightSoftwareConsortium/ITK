// This is core/vnl/algo/vnl_lsqr.h
#ifndef vnl_lsqr_h_
#define vnl_lsqr_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Linear least squares
//
// vnl_lsqr implements an algorithm for large, sparse linear systems and
// sparse, linear least squares. It is a wrapper for the LSQR algorithm
// of Paige and Saunders (ACM TOMS 583). The sparse system is encapsulated
// by a vnl_linear_system.
//
// \author David Capel, capes@robots
// \date   July 2000
//
// \verbatim
//  Modifications
//   000705 capes@robots initial version.
//   4/4/01 LSB (Manchester) Documentation tidied
// \endverbatim
//-----------------------------------------------------------------------------

#include <vnl/vnl_vector.h>
#include <vnl/vnl_linear_system.h>
#include <vcl_iosfwd.h>

//: Linear least squares
//  vnl_lsqr implements an algorithm for large, sparse linear systems and
//  sparse, linear least squares. It is a wrapper for the LSQR algorithm
//  of Paige and Saunders (ACM TOMS 583). The sparse system is encapsulated
//  by a vnl_linear_system.

class vnl_lsqr
{
 public:
  vnl_lsqr(vnl_linear_system& ls) :
    ls_(&ls), max_iter_(4*ls.get_number_of_unknowns()) {}

  ~vnl_lsqr();

  void set_max_iterations(long max_iter) { max_iter_ = max_iter; }

  //: Perform the minimization starting from x=0 and putting the result into x.
  // Return code may be translated with translate_return_code(), or the result of the
  // minimization may be printed in more detail with diagnose_outcome()
  int minimize(vnl_vector<double>& x);

  long get_number_of_iterations() const { return num_iter_; }

  //: Pontificate about the outcome of the last minimization.
  void diagnose_outcome(vcl_ostream& os) const;

  static void translate_return_code(vcl_ostream& os, int return_code);

  //: Return the residual norm estimate:
  double get_resid_norm_estimate() const { return resid_norm_estimate_; }

  //: Get the return code for the last minimization
  inline int return_code() const { return return_code_; }

 protected:
  vnl_linear_system* ls_;
  long max_iter_;
  long num_iter_;
  double resid_norm_estimate_;
  double result_norm_estimate_;
  double A_condition_estimate_;
  double result_norm_;
  long return_code_;

  static int aprod_(long* mode, long* m, long* n, double* x, double* y,
                    long* leniw, long* lenrw, long* iw, double* rw,
                    void* userdata);
};

#endif // vnl_lsqr_h_
