#ifndef vnl_amoeba_h_
#define vnl_amoeba_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_amoeba - Nelder-Meade downhill simplex
// .LIBRARY	vnl-algo
// .HEADER	vxl Package
// .INCLUDE	vnl/algo/vnl_amoeba.h
// .FILE	vnl_amoeba.cxx
//
// .SECTION Description
//    vnl_amoeba is an implementation of the Nelder-Meade downhill simplex
//    algorithm.  For most problems, it's a few times slower than
//    vnl_levenberg_marquardt, but it can perform much better on noisy error
//    functions.
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 23 Oct 97
//
// .SECTION Modifications
//     971023 AWF Initial version.
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matops.h>

class vnl_cost_function;
class vnl_least_squares_function;

//: Nelder-Meade downhill simplex.

class vnl_amoeba {
public:
  int verbose;
  int maxiter;
  double X_tolerance;
  double F_tolerance;
  double relative_diameter;

  vnl_amoeba(vnl_cost_function& f);

  void set_delta(vnl_vector<double> const& delta_x);
  void minimize(vnl_vector<double>& x);
  int get_num_evaluations() const { return num_evaluations_; }
  
public:
  static void minimize(vnl_cost_function& f, vnl_vector<double>& x);
  static void minimize(vnl_cost_function& f, vnl_vector<double>& x, double delta);
  static void minimize(vnl_least_squares_function& f, vnl_vector<double>& x);

  static bool default_verbose;
  
protected:
  vnl_cost_function* fptr;
  int num_evaluations_;
};

// Private class needs to be declared here in order to instantiate STL container of it.
struct vnl_amoeba_SimplexCorner {
  vnl_vector<double> v;
  double fv;
  
  vnl_amoeba_SimplexCorner(int = 0);
  vnl_amoeba_SimplexCorner& operator= (const vnl_amoeba_SimplexCorner& that);
  static int compare(const vnl_amoeba_SimplexCorner & s1, const vnl_amoeba_SimplexCorner &s2);
};

#endif // vnl_amoeba_h_
