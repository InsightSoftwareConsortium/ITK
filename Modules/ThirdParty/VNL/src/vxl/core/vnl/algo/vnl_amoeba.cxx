// This is core/vnl/algo/vnl_amoeba.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   23 Oct 97
//-----------------------------------------------------------------------------

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <vector>
#include "vnl_amoeba.h"

#include <vcl_compiler.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_cost_function.h>
#include <vnl/vnl_least_squares_function.h>

bool vnl_amoeba::default_verbose = false;

vnl_amoeba::vnl_amoeba(vnl_cost_function& f)
  : fptr(&f), num_evaluations_(0)
{
  verbose = default_verbose;
  maxiter = f.get_number_of_unknowns() * 200;
  X_tolerance = 1e-8;
  F_tolerance = 1e-4;
  relative_diameter = 0.05;
  zero_term_delta = 0.00025;
}


struct vnl_amoebaFit : public vnl_amoeba
{
  int cnt;

  vnl_amoebaFit(vnl_amoeba& a): vnl_amoeba(a) {
    cnt = 0;
  }

  //: Initialise the simplex given one corner, x (scale each element to get other corners)
  void set_up_simplex_relative(std::vector<vnl_amoeba_SimplexCorner>& simplex,
                               const vnl_vector<double>& x);

  //: Initialise the simplex given one corner, x and displacements of others
  void set_up_simplex_absolute(std::vector<vnl_amoeba_SimplexCorner>& simplex,
                               const vnl_vector<double>& x,
                               const vnl_vector<double>& dx);

  //: Perform optimisation.  Start simplex defined by scaling elements of x
  void amoeba(vnl_vector<double>& x);

  //: Perform optimisation.  Start simplex defined by adding dx[i] to each x[i]
  void amoeba(vnl_vector<double>& x, const vnl_vector<double>& dx);

  //: Perform optimisation, given simplex to start
  void amoeba(vnl_vector<double>& x, std::vector<vnl_amoeba_SimplexCorner>& simplex);

  double f(const vnl_vector<double>& x) {
    return fptr->f(x);
  }

  void set_corner(vnl_amoeba_SimplexCorner * s,
                  const vnl_vector<double>& v)
  {
    s->v = v;
    s->fv = f(v);
    cnt++;
  }
  void set_corner_a_plus_bl(vnl_amoeba_SimplexCorner * s,
                            const vnl_vector<double>& vbar,
                            const vnl_vector<double>& v,
                            double lambda)
  {
    s->v = (1 - lambda) * vbar + lambda * v;
    s->fv = f(s->v);
    cnt++;
  }
};

int vnl_amoeba_SimplexCorner::compare(vnl_amoeba_SimplexCorner const& s1,
                                      vnl_amoeba_SimplexCorner const& s2)
{
  return vnl_math::sgn(s1.fv - s2.fv);
}

static
int compare_aux(const void * s1, const void * s2)
{
  return vnl_amoeba_SimplexCorner::compare(*(const vnl_amoeba_SimplexCorner*)s1,
                                           *(const vnl_amoeba_SimplexCorner*)s2);
}

static
void sort_simplex(std::vector<vnl_amoeba_SimplexCorner>& simplex)
{
  std::qsort(&simplex[0], simplex.size(), sizeof simplex[0], compare_aux);
}

static
double maxabsdiff(const vnl_vector<double>& a, const vnl_vector<double>& b)
{
  double v = 0;
  for (unsigned i = 0; i < a.size(); ++i) {
    double ad = vnl_math::abs(a[i] - b[i]);
    if (ad > v)
      v = ad;
  }
  return v;
}

static
double sorted_simplex_fdiameter(const std::vector<vnl_amoeba_SimplexCorner>& simplex)
{
  return simplex[simplex.size()-1].fv - simplex[0].fv;
}

static
double simplex_diameter(const std::vector<vnl_amoeba_SimplexCorner>& simplex)
{
  double max = 0;
  for (unsigned i = 0; i < simplex.size() - 1; i++) {
    double thismax = maxabsdiff(simplex[i].v, simplex[i+1].v);
    if (thismax > max)
      max = thismax;
  }
  return max;
}


std::ostream& operator<<(std::ostream& s, const vnl_amoeba_SimplexCorner& simplex)
{
  s << 'S' << simplex.fv << ' ';
  return s;
}

std::ostream& operator<<(std::ostream& s, const std::vector<vnl_amoeba_SimplexCorner>& simplex)
{
  for (unsigned i = 0; i < simplex.size(); ++i)
    s << simplex[i].fv << ' ';
  return s;
}


bool operator==(const vnl_amoeba_SimplexCorner& a, const vnl_amoeba_SimplexCorner& b)
{
  return (&a) == (&b);
}

//: Initialise the simplex given one corner, x
void vnl_amoebaFit::set_up_simplex_relative(std::vector<vnl_amoeba_SimplexCorner>& simplex,
                                            const vnl_vector<double>& x)
{
  int n = x.size();

  simplex[0].v = x;
  simplex[0].fv = f(x);

  // Following improvement suggested by L.Pfeffer at Stanford
  const double usual_delta = relative_diameter;             // 5 percent deltas for non-zero terms
  //const double zero_term_delta = 0.00025;      // Even smaller delta for zero elements of x
//  vnl_vector<double> y(n);
  for (int j = 0; j < n; ++j) {
    vnl_amoeba_SimplexCorner *s = &simplex[j+1];
    s->v = x;

    // perturb s->v(j)
    if (vnl_math::abs(s->v[j]) > zero_term_delta)
      s->v[j] = (1 + usual_delta)*s->v[j];
    else
      s->v[j] = zero_term_delta;

    s->fv = f(s->v);
  }
}

//: Initialise the simplex given one corner, x and displacements of others
void vnl_amoebaFit::set_up_simplex_absolute(std::vector<vnl_amoeba_SimplexCorner>& simplex,
                                            const vnl_vector<double>& x,
                                            const vnl_vector<double>& dx)
{
  int n = x.size();

  simplex[0].v = x;
  simplex[0].fv = f(x);

  for (int j = 0; j < n; ++j) {
    vnl_amoeba_SimplexCorner *s = &simplex[j+1];
    s->v = x;

    // perturb s->v(j)
    s->v[j] = s->v[j] + dx[j];

    s->fv = f(s->v);
  }
}

//: FMINS Minimize a function of several variables.
//  FMINS('F',X0) attempts to return a vector x which is a local minimizer
//  of F(x) near the starting vector X0.  'F' is a string containing the
//  name of the objective function to be minimized.  F(x) should be a
//  scalar valued function of a vector variable.
//
//  FMINS('F',X0,OPTIONS) uses a vector of control parameters.
//  If OPTIONS(1) is nonzero, intermediate steps in the solution are
//  displayed; the default is OPTIONS(1) = 0.  OPTIONS(2) is the termination
//  tolerance for x; the default is 1.e-4.  OPTIONS(3) is the termination
//  tolerance for F(x); the default is 1.e-4.  OPTIONS(14) is the maximum
//  number of steps; the default is OPTIONS(14) = 500.  The other components
//  of OPTIONS are not used as input control parameters by FMIN.  For more
//  information, see FOPTIONS.
//
//  FMINS('F',X0,OPTIONS,[],P1,P2,...) provides for up to 10 additional
//  arguments which are passed to the objective function, F(X,P1,P2,...)
//
//  FMINS uses a simplex search method.
//
//  See also FMIN.
//
//  Reference: J. E. Dennis, Jr. and D. J. Woods, New Computing
//  Environments: Microcomputers in Large-Scale Computing,
//  edited by A. Wouk, SIAM, 1987, pp. 116-122.

void vnl_amoebaFit::amoeba(vnl_vector<double>& x)
{
// Set up a simplex near the initial guess.
  int n = x.size();
  std::vector<vnl_amoeba_SimplexCorner> simplex(n+1, vnl_amoeba_SimplexCorner(n));

  set_up_simplex_relative(simplex,x);
  amoeba(x,simplex);
}

void vnl_amoebaFit::amoeba(vnl_vector<double>& x, const vnl_vector<double>& dx)
{
// Set up a simplex near the initial guess.
  int n = x.size();
  std::vector<vnl_amoeba_SimplexCorner> simplex(n+1, vnl_amoeba_SimplexCorner(n));

  set_up_simplex_absolute(simplex,x,dx);
  amoeba(x,simplex);
}

    //: Perform optimisation, given simplex to start
void vnl_amoebaFit::amoeba(vnl_vector<double>& x,
                           std::vector<vnl_amoeba_SimplexCorner>& simplex)
{
  int n = x.size();
  sort_simplex(simplex);

  if (verbose > 1) {
    std::cerr << "initial\n" << simplex;
  }
  else if (verbose) {
    std::cerr << "initial: " << simplex << '\n';
  }

  // Iterate until the diameter of the simplex is less than X_tolerance.
  vnl_amoeba_SimplexCorner reflect(n);
  vnl_amoeba_SimplexCorner expand(n);
  vnl_amoeba_SimplexCorner contract(n);
  vnl_amoeba_SimplexCorner shrink(n);
  vnl_amoeba_SimplexCorner *next;

  vnl_vector<double> vbar(n);
  while (cnt < maxiter) {
    if (simplex_diameter(simplex) < X_tolerance &&
        sorted_simplex_fdiameter(simplex) < F_tolerance)
      break;

    // One step of the Nelder-Mead simplex algorithm
    for (int k =  0; k < n; ++k)  {
      vbar[k] = 0;
      for (int i = 0; i < n; ++i)
        vbar[k] += simplex[i].v[k];
      vbar[k] /= n;
    }

    set_corner_a_plus_bl(&reflect, vbar, simplex[n].v, -1);

    next = &reflect;
    const char *how = "reflect ";
    if (reflect.fv < simplex[n-1].fv) {
      // Reflection not totally crap...
      if (reflect.fv < simplex[0].fv) {
        // Reflection actually the best, try expanding
        set_corner_a_plus_bl(&expand, vbar, reflect.v, 2);

        if (expand.fv < simplex[0].fv) {
          next = &expand;
          how = "expand  ";
        }
      }
    }
    else {
      // Reflection *is* totally crap...
      {
        vnl_amoeba_SimplexCorner *tmp = &simplex[n];
        if (reflect.fv < tmp->fv)
          // replace simplex[n] by reflection as at least it's better than that
          tmp = &reflect;
        set_corner_a_plus_bl(&contract, vbar, tmp->v, 0.5);
      }

      if (contract.fv < simplex[0].fv) {
        // The contraction point was really good, hold it there
        next = &contract;
        how = "contract";
      }
      else {
        // The contraction point was only average, shrink the entire simplex.
        for (int j = 1; j < n; ++j)

          set_corner_a_plus_bl(&simplex[j], simplex[0].v, simplex[j].v, 0.5);
        set_corner_a_plus_bl(&shrink, simplex[0].v, simplex[n].v, 0.5);

        next = &shrink;
        how = "shrink  ";
      }
    }
    simplex[n] = *next;

    sort_simplex(simplex);

    // Print debugging info
    if (verbose) {
      char buf[16383];
      std::sprintf(buf, "iter %5d: %s ", cnt, how);
      std::cerr << buf;
      if (verbose ==2)
        std::cerr << "\nFirst corner: " << simplex[0].v;
      if (verbose > 1)
      {
        std::streamsize a = std::cerr.width(10);
        std::cerr << '\n' << simplex << '\n';
        std::cerr.width(a);
      }
      else if (verbose)
        std::cerr << simplex << '\n';
    }
  }
  num_evaluations_ = cnt;
  x = simplex[0].v;
  end_error_ = simplex[0].fv;
}

//: Modify x to minimise function supplied in constructor
//  Start simplex defined by scaling elements of x
void vnl_amoeba::minimize(vnl_vector<double>& x)
{
  vnl_amoebaFit af(*this);
  af.amoeba(x);
  num_evaluations_ = af.num_evaluations_;
  end_error_ = af.end_error_;
}

//: Perform optimisation.  Start simplex defined by adding dx[i] to each x[i]
void vnl_amoeba::minimize(vnl_vector<double>& x, const vnl_vector<double>& dx)
{
  vnl_amoebaFit af(*this);
  af.amoeba(x,dx);
  num_evaluations_ = af.num_evaluations_;
  end_error_ = af.end_error_;
}


//: Static method
void vnl_amoeba::minimize(vnl_cost_function& f, vnl_vector<double>& x)
{
  minimize(f, x, 0);
}

//: Static method
void vnl_amoeba::minimize(vnl_cost_function& f, vnl_vector<double>& x, double delta)
{
  vnl_amoeba a(f);
  a.verbose = vnl_amoeba::default_verbose;
  if (delta != 0)
    a.relative_diameter = delta;
  vnl_amoebaFit amoeba(a);
  amoeba.amoeba(x);
}

//: Static method
void vnl_amoeba::minimize(vnl_cost_function& f, vnl_vector<double>& x,
                          const vnl_vector<double>& dx)
{
  vnl_amoeba a(f);
  a.verbose = vnl_amoeba::default_verbose;
  vnl_amoebaFit amoeba(a);
  amoeba.amoeba(x,dx);
}


class vnl_amoeba_LSCF : public vnl_cost_function
{
  vnl_least_squares_function* ls_;
  vnl_vector<double> fx;
 public:
  vnl_amoeba_LSCF(vnl_least_squares_function& ls)
   : vnl_cost_function(ls.get_number_of_unknowns()),
     ls_(&ls), fx(ls.get_number_of_residuals()) {}

  ~vnl_amoeba_LSCF() {}

  double f(vnl_vector<double> const& x) {
    ls_->f(x, fx);
    return fx.squared_magnitude();
  }
};

void vnl_amoeba::minimize(vnl_least_squares_function& f, vnl_vector<double>& x)
{
  vnl_amoeba_LSCF lsf(f);
  minimize(lsf, x);
}

/////////////////////////////////////////////////////////////////////////////
vnl_amoeba_SimplexCorner::vnl_amoeba_SimplexCorner(int n) : v(n) {}

vnl_amoeba_SimplexCorner& vnl_amoeba_SimplexCorner::operator=(const vnl_amoeba_SimplexCorner& that)
{
  v = that.v;
  fv = that.fv;
  return *this;
}

//--------------------------------------------------------------------------------
