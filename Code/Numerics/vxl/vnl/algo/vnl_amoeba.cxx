//-*- c++ -*-------------------------------------------------------------------
#ifdef __GNUC__
#pragma implementation
#endif
//
// Class: vnl_amoeba
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 23 Oct 97
//
//-----------------------------------------------------------------------------

#include "vnl_amoeba.h"

#include <vcl_cstdlib.h>
#include <vcl_cstdio.h>
#include <vcl_iostream.h>
#include <vcl_vector.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_matops.h>
#include <vnl/vnl_cost_function.h>
#include <vnl/vnl_least_squares_function.h>

bool vnl_amoeba::default_verbose = false;

vnl_amoeba::vnl_amoeba(vnl_cost_function& f):
  fptr(&f)
{
  verbose = default_verbose;
  maxiter = f.get_number_of_unknowns() * 200;
  X_tolerance = 1e-8;
  F_tolerance = 1e-4;
  relative_diameter = 0.05;
}

typedef vnl_amoeba_SimplexCorner SimplexCorner;

struct vnl_amoebaFit : public vnl_amoeba {
  int cnt;

  vnl_amoebaFit(vnl_amoeba& a): vnl_amoeba(a) {
    cnt = 0;
  }
  
  void amoeba(vnl_vector<double>& x);

  double f(const vnl_vector<double>& x) {
    return fptr->f(x);
  }
  
  void set_corner(SimplexCorner * s, const vnl_vector<double>& v)
  {
    s->v = v;
    s->fv = f(v);
    cnt++;
  }
  void set_corner_a_plus_bl(SimplexCorner * s, const vnl_vector<double>& vbar, const vnl_vector<double>& v, double lambda)
  {
    s->v = (1 - lambda) * vbar + lambda * v;
    s->fv = f(s->v);
    cnt++;
  }
};

int SimplexCorner::compare(const SimplexCorner& s1, const SimplexCorner& s2)
{
  return vnl_math_sgn(s1.fv - s2.fv);
}

#ifdef VCL_SUNPRO_CC
extern "C"
#else
static
#endif
int compare_aux(const void * s1, const void * s2)
{
  return SimplexCorner::compare(*(const SimplexCorner*)s1, *(const SimplexCorner*)s2);
}

void sort_simplex(vcl_vector<SimplexCorner>& simplex)
{
  qsort(&simplex[0], simplex.size(), sizeof simplex[0], compare_aux);
}

double maxabsdiff(const vnl_vector<double>& a, const vnl_vector<double>& b)
{
  double v = 0;
  for(unsigned i = 0; i < a.size(); ++i) {
    double ad = vnl_math_abs(a[i] - b[i]);
    if (ad > v)
      v = ad;
  }
  return v;
}

double sorted_simplex_fdiameter(const vcl_vector<SimplexCorner>& simplex)
{
  return simplex[simplex.size()-1].fv - simplex[0].fv;
}

double simplex_fdiameter(const vcl_vector<SimplexCorner>& simplex)
{
  // simplex assumed sorted, so fdiam is n - 0
  double max = 0;
  for(unsigned i = 1; i < simplex.size(); i++) {
    double thismax = vnl_math_abs(simplex[0].fv - simplex[i].fv);
    if (thismax > max)
      max = thismax;
  }
  return max;
}

double simplex_diameter(const vcl_vector<SimplexCorner>& simplex)
{
  double max = 0;
  for(unsigned i = 0; i < simplex.size() - 1; i++) {
    double thismax = maxabsdiff(simplex[i].v, simplex[i+1].v);
    if (thismax > max)
      max = thismax;
  }
  return max;
}


vcl_ostream& operator<<(vcl_ostream& s, const SimplexCorner& simplex)
{
  s << "S" << simplex.fv << " ";
  return s;
}

vcl_ostream& operator<<(vcl_ostream& s, const vcl_vector<SimplexCorner>& simplex)
{
  for(unsigned i = 0; i < simplex.size(); ++i)
    s << simplex[i].fv << " ";
  return s;
}


bool operator==(const SimplexCorner& a, const SimplexCorner& b)
{
  return (&a) == (&b);
}

void vnl_amoebaFit::amoeba(vnl_vector<double>& x)
{
  
//FMINS	Minimize a function of several variables.
//	FMINS('F',X0) attempts to return a vector x which is a local minimizer 
//	of F(x) near the starting vector X0.  'F' is a string containing the
//	name of the objective function to be minimized.  F(x) should be a
//	scalar valued function of a vector variable.
//
//	FMINS('F',X0,OPTIONS) uses a vector of control parameters.
//	If OPTIONS(1) is nonzero, intermediate steps in the solution are
//	displayed; the default is OPTIONS(1) = 0.  OPTIONS(2) is the termination
//	tolerance for x; the default is 1.e-4.  OPTIONS(3) is the termination
//	tolerance for F(x); the default is 1.e-4.  OPTIONS(14) is the maximum
//	number of steps; the default is OPTIONS(14) = 500.  The other components
//	of OPTIONS are not used as input control parameters by FMIN.  For more
//	information, see FOPTIONS.
//
//	FMINS('F',X0,OPTIONS,[],P1,P2,...) provides for up to 10 additional
//	arguments which are passed to the objective function, F(X,P1,P2,...)
//
//	FMINS uses a Simplex search method.
//
//	See also FMIN. 

//	Reference: J. E. Dennis, Jr. and D. J. Woods, New Computing
//	Environments: Microcomputers in Large-Scale Computing,
//	edited by A. Wouk, SIAM, 1987, pp. 116-122.

// Set up a simplex near the initial guess.
  int n = x.size();
  vcl_vector<SimplexCorner> simplex(n+1, SimplexCorner(n));
  
  simplex[0].v = x;
  simplex[0].fv = f(x);

  cnt = 0;
  
  // Following improvement suggested by L.Pfeffer at Stanford
  const double usual_delta = relative_diameter;             // 5 percent deltas for non-zero terms
  const double zero_term_delta = 0.00025;      // Even smaller delta for zero elements of x
  vnl_vector<double> y(n);
  for(int j = 0; j < n; ++j) {
    SimplexCorner *s = &simplex[j+1];
    s->v = x;

    // perturb s->v(j)
    if (vnl_math_abs(s->v[j]) > zero_term_delta)
      s->v[j] = (1 + usual_delta)*s->v[j];
    else
      s->v[j] = zero_term_delta;

    ++cnt;
    s->fv = f(s->v);
  }
  sort_simplex(simplex);

  if (verbose > 1) {
    vcl_cerr << "initial\n" << simplex;
  } else if (verbose) {
    vcl_cerr << "initial: " << simplex << vcl_endl;
  }

  // Iterate until the diameter of the simplex is less than X_tolerance.
  SimplexCorner reflect(n);
  SimplexCorner expand(n);
  SimplexCorner contract(n);
  SimplexCorner shrink(n);
  SimplexCorner *next;

  vnl_vector<double> vbar(n);
  while (cnt < maxiter) {
    if (simplex_diameter(simplex) < X_tolerance &&
	sorted_simplex_fdiameter(simplex) < F_tolerance)
      break;

    // One step of the Nelder-Mead simplex algorithm
    for(int k =  0; k < n; ++k)  {
      vbar[k] = 0;
      for(int i = 0; i < n; ++i)
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
    } else {
      // Reflection *is* totally crap...
      {
	SimplexCorner *tmp = &simplex[n];
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
	for(int j = 1; j < n; ++j)

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
      sprintf(buf, "iter %5d: %s ", cnt, how);
      vcl_cerr << buf;
      if (verbose > 1)
	vcl_cerr << vcl_endl << simplex << vcl_endl;
      else if (verbose)
	vcl_cerr << simplex << vcl_endl;
    }
  }
  num_evaluations_ = cnt;
  x = simplex[0].v;
}

// -- Method
void vnl_amoeba::minimize(vnl_vector<double>& x)
{
  vnl_amoebaFit af(*this);
  af.amoeba(x);
  num_evaluations_ = af.num_evaluations_;
}

// -- Static method
void vnl_amoeba::minimize(vnl_cost_function& f, vnl_vector<double>& x)
{
  minimize(f, x, 0);
}

// -- Static method
void vnl_amoeba::minimize(vnl_cost_function& f, vnl_vector<double>& x, double delta)
{
  vnl_amoeba a(f);
  a.verbose = vnl_amoeba::default_verbose;
  if (delta != 0) 
    a.relative_diameter = delta;
  vnl_amoebaFit amoeba(a);
  amoeba.amoeba(x);
}



class vnl_amoeba_LSCF : public vnl_cost_function {
  vnl_least_squares_function* ls_;
  vnl_vector<double> fx;
public:

  vnl_amoeba_LSCF(vnl_least_squares_function& ls):
    vnl_cost_function(ls.get_number_of_unknowns()),
    ls_(&ls),
    fx(ls.get_number_of_residuals())
  {
  }

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
vnl_amoeba_SimplexCorner::vnl_amoeba_SimplexCorner(int n):
  v(n)
{
}

vnl_amoeba_SimplexCorner& vnl_amoeba_SimplexCorner::operator=(const vnl_amoeba_SimplexCorner& that)
{
  v = that.v;
  fv = that.fv;
  return *this;
}

//--------------------------------------------------------------------------------
