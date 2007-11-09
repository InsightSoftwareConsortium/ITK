// This is core/vnl/algo/vnl_rnpoly_solve.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
#include "vnl_rnpoly_solve.h"

#include <vcl_cmath.h>
#include <vcl_cassert.h>
#ifdef DEBUG
#include <vcl_iostream.h>
#include <vcl_fstream.h>
#endif

static unsigned int dim_ = 0; // dimension of the problem
static unsigned int max_deg_ = 0; // maximal degree
static unsigned int max_nterms_ = 0; // maximal number of terms

//: This is a local implementation of a minimal "complex number" class, for internal use only
class vnl_rnpoly_solve_cmplx
{
 public:
  double R;
  double C;
  vnl_rnpoly_solve_cmplx(double a=0, double b=0) : R(a), C(b) {}
  inline double norm() const { return R*R+C*C; }
  inline vnl_rnpoly_solve_cmplx operator-() const
  { return vnl_rnpoly_solve_cmplx(-R, -C); }
  inline vnl_rnpoly_solve_cmplx operator+(vnl_rnpoly_solve_cmplx const& Y) const
  { return vnl_rnpoly_solve_cmplx(R+Y.R, C+Y.C); }
  inline vnl_rnpoly_solve_cmplx operator-(vnl_rnpoly_solve_cmplx const& Y) const
  { return vnl_rnpoly_solve_cmplx(R-Y.R, C-Y.C); }
  inline vnl_rnpoly_solve_cmplx& operator+=(vnl_rnpoly_solve_cmplx const& Y)
  { R+=Y.R; C+=Y.C; return *this; }
  inline vnl_rnpoly_solve_cmplx& operator-=(vnl_rnpoly_solve_cmplx const& Y)
  { R-=Y.R; C-=Y.C; return *this; }
  inline vnl_rnpoly_solve_cmplx operator*(vnl_rnpoly_solve_cmplx const& Y) const
  { return vnl_rnpoly_solve_cmplx(R*Y.R-C*Y.C, R*Y.C+C*Y.R); }
  inline vnl_rnpoly_solve_cmplx operator/(vnl_rnpoly_solve_cmplx const& Y) const
  { double N=1.0/Y.norm(); return vnl_rnpoly_solve_cmplx((R*Y.R+C*Y.C)*N, (C*Y.R-R*Y.C)*N); }
  inline vnl_rnpoly_solve_cmplx operator*(double T) const
  { return vnl_rnpoly_solve_cmplx(R*T, C*T); }
  inline vnl_rnpoly_solve_cmplx& operator*=(double T)
  { R*=T; C*=T; return *this; }
  inline vnl_rnpoly_solve_cmplx& operator*=(vnl_rnpoly_solve_cmplx const& Y)
  { double r=R*Y.R-C*Y.C; C=R*Y.C+C*Y.R; R=r; return *this; }
  inline vnl_rnpoly_solve_cmplx& operator/=(vnl_rnpoly_solve_cmplx const& Y)
  { return *this = operator/(Y); }
};

static const double twopi = 6.2831853071795864769;

static const double epsilonB  = 2.e-03;
static const vnl_rnpoly_solve_cmplx  epsilonZ  = vnl_rnpoly_solve_cmplx(1.e-04,1.e-04);
static const double final_eps = 1.e-10;
static const double stepinit  = 1.e-02;


vcl_vector<vnl_vector<double>*> vnl_rnpoly_solve::realroots(double tol)
{
  tol *= tol; // squared tolerance
  vcl_vector<vnl_vector<double>*> rr;
  vcl_vector<vnl_vector<double>*>::iterator rp = r_.begin(), ip = i_.begin();
  for (; rp != r_.end() && ip != i_.end(); ++rp, ++ip)
    if ((*ip)->squared_magnitude() < tol)
      rr.push_back(*rp);

  return rr;
}


//------------------------- INPTBR ---------------------------
//: Initialize random variables
// This will initialize the random variables which are used
// to preturb the starting point so as to have measure zero
// probability that we will start at a singular point.
static void inptbr(vcl_vector<vnl_rnpoly_solve_cmplx>& p, vcl_vector<vnl_rnpoly_solve_cmplx>& q)
{
  vnl_rnpoly_solve_cmplx pp[10],qq[10];

  pp[0] = vnl_rnpoly_solve_cmplx(.12324754231,  .76253746298);
  pp[1] = vnl_rnpoly_solve_cmplx(.93857838950,  -.99375892810);
  pp[2] = vnl_rnpoly_solve_cmplx(-.23467908356, .39383930009);
  pp[3] = vnl_rnpoly_solve_cmplx(.83542556622,  -.10192888288);
  pp[4] = vnl_rnpoly_solve_cmplx(-.55763522521, -.83729899911);
  pp[5] = vnl_rnpoly_solve_cmplx(-.78348738738, -.10578234903);
  pp[6] = vnl_rnpoly_solve_cmplx(.03938347346,  .04825184716);
  pp[7] = vnl_rnpoly_solve_cmplx(-.43428734331, .93836289418);
  pp[8] = vnl_rnpoly_solve_cmplx(-.99383729993, -.40947822291);
  pp[9] = vnl_rnpoly_solve_cmplx(.09383736736,  .26459172298);

  qq[0] = vnl_rnpoly_solve_cmplx(.58720452864,  .01321964722);
  qq[1] = vnl_rnpoly_solve_cmplx(.97884134700,  -.14433009712);
  qq[2] = vnl_rnpoly_solve_cmplx(.39383737289,  .4154322311);
  qq[3] = vnl_rnpoly_solve_cmplx(-.03938376373, -.61253112318);
  qq[4] = vnl_rnpoly_solve_cmplx(.39383737388,  -.26454678861);
  qq[5] = vnl_rnpoly_solve_cmplx(-.0093837766,  .34447867861);
  qq[6] = vnl_rnpoly_solve_cmplx(-.04837366632, .48252736790);
  qq[7] = vnl_rnpoly_solve_cmplx(.93725237347,  -.54356527623);
  qq[8] = vnl_rnpoly_solve_cmplx(.39373957747,  .65573434564);
  qq[9] = vnl_rnpoly_solve_cmplx(-.39380038371, .98903450052);

  p.resize(dim_); q.resize(dim_);
  for (unsigned int j=0; j<dim_; ++j) { int jj=j%10; p[j]=pp[jj]; q[j]=qq[jj]; }
}

//-----------------------------  POWR  -----------------------
//: This returns the complex number y raised to the nth degree
static inline vnl_rnpoly_solve_cmplx powr(int n,vnl_rnpoly_solve_cmplx const& y)
{
  vnl_rnpoly_solve_cmplx x(1,0);
  if (n>0) while (n--) x *= y;
  else     while (n++) x /= y;
  return x;
}


static void initr(vcl_vector<unsigned int> const& ideg,
                  vcl_vector<vnl_rnpoly_solve_cmplx> const& p,
                  vcl_vector<vnl_rnpoly_solve_cmplx> const& q,
                  vcl_vector<vnl_rnpoly_solve_cmplx>& r,
                  vcl_vector<vnl_rnpoly_solve_cmplx>& pdg,
                  vcl_vector<vnl_rnpoly_solve_cmplx>& qdg)
{
  assert(ideg.size()==dim_);
  assert(p.size()==dim_);
  assert(q.size()==dim_);
  pdg.resize(dim_); qdg.resize(dim_); r.resize(dim_);
  for (unsigned int j=0;j<dim_;j++)
  {
    pdg[j] = powr(ideg[j],p[j]);
    qdg[j] = powr(ideg[j],q[j]);
    r[j] = q[j] / p[j];
  }
}


//-------------------------------- DEGREE -------------------------------
//: This will compute the degree of the polynomial based upon the index.
static inline int degree(int index)
{
  return (index<0) ? 0 : (index % max_deg_) + 1;
}


//-------------------------- FFUNR -------------------------
//: Evaluate the target system component of h.
//  This is the system of equations that we are trying to find the roots.
static void ffunr(vcl_vector<double> const& coeff,
                  vcl_vector<int> const& polyn,
                  vcl_vector<unsigned int> const& terms,
                  vcl_vector<vnl_rnpoly_solve_cmplx> const& x,
                  vcl_vector<vnl_rnpoly_solve_cmplx>& pows,
                  vcl_vector<vnl_rnpoly_solve_cmplx>& f,
                  vcl_vector<vnl_rnpoly_solve_cmplx>& df)
{
  assert(terms.size()==dim_);
  assert(x.size()==dim_);
  // Compute all possible powers for each variable
  pows.resize(max_deg_*dim_);
  for (unsigned int i=0;i<dim_;i++) // for all variables
  {
    int index = max_deg_*i;
    pows[index]=x[i];
    for (unsigned int j=1;j<max_deg_;++j,++index) // for all powers
      pows[index+1]= pows[index] * x[i];
  }

  // Initialize the new arrays
  for (unsigned int i=0; i<dim_; ++i)
  {
    f[i]=vnl_rnpoly_solve_cmplx(0,0);
    for (unsigned int j=0;j<dim_;j++)
      df[i*dim_+j]=vnl_rnpoly_solve_cmplx(0,0);
  }

  for (unsigned int i=0; i<dim_; ++i) // Across equations
    for (unsigned int j=0; j<terms[i]; ++j) // Across terms
    {
      vnl_rnpoly_solve_cmplx tmp(1,0);
      for (unsigned int k=0; k<dim_; ++k) // For each variable
      {
        int index=polyn[i*dim_*max_nterms_+j*dim_+k];
        if (index>=0)
          tmp *= pows[index];
      }
      f[i] += tmp * coeff[i*max_nterms_+j];
    }

  // Compute the Derivative!
  for (int i=dim_-1;i>=0;i--) // Over equations
    for (int l=dim_-1;l>=0;l--) // With respect to each variable
    {
      vnl_rnpoly_solve_cmplx& df_il = df[i*dim_+l];
      for (int j=terms[i]-1;j>=0;j--) // Over terms in each equation
        if (polyn[i*dim_*max_nterms_+j*dim_+l]>=0) // if 0 deg in l, df term is 0
        {
          vnl_rnpoly_solve_cmplx tmp = vnl_rnpoly_solve_cmplx(1,0);
          for (int k=dim_-1;k>=0;k--)        // Over each variable in each term
          {
            int index=polyn[i*dim_*max_nterms_+j*dim_+k];
            if (index>=0)
            {
              if (k==l)
              {
                int deg = degree(index);
                if (deg > 1)
                  tmp *= pows[index-1];
                tmp *= (double)deg;
              }
              else
                tmp *= pows[index];
            }
          } // end for k
          df_il += tmp * coeff[i*max_nterms_+j];
        }
    } // end for l
}


//--------------------------- GFUNR --------------------------
//: Evaluate starting system component
// Evaluate the starting system component of h from a system
// of equations that we already know the roots. (ex: x^n - 1)
static void gfunr(vcl_vector<unsigned int> const& ideg,
                  vcl_vector<vnl_rnpoly_solve_cmplx> const& pdg,
                  vcl_vector<vnl_rnpoly_solve_cmplx> const& qdg,
                  vcl_vector<vnl_rnpoly_solve_cmplx> const& pows,
                  vcl_vector<vnl_rnpoly_solve_cmplx>& g,
                  vcl_vector<vnl_rnpoly_solve_cmplx>& dg)
{
  assert(ideg.size()==dim_);
  assert(g.size()==dim_);
  assert(dg.size()==dim_);
  vcl_vector<vnl_rnpoly_solve_cmplx> pxdgm1(dim_), pxdg(dim_);

  for (unsigned int j=0; j<dim_; ++j)
  {
    vnl_rnpoly_solve_cmplx tmp;
    if (ideg[j] <= 1)
      tmp = vnl_rnpoly_solve_cmplx(1,0);
    else
      tmp = pows[j*max_deg_+ideg[j]-2];

    pxdgm1[j] = pdg[j] * tmp;
  }

  for (unsigned int j=0; j<dim_; ++j)
  {
    int index = j*max_deg_+ideg[j]-1;
    pxdg[j] = pdg[j] * pows[index];
  }

  for (unsigned int j=0; j<dim_; ++j)
  {
    g[j]  = pxdg[j] - qdg[j];
    dg[j] = pxdgm1[j] * ideg[j];
  }
}


//-------------------------- HFUNR --------------------------
//: This is the routine that traces the curve from the gfunr to the f function
//  (i.e. Evaluate the continuation function)
static void hfunr(vcl_vector<unsigned int> const& ideg,
                  vcl_vector<vnl_rnpoly_solve_cmplx> const& pdg,
                  vcl_vector<vnl_rnpoly_solve_cmplx> const& qdg,
                  double t,
                  vcl_vector<vnl_rnpoly_solve_cmplx> const& x,
                  vcl_vector<vnl_rnpoly_solve_cmplx>& h,
                  vcl_vector<vnl_rnpoly_solve_cmplx>& dhx,
                  vcl_vector<vnl_rnpoly_solve_cmplx>& dht,
                  vcl_vector<int> const& polyn,
                  vcl_vector<double> const& coeff,
                  vcl_vector<unsigned int> const& terms)
{
  assert(ideg.size()==dim_);
  assert(terms.size()==dim_);
  assert(x.size()==dim_);
  assert(h.size()==dim_);
  assert(dht.size()==dim_);
  assert(dhx.size()==dim_*dim_);
  vcl_vector<vnl_rnpoly_solve_cmplx> df(dim_*dim_),dg(dim_),f(dim_),g(dim_);
  vcl_vector<vnl_rnpoly_solve_cmplx> pows;  //  powers of variables [dim_ equations] [max_deg_ possible powers]

  ffunr(coeff,polyn,terms,x,pows,f,df);
  gfunr(ideg,pdg,qdg,pows,g,dg);
  assert(f.size()==dim_);
  assert(g.size()==dim_);
  assert(dg.size()==dim_);
  assert(df.size()==dim_*dim_);

  double onemt=1.0 - t;
  for (unsigned int j=0; j<dim_; ++j)
  {
    for (unsigned int i=0; i<dim_; ++i)
      dhx[j*dim_+i] = df[j*dim_+i] * t;

    dhx[j*dim_+j] += dg[j]*onemt;
    dht[j] = f[j] - g[j];
    h[j] = f[j] * t + g[j] * onemt;
  }
}


//------------------------ LU DECOMPOSITION --------------------------
//: This performs LU decomposition on a matrix.
static int ludcmp(vcl_vector<vnl_rnpoly_solve_cmplx>& a, vcl_vector<int>& indx)
{
  vcl_vector<double> vv(dim_);

  // Loop over rows to get the implicit scaling information
  for (unsigned int i=0; i<dim_; ++i)
  {
    double big = 0.0;
    for (unsigned int j=0; j<dim_; ++j)
    {
      double temp = a[i*dim_+j].norm();
      if (temp > big) big = temp;
    }
    if (big == 0.0) return 1;
    vv[i]=1.0/vcl_sqrt(big);
  }

  // This is the loop over columns of Crout's method
  for (unsigned int j=0; j<dim_; ++j)
  {
    for (unsigned int i=0; i<j; ++i)
      for (unsigned int k=0; k<i; ++k)
        a[i*dim_+j] -= a[i*dim_+k] * a[k*dim_+j];

    // Initialize for the search for largest pivot element
    double big = 0.0;
    unsigned int imax = 0;

    for (unsigned int i=j; i<dim_; ++i)
    {
      for (unsigned int k=0; k<j; ++k)
        a[i*dim_+j] -= a[i*dim_+k] * a[k*dim_+j];

      // Is the figure of merit for the pivot better than the best so far?
      double rdum = vv[i]*a[i*dim_+j].norm();
      if (rdum >= big) { big = rdum; imax = i; }
    }

    // Do we need to interchange rows?
    if (j != imax)
    {
      // Yes, do so...
      for (unsigned int k=0; k<dim_; ++k)
      {
        vnl_rnpoly_solve_cmplx dum = a[imax*dim_+k];
        a[imax*dim_+k] = a[j*dim_+k]; a[j*dim_+k] = dum;
      }

      // Also interchange the scale factor
      vv[imax]=vv[j];
    }
    indx[j]=imax;

    vnl_rnpoly_solve_cmplx& ajj = a[j*dim_+j];
    if (ajj.norm() == 0.0)
      ajj = epsilonZ;

    // Now, finally, divide by the pivot element
    if (j+1 != dim_)
    {
      vnl_rnpoly_solve_cmplx dum = vnl_rnpoly_solve_cmplx(1,0) / ajj;

      // If the pivot element is zero the matrix is singular.
      for (unsigned int i=j+1; i<dim_; ++i)
        a[i*dim_+j] *= dum;
    }
  }
  return 0;
}


// ------------------------- LU Back Substitution -------------------------
static void lubksb(vcl_vector<vnl_rnpoly_solve_cmplx> const& a,
                   vcl_vector<int> const& indx,
                   vcl_vector<vnl_rnpoly_solve_cmplx> const& bb,
                   vcl_vector<vnl_rnpoly_solve_cmplx>& b)
{
  int ii=-1;
  for (unsigned int k=0; k<dim_; ++k)
    b[k] = bb[k];

  for (unsigned int i=0; i<dim_; ++i)
  {
    int ip = indx[i];
    vnl_rnpoly_solve_cmplx sum = b[ip];
    b[ip] = b[i];

    if (ii>=0)
      for (unsigned int j=ii;j<i;++j)
        sum -= a[i*dim_+j] * b[j];
    else
      // A nonzero element was encountered, so from now on we
      // will have to do the sums in the loop above
      if (sum.norm() > 0) ii = i;

    b[i] = sum;
  }

  // Now do the backsubstitution
  for (int i=dim_-1;i>=0;i--)
  {
    for (unsigned int j=i+1; j<dim_; ++j)
      b[i] -= a[i*dim_+j] * b[j];

    b[i] /= a[i*dim_+i];
  }
}


//-------------------------- LINNR -------------------
//: Solve a complex system of equations by using l-u decomposition and then back substitution.
static int linnr(vcl_vector<vnl_rnpoly_solve_cmplx>& dhx,
                 vcl_vector<vnl_rnpoly_solve_cmplx> const& rhs,
                 vcl_vector<vnl_rnpoly_solve_cmplx>& resid)
{
  vcl_vector<int> irow(dim_);
  if (ludcmp(dhx,irow)==1) return 1;
  lubksb(dhx,irow,rhs,resid);
  return 0;
}


//-----------------------  XNORM  --------------------
//: Finds the unit normal of a vector v
static double xnorm(vcl_vector<vnl_rnpoly_solve_cmplx> const& v)
{
  assert(v.size()==dim_);
  double txnorm=0.0;
  for (unsigned int j=0; j<dim_; ++j)
    txnorm += vcl_fabs(v[j].R) + vcl_fabs(v[j].C);
  return txnorm;
}

//---------------------- PREDICT ---------------------
//: Predict new x vector using Taylor's Expansion.
static void predict(vcl_vector<unsigned int> const& ideg,
                    vcl_vector<vnl_rnpoly_solve_cmplx> const& pdg,
                    vcl_vector<vnl_rnpoly_solve_cmplx> const& qdg,
                    double step, double& t,
                    vcl_vector<vnl_rnpoly_solve_cmplx>& x,
                    vcl_vector<int> const& polyn,
                    vcl_vector<double> const& coeff,
                    vcl_vector<unsigned int> const& terms)
{
  assert(ideg.size()==dim_);
  assert(terms.size()==dim_);
  assert(x.size()==dim_);

  double maxdt =.2; // Maximum change in t for a given step.  If dt is
                    // too large, there seems to be greater chance of
                    // jumping to another path.  Set this to 1 if you
                    // don't care.
  vcl_vector<vnl_rnpoly_solve_cmplx> dht(dim_),dhx(dim_*dim_),dz(dim_),h(dim_),rhs(dim_);
  // Call the continuation function that we are tracing
  hfunr(ideg,pdg,qdg,t,x,h,dhx,dht,polyn,coeff,terms);

  for (unsigned int j=0; j<dim_; ++j)
    rhs[j] = - dht[j];

  // Call the function that solves a complex system of equations
  if (linnr(dhx,rhs,dz) == 1) return;

  // Find the unit normal of a vector and normalize our step
  double factor = step/(1+xnorm(dz));
  if (factor>maxdt) factor = maxdt;

  bool tis1=true;
  if (t+factor>1) { tis1 = false; factor = 1.0 - t; }

  // Update this path with the predicted next point
  for (unsigned int j=0; j<dim_; ++j)
    x[j] += dz[j] * factor;

  if (tis1) t += factor;
  else      t = 1.0;
}


//------------------------- CORRECT --------------------------
//: Correct the predicted point to lie near the actual curve
// Use Newton's Method to do this.
// Returns:
// 0: Converged
// 1: Singular Jacobian
// 2: Didn't converge in 'loop' iterations
// 3: If the magnitude of x > maxroot
static int correct(vcl_vector<unsigned int> const& ideg, int loop, double eps,
                   vcl_vector<vnl_rnpoly_solve_cmplx> const& pdg,
                   vcl_vector<vnl_rnpoly_solve_cmplx> const& qdg,
                   double t,
                   vcl_vector<vnl_rnpoly_solve_cmplx>& x,
                   vcl_vector<int> const& polyn,
                   vcl_vector<double> const& coeff,
                   vcl_vector<unsigned int> const& terms)
{
  double maxroot= 1000;// Maximum size of root where it is considered heading to infinity
  vcl_vector<vnl_rnpoly_solve_cmplx> dhx(dim_*dim_),dht(dim_),h(dim_),resid(dim_);

  assert(ideg.size()==dim_);
  assert(terms.size()==dim_);
  assert(x.size()==dim_);

  for (int i=0;i<loop;i++)
  {
    hfunr(ideg,pdg,qdg,t,x,h,dhx,dht,polyn,coeff,terms);

    // If linnr = 1, error
    if (linnr(dhx,h,resid)==1) return 1;

    for (unsigned int j=0; j<dim_; ++j)
      x[j] -= resid[j];

    double xresid = xnorm(resid);
    if (xresid < eps) return 0;
    if (xresid > maxroot) return 3;
  }
  return 2;
}


//-------------------------- TRACE ---------------------------
//: This is the continuation routine.
// It will trace a curve from a known point in the complex plane to an unknown
// point in the complex plane.  The new end point is the root
// to a polynomial equation that we are trying to solve.
// It will return the following codes:
//      0: Maximum number of steps exceeded
//      1: Path converged
//      2: Step size became too small
//      3: Path Heading to infinity
//      4: Singular Jacobian on Path
static int trace(vcl_vector<vnl_rnpoly_solve_cmplx>& x,
                 vcl_vector<unsigned int> const& ideg,
                 vcl_vector<vnl_rnpoly_solve_cmplx> const& pdg,
                 vcl_vector<vnl_rnpoly_solve_cmplx> const& qdg,
                 vcl_vector<int> const& polyn,
                 vcl_vector<double> const& coeff,
                 vcl_vector<unsigned int> const& terms)
{
  assert(ideg.size()==dim_);
  assert(terms.size()==dim_);
  assert(x.size()==dim_);

  int maxns=500;  // Maximum number of path steps
  int maxit=5;    // Maximum number of iterations to correct a step.
                  // For each step, Newton-Raphson is used to correct
                  // the step.  This should be at least 3 to improve
                  // the chances of convergence. If function is well
                  // behaved, fewer than maxit steps will be needed

  double eps=0;                     // epsilon value used in correct
  double epsilonS=1.0e-3 * epsilonB;// smallest path step for t>.95
  double stepmin=1.0e-5 * stepinit; // Minimum stepsize allowed
  double step=stepinit;             // stepsize
  double t=0.0;                     // Continuation parameter 0<t<1
  double oldt=0.0;                  // The previous t value
  vcl_vector<vnl_rnpoly_solve_cmplx> oldx = x; // the previous path value
  int nadv=0;

  for (int numstep=0;numstep<maxns;numstep++)
  {
    // Taylor approximate the next point
    predict(ideg,pdg,qdg,step,t,x,polyn,coeff,terms);

    //if (t>1.0) t=1.0;

    if (t > .95)
    {
      if (eps != epsilonS) step = step/4.0;
      eps = epsilonS;
    }else
      eps = epsilonB;
#ifdef DEBUG
    vcl_cout << "t=" << t << vcl_endl;
#endif

    if (t>=.99999)                      // Path converged
    {
#ifdef DEBUG
      vcl_cout << "path converged\n" << vcl_flush;
#endif
      double factor = (1.0-oldt)/(t-oldt);
      for (unsigned int j=0; j<dim_; ++j)
        x[j] = oldx[j] + (x[j]-oldx[j]) * factor;
      t = 1.0;
      int cflag=correct(ideg,10*maxit,final_eps,pdg,qdg,t,x, polyn, coeff,terms);
      if ((cflag==0) ||(cflag==2))
        return 1;       // Final Correction converged
      else if (cflag==3)
        return 3;       // Heading to infinity
      else return 4;    // Singular solution
    }

    // Newton's method brings us back to the curve
    int cflag=correct(ideg,maxit,eps,pdg,qdg,t,x,polyn, coeff,terms);
    if (cflag==0)
    {
      // Successful step
      if ((++nadv)==maxit) { step *= 2; nadv=0; }   // Increase the step size
      // Make note of our new location
      oldt = t;
      oldx = x;
    }
    else
    {
      nadv=0;
      step /= 2.0;

      if (cflag==3) return 3;           // Path heading to infinity
      if (step<stepmin) return 2;       // Path failed StepSizeMin exceeded

      // Reset the values since we stepped to far, and try again
      t = oldt;
      x = oldx;
    }
  }// end of the loop numstep

  return 0;
}


//-------------------------- STRPTR ---------------------------
//: This will find a starting point on the 'g' function circle.
// The new point to start tracing is stored in the x array.
static void strptr(vcl_vector<unsigned int>& icount,
                   vcl_vector<unsigned int> const& ideg,
                   vcl_vector<vnl_rnpoly_solve_cmplx> const& r,
                   vcl_vector<vnl_rnpoly_solve_cmplx>& x)
{
  assert(ideg.size()==dim_);
  assert(r.size()==dim_);
  x.resize(dim_);

  for (unsigned int i=0; i<dim_; ++i)
    if (icount[i] >= ideg[i]) icount[i] = 1;
    else                    { icount[i]++; break; }

  for (unsigned int j=0; j<dim_; ++j)
  {
    double angle = twopi / ideg[j] * icount[j];
    x[j] = r[j] * vnl_rnpoly_solve_cmplx (vcl_cos(angle), vcl_sin(angle));
  }
}


static vcl_vector<vcl_vector<vnl_rnpoly_solve_cmplx> >
Perform_Distributed_Task(vcl_vector<unsigned int> const& ideg,
                         vcl_vector<unsigned int> const& terms,
                         vcl_vector<int> const& polyn,
                         vcl_vector<double> const& coeff)
{
  assert(ideg.size()==dim_);

  vcl_vector<vcl_vector<vnl_rnpoly_solve_cmplx> > sols;
  vcl_vector<vnl_rnpoly_solve_cmplx> pdg, qdg, p, q, r, x;
  vcl_vector<unsigned int> icount(dim_,1); icount[0]=0;
  bool solflag; // flag used to remember if a root is found
#ifdef DEBUG
  char const* FILENAM = "/tmp/cont.results";
  vcl_ofstream F(FILENAM);
  if (!F)
  {
    vcl_cerr<<"could not open "<<FILENAM<<" for writing\nplease erase old file first\n";
    F = vcl_cerr;
  }
  else
    vcl_cerr << "Writing to " << FILENAM << '\n';
#endif
  // Initialize some variables
  inptbr(p,q);
  initr(ideg,p,q,r,pdg,qdg);

  // int Psize = 2*dim_*sizeof(double);
  int totdegree = 1;            // Total degree of the system
  for (unsigned int j=0;j<dim_;j++)  totdegree *= ideg[j];

  // *************  Send initial information ****************
  //Initialize(dim_,maxns,maxdt,maxit,maxroot,
  //           terms,ideg,pdg,qdg,coeff,polyn);
  while ((totdegree--) > 0)
  {
    // Compute path to trace
    strptr(icount,ideg,r,x);

    // Tell the client which path you want it to trace
    solflag = 1 == trace(x,ideg,pdg,qdg,polyn,coeff,terms);
    // Save the solution for future reference
    if (solflag)
    {
#ifdef DEBUG
      for (unsigned int i=0; i<dim_; ++i)
        F << '<' << x[dim_-i-1].R << ' ' << x[dim_-i-1].C << '>';
      F << vcl_endl;
#endif
      sols.push_back(x);
    }
#ifdef DEBUG
    // print something out for each root
    if (solflag) vcl_cout << '.';
    else         vcl_cout << '*';
    vcl_cout.flush();
#endif
  }

#ifdef DEBUG
  vcl_cout<< vcl_endl;
#endif

  return sols;
}


//----------------------- READ INPUT ----------------------
//: This will read the input polynomials from a data file.
void vnl_rnpoly_solve::Read_Input(vcl_vector<unsigned int>& ideg,
                                  vcl_vector<unsigned int>& terms,
                                  vcl_vector<int>& polyn,
                                  vcl_vector<double>& coeff)
{
  // Read the number of equations
  dim_ = ps_.size();

  ideg.resize(dim_); terms.resize(dim_);
  // Start reading in the array values
  max_deg_=0;
  max_nterms_=0;
  for (unsigned int i=0;i<dim_;i++)
  {
    ideg[i] = ps_[i]->ideg_;
    terms[i] = ps_[i]->nterms_;
    if (ideg[i] > max_deg_)
      max_deg_ = ideg[i];
    if (terms[i] > max_nterms_)
      max_nterms_ = terms[i];
  }
  coeff.resize(dim_*max_nterms_);
  polyn.resize(dim_*max_nterms_*dim_);
  for (unsigned int i=0;i<dim_;i++)
  {
    for (unsigned int k=0;k<terms[i];k++)
    {
      coeff[i*max_nterms_+k] = ps_[i]->coeffs_(k);
      for (unsigned int j=0;j<dim_;j++)
      {
        int deg = ps_[i]->polyn_(k,j);
        polyn[i*dim_*max_nterms_+k*dim_+j] = deg ? int(j*max_deg_)+deg-1 : -1;
      }
    }
  }
}


vnl_rnpoly_solve::~vnl_rnpoly_solve()
{
  while (r_.size() > 0) { delete r_.back(); r_.pop_back(); }
  while (i_.size() > 0) { delete i_.back(); i_.pop_back(); }
}

bool vnl_rnpoly_solve::compute()
{
  vcl_vector<unsigned int> ideg, terms;
  vcl_vector<int> polyn;
  vcl_vector<double> coeff;

  Read_Input(ideg,terms,polyn,coeff); // returns number of equations
  assert(ideg.size()==dim_);
  assert(terms.size()==dim_);
  assert(polyn.size()==dim_*max_nterms_*dim_);
  assert(coeff.size()==dim_*max_nterms_);

  int totdegree = 1;
  for (unsigned int j=0; j<dim_; ++j) totdegree *= ideg[j];

  vcl_vector<vcl_vector<vnl_rnpoly_solve_cmplx> > ans = Perform_Distributed_Task(ideg,terms,polyn,coeff);

  // Print out the answers
  vnl_vector<double> * rp, *ip;
#ifdef DEBUG
  vcl_cout << "Total degree: " << totdegree << vcl_endl
           << "# solutions : " << ans.size() << vcl_endl;
#endif
  for (unsigned int i=0; i<ans.size(); ++i)
  {
    assert(ans[i].size()==dim_);
    rp=new vnl_vector<double>(dim_); r_.push_back(rp);
    ip=new vnl_vector<double>(dim_); i_.push_back(ip);
    for (unsigned int j=0; j<dim_; ++j)
    {
#ifdef DEBUG
      vcl_cout << ans[i][j].R << " + j " << ans[i][j].C << vcl_endl;
#endif
      (*rp)[j]=ans[i][j].R; (*ip)[j]=ans[i][j].C;
    }
#ifdef DEBUG
    vcl_cout<< vcl_endl;
#endif
  }
  return true;
}
