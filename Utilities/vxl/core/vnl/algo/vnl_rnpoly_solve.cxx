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
#include <vcl_cstdio.h>
#include <vcl_iostream.h>
#endif

// fsm: moved ::M_ and ::T_ into the namespace of vnl_rnpoly_solve, as they
// were causing multiply defined symbols for static builds. if your compiler cannot
// cope with the next two lines, replace them with #defines.
static const unsigned int M_ = vnl_rnpoly_solve::M_;
static const unsigned int T_ = vnl_rnpoly_solve::T_;

static const unsigned int P_ = M_-1;  // Maximum power for any variable in a term
static const unsigned int LEN_= 3080; // Maximum number of roots

//: This is a local implementation of a "complex number" class, for internal use only
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
            { double N=1.0/Y.norm();
              return vnl_rnpoly_solve_cmplx((R*Y.R+C*Y.C)*N, (C*Y.R-R*Y.C)*N); }
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
static void inptbr(int n, vnl_rnpoly_solve_cmplx p[M_], vnl_rnpoly_solve_cmplx q[M_])
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

  for (int j=n-1;j>=0;--j) { int jj=j%10; p[j]=pp[jj]; q[j]=qq[jj]; }
}

//-----------------------------  POWR  -----------------------
//: This returns the complex number y raised to the nth degree
static inline vnl_rnpoly_solve_cmplx powr(int n,vnl_rnpoly_solve_cmplx const& y)
{
    vnl_rnpoly_solve_cmplx x (1,0);
    if (n>0) while (n--) x *= y;
    else     while (n++) x /= y;
    return x;
}


static void initr(int n,int ideg[M_], vnl_rnpoly_solve_cmplx p[M_], vnl_rnpoly_solve_cmplx q[M_],
                  vnl_rnpoly_solve_cmplx r[M_], vnl_rnpoly_solve_cmplx pdg[M_], vnl_rnpoly_solve_cmplx qdg[M_])
{
  for (int j=0;j<n;j++)
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
  return (index<0) ? 0 : (index % P_) + 1;
}


//-------------------------- FFUNR -------------------------
//: Evaluate the target system component of h.
//  This is the system of equations that we are trying to find the roots.
static void ffunr(double coeff[M_][T_], int polyn[M_][T_][M_], int n,
                  int terms[M_], vnl_rnpoly_solve_cmplx x[M_], vnl_rnpoly_solve_cmplx pows[M_*P_],
                  int max_deg, vnl_rnpoly_solve_cmplx f[M_], vnl_rnpoly_solve_cmplx df[M_][M_])
{
  // Compute all possible powers for each variable
  for (int i=0;i<n;i++) // for all variables
  {
    int index = P_*i;
    pows[index]=x[i];
    for (int j=1;j<max_deg;++j,++index) // for all powers
      pows[index+1]= pows[index] * x[i];
  }

  // Initialize the new arrays
  for (int i=0;i<n;i++)
  {
    f[i]=vnl_rnpoly_solve_cmplx(0,0);
    for (int j=0;j<n;j++)
      df[i][j]=vnl_rnpoly_solve_cmplx(0,0);
  }

  for (int i=n-1;i>=0;i--) // Across equations
    for (int j=terms[i]-1;j>=0;j--) // Across terms
    {
      vnl_rnpoly_solve_cmplx tmp (1,0);
      for (int k=n-1;k>=0;k--) // For each variable
      {
        int index=polyn[i][j][k];
        if (index>=0)
          tmp *= pows[index];
      }
      f[i] += tmp * coeff[i][j];
    }

  // Compute the Derivative!
  for (int i=n-1;i>=0;i--) // Over equations
    for (int l=n-1;l>=0;l--) // With respect to each variable
    {
      vnl_rnpoly_solve_cmplx& df_il = df[i][l];
      for (int j=terms[i]-1;j>=0;j--) // Over terms in each equation
        if (polyn[i][j][l]>=0)        // if 0 deg in l, df term is 0
        {
          vnl_rnpoly_solve_cmplx tmp = vnl_rnpoly_solve_cmplx(1,0);
          for (int k=n-1;k>=0;k--)        // Over each variable in each term
          {
            int index=polyn[i][j][k];
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
          df_il += tmp * coeff[i][j];
        }
    } // end for l
}


//--------------------------- GFUNR --------------------------
//: Evaluate starting system component
// Evaluate the starting system component of h from a system
// of equations that we already know the roots. (ex: x^n - 1)
static void gfunr(int len, int ideg[M_], vnl_rnpoly_solve_cmplx pdg[M_], vnl_rnpoly_solve_cmplx qdg[M_],
                  vnl_rnpoly_solve_cmplx /*x*/ [M_], vnl_rnpoly_solve_cmplx pows[M_*P_],
                  vnl_rnpoly_solve_cmplx g[M_], vnl_rnpoly_solve_cmplx dg[M_])
{
  vnl_rnpoly_solve_cmplx pxdgm1[M_], pxdg[M_];

  for (int j=0;j<len;j++)
  {
    vnl_rnpoly_solve_cmplx tmp;
    if (ideg[j] == 1)
      tmp = vnl_rnpoly_solve_cmplx(1,0);
    else
      tmp = pows[j*P_+(ideg[j]-2)];

    pxdgm1[j] = pdg[j] * tmp;
  }

  for (int j=0;j<len;j++)
  {
    int index = j*P_+(ideg[j]-1);
    pxdg[j] = pdg[j] * pows[index];
  }

  for (int j=len-1;j>=0;j--)
  {
    g[j]  = pxdg[j] - qdg[j];
    dg[j] = pxdgm1[j] * ideg[j];
  }
}


//-------------------------- HFUNR --------------------------
//: This is the routine that traces the curve from the gfunr to the f function
//  (i.e. Evaluate the continuation function)
static void hfunr(int len,int ideg[M_], vnl_rnpoly_solve_cmplx pdg[M_], vnl_rnpoly_solve_cmplx qdg[M_],
                  double t, vnl_rnpoly_solve_cmplx x[M_], vnl_rnpoly_solve_cmplx h[M_], vnl_rnpoly_solve_cmplx dhx[M_][M_],
                  vnl_rnpoly_solve_cmplx dht[M_], int polyn[M_][T_][M_], double coeff[M_][T_],
                  int terms[M_],int max_deg)
{
  vnl_rnpoly_solve_cmplx df[M_][M_],dg[M_],f[M_],g[M_];
  vnl_rnpoly_solve_cmplx pows[M_*P_];  //  powers of variables [M_ equations] [P_ possible powers]

  ffunr(coeff,polyn,len,terms,x,pows,max_deg,f,df);
  gfunr(len,ideg,pdg,qdg,x,pows,g,dg);

  double onemt=1.0 - t;
  for (int j=0;j<len;j++)
  {
    vnl_rnpoly_solve_cmplx *end_ptr = &dhx[j][len], *df_jk_ptr= &df[j][0];
    for (vnl_rnpoly_solve_cmplx* dhx_ptr = &dhx[j][0]; dhx_ptr< end_ptr;++dhx_ptr,++df_jk_ptr)
      (*dhx_ptr) = (*df_jk_ptr) * t;

    dhx[j][j] += dg[j]*onemt;
    dht[j] = f[j] - g[j];
    h[j] = f[j] * t + g[j] * onemt;
  }
}


//------------------------ LU DECOMPOSITION --------------------------
//: This performs LU decomposition on a matrix.
static int ludcmp(vnl_rnpoly_solve_cmplx a[M_][M_], int n, int indx[M_])
{
  double vv[M_];

  // Loop over rows to get the implicit scaling information
  for (int i=0;i<n;i++)
  {
    double big = 0.0;
    vnl_rnpoly_solve_cmplx *endptr = &a[i][0] + n;
    for (vnl_rnpoly_solve_cmplx *aptr=&a[i][0]; aptr<endptr; ++aptr)
    {
      double temp = aptr->norm();
      if (temp > big) big = temp;
    }
    if (big == 0.0) return 1;
    vv[i]=1.0/vcl_sqrt(big);
  }

  // This is the loop over columns of Crout's method
  for (int j=0;j<n;++j)
  {
    vnl_rnpoly_solve_cmplx *a_ij_ptr = &a[0][j];

    for (int i=0;i<j;++i,a_ij_ptr+=M_)
    {
      vnl_rnpoly_solve_cmplx *a_ik_ptr = &a[i][0];
      vnl_rnpoly_solve_cmplx *a_kj_ptr = &a[0][j];
      for (int k=0;k<i;++k,++a_ik_ptr,a_kj_ptr+=M_)
        (*a_ij_ptr) -= (*a_ik_ptr) * (*a_kj_ptr);
    }

    // Initialize for the search for largest pivot element
    double big = 0.0;
    int imax = 0;
    a_ij_ptr= &a[j][j];

    for (int i=j;i<n;++i,a_ij_ptr+=M_)
    {
      vnl_rnpoly_solve_cmplx *a_ik_ptr = &a[i][0];
      vnl_rnpoly_solve_cmplx *a_kj_ptr = &a[0][j];
      for (int k=0;k<j;++k,++a_ik_ptr,a_kj_ptr+=M_)
        (*a_ij_ptr) -= (*a_ik_ptr) * (*a_kj_ptr);

      // Is the figure of merit for the pivot better than the best so far?
      double rdum = vv[i]*a_ij_ptr->norm();
      if (rdum >= big) { big = rdum; imax = i; }
    }

    // Do we need to interchange rows?
    if (j != imax)
    {
      // Yes, do so...
      vnl_rnpoly_solve_cmplx *endptr = &a[imax][0] + n;
      vnl_rnpoly_solve_cmplx *a_jk_ptr = &a[j][0];
      for (vnl_rnpoly_solve_cmplx *aptr=&a[imax][0];aptr<endptr;++aptr,++a_jk_ptr)
      {
        vnl_rnpoly_solve_cmplx dum = *aptr; *aptr=*a_jk_ptr; *a_jk_ptr = dum;
      }

      // Also interchange the scale factor
      vv[imax]=vv[j]; }
    indx[j]=imax;

    vnl_rnpoly_solve_cmplx* aptr = &a[j][j];
    if (aptr->norm() == 0.0)
      *aptr = epsilonZ;

    // Now, finally, divide by the pivot element
    if (j != (n-1))
    {
      vnl_rnpoly_solve_cmplx dum = vnl_rnpoly_solve_cmplx(1,0) / (*aptr);

      // If the pivot element is zero the matrix is singular.
      a_ij_ptr=&a[j+1][j];
      for (int i=j+1;i<n;++i,a_ij_ptr+=M_)
        (*a_ij_ptr) = (*a_ij_ptr) * dum;
    }
  }
  return 0;
}


// ------------------------- LU Back Substitution -------------------------
static void lubksb(vnl_rnpoly_solve_cmplx a[M_][M_], int n, int indx[M_],
                   vnl_rnpoly_solve_cmplx bb[M_], vnl_rnpoly_solve_cmplx b[M_])
{
  int ii=-1;
  vnl_rnpoly_solve_cmplx *bbptr = &bb[0];
  vnl_rnpoly_solve_cmplx *endptr = &b[0] + n;
  for (vnl_rnpoly_solve_cmplx* bptr= &b[0]; bptr < endptr; bptr++,++bbptr)
    *bptr= *bbptr;

  for (int i=0;i<n;i++)
  {
    int ip = indx[i];
    vnl_rnpoly_solve_cmplx sum = b[ip];
    b[ip] = b[i];

    if (ii>=0)
      for (int j=ii;j<i;++j)
        sum -= a[i][j] * b[j];
    else
      // A nonzero element was encountered, so from now on we
      // will have to do the sums in the loop above
      if (sum.norm() > 0) ii = i;

    b[i] = sum;
  }

  // Now do the backsubstitution
  for (int i=n-1;i>=0;i--)
  {
    vnl_rnpoly_solve_cmplx *endptr = &b[n];
    vnl_rnpoly_solve_cmplx *a_ij_ptr = &a[i][i+1];
    for (vnl_rnpoly_solve_cmplx *bptr=&b[i+1]; bptr<endptr; ++bptr,++a_ij_ptr)
      b[i] -= (*a_ij_ptr) * (*bptr);

    b[i] = b[i] / a[i][i];
  }
}


//-------------------------- LINNR -------------------
//: Solve a complex system of equations by using l-u decomposition and then back substitution.
static int linnr(int len,vnl_rnpoly_solve_cmplx dhx[M_][M_],
                 vnl_rnpoly_solve_cmplx rhs[M_],
                 vnl_rnpoly_solve_cmplx resid[M_])
{
  int irow[M_];
  if (ludcmp(dhx,len,irow)==1) return 1;
  lubksb(dhx,len,irow,rhs,resid);
  return 0;
}


//-----------------------  XNORM  --------------------
//: Finds the unit normal of a vector v
static double xnorm(int n, vnl_rnpoly_solve_cmplx v[])
{
  double txnorm=0.0;
  for (int j=n-1;j>=0; --j)
    txnorm += vcl_fabs(v[j].R) + vcl_fabs(v[j].C);
  return txnorm;
}

//---------------------- PREDICT ---------------------
//: Predict new x vector using Taylor's Expansion.
static void predict(int len, int ideg[M_], vnl_rnpoly_solve_cmplx pdg[M_], vnl_rnpoly_solve_cmplx qdg[M_],
                    double step, double& t, vnl_rnpoly_solve_cmplx x[M_], int polyn[M_][T_][M_],
                    double coeff[M_][T_], int terms[M_], int max_deg)
{
  double maxdt =.2; // Maximum change in t for a given step.  If dt is
                    // too large, there seems to be greater chance of
                    // jumping to another path.  Set this to 1 if you
                    // don't care.
  vnl_rnpoly_solve_cmplx dht[M_],dhx[M_][M_],dz[M_],h[M_],rhs[M_];
  // Call the continuation function that we are tracing
  hfunr(len,ideg,pdg,qdg,t,x,h,dhx,dht,polyn,coeff,terms,max_deg);

  for (int j=len-1;j>=0;j--)
    rhs[j] = - dht[j];

  // Call the function that solves a complex system of equations
  if (linnr(len,dhx,rhs,dz) == 1) return;

  // Find the unit normal of a vector and normalize our step
  double factor = step/(1+xnorm(len,dz));
  if (factor>maxdt) factor = maxdt;

  bool tis1=true;
  if (t+factor>1) { tis1 = false; factor = 1.0 - t; }

  // Update this path with the predicted next point
  for (int j=len-1;j>=0;j--)
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
// 3: If the magnitude of X > maxroot
static int correct(int len,int ideg[M_], int loop, double eps,
                   vnl_rnpoly_solve_cmplx pdg[M_], vnl_rnpoly_solve_cmplx qdg[M_], double t,
                   vnl_rnpoly_solve_cmplx x[M_], int polyn[M_][T_][M_], double coeff[M_][T_],
                   int terms[M_], int max_deg)
{
  double maxroot= 1000;// Maximum size of root where it is considered heading to infinity
  vnl_rnpoly_solve_cmplx dhx[M_][M_],dht[M_], h[M_],resid[M_];

  for (int i=0;i<loop;i++)
  {
    hfunr(len,ideg,pdg,qdg,t,x,h,dhx,dht,polyn,coeff,terms,max_deg);

    // If linnr = 1, error
    if (linnr(len,dhx,h,resid)==1) return 1;

    for (int j=len-1;j>=0;j--)
      x[j] -= resid[j];

    double xresid = xnorm(len,resid);
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
static int trace (int len, vnl_rnpoly_solve_cmplx x[M_], int ideg[M_],
                  vnl_rnpoly_solve_cmplx pdg[M_], vnl_rnpoly_solve_cmplx qdg[M_],
                  int polyn[M_][T_][M_], double coeff[M_][T_],
                  int terms[M_], int max_deg)
{
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
  vnl_rnpoly_solve_cmplx oldx[M_];  // the previous path value
  int nadv=0;

  // Remember the original point
  for (int j=len-1;j>=0;j--)
    oldx[j] = x[j];

  for (int numstep=0;numstep<maxns;numstep++)
  {
    // Taylor approximate the next point
    predict(len,ideg,pdg,qdg,step,t,x,polyn,coeff,terms,max_deg);

    //if (t>1.0) t=1.0;

    if (t > .95)
    {
      if (eps != epsilonS) step = step/4.0;
      eps = epsilonS;
    }else
      eps = epsilonB;
#ifdef DEBUG
    vcl_printf ("t=%.15f\n",t); fflush(stdout);
#endif

    if (t>=.99999)                      // Path converged
    {
#ifdef DEBUG
      vcl_printf ("path converged\n");
#endif
      double factor = (1.0-oldt)/(t-oldt);
      for (int j=len-1;j>=0;j--)
        x[j] = oldx[j] + (x[j]-oldx[j]) * factor;
      t = 1.0;
      int cflag=correct(len,ideg,10*maxit,final_eps,pdg,qdg,t,x, polyn, coeff,terms, max_deg);
      if ((cflag==0) ||(cflag==2))
        return 1;       // Final Correction converged
      else if (cflag==3)
        return 3;       // Heading to infinity
      else return 4;    // Singular solution
    }

    // Newton's method brings us back to the curve
    int cflag=correct(len,ideg,maxit,eps,pdg,qdg,t,x,polyn, coeff,terms,max_deg);
    if (cflag==0)
    {
      // Successful step
      if ((++nadv)==maxit) { step *= 2; nadv=0; }   // Increase the step size
      // Make note of our new location
      oldt=t;
      for (int j=len-1;j>=0;j--)
        oldx[j] = x[j];
    }
    else
    {
      nadv=0;
      step /= 2.0;

      if (cflag==3) return 3;           // Path heading to infinity
      if (step<stepmin) return 2;       // Path failed StepSizeMin exceeded

      // Reset the values since we stepped to far, and try again
      t = oldt;
      for (int j=len-1;j>=0;j--)
        x[j] = oldx[j];
    }
  }// end of the loop numstep

  return 0;
}


//-------------------------- STRPTR ---------------------------
//: This will find a starting point on the 'g' function circle.
// The new point to start tracing is stored in the x array.
static void strptr(int n,int icount[M_],int ideg[M_], vnl_rnpoly_solve_cmplx r[M_],vnl_rnpoly_solve_cmplx x[M_])
{
  for (int i=0;i<n;i++)
    if (icount[i] >= ideg[i]) icount[i] = 1;
    else                    { icount[i]++; break; }

  for (int j=0;j<n;j++)
  {
    double angle = twopi / ideg[j] * icount[j];
    x[j] = r[j] * vnl_rnpoly_solve_cmplx (vcl_cos(angle), vcl_sin(angle));
  }
}


static int Perform_Distributed_Task(int points,vnl_rnpoly_solve_cmplx sols[LEN_][M_],
                                    int ideg[M_],int terms[M_],
                                    int polyn[M_][T_][M_],double coeff[M_][T_])
{
  vnl_rnpoly_solve_cmplx p[M_], q[M_], r[M_], pdg[M_], qdg[M_], x[M_];
  int icount[M_];
  int NumSols=0;
  bool solflag; // flag used to remember if a root is found
  int max_deg=P_;
#ifdef DEBUG
  char const* FILENAM = "/tmp/cont.results";
  FILE *F = vcl_fopen(FILENAM,"w");
  if (!F)
  {
    vcl_cerr<<"could not open "<<FILENAM<<"\nplease erase old file first\n";
    F = stderr;
  }
  else
    vcl_fprintf(stderr, "Writing to %s\n", FILENAM);
#endif
  // Initialize some variables
  inptbr(points,p,q);
  initr(points,ideg,p,q,r,pdg,qdg);

  // int Psize = 2*points*sizeof(double);
  int totdegree = 1;            // Total degree of the system
  for (int j=0;j<points;j++)  totdegree *= ideg[j];
  icount[0]=0;
  for (int j=points-1;j>0;j--) icount[j]=1;
  for (int i=0;i<points;i++)
    if (ideg[i] > max_deg)
      max_deg = ideg[i];

  // *************  Send initial information ****************
  //Initialize(points,maxns,maxdt,maxit,maxroot,
  //           terms,ideg,pdg,qdg,coeff,polyn);
  while ((totdegree--) > 0)
  {
    // Compute path to trace
    strptr(points,icount,ideg,r,x);

    // Tell the client which path you want it to trace
    solflag = 1 == trace (points,x,ideg,pdg,qdg,polyn,coeff,terms,max_deg);
    // Save the solution for future reference
    if (solflag)
    {
      for (int i=points-1;i>=0;i--)
      {
        sols[NumSols][i] = x[i];
#ifdef DEBUG
        vcl_fprintf(F,"<%f  %f>",x[points-i-1].R,x[points-i-1].C);
#endif
      }
      ++NumSols;
#ifdef DEBUG
      vcl_fprintf(F,"\n");
      vcl_fflush(F);
#endif
    }
#ifdef DEBUG
    // print something out for each root
    if (solflag) vcl_cout << '.';
    else         vcl_cout << '*';
    vcl_cout.flush();
#endif
  }

#ifdef DEBUG
  if (F != stderr) vcl_fclose(F);
  vcl_cout<< vcl_endl;
#endif

  return NumSols;
}


//----------------------- READ INPUT ----------------------
//: This will read the input polynomials from a data file.
int vnl_rnpoly_solve::Read_Input(int ideg[M_], int terms[M_],
                                 int polyn[M_][T_][M_], double coeff[M_][T_])
{
  // Read the number of equations
  unsigned int n = ps_.size();

  // Initialize the array's to zero
  for (unsigned int i=0;i<n;i++)
    for (unsigned int k=0;k<T_;k++)
      coeff[i][k] = 0.0;
  for (unsigned int i=0;i<n;i++)
    ideg[i] = terms[i] = 0;
  for (unsigned int i=0;i<n;i++)
    for (unsigned int k=0;k<T_;k++)
      for (unsigned int j=0;j<n;j++)
        polyn[i][k][j]=0;
  // Start reading in the array values
  for (unsigned int i=0;i<n;i++)
  {
    ideg[i]  = ps_[i]->ideg_;
    terms[i] = ps_[i]->nterms_;

    for (int k=0;k<terms[i];k++)
    {
      coeff[i][k] = ps_[i]->coeffs_(k);
      for (unsigned int j=0;j<n;j++)
      {
        int deg = ps_[i]->polyn_(k,j);
        if (deg) polyn[i][k][j] = (j*P_)+(deg-1);
        else     polyn[i][k][j] = -1;
      }
    }
  }
  return n;
}


vnl_rnpoly_solve::~vnl_rnpoly_solve()
{
  while (r_.size() > 0) { delete r_.back(); r_.pop_back(); }
  while (i_.size() > 0) { delete i_.back(); i_.pop_back(); }
}

bool vnl_rnpoly_solve::compute()
{
  int ideg[M_], terms[M_], polyn[M_][T_][M_];
  double coeff[M_][T_];

  int p = Read_Input(ideg,terms,polyn,coeff);

  unsigned int totdegree = 1;
  for (int j=0;j<p;++j) totdegree *= ideg[j];
  assert(totdegree < LEN_);

  vnl_rnpoly_solve_cmplx ans[LEN_][M_];
  int NumSols = Perform_Distributed_Task(p,ans,ideg,terms,polyn,coeff);

  // Print out the answers
  vnl_vector<double> * rp, *ip;
#ifdef DEBUG
  vcl_cout << "Total degree: " << totdegree << vcl_endl
           << "# solutions : " << NumSols << vcl_endl;
#endif
  for (int i=0;i<NumSols;i++)
  {
    rp=new vnl_vector<double>(p); r_.push_back(rp);
    ip=new vnl_vector<double>(p); i_.push_back(ip);
    for (int j=0;j<p;j++)
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
