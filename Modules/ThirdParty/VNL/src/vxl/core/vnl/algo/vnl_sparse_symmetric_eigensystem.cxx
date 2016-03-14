// This is core/vnl/algo/vnl_sparse_symmetric_eigensystem.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file

#include <cstring>
#include <cstdlib>
#include <iostream>
#include <vector>
#include "vnl_sparse_symmetric_eigensystem.h"
#include "vnl_sparse_lu.h"
#include <vnl/vnl_vector_ref.h>
#include <vcl_cassert.h>
#include <vcl_compiler.h>

#include <vnl/algo/vnl_netlib.h> // dnlaso_() dseupd_() dsaupd_()

static vnl_sparse_symmetric_eigensystem * current_system = VXL_NULLPTR;

//------------------------------------------------------------
//: Callback for multiplying our matrix by a number of vectors.
//  The input is p, which is an NxM matrix.
//  This function returns q = A p, where A is the current sparse matrix.
static
void sse_op_callback(const long* n,
                     const long* m,
                     const double* p,
                     double* q)
{
  assert(current_system != 0);

  current_system->CalculateProduct(*n,*m,p,q);
}

//------------------------------------------------------------
//: Callback for saving the Lanczos vectors as required by dnlaso.
// If k=0, save the m columns of q as the (j-m+1)th through jth
// vectors.  If k=1 then return the (j-m+1)th through jth vectors in
// q.
static
void sse_iovect_callback(const long* n,
                         const long* m,
                         double* q,
                         const long* j,
                         const long* k)
{
  assert(current_system != 0);

  if (*k==0)
    current_system->SaveVectors(*n,*m,q,*j-*m);
  else if (*k==1)
    current_system->RestoreVectors(*n,*m,q,*j-*m);
}

vnl_sparse_symmetric_eigensystem::vnl_sparse_symmetric_eigensystem()
  : nvalues(0), vectors(VXL_NULLPTR), values(VXL_NULLPTR)
{
}

vnl_sparse_symmetric_eigensystem::~vnl_sparse_symmetric_eigensystem()
{
  delete[] vectors; vectors = VXL_NULLPTR;
  delete[] values; values = VXL_NULLPTR;
  for (unsigned i=0; i<temp_store.size(); ++i)
    delete temp_store[i];
  temp_store.clear();
}

//------------------------------------------------------------
//: Here is where the fortran converted code gets called.
// The sparse matrix M is assumed to be symmetric.  The n smallest
// eigenvalues and their corresponding eigenvectors are calculated if
// smallest is true (the default).  Otherwise the n largest eigenpairs
// are found.  The accuracy of the eigenvalues is to nfigures decimal
// digits.  Returns 0 if successful, non-zero otherwise.
int vnl_sparse_symmetric_eigensystem::CalculateNPairs(vnl_sparse_matrix<double>& M,
                                                      int n,
                                                      bool smallest,
                                                      long nfigures)
{
  mat = &M;

  // Clear current vectors.
  if (vectors) {
    delete[] vectors; vectors = VXL_NULLPTR;
    delete[] values; values = VXL_NULLPTR;
  }
  nvalues = 0;

  current_system = this;

  long dim = mat->columns();
  long nvals = (smallest)?-n:n;
  long nperm = 0;
  long nmval = n;
  long nmvec = dim;
  std::vector<double> temp_vals(n*4);
  std::vector<double> temp_vecs(n*dim);

  // set nblock = std::max(10, dim/6) :
  long nblock = (dim<60) ? dim/6 : 10;

  // isn't this rather a lot ? -- fsm
  long maxop = dim*10;      // dim*20;

  // set maxj = std::max(40, maxop*nblock, 6*nblock+1) :
  long maxj = maxop*nblock; // 2*n+1;
  long t1 = 6*nblock+1;
  if (maxj < t1) maxj = t1;
  if (maxj < 40) maxj = 40;

  // Calculate size of workspace needed.  These expressions come from
  // the LASO documentation.
  int work_size = dim*nblock;
  int t2 = maxj*(2*nblock+3) + 2*n + 6 + (2*nblock+2)*(nblock+1);
  if (work_size < t2) work_size = t2;
  work_size += 2*dim*nblock + maxj*(nblock + n + 2) + 2*nblock*nblock + 3*n;
  std::vector<double> work(work_size+10);

  // Set starting vectors to zero.
  for (int i=0; i<dim*nblock; ++i)
    work[i] = 0.0;

  std::vector<long> ind(n);

  long ierr = 0;

  v3p_netlib_dnlaso_(sse_op_callback, sse_iovect_callback,
                     &dim, &nvals, &nfigures, &nperm,
                     &nmval, &temp_vals[0],
                     &nmvec, &temp_vecs[0],
                     &nblock,
                     &maxop,
                     &maxj,
                     &work[0],
                     &ind[0],
                     &ierr);
  if (ierr > 0)
  {
    if (ierr & 0x1)
      std::cerr << "Error: vnl_sparse_symmetric_eigensystem: N < 6*NBLOCK\n";
    if (ierr & 0x2)
      std::cerr << "Error: vnl_sparse_symmetric_eigensystem: NFIG < 0\n";
    if (ierr & 0x4)
      std::cerr << "Error: vnl_sparse_symmetric_eigensystem: NMVEC < N\n";
    if (ierr & 0x8)
      std::cerr << "Error: vnl_sparse_symmetric_eigensystem: NPERM < 0\n";
    if (ierr & 0x10)
      std::cerr << "Error: vnl_sparse_symmetric_eigensystem: MAXJ < 6*NBLOCK\n";
    if (ierr & 0x20)
      std::cerr << "Error: vnl_sparse_symmetric_eigensystem: NVAL < max(1,NPERM)\n";
    if (ierr & 0x40)
      std::cerr << "Error: vnl_sparse_symmetric_eigensystem: NVAL > NMVAL\n";
    if (ierr & 0x80)
      std::cerr << "Error: vnl_sparse_symmetric_eigensystem: NVAL > MAXOP\n";
    if (ierr & 0x100)
      std::cerr << "Error: vnl_sparse_symmetric_eigensystem: NVAL > MAXJ/2\n";
    if (ierr & 0x200)
      std::cerr << "Error: vnl_sparse_symmetric_eigensystem: NBLOCK < 1\n";
  }
  else if (ierr < 0)
  {
    if (ierr == -1)
      std::cerr << "Error: vnl_sparse_symmetric_eigensystem:\n"
               << "  poor initial vectors chosen\n";
    else if (ierr == -2)
      std::cerr << "Error: vnl_sparse_symmetric_eigensystem:\n"
               << "  reached maximum operations " << maxop
               << " without finding all eigenvalues,\n"
               << "  found " << nperm << " eigenvalues\n";
    else if (ierr == -8)
      std::cerr << "Error: vnl_sparse_symmetric_eigensystem:\n"
               << "  disastrous loss of orthogonality - internal error\n";
  }

  // Copy the eigenvalues and vectors.
  nvalues = n;
  vectors = new vnl_vector<double>[n];
  values = new double[n];
  for (int i=0; i<n; ++i) {
    values[i] = temp_vals[i];
    vnl_vector<double> vec(dim,0.0);
    for (int j=0; j<dim; ++j)
      vec[j] = temp_vecs[j + dim*i];
    vectors[i] = vec;
  }

  // Delete temporary space.
  for (unsigned i=0; i<temp_store.size(); ++i)
    delete [] temp_store[i];
  temp_store.clear();

  return ierr;
}


//------------------------------------------------------------
//: Here is where the fortran converted code gets called.
// The sparse matrix A is assumed to be symmetric.
// Find n eigenvalue/eigenvectors of the eigenproblem A * x = lambda * B * x.
// !smallest and !magnitude - compute the N largest (algebraic) eigenvalues
//  smallest and !magnitude - compute the N smallest (algebraic) eigenvalues
// !smallest and  magnitude - compute the N largest (magnitude) eigenvalues
//  smallest and  magnitude - compute the nev smallest (magnitude) eigenvalues
// set sigma for shift/invert mode
int vnl_sparse_symmetric_eigensystem::CalculateNPairs(
                      vnl_sparse_matrix<double>& A, vnl_sparse_matrix<double>& B, int nEV,
                      double tolerance, int numberLanczosVecs,
                      bool smallest, bool magnitude,
                      int maxIterations,
                      double sigma)
{
  mat = &A;
  Bmat = &B;

  // Clear current vectors.
  if (vectors) {
    delete[] vectors; vectors = VXL_NULLPTR;
    delete[] values; values = VXL_NULLPTR;
  }
  nvalues = 0;

  const long whichLength = 2;
  char which[whichLength + 1];
  which[whichLength] = '\0';
  if (smallest)
    which[0] = 'S';
  else
    which[0] = 'L';

  if (magnitude)
    which[1] = 'M';
  else
    which[1] = 'A';

  long  matSize = mat->columns();          // Dimension of the eigenproblem.
  long  ido = 0;        // ido == 0 means initialization

  long  nconv = 0L;     // Number of "converged" Ritz values.
  long  numberLanczosVecsL = numberLanczosVecs;    // number of vectors to calc
  long  nEVL = nEV;    // long number of EVs to calc

  double *resid = new double[matSize];
  std::memset((void*) resid, 0, sizeof(double)*matSize);

  if (maxIterations <= 0)
    maxIterations =  nEVL * 100;

  if (numberLanczosVecsL <= 0)
    numberLanczosVecsL = 2 * nEVL + 1;
  numberLanczosVecsL = (numberLanczosVecsL > matSize ? matSize : numberLanczosVecsL);
  double *V = new double[matSize * numberLanczosVecsL + 1];

#define DONE 99
  const int genEigProblemLength = 1;
  char genEigProblem = 'G';
  long    info = 0;   // Initialization info (INPUT) and error flag (OUTPUT)

#define IPARAMSIZE 12
  long iParam[IPARAMSIZE];
  // for the sake of consistency with parameter indices in FORTRAN,
  // start at index 1...
  iParam[0] = 0;
  iParam[1] = 1;   //  always auto-shift
  iParam[2] = 0;   //  no longer referenced
  iParam[3] = maxIterations;
  iParam[4] = 1;   // NB: blocksize to be used in the recurrence.
                   // The code currently works only for NB = 1.

  iParam[5] = 0;   // output - number of converged Ritz values
  iParam[6] = 0;   // No longer referenced. Implicit restarting is ALWAYS used

  long mode;

  // if we have a sigma, it's mode 3, otherwise, mode 2
  // the mode determines the OP used in the solution
  // determine OP
  vnl_sparse_matrix<double> OP;
  if (sigma != 0.0)
  {
    // K*x = lambda*M*x, K symmetric, M symmetric positive semi-definite
    // OP = (inv[K - sigma*M])*M  and  B = M.
    // Shift-and-Invert mode
    mode = 3;
    // determine OP

    OP = B;
    OP *= sigma;
    OP = A - OP;
//vsl_print_summary(std::cout, OP);
  }
  else
  {
    // A*x = lambda*M*x, A symmetric, M symmetric positive definite
    // OP = inv[M]*A  and  B = M.
    mode = 2;
    OP = B;
  }
  // iParam[7] is the mode of the solution
  iParam[7] = mode;

  // decompose for using in "multiplying" intermediate results
  vnl_sparse_lu opLU(OP);

//std::cout << opLU << std::endl;

  iParam[8] = 0;   //  parameter for user supplied shifts - not used here

  // iParam 9 - 11 are output
  iParam[9] = 0;   // total number of OP*x operations
  iParam[10] = 0;   // total number of B*x operations if BMAT='G'
  iParam[11] = 0;   // total number of steps of re-orthogonalization

  // output vector filled with address information for intermediate data used
  // by the solver
  // use FORTRAN indexing again...
  long iPntr[IPARAMSIZE];
  for (int clrIx = 0; clrIx < IPARAMSIZE; clrIx++)
    iPntr[clrIx]= 0;

  // Double precision work array of length 3*N.
  double *workd = new double[3 * matSize + 1];

  // Double precision work array of length 3*N.
  long lworkl = numberLanczosVecsL * (numberLanczosVecsL+9);

  // Double precision work array of length at least NCV**2 + 8*NCV
  double *workl = new double[lworkl + 1];

  vnl_vector<double> workVector;

  while (true)
  {
    // Calling arpack routine dsaupd.
    v3p_netlib_dsaupd_(
      &ido, &genEigProblem, &matSize, which,
      &nEVL, &tolerance, resid, &numberLanczosVecsL, &V[1], &matSize,
          &iParam[1], &iPntr[1], &workd[1], &workl[1], &lworkl, &info,
          genEigProblemLength, whichLength);

    // Checking if aupp is done
    if (ido==DONE)
    {
      nconv = iParam[5];
      break;
    }
    else
    {
      switch (info) {
        case    -8:  // Could not perform LAPACK eigenvalue calculation
        case    -9:  // Starting vector is zero
        case -9999:  // Could not build an Arnoldi factorization
          return info;
          break;
        case     0:  // success
        case     1:  // hit maxIterations - should be DONE
        case     3:  // No shifts could be applied during a cycle of IRAM iteration
          break;
        default   :  // unknown ARPACK error
          return info;
      }

      // setting z pointer to ( = Bx) into workd
      if (ido == -1)
        iPntr[3] = iPntr[2] + matSize;

      vnl_vector_ref<double> x(matSize, &workd[iPntr[1]]);
      vnl_vector_ref<double> y(matSize, &workd[iPntr[2]]);
      vnl_vector_ref<double> z(matSize, &workd[iPntr[3]]);  // z = Bx

      switch (ido)
      {
        case -1:
            // Performing y <- OP*x for the first time when mode != 2.
            if (mode != 2)
              B.mult(x, z);
            // no "break;" - initialization continues below
        case  1:
            // Performing y <- OP*w.
            if (mode != 2)
              opLU.solve(z, &y);
            else
              {
              A.mult(x, workVector);
              x.update(workVector);
              opLU.solve(x, &y);
              }
          break;
        case  2:
            B.mult(x, y);
          break;
        default:
            break;
      }
    }
  }

  long rvec   = 1;  // get the values and vectors

  // which Ritz vctors do we want?
  const int howMnyLength = 1;
  char howMny = 'A';  // all

  // selection vector for which Ritz vectors to calc.
  // we want them all, so allocate the space (dseupd uses it)
  vnl_vector<long> select(numberLanczosVecsL);

  // allocate eVals and eVecs
  nvalues = nconv;
  values = new double[nvalues];
  vectors = new vnl_vector<double>[nvalues];

  // hold the eigenvectors
  double *Z = new double[nvalues * matSize];

  v3p_netlib_dseupd_(&rvec, &howMny, select.data_block(), values, Z, &matSize, &sigma, &genEigProblem,
                     &matSize, which, &nEVL, &tolerance, resid, &numberLanczosVecsL, &V[1], &matSize, &iParam[1],
                     &iPntr[1], &workd[1], &workl[1], &lworkl, &info,
                     howMnyLength, genEigProblemLength, whichLength);

  // Copy the eigenvectors
  int evIx;
  for (evIx = 0; evIx < nvalues; evIx++)
  {
    vnl_vector_ref<double> tempEVec(matSize, &Z[evIx * matSize]);
    vectors[evIx] = tempEVec;
  }

  // Delete temporary space.
  delete[] Z;
  delete[] resid;
  delete[] V;
  delete[] workd;
  delete[] workl;

  return info;
}


//------------------------------------------------------------
//: Callback from solver to calculate the product A p.
int vnl_sparse_symmetric_eigensystem::CalculateProduct(int n, int m,
                                                       const double* p,
                                                       double* q)
{
  // Call the special multiply method on the matrix.
  mat->mult(n,m,p,q);

  return 0;
}

//------------------------------------------------------------
//: Callback to store vectors for dnlaso.
int vnl_sparse_symmetric_eigensystem::SaveVectors(int n, int m,
                                                  const double* q,
                                                  int base)
{
  // Store the contents of q.  Basically this is a fifo.  When a write
  // with base=0 is called, we start another fifo.
  if (base == 0) {
    for (unsigned i=0; i<temp_store.size(); ++i)
      delete temp_store[i];
    temp_store.clear();
  }

  double* temp = new double[n*m];
  std::memcpy(temp,q,n*m*sizeof(double));
#ifdef DEBUG
    std::cout << "Save vectors " << base << ' ' << temp << '\n';
#endif

  temp_store.push_back(temp);

  return 0;
}

//------------------------------------------------------------
//: Callback to restore vectors for dnlaso.
int vnl_sparse_symmetric_eigensystem::RestoreVectors(int n, int m,
                                                     double* q,
                                                     int base)
{
  // Store the contents of q.  Basically this is a fifo.  When a read
  // with base=0 is called, we start another fifo.
  static int read_idx = 0;
  if (base == 0)
    read_idx = 0;

  double* temp = temp_store[read_idx];
  std::memcpy(q,temp,n*m*sizeof(double));
#ifdef DEBUG
    std::cout << "Restore vectors " << base << ' ' << temp << '\n';
#endif

  read_idx++;
  return 0;
}

//------------------------------------------------------------
//: Return a calculated eigenvector.
vnl_vector<double> vnl_sparse_symmetric_eigensystem::get_eigenvector(int i) const
{
  assert(i>=0 && i<nvalues);
  return vectors[i];
}

double vnl_sparse_symmetric_eigensystem::get_eigenvalue(int i) const
{
  assert(i>=0 && i<nvalues);
  return values[i];
}
