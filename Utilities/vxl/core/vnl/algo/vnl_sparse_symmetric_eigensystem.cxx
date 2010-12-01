// This is core/vnl/algo/vnl_sparse_symmetric_eigensystem.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file

#include "vnl_sparse_symmetric_eigensystem.h"
#include <vcl_cassert.h>
#include <vcl_cstring.h>
#include <vcl_iostream.h>
#include <vcl_vector.h>

#include <vnl/algo/vnl_netlib.h> // dnlaso_()

static vnl_sparse_symmetric_eigensystem * current_system = 0;

#ifdef VCL_SUNPRO_CC
# define FUNCTION extern "C"
#else
# define FUNCTION static
#endif

//------------------------------------------------------------
//: Callback for multiplying our matrix by a number of vectors.
//  The input is p, which is an NxM matrix.
//  This function returns q = A p, where A is the current sparse matrix.
FUNCTION
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
FUNCTION
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
  : nvalues(0), vectors(0), values(0)
{
}

vnl_sparse_symmetric_eigensystem::~vnl_sparse_symmetric_eigensystem()
{
  delete[] vectors; vectors = 0;
  delete[] values; values = 0;
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
    delete[] vectors; vectors = 0;
    delete[] values; values = 0;
  }
  nvalues = 0;

  current_system = this;

  long dim = mat->columns();
  long nvals = (smallest)?-n:n;
  long nperm = 0;
  long nmval = n;
  long nmvec = dim;
  vcl_vector<double> temp_vals(n*4);
  vcl_vector<double> temp_vecs(n*dim);

  // set nblock = vcl_max(10, dim/6) :
  long nblock = (dim<60) ? dim/6 : 10;

  // isn't this rather a lot ? -- fsm
  long maxop = dim*10;      // dim*20;

  // set maxj = vcl_max(40, maxop*nblock, 6*nblock+1) :
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
  vcl_vector<double> work(work_size+10);

  // Set starting vectors to zero.
  for (int i=0; i<dim*nblock; ++i)
    work[i] = 0.0;

  vcl_vector<long> ind(n);

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
      vcl_cerr << "Error: vnl_sparse_symmetric_eigensystem: N < 6*NBLOCK\n";
    if (ierr & 0x2)
      vcl_cerr << "Error: vnl_sparse_symmetric_eigensystem: NFIG < 0\n";
    if (ierr & 0x4)
      vcl_cerr << "Error: vnl_sparse_symmetric_eigensystem: NMVEC < N\n";
    if (ierr & 0x8)
      vcl_cerr << "Error: vnl_sparse_symmetric_eigensystem: NPERM < 0\n";
    if (ierr & 0x10)
      vcl_cerr << "Error: vnl_sparse_symmetric_eigensystem: MAXJ < 6*NBLOCK\n";
    if (ierr & 0x20)
      vcl_cerr << "Error: vnl_sparse_symmetric_eigensystem: NVAL < max(1,NPERM)\n";
    if (ierr & 0x40)
      vcl_cerr << "Error: vnl_sparse_symmetric_eigensystem: NVAL > NMVAL\n";
    if (ierr & 0x80)
      vcl_cerr << "Error: vnl_sparse_symmetric_eigensystem: NVAL > MAXOP\n";
    if (ierr & 0x100)
      vcl_cerr << "Error: vnl_sparse_symmetric_eigensystem: NVAL > MAXJ/2\n";
    if (ierr & 0x200)
      vcl_cerr << "Error: vnl_sparse_symmetric_eigensystem: NBLOCK < 1\n";
  }
  else if (ierr < 0)
  {
    if (ierr == -1)
      vcl_cerr << "Error: vnl_sparse_symmetric_eigensystem:\n"
               << "  poor initial vectors chosen\n";
    else if (ierr == -2)
      vcl_cerr << "Error: vnl_sparse_symmetric_eigensystem:\n"
               << "  reached maximum operations " << maxop
               << " without finding all eigenvalues,\n"
               << "  found " << nperm << " eigenvalues\n";
    else if (ierr == -8)
      vcl_cerr << "Error: vnl_sparse_symmetric_eigensystem:\n"
               << "  disastrous loss of orthogonality - internal error\n";
  }

  // Copy the eigenvalues and vectors.
  nvalues = n;
  vectors = new vnl_vector<double>[n];
  values = new double[n];
  for (int i=0; i<n; ++i) {
    values[i] = temp_vals[i];
#if 0
    vcl_cout << "value " << temp_vals[i]
             << " accuracy " << temp_vals[i+n*2] << vcl_endl;
#endif
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
  vcl_memcpy(temp,q,n*m*sizeof(double));
#ifdef DEBUG
    vcl_cout << "Save vectors " << base << ' ' << temp << '\n';
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
  vcl_memcpy(q,temp,n*m*sizeof(double));
#ifdef DEBUG
    vcl_cout << "Restore vectors " << base << ' ' << temp << '\n';
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
