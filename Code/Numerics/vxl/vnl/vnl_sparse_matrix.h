#ifndef vnl_sparse_matrix_h_
#define vnl_sparse_matrix_h_
#ifdef __GNUC__
#pragma interface
#endif
// .NAME	vnl_sparse_matrix
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_sparse_matrix.h
// .FILE	vnl_sparse_matrix.txx
// .SECTION Description
//    vnl_sparse_matrix<T> - Simple sparse matrix.  Only those values which
//    are non-zero are stored. The sparse matrix currently supports
//    only getting/putting elements, and multiply by vector or another
//    sparse matrix.
//
//    Each row is stored as a vector of vcl_pair<unsigned int,T>, where the first
//    of the pair indicates the column index, and the second the
//    value.  All rows are stored, as vcl_vector< row >;
//
// .SECTION Author
//     Rupert W. Curwen, GE CR&D, 20 Oct 98
//
// .SECTION Modifications
//
//     Robin Flatland 5/31/99 Added pre_mult(lhs,result), where
//                            lhs is a vector.
//
//     Robin Flatland 6/08/99 Added iterator that allows sequential
//                            access to non-zero values in matrix.
//                            Iterator is controlled using reset, next,
//                            getrow, getcolumn, and value.
//
//     David Capel May 2000   Added set_row, scale_row, mult, vcat and const
//                            methods where appropriate.

#include <vcl_vector.h>
#include <vnl/vnl_vector.h>
#include <vcl_functional.h>

template <class T>
class vnl_sparse_matrix_pair {
public:
  unsigned int first;
  T second;
  
  vnl_sparse_matrix_pair() : first(0), second(T(0)) {}
    
  vnl_sparse_matrix_pair(unsigned int const& a, T const& b) : first(a), second(b) {}
  vnl_sparse_matrix_pair(const vnl_sparse_matrix_pair<T>& o) : first(o.first), second(o.second) {}

  vnl_sparse_matrix_pair<T>& operator=(vnl_sparse_matrix_pair const &o) {
    if (&o != this) {
      first = o.first;
      second = o.second;
    }
    return *this;
  }
  
  struct less : public vcl_binary_function<vnl_sparse_matrix_pair, vnl_sparse_matrix_pair, bool> {
    bool operator() (vnl_sparse_matrix_pair const& p1, vnl_sparse_matrix_pair const& p2) { 
      return p1.first < p2.first; 
    }
  };
};

//: Simple sparse matrix

//KYM: Added the following so set_row will compile on gcc 2.7.2
typedef vcl_vector<int> vnl_sparse_matrix_vector_int_gcc272_hack;

template <class T>
class vnl_sparse_matrix {
public:
  typedef vnl_sparse_matrix_pair<T> pair_t;
#if defined(VCL_SUNPRO_CC)
  typedef vcl_vector < typename pair_t > row ; 
  typedef vcl_vector < typename row > vnl_sparse_matrix_elements;
#else
  //#if defined(VCL_GCC_295) || defined(VCL_EGCS) || defined(VCL_GCC_27) || defined(VCL_SGI_CC)// it barfs -- fsm
  // also KAI C++ 4.0
  typedef vcl_vector < pair_t > row ; 
  typedef vcl_vector < row > vnl_sparse_matrix_elements;
#endif

  // typedef vcl_vector<typename pair_t> row;

  //: Construct an empty matrix
  vnl_sparse_matrix();

  //: Construct an empty m*n matrix
  vnl_sparse_matrix(unsigned int m, unsigned int n);

  //: Construct an m*n Matrix and copy rhs into it.
  vnl_sparse_matrix(const vnl_sparse_matrix<T>& rhs);

  //: Copy another vnl_sparse_matrix<T> into this.
  vnl_sparse_matrix<T>& operator=(const vnl_sparse_matrix<T>& rhs);

  //: Multiply this*rhs, another sparse matrix.
  void mult(const vnl_sparse_matrix<T>& rhs, vnl_sparse_matrix<T>& result) const;

  //: Multiply this*rhs, where rhs is a vector.
  void mult(const vnl_vector<T>& rhs, vnl_vector<T>& result) const;

  //: Multiply this*p, a fortran order matrix.
  void mult(unsigned int n, unsigned int m, const T* p, T* q) const;

  //: Multiplies lhs*this, where lhs is a vector
  void pre_mult(const vnl_vector<T>& lhs, vnl_vector<T>& result) const;

  //: Add rhs to this.
  void add(const vnl_sparse_matrix<T>& rhs, vnl_sparse_matrix<T>& result) const;

  //: Subtract rhs from this.
  void subtract(const vnl_sparse_matrix<T>& rhs, vnl_sparse_matrix<T>& result) const;

  //: Get a reference to an entry in the matrix.
  T& operator()(unsigned int row, unsigned int column);

  //: Get diag(A_tranpose * A).
  // Useful for forming Jacobi preconditioners for linear solvers.
  void diag_AtA(vnl_vector<T>& result) const;

  //: Set a whole row at once. Much faster.
  void set_row(unsigned int r, 
	       vcl_vector<int> const& cols, 
	       vcl_vector<T> const& vals);

  //: Laminate matrix A onto the bottom of this one
  vnl_sparse_matrix<T>& vcat(vnl_sparse_matrix<T> const& A);

  //: Get the number of rows in the matrix.
  unsigned int rows() const { return rs_; }

  //: Get the number of columns in the matrix.
  unsigned int columns() const { return cs_; }

  //: Return whether a given row is empty
  bool empty_row(unsigned int r) const { return elements[r].empty(); }

  //: This is occasionally useful.
  double sum_row(unsigned int r);

  //: Useful for normalizing row sums in convolution operators
  void scale_row(unsigned int r, T scale);

  //: Resizes the array to have r rows and c cols
  //    Currently not implemented.
  void resize( int r, int c );

  //: Resets the internal iterator 
  void reset();

  //: Moves the internal iterator to next non-zero entry in matrix.
  // Returns true if there is another value, false otherwise. Use
  // in combination with methods reset, getrow, getcolumn, and value.
  bool next();
 
  //: Returns the row of the entry pointed to by internal iterator.
  int getrow();

  //: Returns the column of the entry pointed to by internal iterator.
  int getcolumn();

  //: Returns the value pointed to by the internal iterator.
  T value();  


protected:
  vnl_sparse_matrix_elements elements;
  unsigned int rs_, cs_;

  // internal iterator
  unsigned int itr_row;
  typename row::iterator itr_cur;  
  bool itr_isreset;
};

#endif // vnl_sparse_matrix_h_
