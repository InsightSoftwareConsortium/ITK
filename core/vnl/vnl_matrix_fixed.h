// This is core/vnl/vnl_matrix_fixed.h
#ifndef vnl_matrix_fixed_h_
#define vnl_matrix_fixed_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief fixed size matrix
//
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   04 Aug 96
//
// \verbatim
//  Modifications
//   Peter Vanroose, 23 Nov 1996:  added explicit copy constructor
//   LSB (Manchester) 15/03/2001:  added Binary I/O and tidied up the documentation
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
//   Oct.2002 - Amitha Perera  - separated vnl_matrix and vnl_matrix_fixed,
//                               removed necessity for vnl_matrix_fixed_ref
//   Oct.2002 - Peter Vanroose - added inplace_transpose() method
//   Jul.2003 - Paul Smyth     - fixed end() bug, made op*=() more general
//   Mar.2009 - Peter Vanroose - added arg_min() and arg_max()
//   Oct.2010 - Peter Vanroose - mutators and filling methods now return *this
//   Jan.2011 - Peter Vanroose - added methods set_diagonal() & get_diagonal()
// \endverbatim
//-----------------------------------------------------------------------------

#include <cstring>
#include <iosfwd>
#include <vcl_cassert.h>
#include <vcl_compiler.h>

#include "vnl_matrix.h"
#include "vnl_matrix_ref.h"
#include <vnl/vnl_vector.h>
#include <vnl/vnl_vector_fixed.h> // needed for e.g. vnl_matrix_fixed_mat_vec_mult()
#include <vnl/vnl_c_vector.h>
#include <vnl/vnl_config.h> // for VNL_CONFIG_CHECK_BOUNDS
#include "vnl/vnl_export.h"

VCL_TEMPLATE_EXPORT template <class T, unsigned int num_rows, unsigned int num_cols> class vnl_matrix_fixed;

// This mess is for a MSVC6 workaround.
//
// The problem: the matrix-matrix operator* should be written as a
// non-member function since vxl (currently) forbids the use of member
// templates. However, when declared as
// \code
//     template <class T, unsigned m, unsigned n, unsigned o>
//     matrix<T,m,o> operator*( matrix<T,m,n>, matrix<T,n,o> );
// \endcode
// MSVC6 does not find it. A solution is to declare it as a member
// template. However, the obvious
// \code
//     template <unsigned o>
//     matrix<T,num_rows,o> operator*( matrix<T,num_cols,o> );
// \endcode
// causes an internal compiler error. It turns out that if the new
// template parameter "o" comes _first_, then all is okay. Now, we
// can't change the signature of vnl_matrix_fixed to <unsigned num_cols,
// unsigned num_rows, type>, so we use a "hidden" helper matrix. Except
// that user defined conversion operators and conversion constructors
// are not called for templated functions. So we have to use a helper
// base class. The base class is empty, which means that there is no
// loss in space or time efficiency. Finally, we have:
// \code
//   template <unsigned num_cols, unsigned num_rows, class T>
//   class fake_base { };
//
//   template <class T, unsigned num_rows, unsigned num_cols>
//   class matrix : public fake_base<num_cols,num_rows,T>
//   {
//      template <unsigned o>
//      matrix<T,num_rows,o>  operator*( fake_base<o,num_cols,T> );
//   };
// \endcode
// Notice how "o" is first in the list of template parameters. Since
// base class conversions _are_ performed during template matching,
// matrix<T,m,n> is matched as fake_base<n,m,T>, and all is good. For
// some values of good.
//
// Of course, all this trickery is pre-processed away for conforming
// compilers.
//
template <class T, unsigned int num_rows, unsigned int num_cols>
class vnl_matrix_fixed;
template <class T, unsigned M, unsigned N>
inline
vnl_vector_fixed<T, M> vnl_matrix_fixed_mat_vec_mult(const vnl_matrix_fixed<T, M, N>& a, const vnl_vector_fixed<T, N>& b);
template <class T, unsigned M, unsigned N, unsigned O>
inline
vnl_matrix_fixed<T, M, O> vnl_matrix_fixed_mat_mat_mult(const vnl_matrix_fixed<T, M, N>& a, const vnl_matrix_fixed<T, N, O>& b);

//: Fixed size, stack-stored, space-efficient matrix.
// vnl_matrix_fixed is a fixed-length, stack storage vector. It has
// the same storage size as a C-style array. It is not related via
// inheritance to vnl_matrix. However, it can be converted cheaply to
// a vnl_matrix_ref.
//
// Read the overview documentation of vnl_vector_fixed.
// The text there applies here.
template <class T, unsigned int num_rows, unsigned int num_cols>
class VNL_TEMPLATE_EXPORT vnl_matrix_fixed
{
  T data_[num_rows][num_cols]; // Local storage

 public:
  typedef vnl_matrix_fixed<T,num_rows,num_cols> self;
  typedef size_t size_type;

  //: Construct an empty num_rows*num_cols matrix
  vnl_matrix_fixed() {}

  //: Construct an empty num_rows*num_cols matrix
  //
  // The sole purpose of this constructor is to match the interface of
  // vnl_matrix, so that algorithms can template over the matrix type
  // itself.  It is illegal to call this constructor without
  // <tt>n==num_rows</tt> and <tt>m==num_cols</tt>.
  vnl_matrix_fixed( unsigned VXL_USED_IN_DEBUG(n), unsigned VXL_USED_IN_DEBUG(m) )
  {
    assert( n == num_rows && m == num_cols );
  }

  //: Construct an m*n matrix and fill with value
  explicit vnl_matrix_fixed(T value)
  {
    T* p = data_[0];
    unsigned int n = num_rows * num_cols;
    while (n--)
      *p++ = value;
  }

  //: Construct an m*n Matrix and copy data into it row-wise.
  explicit vnl_matrix_fixed(const T* datablck)
  {
    std::memcpy(data_[0], datablck, num_rows*num_cols*sizeof(T));
  }

  //: Construct an m*n Matrix and copy rhs into it.
  //  Abort if rhs is not the same size.
  vnl_matrix_fixed(const vnl_matrix_fixed& rhs)
  {
    std::memcpy(data_[0], rhs.data_block(), num_rows*num_cols*sizeof(T));
  }

  //: Construct an m*n Matrix and copy rhs into it.
  //  Abort if rhs is not the same size.
  vnl_matrix_fixed(const vnl_matrix<T>& rhs)
  {
    assert(rhs.rows() == num_rows && rhs.columns() == num_cols);
    std::memcpy(data_[0], rhs.data_block(), num_rows*num_cols*sizeof(T));
  }

  //  Destruct the m*n matrix.
  // An explicit destructor seems to be necessary, at least for gcc 3.0.0,
  // to avoid the compiler generating multiple versions of it.
  // (This way, a weak symbol is generated; otherwise not.  A bug of gcc 3.0.)
  ~vnl_matrix_fixed() {}

  //: Set all elements to value v
  // Complexity $O(r.c)$
  vnl_matrix_fixed& operator= (T const&v) { fill(v); return *this; }

  //: Copy a vnl_matrix into this.
  //  Abort if rhs is not the same size.
  vnl_matrix_fixed& operator=(const vnl_matrix<T>& rhs)
  {
    assert(rhs.rows() == num_rows && rhs.columns() == num_cols);
    std::memcpy(data_[0], rhs.data_block(), num_rows*num_cols*sizeof(T));
    return *this;
  }

  //: Copy another vnl_matrix_fixed<T,m,n> into this.
  vnl_matrix_fixed& operator=(const vnl_matrix_fixed& rhs)
  {
    std::memcpy(data_[0], rhs.data_block(), num_rows*num_cols*sizeof(T));
    return *this;
  }

// Basic 2D-Array functionality-------------------------------------------

  //: Return the total number of elements stored by the matrix.
  // This equals rows() * cols()
  inline unsigned int size() const { return num_rows*num_cols; }

  //: Return the number of rows.
  inline unsigned int rows() const { return num_rows; }

  //: Return the number of columns.
  // A synonym for columns().
  inline unsigned int cols() const { return num_cols; }

  //: Return the number of columns.
  // A synonym for cols().
  inline unsigned int columns() const { return num_cols; }

  //: set element
  inline void put (unsigned r, unsigned c, T const& v)
  {
#if VNL_CONFIG_CHECK_BOUNDS
    if (r >= num_rows)                // If invalid size specified
      vnl_error_matrix_row_index("put", r); // Raise exception
    if (c >= num_cols)                // If invalid size specified
      vnl_error_matrix_col_index("put", c); // Raise exception
#endif
    this->data_[r][c] = v;
  }

  //: get element
  inline T get (unsigned r, unsigned c) const
  {
#if VNL_CONFIG_CHECK_BOUNDS
    if (r >= num_rows)                // If invalid size specified
      vnl_error_matrix_row_index("get", r); // Raise exception
    if (c >= num_cols)                // If invalid size specified
      vnl_error_matrix_col_index("get", c); // Raise exception
#endif
    return this->data_[r][c];
  }

  //: set element, and return *this
  vnl_matrix_fixed& set (unsigned r, unsigned c, T const& v) { (*this)(r,c) = v; return *this; }

  //: return pointer to given row
  // No boundary checking here.
  T       * operator[] (unsigned r) { return data_[r]; }

  //: return pointer to given row
  // No boundary checking here.
  T const * operator[] (unsigned r) const { return data_[r]; }

  //: Access an element for reading or writing
  // There are assert style boundary checks - #define NDEBUG to turn them off.
  T       & operator() (unsigned r, unsigned c)
  {
#if VNL_CONFIG_CHECK_BOUNDS  && (!defined NDEBUG)
    assert(r<rows());   // Check the row index is valid
    assert(c<cols());   // Check the column index is valid
#endif
    return this->data_[r][c];
  }

  //: Access an element for reading
  // There are assert style boundary checks - #define NDEBUG to turn them off.
  T const & operator() (unsigned r, unsigned c) const
  {
#if VNL_CONFIG_CHECK_BOUNDS  && (!defined NDEBUG)
    assert(r<rows());   // Check the row index is valid
    assert(c<cols());   // Check the column index is valid
#endif
    return this->data_[r][c];
  }

  // ----------------------- Filling and copying -----------------------

  //: Sets all elements of matrix to specified value, and returns "*this".
  //  Complexity $O(r.c)$
  //  Returning "*this" allows "chaining" two or more operations:
  //  e.g., to set a matrix to a column-normalized all-elements-equal matrix, say
  //  \code
  //     M.fill(1).normalize_columns();
  //  \endcode
  //  Returning "*this" also allows passing such a matrix as argument
  //  to a function f, without having to name the constructed matrix:
  //  \code
  //     f(vnl_matrix_fixed<double,5,5>(1.0).normalize_columns());
  //  \endcode
  vnl_matrix_fixed& fill(T);

  //: Sets all diagonal elements of matrix to specified value; returns "*this".
  //  Complexity $O(\min(r,c))$
  //  Returning "*this" allows "chaining" two or more operations:
  //  e.g., to set a 3x3 matrix to [5 0 0][0 10 0][0 0 15], just say
  //  \code
  //     M.fill_diagonal(5).scale_row(1,2).scale_column(2,3);
  //  \endcode
  //  Returning "*this" also allows passing a diagonal-filled matrix as argument
  //  to a function f, without having to name the constructed matrix:
  //  \code
  //     f(vnl_matrix_fixed<double,3,3>().fill_diagonal(5));
  //  \endcode
  vnl_matrix_fixed& fill_diagonal(T);

  //: Sets the diagonal elements of this matrix to the specified list of values.
  //  Returning "*this" allows "chaining" two or more operations: see the
  //  reasoning (and the examples) in the documentation for method
  //  fill_diagonal().
  vnl_matrix_fixed& set_diagonal(vnl_vector<T> const&);

  //: Fills (laminates) this matrix with the given data, then returns it.
  //  We assume that the argument points to a contiguous rows*cols array, stored rowwise.
  //  No bounds checking on the array.
  //  Returning "*this" allows "chaining" two or more operations:
  //  e.g., to fill a square matrix column-wise, fill it rowwise then transpose:
  //  \code
  //     M.copy_in(array).inplace_transpose();
  //  \endcode
  //  Returning "*this" also allows passing a filled-in matrix as argument
  //  to a function f, without having to name the constructed matrix:
  //  \code
  //     f(vnl_matrix_fixed<double,3,3>().copy_in(array));
  //  \endcode
  vnl_matrix_fixed& copy_in(T const *);

  //: Fills (laminates) this matrix with the given data, then returns it.
  //  A synonym for copy_in()
  vnl_matrix_fixed& set(T const *d) { return copy_in(d); }

  //: Fills the given array with this matrix.
  //  We assume that the argument points to a contiguous rows*cols array, stored rowwise.
  //  No bounds checking on the array.
  void copy_out(T *) const;

  //: Transposes this matrix efficiently, if it is square, and returns it.
  //  Returning "*this" allows "chaining" two or more operations:
  //  e.g., to fill a square matrix column-wise, fill it rowwise then transpose:
  //  \code
  //     M.copy_in(array).inplace_transpose();
  //  \endcode
  vnl_matrix_fixed& inplace_transpose();

  // ----------------------- Arithmetic --------------------------------
  // note that these functions should not pass scalar as a const&.
  // Look what would happen to A /= A(0,0).

  //: Add \a s to each element of lhs matrix in situ
  vnl_matrix_fixed& operator+= (T s)
  {
    self::add( data_block(), s, data_block() ); return *this;
  }

  //: Subtract \a s from each element of lhs matrix in situ
  vnl_matrix_fixed& operator-= (T s)
  {
    self::sub( data_block(), s, data_block() ); return *this;
  }

  //:
  vnl_matrix_fixed& operator*= (T s)
  {
    self::mul( data_block(), s, data_block() ); return *this;
  }

  //:
  vnl_matrix_fixed& operator/= (T s)
  {
    self::div( data_block(), s, data_block() ); return *this;
  }

  //:
  vnl_matrix_fixed& operator+= (vnl_matrix_fixed const& m)
  {
    self::add( data_block(), m.data_block(), data_block() ); return *this;
  }

  //:
  vnl_matrix_fixed& operator+= (vnl_matrix<T> const& m)
  {
    assert( m.rows() == rows() && m.cols() == cols() );
    self::add( data_block(), m.data_block(), data_block() ); return *this;
  }

  //:
  vnl_matrix_fixed& operator-= (vnl_matrix_fixed const& m)
  {
    self::sub( data_block(), m.data_block(), data_block() ); return *this;
  }

  //:
  vnl_matrix_fixed& operator-= (vnl_matrix<T> const& m)
  {
    assert( m.rows() == rows() && m.cols() == cols() );
    self::sub( data_block(), m.data_block(), data_block() );
    return *this;
  }

  //: Negate all elements of matrix
  vnl_matrix_fixed operator- () const
  {
    vnl_matrix_fixed r;
    self::sub( T(0), data_block(), r.data_block() );
    return r;
  }

  //:
  vnl_matrix_fixed& operator*= (vnl_matrix_fixed<T,num_cols,num_cols> const& s)
  {
    vnl_matrix_fixed<T, num_rows, num_cols> out;
    for (unsigned i = 0; i < num_rows; ++i)
      for (unsigned j = 0; j < num_cols; ++j)
      {
        T accum = this->data_[i][0] * s(0,j);
        for (unsigned k = 1; k < num_cols; ++k)
          accum += this->data_[i][k] * s(k,j);
        out(i,j) = accum;
      }
    return *this = out;
  }

  ////--------------------------- Additions ----------------------------

  //: Make a new matrix by applying function to each element.
  vnl_matrix_fixed apply(T (*f)(T)) const;

  //: Make a new matrix by applying function to each element.
  vnl_matrix_fixed apply(T (*f)(T const&)) const;

  //: Make a vector by applying a function across rows.
  vnl_vector_fixed<T,num_rows> apply_rowwise(T (*f)(vnl_vector_fixed<T,num_cols> const&)) const;

  //: Make a vector by applying a function across columns.
  vnl_vector_fixed<T,num_cols> apply_columnwise(T (*f)(vnl_vector_fixed<T,num_rows> const&)) const;

  //: Return transpose
  vnl_matrix_fixed<T,num_cols,num_rows> transpose() const;

  //: Return conjugate transpose
  vnl_matrix_fixed<T,num_cols,num_rows> conjugate_transpose() const;

  //: Set values of this matrix to those of M, starting at [top,left]
  vnl_matrix_fixed& update(vnl_matrix<T> const&, unsigned top=0, unsigned left=0);

  //: Set the elements of the i'th column to v[i]  (No bounds checking)
  vnl_matrix_fixed& set_column(unsigned i, T const * v);

  //: Set the elements of the i'th column to value, then return *this.
  vnl_matrix_fixed& set_column(unsigned i, T value );

  //: Set j-th column to v, then return *this.
  vnl_matrix_fixed& set_column(unsigned j, vnl_vector<T> const& v);

  //: Set j-th column to v, then return *this.
  vnl_matrix_fixed& set_column(unsigned j, vnl_vector_fixed<T,num_rows> const& v);

  //: Set columns to those in M, starting at starting_column, then return *this.
  vnl_matrix_fixed& set_columns(unsigned starting_column, vnl_matrix<T> const& M);

  //: Set the elements of the i'th row to v[i]  (No bounds checking)
  vnl_matrix_fixed& set_row   (unsigned i, T const * v);

  //: Set the elements of the i'th row to value, then return *this.
  vnl_matrix_fixed& set_row   (unsigned i, T value );

  //: Set the i-th row, then return *this.
  vnl_matrix_fixed& set_row   (unsigned i, vnl_vector<T> const&);

  //: Set the i-th row, then return *this.
  vnl_matrix_fixed& set_row   (unsigned i, vnl_vector_fixed<T,num_cols> const&);

  //: Extract a sub-matrix of size r x c, starting at (top,left)
  //  Thus it contains elements  [top,top+r-1][left,left+c-1]
  vnl_matrix<T> extract (unsigned r,  unsigned c,
                         unsigned top=0, unsigned left=0) const;

  //: Extract a sub-matrix starting at (top,left)
  //
  //  The output is stored in \a sub_matrix, and it should have the
  //  required size on entry.  Thus the result will contain elements
  //  [top,top+sub_matrix.rows()-1][left,left+sub_matrix.cols()-1]
  void extract ( vnl_matrix<T>& sub_matrix,
                 unsigned top=0, unsigned left=0) const;

  //: Get a vector equal to the given row
  vnl_vector_fixed<T,num_cols> get_row   (unsigned row) const;

  //: Get a vector equal to the given column
  vnl_vector_fixed<T,num_rows> get_column(unsigned col) const;

  //: Get a matrix composed of rows from the indices specified in the supplied vector.
  vnl_matrix<T> get_rows(vnl_vector<unsigned int> i) const;

  //: Get a matrix composed of columns from the indices specified in the supplied vector.
  vnl_matrix<T> get_columns(vnl_vector<unsigned int> i) const;

  //: Get n rows beginning at rowstart
  vnl_matrix<T> get_n_rows   (unsigned rowstart, unsigned n) const;

  //: Get n columns beginning at colstart
  vnl_matrix<T> get_n_columns(unsigned colstart, unsigned n) const;

  //: Return a vector with the content of the (main) diagonal
  vnl_vector<T> get_diagonal() const;

  //: Flatten row-major (C-style)
  vnl_vector_fixed<T,num_rows*num_cols> flatten_row_major() const;

  //: Flatten column-major (Fortran-style)
  vnl_vector_fixed<T,num_rows*num_cols> flatten_column_major() const;

  // ==== mutators ====

  //: Sets this matrix to an identity matrix, then returns "*this".
  //  Returning "*this" allows e.g. passing an identity matrix as argument to
  //  a function f, without having to name the constructed matrix:
  //  \code
  //     f(vnl_matrix_fixed<double,5,5>().set_identity());
  //  \endcode
  //  Returning "*this" also allows "chaining" two or more operations:
  //  e.g., to set a 3x3 matrix to [3 0 0][0 2 0][0 0 1], one could say
  //  \code
  //     M.set_identity().scale_row(0,3).scale_column(1,2);
  //  \endcode
  //  If the matrix is not square, anyhow set main diagonal to 1, the rest to 0.
  vnl_matrix_fixed& set_identity();

  //: Reverses the order of rows, and returns "*this".
  //  Returning "*this" allows "chaining" two or more operations:
  //  e.g., to flip both up-down and left-right, one could just say
  //  \code
  //     M.flipud().fliplr();
  //  \endcode
  vnl_matrix_fixed& flipud();

  //: Reverses the order of columns, and returns "*this".
  //  Returning "*this" allows "chaining" two or more operations:
  //  e.g., to flip both up-down and left-right, one could just say
  //  \code
  //     M.flipud().fliplr();
  //  \endcode
  vnl_matrix_fixed& fliplr();

  //: Normalizes each row so it is a unit vector, and returns "*this".
  //  Zero rows are not modified
  //  Returning "*this" allows "chaining" two or more operations:
  //  e.g., to set a matrix to a row-normalized all-elements-equal matrix, say
  //  \code
  //     M.fill(1).normalize_rows();
  //  \endcode
  //  Returning "*this" also allows passing such a matrix as argument
  //  to a function f, without having to name the constructed matrix:
  //  \code
  //     f(vnl_matrix_fixed<double,5,5>(1.0).normalize_rows());
  //  \endcode
  vnl_matrix_fixed& normalize_rows();

  //: Normalizes each column so it is a unit vector, and returns "*this".
  //  Zero columns are not modified
  //  Returning "*this" allows "chaining" two or more operations:
  //  e.g., to set a matrix to a column-normalized all-elements-equal matrix, say
  //  \code
  //     M.fill(1).normalize_columns();
  //  \endcode
  //  Returning "*this" also allows passing such a matrix as argument
  //  to a function f, without having to name the constructed matrix:
  //  \code
  //     f(vnl_matrix_fixed<double,5,5>(1.0).normalize_columns());
  //  \endcode
  vnl_matrix_fixed& normalize_columns();

  //: Scales elements in given row by a factor T, and returns "*this".
  //  Returning "*this" allows "chaining" two or more operations:
  //  e.g., to set a 3x3 matrix to [3 0 0][0 2 0][0 0 1], one could say
  //  \code
  //     M.set_identity().scale_row(0,3).scale_column(1,2);
  //  \endcode
  vnl_matrix_fixed& scale_row   (unsigned row, T value);

  //: Scales elements in given column by a factor T, and returns "*this".
  //  Returning "*this" allows "chaining" two or more operations:
  //  e.g., to set a 3x3 matrix to [3 0 0][0 2 0][0 0 1], one could say
  //  \code
  //     M.set_identity().scale_row(0,3).scale_column(1,2);
  //  \endcode
  vnl_matrix_fixed& scale_column(unsigned col, T value);

  //: Swap this matrix with that matrix
  void swap(vnl_matrix_fixed<T,num_rows,num_cols> & that);

  //: Type def for norms.
  typedef typename vnl_c_vector<T>::abs_t abs_t;

  //: Return sum of absolute values of elements
  abs_t array_one_norm() const { return vnl_c_vector<T>::one_norm(begin(), size()); }

  //: Return square root of sum of squared absolute element values
  abs_t array_two_norm() const { return vnl_c_vector<T>::two_norm(begin(), size()); }

  //: Return largest absolute element value
  abs_t array_inf_norm() const { return vnl_c_vector<T>::inf_norm(begin(), size()); }

  //: Return sum of absolute values of elements
  abs_t absolute_value_sum() const { return array_one_norm(); }

  //: Return largest absolute value
  abs_t absolute_value_max() const { return array_inf_norm(); }

  // $ || M ||_1 := \max_j \sum_i | M_{ij} | $
  abs_t operator_one_norm() const;

  // $ || M ||_\inf := \max_i \sum_j | M_{ij} | $
  abs_t operator_inf_norm() const;

  //: Return Frobenius norm of matrix (sqrt of sum of squares of its elements)
  abs_t frobenius_norm() const { return vnl_c_vector<T>::two_norm(begin(), size()); }

  //: Return Frobenius norm of matrix (sqrt of sum of squares of its elements)
  abs_t fro_norm() const { return frobenius_norm(); }

  //: Return RMS of all elements
  abs_t rms() const { return vnl_c_vector<T>::rms_norm(begin(), size()); }

  //: Return minimum value of elements
  T min_value() const { return vnl_c_vector<T>::min_value(begin(), size()); }

  //: Return maximum value of elements
  T max_value() const { return vnl_c_vector<T>::max_value(begin(), size()); }

  //: Return location of minimum value of elements
  unsigned arg_min() const { return vnl_c_vector<T>::arg_min(begin(), size()); }

  //: Return location of maximum value of elements
  unsigned arg_max() const { return vnl_c_vector<T>::arg_max(begin(), size()); }

  //: Return mean of all matrix elements
  T mean() const { return vnl_c_vector<T>::mean(begin(), size()); }

  // predicates

  //: Return true iff the size is zero.
  bool empty() const { return num_rows==0 && num_cols==0; }

  //:  Return true if all elements equal to identity.
  bool is_identity() const;

  //:  Return true if all elements equal to identity, within given tolerance
  bool is_identity(double tol) const;

  //: Return true if all elements equal to zero.
  bool is_zero() const;

  //: Return true if all elements equal to zero, within given tolerance
  bool is_zero(double tol) const;

  //:  Return true if all elements of both matrices are equal, within given tolerance
  bool is_equal(vnl_matrix_fixed<T,num_rows,num_cols> const& rhs, double tol) const;

  //: Return true if finite
  bool is_finite() const;

  //: Return true if matrix contains NaNs
  bool has_nans() const;

  //: abort if size is not as expected
  // This function does or tests nothing if NDEBUG is defined
  void assert_size(unsigned VXL_USED_IN_DEBUG(nr_rows), unsigned VXL_USED_IN_DEBUG(nr_cols) ) const
  {
#ifndef NDEBUG
    assert_size_internal(nr_rows, nr_cols);
#endif
  }

  //: abort if matrix contains any INFs or NANs.
  // This function does or tests nothing if NDEBUG is defined
  void assert_finite() const
  {
#ifndef NDEBUG
    assert_finite_internal();
#endif
  }

  ////----------------------- Input/Output ----------------------------

  // : Read a vnl_matrix from an ascii std::istream, automatically determining file size if the input matrix has zero size.
  bool read_ascii(std::istream& s);

  //--------------------------------------------------------------------------------

  //: Access the contiguous block storing the elements in the matrix row-wise. O(1).
  // 1d array, row-major order.
  T const* data_block () const { return data_[0]; }

  //: Access the contiguous block storing the elements in the matrix row-wise. O(1).
  // 1d array, row-major order.
  T      * data_block () { return data_[0]; }


  //----------------------------------------------------------------------
  // Conversion to vnl_matrix_ref.

  // The const version of as_ref should return a const vnl_matrix_ref
  // so that the vnl_matrix_ref::non_const() cannot be used on
  // it. This prevents a const vnl_matrix_fixed from being cast into a
  // non-const vnl_matrix reference, giving a slight increase in type safety.

  //: Explicit conversion to a vnl_matrix_ref.
  // This is a cheap conversion for those functions that have an interface
  // for vnl_matrix but not for vnl_matrix_fixed. There is also a
  // conversion operator that should work most of the time.
  // \sa vnl_matrix_ref::non_const
  vnl_matrix_ref<T> as_ref() { return vnl_matrix_ref<T>( num_rows, num_cols, data_block() ); }

  //: Explicit conversion to a vnl_matrix_ref.
  // This is a cheap conversion for those functions that have an interface
  // for vnl_matrix but not for vnl_matrix_fixed. There is also a
  // conversion operator that should work most of the time.
  // \sa vnl_matrix_ref::non_const
  const vnl_matrix_ref<T> as_ref() const { return vnl_matrix_ref<T>( num_rows, num_cols, const_cast<T*>(data_block()) ); }

  //: Cheap conversion to vnl_matrix_ref
  // Sometimes, such as with templated functions, the compiler cannot
  // use this user-defined conversion. For those cases, use the
  // explicit as_ref() method instead.
  operator const vnl_matrix_ref<T>() const { return vnl_matrix_ref<T>( num_rows, num_cols, const_cast<T*>(data_block()) ); }

  //: Convert to a vnl_matrix.
  const vnl_matrix<T> as_matrix() const { return vnl_matrix<T>(const_cast<T*>(data_block()),num_rows,num_cols); }

  //----------------------------------------------------------------------

  typedef T element_type;

  //: Iterators
  typedef T       *iterator;
  //: Iterator pointing to start of data
  iterator       begin() { return data_[0]; }
  //: Iterator pointing to element beyond end of data
  iterator       end() { return begin() + size(); }

  //: Const iterators
  typedef T const *const_iterator;
  //: Iterator pointing to start of data
  const_iterator begin() const { return data_[0]; }
  //: Iterator pointing to element beyond end of data
  const_iterator end() const { return begin() + size(); }

  //--------------------------------------------------------------------------------

  //: Return true if *this == rhs
  bool operator_eq (vnl_matrix_fixed const & rhs) const
  {
    return equal( this->data_block(), rhs.data_block() );
  }

  //: Equality operator
  bool operator==(vnl_matrix_fixed const &that) const { return  this->operator_eq(that); }

  //: Inequality operator
  bool operator!=(vnl_matrix_fixed const &that) const { return !this->operator_eq(that); }

  //: Equality operator
  bool operator==(vnl_matrix<T> const &that) const { return  this->operator_eq(that); }

  //: Inequality operator
  bool operator!=(vnl_matrix<T> const &that) const { return !this->operator_eq(that); }

  //: Print matrix to os in some hopefully sensible format
  void print(std::ostream& os) const;

//--------------------------------------------------------------------------------


  // Helper routines for arithmetic. These routines know the size from
  // the template parameters. The vector-vector operations are
  // element-wise.

  static void add( const T* a, const T* b, T* r );
  static void add( const T* a, T b, T* r );
  static void sub( const T* a, const T* b, T* r );
  static void sub( const T* a, T b, T* r );
  static void sub( T a, const T* b, T* r );
  static void mul( const T* a, const T* b, T* r );
  static void mul( const T* a, T b, T* r );
  static void div( const T* a, const T* b, T* r );
  static void div( const T* a, T b, T* r );

  static bool equal( const T* a, const T* b );

 private:
  void assert_finite_internal() const;

  void assert_size_internal(unsigned, unsigned) const;
};

// Make the operators below inline because (1) they are small and
// (2) we then have less explicit instantiation trouble.


// --- Matrix-scalar -------------------------------------------------------------

template <class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> operator+( const vnl_matrix_fixed<T,m,n>& mat1, const vnl_matrix_fixed<T,m,n>& mat2 )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::add( mat1.data_block(), mat2.data_block(), r.data_block() );
  return r;
}

template <class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> operator+( const vnl_matrix_fixed<T,m,n>& mat, T s )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::add( mat.data_block(), s, r.data_block() );
  return r;
}

template <class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> operator+( const T& s,
                                   const vnl_matrix_fixed<T,m,n>& mat )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::add( mat.data_block(), s, r.data_block() );
  return r;
}

template <class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> operator-( const vnl_matrix_fixed<T,m,n>& mat1, const vnl_matrix_fixed<T,m,n>& mat2 )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::sub( mat1.data_block(), mat2.data_block(), r.data_block() );
  return r;
}

template <class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> operator-( const vnl_matrix_fixed<T,m,n>& mat, T s )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::sub( mat.data_block(), s, r.data_block() );
  return r;
}

template <class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> operator-( const T& s,
                                   const vnl_matrix_fixed<T,m,n>& mat )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::sub( s, mat.data_block(), r.data_block() );
  return r;
}

template <class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> operator*( const vnl_matrix_fixed<T,m,n>& mat, T s )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::mul( mat.data_block(), s, r.data_block() );
  return r;
}

template <class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> operator*( const T& s,
                                   const vnl_matrix_fixed<T,m,n>& mat )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::mul( mat.data_block(), s, r.data_block() );
  return r;
}

template <class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> operator/( const vnl_matrix_fixed<T,m,n>& mat, T s )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::div( mat.data_block(), s, r.data_block() );
  return r;
}


template <class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> element_product( const vnl_matrix_fixed<T,m,n>& mat1,
                                         const vnl_matrix_fixed<T,m,n>& mat2 )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::mul( mat1.data_block(), mat2.data_block(), r.data_block() );
  return r;
}


template <class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> element_quotient( const vnl_matrix_fixed<T,m,n>& mat1,
                                          const vnl_matrix_fixed<T,m,n>& mat2)
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::div( mat1.data_block(), mat2.data_block(), r.data_block() );
  return r;
}


// The following two functions are helper functions keep the
// matrix-matrix and matrix-vector multiplication code in one place,
// so that bug fixes, etc, can be localized.
template <class T, unsigned M, unsigned N>
inline
vnl_vector_fixed<T, M>
vnl_matrix_fixed_mat_vec_mult(const vnl_matrix_fixed<T, M, N>& a,
                              const vnl_vector_fixed<T, N>& b)
{
  vnl_vector_fixed<T, M> out;
  for (unsigned i = 0; i < M; ++i)
  {
    T accum = a(i,0) * b(0);
    for (unsigned k = 1; k < N; ++k)
      accum += a(i,k) * b(k);
    out(i) = accum;
  }
  return out;
}

template <class T, unsigned M, unsigned N>
inline
vnl_vector_fixed<T, N>
vnl_matrix_fixed_vec_mat_mult(const vnl_vector_fixed<T, M>& a,
                              const vnl_matrix_fixed<T, M, N>& b)
{
  vnl_vector_fixed<T, N> out;
  for (unsigned i = 0; i < N; ++i)
  {
    T accum = a(0) * b(0,i);
    for (unsigned k = 1; k < M; ++k)
      accum += a(k) * b(k,i);
    out(i) = accum;
  }
  return out;
}

// see comment above
template <class T, unsigned M, unsigned N, unsigned O>
inline
vnl_matrix_fixed<T, M, O>
vnl_matrix_fixed_mat_mat_mult(const vnl_matrix_fixed<T, M, N>& a,
                              const vnl_matrix_fixed<T, N, O>& b)
{
  vnl_matrix_fixed<T, M, O> out;
  for (unsigned i = 0; i < M; ++i)
    for (unsigned j = 0; j < O; ++j)
    {
      T accum = a(i,0) * b(0,j);
      for (unsigned k = 1; k < N; ++k)
        accum += a(i,k) * b(k,j);
      out(i,j) = accum;
    }
  return out;
}

// The version for correct compilers

//: Multiply  conformant vnl_matrix_fixed (M x N) and vector_fixed (N)
// \relatesalso vnl_vector_fixed
// \relatesalso vnl_matrix_fixed
template <class T, unsigned M, unsigned N>
inline
vnl_vector_fixed<T, M> operator*(const vnl_matrix_fixed<T, M, N>& a, const vnl_vector_fixed<T, N>& b)
{
  return vnl_matrix_fixed_mat_vec_mult(a, b);
}

//: Multiply  conformant vector_fixed (M) and vnl_matrix_fixed (M x N)
// \relatesalso vnl_vector_fixed
// \relatesalso vnl_matrix_fixed
template <class T, unsigned M, unsigned N>
inline
vnl_vector_fixed<T, N> operator*(const vnl_vector_fixed<T, M>& a, const vnl_matrix_fixed<T, M, N>& b)
{
  return vnl_matrix_fixed_vec_mat_mult(a, b);
}

//: Multiply two conformant vnl_matrix_fixed (M x N) times (N x O)
// \relatesalso vnl_matrix_fixed
template <class T, unsigned M, unsigned N, unsigned O>
inline
vnl_matrix_fixed<T, M, O> operator*(const vnl_matrix_fixed<T, M, N>& a, const vnl_matrix_fixed<T, N, O>& b)
{
  return vnl_matrix_fixed_mat_mat_mult(a, b);
}


// These overloads for the common case of mixing a fixed with a
// non-fixed. Because the operator* are templated, the fixed will not
// be automatically converted to a non-fixed-ref. These do it for you.

template <class T, unsigned m, unsigned n>
inline vnl_matrix<T> operator+( const vnl_matrix_fixed<T,m,n>& a, const vnl_matrix<T>& b )
{
  return a.as_ref() + b;
}

template <class T, unsigned m, unsigned n>
inline vnl_matrix<T> operator+( const vnl_matrix<T>& a, const vnl_matrix_fixed<T,m,n>& b )
{
  return a + b.as_ref();
}

template <class T, unsigned m, unsigned n>
inline vnl_matrix<T> operator-( const vnl_matrix_fixed<T,m,n>& a, const vnl_matrix<T>& b )
{
  return a.as_ref() - b;
}

template <class T, unsigned m, unsigned n>
inline vnl_matrix<T> operator-( const vnl_matrix<T>& a, const vnl_matrix_fixed<T,m,n>& b )
{
  return a - b.as_ref();
}

template <class T, unsigned m, unsigned n>
inline vnl_matrix<T> operator*( const vnl_matrix_fixed<T,m,n>& a, const vnl_matrix<T>& b )
{
  return a.as_ref() * b;
}

template <class T, unsigned m, unsigned n>
inline vnl_matrix<T> operator*( const vnl_matrix<T>& a, const vnl_matrix_fixed<T,m,n>& b )
{
  return a * b.as_ref();
}

template <class T, unsigned m, unsigned n>
inline vnl_vector<T> operator*( const vnl_matrix_fixed<T,m,n>& a, const vnl_vector<T>& b )
{
  return a.as_ref() * b;
}

template <class T, unsigned n>
inline vnl_vector<T> operator*( const vnl_matrix<T>& a, const vnl_vector_fixed<T,n>& b )
{
  return a * b.as_ref();
}


// --- I/O operations ------------------------------------------------------------

template <class T, unsigned m, unsigned n>
inline
std::ostream& operator<< (std::ostream& os, vnl_matrix_fixed<T,m,n> const& mat)
{
  mat.print(os);
  return os;
}

template <class T, unsigned m, unsigned n>
inline
std::istream& operator>> (std::istream& is, vnl_matrix_fixed<T,m,n>& mat)
{
  mat.read_ascii(is);
  return is;
}

//:
// \relatesalso vnl_vector_fixed
template <class T, unsigned m, unsigned n> VNL_TEMPLATE_EXPORT
vnl_matrix_fixed<T,m,n> outer_product(vnl_vector_fixed<T,m> const& a, vnl_vector_fixed<T,n> const& b);

#define VNL_MATRIX_FIXED_INSTANTIATE(T, M, N) \
extern "please include vnl/vnl_matrix_fixed.hxx instead"

#endif // vnl_matrix_fixed_h_
