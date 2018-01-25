// This is core/vnl/vnl_matrix.h
#ifndef vnl_matrix_h_
#define vnl_matrix_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
#ifdef __INTEL_COMPILER
#pragma warning disable 444
#endif
//:
// \file
// \brief An ordinary mathematical matrix
// \verbatim
//  Modifications
//   Apr 21, 1989 - MBN - Initial design and implementation
//   Jun 22, 1989 - MBN - Removed non-destructive methods
//   Aug 09, 1989 - LGO - Inherit from Generic
//   Aug 20, 1989 - MBN - Changed template usage to reflect new syntax
//   Sep 11, 1989 - MBN - Added conditional exception handling and base class
//   Oct 05, 1989 - LGO - Don't re-allocate data in operator= when same size
//   Oct 19, 1989 - LGO - Add extra parameter to varargs constructor
//   Oct 19, 1989 - MBN - Added optional argument to set_compare method
//   Dec 08, 1989 - LGO - Allocate column data in one chunk
//   Dec 08, 1989 - LGO - Clean-up get and put, add const everywhere.
//   Dec 19, 1989 - LGO - Remove the map and reduce methods
//   Feb 22, 1990 - MBN - Changed size arguments from int to unsigned int
//   Jun 30, 1990 - MJF - Added base class name to constructor initializer
//   Feb 21, 1992 - VDN - New lite version
//   May 05, 1992 - VDN - Use envelope to avoid unnecessary copying
//   Sep 30, 1992 - VDN - Matrix inversion with singular value decomposition
//   Aug 21, 1996 - AWF - set_identity, normalize_rows, scale_row.
//   Sep 30, 1996 - AWF - set_row/column methods. Const-correct data_block().
//   14 Feb 1997  - AWF - get_n_rows, get_n_columns.
//   20 Mar 1997  - PVR - get_row, get_column.
//   24-Oct-2010 - Peter Vanroose - mutators and filling methods now return *this
//   18-Jan-2011 - Peter Vanroose - added methods set_diagonal() & get_diagonal()
// \endverbatim

#include <iosfwd>
#include <vcl_compiler.h>
#include <vnl/vnl_tag.h>
#include <vnl/vnl_c_vector.h>
#include <vnl/vnl_config.h>
#include <vnl/vnl_error.h>
#ifndef NDEBUG
# if VNL_CONFIG_CHECK_BOUNDS
#  include <vcl_cassert.h>
# endif
#else
# undef VNL_CONFIG_CHECK_BOUNDS
# define VNL_CONFIG_CHECK_BOUNDS 0
# undef ERROR_CHECKING
#endif
#include "vnl/vnl_export.h"

VCL_TEMPLATE_EXPORT template <class T> class vnl_vector;
VCL_TEMPLATE_EXPORT template <class T> class vnl_matrix;

//--------------------------------------------------------------------------------

#ifndef DOXYGEN_SHOULD_SKIP_THIS
#define v vnl_vector<T>
#define m vnl_matrix<T>
#endif // DOXYGEN_SHOULD_SKIP_THIS
template <class T> VNL_TEMPLATE_EXPORT m operator+(T const&, m const&);
template <class T> VNL_TEMPLATE_EXPORT m operator-(T const&, m const&);
template <class T> VNL_TEMPLATE_EXPORT m operator*(T const&, m const&);
template <class T> VNL_TEMPLATE_EXPORT m element_product(m const&, m const&);
template <class T> VNL_TEMPLATE_EXPORT m element_quotient(m const&, m const&);
template <class T> VNL_TEMPLATE_EXPORT T dot_product(m const&, m const&);
template <class T> VNL_TEMPLATE_EXPORT T inner_product(m const&, m const&);
template <class T> VNL_TEMPLATE_EXPORT T cos_angle(m const&, m const& );
template <class T> VNL_TEMPLATE_EXPORT std::ostream& operator<<(std::ostream&, m const&);
template <class T> VNL_TEMPLATE_EXPORT std::istream& operator>>(std::istream&, m&);
#undef v
#undef m

//--------------------------------------------------------------------------------

enum VNL_TEMPLATE_EXPORT vnl_matrix_type
{
  vnl_matrix_null,
  vnl_matrix_identity
};

//:  An ordinary mathematical matrix
// The vnl_matrix<T> class implements two-dimensional arithmetic
// matrices  for  a user-specified numeric data type. Using the
// parameterized types facility of C++,  it  is  possible,  for
// example, for the user to create a matrix of rational numbers
// by parameterizing the vnl_matrix class over the Rational  class.
// The  only  requirement  for the type is that it supports the
// basic arithmetic operators.
//
// Note: Unlike   the   other   sequence   classes,   the
// vnl_matrix<T>  class is fixed-size. It will not grow once the
// size has been specified to the constructor or changed by the
// assignment  or  multiplication  operators.  The vnl_matrix<T>
// class is row-based with addresses of rows being cached,  and
// elements accessed as m[row][col].
//
// Note: The matrix can, however, be resized using the set_size(nr,nc) function.
//
// Note: Indexing of the matrix is zero-based, so the top-left element is M(0,0).
//
// Note: Inversion of matrix M, and other operations such as solving systems of linear
// equations are handled by the matrix decomposition classes in vnl/algo, such
// as matrix_inverse, svd, qr etc.
//
// Note: Use a vnl_vector<T> with these matrices.

template<class T>
class VNL_TEMPLATE_EXPORT vnl_matrix
{
 public:
  //: Default constructor creates an empty matrix of size 0,0.
  vnl_matrix() :
    num_rows(0),
    num_cols(0),
    data(VXL_NULLPTR)
  {
  }

  //: Construct a matrix of size r rows by c columns
  // Contents are unspecified.
  // Complexity $O(1)$
  vnl_matrix(unsigned r, unsigned c);                           // r rows, c cols.

  //: Construct a matrix of size r rows by c columns, and all elements equal to v0
  // Complexity $O(r.c)$
  vnl_matrix(unsigned r, unsigned c, T const& v0);              // r rows, c cols, value v0.

  //: Construct a matrix of size r rows by c columns, with a special type
  // Contents are specified by t
  // Complexity $O(r.c)$
  vnl_matrix(unsigned r, unsigned c, vnl_matrix_type t);        // r rows, c cols, special type

  //: Construct a matrix of size r rows by c columns, initialised by an automatic array
  // The first n elements, are initialised row-wise, to values.
  // Complexity $O(n)$
  vnl_matrix(unsigned r, unsigned c, unsigned n, T const values[]);  // use automatic arrays.

  //: Construct a matrix of size r rows by c columns, initialised by a memory block
  // The values are initialise row wise from the data.
  // Complexity $O(r.c)$
  vnl_matrix(T const* data_block, unsigned r, unsigned c);      // fill row-wise.

  //: Copy construct a matrix
  // Complexity $O(r.c)$
  vnl_matrix(vnl_matrix<T> const&);                             // from another matrix.

#ifndef VXL_DOXYGEN_SHOULD_SKIP_THIS
// <internal>
  // These constructors are here so that operator* etc can take
  // advantage of the C++ return value optimization.
  vnl_matrix(vnl_matrix<T> const &, vnl_matrix<T> const &, vnl_tag_add); // M + M
  vnl_matrix(vnl_matrix<T> const &, vnl_matrix<T> const &, vnl_tag_sub); // M - M
  vnl_matrix(vnl_matrix<T> const &, T,                     vnl_tag_mul); // M * s
  vnl_matrix(vnl_matrix<T> const &, T,                     vnl_tag_div); // M / s
  vnl_matrix(vnl_matrix<T> const &, T,                     vnl_tag_add); // M + s
  vnl_matrix(vnl_matrix<T> const &, T,                     vnl_tag_sub); // M - s
  vnl_matrix(vnl_matrix<T> const &, vnl_matrix<T> const &, vnl_tag_mul); // M * M
  vnl_matrix(vnl_matrix<T> &that, vnl_tag_grab)
    : num_rows(that.num_rows), num_cols(that.num_cols), data(that.data)
  { that.num_cols=that.num_rows=0; that.data=VXL_NULLPTR; } // "*this" now uses "that"'s data.
// </internal>
#endif

  //: Matrix destructor
  ~vnl_matrix();

// Basic 2D-Array functionality-------------------------------------------

  //: Return the total number of elements stored by the matrix.
  // This equals rows() * cols()
  inline unsigned int size() const { return this->num_rows*this->num_cols; }

  //: Return the number of rows.
  inline unsigned int rows() const { return this->num_rows; }

  //: Return the number of columns.
  // A synonym for columns().
  inline unsigned int cols() const { return this->num_cols; }

  //: Return the number of columns.
  // A synonym for cols().
  inline unsigned int columns() const { return this->num_cols; }

  //: set element with boundary checks if error checking is on.
  inline void put(unsigned r, unsigned c, T const&);

  //: get element with boundary checks if error checking is on.
  inline T get(unsigned r, unsigned c) const;

  //: return pointer to given row
  // No boundary checking here.
  T       * operator[](unsigned r) { return data[r]; }

  //: return pointer to given row
  // No boundary checking here.
  T const * operator[](unsigned r) const { return data[r]; }

  //: Access an element for reading or writing
  // There are assert style boundary checks - #define NDEBUG to turn them off.
  T       & operator()(unsigned r, unsigned c)
  {
#if VNL_CONFIG_CHECK_BOUNDS
    assert(r<rows());   // Check the row index is valid
    assert(c<cols());   // Check the column index is valid
#endif
    return this->data[r][c];
  }

  //: Access an element for reading
  // There are assert style boundary checks - #define NDEBUG to turn them off.
  T const & operator()(unsigned r, unsigned c) const
  {
#if VNL_CONFIG_CHECK_BOUNDS
    assert(r<rows());   // Check the row index is valid
    assert(c<cols());   // Check the column index is valid
#endif
    return this->data[r][c];
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
  //     f(vnl_matrix<double>(5,5,1.0).normalize_columns());
  //  \endcode
  vnl_matrix& fill(T const&);

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
  //     f(vnl_matrix<double>(3,3).fill_diagonal(5));
  //  \endcode
  vnl_matrix& fill_diagonal(T const&);

  //: Sets the diagonal elements of this matrix to the specified list of values.
  //  Returning "*this" allows "chaining" two or more operations: see the
  //  reasoning (and the examples) in the documentation for method
  //  fill_diagonal().
  vnl_matrix& set_diagonal(vnl_vector<T> const&);

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
  //     f(vnl_matrix<double>(3,3).copy_in(array));
  //  \endcode
  vnl_matrix& copy_in(T const *);

  //: Fills (laminates) this matrix with the given data, then returns it.
  // A synonym for copy_in()
  vnl_matrix& set(T const *d) { return copy_in(d); }

  //: Fills the given array with this matrix.
  //  We assume that the argument points to a contiguous rows*cols array, stored rowwise.
  // No bounds checking on the array.
  void copy_out(T *) const;

  //: Set all elements to value v
  // Complexity $O(r.c)$
  vnl_matrix<T>& operator=(T const&v) { fill(v); return *this; }

  //: Copies all elements of rhs matrix into lhs matrix.
  // Complexity $O(\min(r,c))$
  vnl_matrix<T>& operator=(vnl_matrix<T> const&);

  // ----------------------- Arithmetic --------------------------------
  // note that these functions should not pass scalar as a const&.
  // Look what would happen to A /= A(0,0).

  //: Add rhs to each element of lhs matrix in situ
  vnl_matrix<T>& operator+=(T value);

  //: Subtract rhs from each element of lhs matrix in situ
  vnl_matrix<T>& operator-=(T value);

  //: Scalar multiplication in situ of lhs matrix  by rhs
  vnl_matrix<T>& operator*=(T value);

  //: Scalar division of lhs matrix  in situ by rhs
  vnl_matrix<T>& operator/=(T value);

  //: Add rhs to lhs  matrix in situ
  vnl_matrix<T>& operator+=(vnl_matrix<T> const&);
  //: Subtract rhs from lhs matrix in situ
  vnl_matrix<T>& operator-=(vnl_matrix<T> const&);
  //: Multiply lhs matrix in situ by rhs
  vnl_matrix<T>& operator*=(vnl_matrix<T> const&rhs) { return *this = (*this) * rhs; }

  //: Negate all elements of matrix
  vnl_matrix<T> operator-() const;


  //: Add rhs to each element of lhs matrix and return result in new matrix
  vnl_matrix<T> operator+(T const& v) const { return vnl_matrix<T>(*this, v, vnl_tag_add()); }

  //: Subtract rhs from each element of lhs matrix and return result in new matrix
  vnl_matrix<T> operator-(T const& v) const { return vnl_matrix<T>(*this, v, vnl_tag_sub()); }

  //: Scalar multiplication of lhs matrix by rhs  and return result in new matrix
  vnl_matrix<T> operator*(T const& v) const { return vnl_matrix<T>(*this, v, vnl_tag_mul()); }

  //: Scalar division of lhs matrix by rhs and return result in new matrix
  vnl_matrix<T> operator/(T const& v) const { return vnl_matrix<T>(*this, v, vnl_tag_div()); }

  //: Matrix add rhs to lhs matrix and return result in new matrix
  vnl_matrix<T> operator+(vnl_matrix<T> const& rhs) const { return vnl_matrix<T>(*this, rhs, vnl_tag_add()); }
  //: Matrix subtract rhs from lhs and return result in new matrix
  vnl_matrix<T> operator-(vnl_matrix<T> const& rhs) const { return vnl_matrix<T>(*this, rhs, vnl_tag_sub()); }
  //: Matrix multiply lhs by rhs matrix and return result in new matrix
  vnl_matrix<T> operator*(vnl_matrix<T> const& rhs) const { return vnl_matrix<T>(*this, rhs, vnl_tag_mul()); }

  ////--------------------------- Additions ----------------------------

  //: Make a new matrix by applying function to each element.
  vnl_matrix<T> apply(T (*f)(T)) const;

  //: Make a new matrix by applying function to each element.
  vnl_matrix<T> apply(T (*f)(T const&)) const;

  //: Make a vector by applying a function across rows.
  vnl_vector<T> apply_rowwise(T (*f)(vnl_vector<T> const&)) const;

  //: Make a vector by applying a function across columns.
  vnl_vector<T> apply_columnwise(T (*f)(vnl_vector<T> const&)) const;

  //: Return transpose
  vnl_matrix<T> transpose() const;

  //: Return conjugate transpose
  vnl_matrix<T> conjugate_transpose() const;

  //: Set values of this matrix to those of M, starting at [top,left]
  vnl_matrix<T>& update(vnl_matrix<T> const&, unsigned top=0, unsigned left=0);

  //: Set the elements of the i'th column to v[i]  (No bounds checking)
  vnl_matrix& set_column(unsigned i, T const * v);

  //: Set the elements of the i'th column to value, then return *this.
  vnl_matrix& set_column(unsigned i, T value );

  //: Set j-th column to v, then return *this.
  vnl_matrix& set_column(unsigned j, vnl_vector<T> const& v);

  //: Set columns to those in M, starting at starting_column, then return *this.
  vnl_matrix& set_columns(unsigned starting_column, vnl_matrix<T> const& M);

  //: Set the elements of the i'th row to v[i]  (No bounds checking)
  vnl_matrix& set_row(unsigned i, T const * v);

  //: Set the elements of the i'th row to value, then return *this.
  vnl_matrix& set_row(unsigned i, T value );

  //: Set the i-th row
  vnl_matrix& set_row(unsigned i, vnl_vector<T> const&);

  //: Extract a sub-matrix of size r x c, starting at (top,left)
  //  Thus it contains elements  [top,top+r-1][left,left+c-1]
  vnl_matrix<T> extract(unsigned r, unsigned c,
                        unsigned top=0, unsigned left=0) const;

  //: Extract a sub-matrix starting at (top,left)
  //
  //  The output is stored in \a sub_matrix, and it should have the
  //  required size on entry.  Thus the result will contain elements
  //  [top,top+sub_matrix.rows()-1][left,left+sub_matrix.cols()-1]
  void extract ( vnl_matrix<T>& sub_matrix,
                 unsigned top=0, unsigned left=0) const;


  //: Get a vector equal to the given row
  vnl_vector<T> get_row(unsigned r) const;

  //: Get a vector equal to the given column
  vnl_vector<T> get_column(unsigned c) const;

  //: Get a matrix composed of rows from the indices specified in the supplied vector.
  vnl_matrix<T> get_rows(vnl_vector<unsigned int> i) const;

  //: Get a matrix composed of columns from the indices specified in the supplied vector.
  vnl_matrix<T> get_columns(vnl_vector<unsigned int> i) const;

  //: Get n rows beginning at rowstart
  vnl_matrix<T> get_n_rows(unsigned rowstart, unsigned n) const;

  //: Get n columns beginning at colstart
  vnl_matrix<T> get_n_columns(unsigned colstart, unsigned n) const;

  //: Return a vector with the content of the (main) diagonal
  vnl_vector<T> get_diagonal() const;

  //: Flatten row-major (C-style)
  vnl_vector<T> flatten_row_major() const;

  //: Flatten column-major (Fortran-style)
  vnl_vector<T> flatten_column_major() const;

  // ==== mutators ====

  //: Sets this matrix to an identity matrix, then returns "*this".
  //  Returning "*this" allows e.g. passing an identity matrix as argument to
  //  a function f, without having to name the constructed matrix:
  //  \code
  //     f(vnl_matrix<double>(5,5).set_identity());
  //  \endcode
  //  Returning "*this" also allows "chaining" two or more operations:
  //  e.g., to set a 3x3 matrix to [3 0 0][0 2 0][0 0 1], one could say
  //  \code
  //     M.set_identity().scale_row(0,3).scale_column(1,2);
  //  \endcode
  //  If the matrix is not square, anyhow set main diagonal to 1, the rest to 0.
  vnl_matrix& set_identity();

  //: Transposes this matrix efficiently, and returns it.
  //  Returning "*this" allows "chaining" two or more operations:
  //  e.g., to fill a square matrix column-wise, fill it rowwise then transpose:
  //  \code
  //     M.copy_in(array).inplace_transpose();
  //  \endcode
  vnl_matrix& inplace_transpose();

  //: Reverses the order of rows, and returns "*this".
  //  Returning "*this" allows "chaining" two or more operations:
  //  e.g., to flip both up-down and left-right, one could just say
  //  \code
  //     M.flipud().fliplr();
  //  \endcode
  vnl_matrix& flipud();

  //: Reverses the order of columns, and returns "*this".
  //  Returning "*this" allows "chaining" two or more operations:
  //  e.g., to flip both up-down and left-right, one could just say
  //  \code
  //     M.flipud().fliplr();
  //  \endcode
  vnl_matrix& fliplr();

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
  //     f(vnl_matrix<double>(5,5,1.0).normalize_rows());
  //  \endcode
  vnl_matrix& normalize_rows();

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
  //     f(vnl_matrix<double>(5,5,1.0).normalize_columns());
  //  \endcode
  vnl_matrix& normalize_columns();

  //: Scales elements in given row by a factor T, and returns "*this".
  //  Returning "*this" allows "chaining" two or more operations:
  //  e.g., to set a 3x3 matrix to [3 0 0][0 2 0][0 0 1], one could say
  //  \code
  //     M.set_identity().scale_row(0,3).scale_column(1,2);
  //  \endcode
  vnl_matrix& scale_row(unsigned row, T value);

  //: Scales elements in given column by a factor T, and returns "*this".
  //  Returning "*this" allows "chaining" two or more operations:
  //  e.g., to set a 3x3 matrix to [3 0 0][0 2 0][0 0 1], one could say
  //  \code
  //     M.set_identity().scale_row(0,3).scale_column(1,2);
  //  \endcode
  vnl_matrix& scale_column(unsigned col, T value);

  //: Swap this matrix with that matrix
  void swap(vnl_matrix<T> & that);

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
  bool empty() const { return !data || !num_rows || !num_cols; }

  //:  Return true if all elements equal to identity.
  bool is_identity() const;

  //:  Return true if all elements equal to identity, within given tolerance
  bool is_identity(double tol) const;

  //: Return true if all elements equal to zero.
  bool is_zero() const;

  //: Return true if all elements equal to zero, within given tolerance
  bool is_zero(double tol) const;

  //:  Return true if all elements of both matrices are equal, within given tolerance
  bool is_equal(vnl_matrix<T> const& rhs, double tol) const;

  //: Return true if finite
  bool is_finite() const;

  //: Return true if matrix contains NaNs
  bool has_nans() const;

  //: abort if size is not as expected
  // This function does or tests nothing if NDEBUG is defined
  void assert_size(unsigned VXL_USED_IN_DEBUG(r), unsigned VXL_USED_IN_DEBUG(c)) const
  {
#ifndef NDEBUG
    assert_size_internal(r, c);
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

  //: Read a vnl_matrix from an ascii std::istream, automatically determining file size if the input matrix has zero size.
  static vnl_matrix<T> read(std::istream& s);

  // : Read a vnl_matrix from an ascii std::istream, automatically determining file size if the input matrix has zero size.
  bool read_ascii(std::istream& s);

  //--------------------------------------------------------------------------------

  //: Access the contiguous block storing the elements in the matrix row-wise. O(1).
  // 1d array, row-major order.
  T const* data_block() const { return data[0]; }

  //: Access the contiguous block storing the elements in the matrix row-wise. O(1).
  // 1d array, row-major order.
  T      * data_block() { return data[0]; }

  //: Access the 2D array, so that elements can be accessed with array[row][col] directly.
  //  2d array, [row][column].
  T const* const* data_array() const { return data; }

  //: Access the 2D array, so that elements can be accessed with array[row][col] directly.
  //  2d array, [row][column].
  T      *      * data_array() { return data; }

  typedef T element_type;

  //: Iterators
  typedef T       *iterator;
  //: Iterator pointing to start of data
  iterator       begin() { return data?data[0]:VXL_NULLPTR; }
  //: Iterator pointing to element beyond end of data
  iterator       end() { return data?data[0]+num_rows*num_cols:VXL_NULLPTR; }

  //: Const iterators
  typedef T const *const_iterator;
  //: Iterator pointing to start of data
  const_iterator begin() const { return data?data[0]:VXL_NULLPTR; }
  //: Iterator pointing to element beyond end of data
  const_iterator end() const { return data?data[0]+num_rows*num_cols:VXL_NULLPTR; }

  //: Return a reference to this.
  // Useful in code which would prefer not to know if its argument
  // is a matrix, matrix_ref or a matrix_fixed.  Note that it doesn't
  // return a matrix_ref, so it's only useful in templates or macros.
  vnl_matrix<T> const& as_ref() const { return *this; }

  //: Return a reference to this.
  vnl_matrix<T>&       as_ref()       { return *this; }

  //--------------------------------------------------------------------------------

  //: Return true if *this == rhs
  bool operator_eq(vnl_matrix<T> const & rhs) const;

  //: Equality operator
  bool operator==(vnl_matrix<T> const &that) const { return  this->operator_eq(that); }

  //: Inequality operator
  bool operator!=(vnl_matrix<T> const &that) const { return !this->operator_eq(that); }

  //: Print matrix to os in some hopefully sensible format
  void print(std::ostream& os) const;

  //: Make the matrix as if it had been default-constructed.
  void clear();

  //: Resize to r rows by c columns. Old data lost.
  // Returns true if size changed.
  bool set_size(unsigned r, unsigned c);

//--------------------------------------------------------------------------------

 protected:
  unsigned num_rows;   // Number of rows
  unsigned num_cols;   // Number of columns
  T** data;            // Pointer to the vnl_matrix

#if VCL_HAS_SLICED_DESTRUCTOR_BUG
  // Since this bug exists, we need a flag that can be set during
  // construction to tell our destructor whether we own data.
  char vnl_matrix_own_data;
#endif

  void assert_size_internal(unsigned r, unsigned c) const;
  void assert_finite_internal() const;

  //: Delete data
  void destroy();

  // inline function template instantiation hack for gcc 2.97 -- fsm
  static void inline_function_tickler();
};


// Definitions of inline functions.


//: Returns the value of the element at specified row and column. O(1).
// Checks for valid range of indices.

template<class T>
inline T vnl_matrix<T>
::get(unsigned r, unsigned c) const
{
#if VNL_CONFIG_CHECK_BOUNDS
  if (r >= this->num_rows)                // If invalid size specified
    vnl_error_matrix_row_index("get", r); // Raise exception
  if (c >= this->num_cols)                // If invalid size specified
    vnl_error_matrix_col_index("get", c); // Raise exception
#endif
  return this->data[r][c];
}

//: Puts value into element at specified row and column. O(1).
// Checks for valid range of indices.

template<class T>
inline void vnl_matrix<T>
::put(unsigned r, unsigned c, T const& v)
{
#if VNL_CONFIG_CHECK_BOUNDS
  if (r >= this->num_rows)                // If invalid size specified
    vnl_error_matrix_row_index("put", r); // Raise exception
  if (c >= this->num_cols)                // If invalid size specified
    vnl_error_matrix_col_index("put", c); // Raise exception
#endif
  this->data[r][c] = v;             // Assign data value
}


// non-member arithmetical operators.

//:
// \relatesalso vnl_matrix
template<class T>
inline vnl_matrix<T> operator*(T const& value, vnl_matrix<T> const& m)
{
  return vnl_matrix<T>(m, value, vnl_tag_mul());
}

//:
// \relatesalso vnl_matrix
template<class T>
inline vnl_matrix<T> operator+(T const& value, vnl_matrix<T> const& m)
{
  return vnl_matrix<T>(m, value, vnl_tag_add());
}

//: Swap two matrices
// \relatesalso vnl_matrix
template<class T>
inline void swap(vnl_matrix<T> &A, vnl_matrix<T> &B) { A.swap(B); }


#endif // vnl_matrix_h_
