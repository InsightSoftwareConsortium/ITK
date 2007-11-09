// This is core/vnl/vnl_matrix.h
#ifndef vnl_matrix_h_
#define vnl_matrix_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief An ordinary mathematical matrix

#include <vcl_iosfwd.h>
#include <vnl/vnl_tag.h>
#include <vnl/vnl_c_vector.h>
#include <vnl/vnl_config.h>
#ifndef NDEBUG
# if VNL_CONFIG_CHECK_BOUNDS
#  include <vnl/vnl_error.h>
#  include <vcl_cassert.h>
# endif
#else
# undef VNL_CONFIG_CHECK_BOUNDS
# define VNL_CONFIG_CHECK_BOUNDS 0
# undef ERROR_CHECKING
#endif

export template <class T> class vnl_vector;
export template <class T> class vnl_matrix;

//--------------------------------------------------------------------------------

#ifndef DOXYGEN_SHOULD_SKIP_THIS
#define v vnl_vector<T>
#define m vnl_matrix<T>
#endif // DOXYGEN_SHOULD_SKIP_THIS
template <class T> m operator+(T const&, m const&);
template <class T> m operator-(T const&, m const&);
template <class T> m operator*(T const&, m const&);
template <class T> m element_product(m const&, m const&);
template <class T> m element_quotient(m const&, m const&);
template <class T> T dot_product(m const&, m const&);
template <class T> T inner_product(m const&, m const&);
template <class T> T cos_angle(m const&, m const& );
template <class T> vcl_ostream& operator<<(vcl_ostream&, m const&);
template <class T> vcl_istream& operator>>(vcl_istream&, m&);
#undef v
#undef m

//--------------------------------------------------------------------------------

enum vnl_matrix_type
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
class vnl_matrix
{
 public:
  //: Default constructor creates an empty matrix of size 0,0.
  vnl_matrix() :
    num_rows(0),
    num_cols(0),
    data(0)
  {
  }

  //: Construct a matrix of size r rows by c columns
  // Contents are unspecified.
  // Complexity $O(1)$
  vnl_matrix(unsigned r, unsigned c);                           // r rows, c cols.

  //: Construct a matrix of size r rows by c columns, and all emelemnts equal to v0
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
  { that.num_cols=that.num_rows=0; that.data=0; } // "*this" now uses "that"'s data.
// </internal>
#endif

  //: Matrix destructor
  ~vnl_matrix();

// Basic 2D-Array functionality-------------------------------------------

  //: Return number of rows
  unsigned rows()    const { return num_rows; }

  //: Return number of columns
  // A synonym for cols()
  unsigned columns()  const { return num_cols; }

  //: Return number of columns
  // A synonym for columns()
  unsigned cols()    const { return num_cols; }

  //: Return number of elements
  // This equals rows() * cols()
  unsigned size()    const { return rows()*cols(); }

  //: set element with boundary checks if error checking is on.
  void put(unsigned r, unsigned c, T const&);

  //: get element with boundary checks if error checking is on.
  T    get(unsigned r, unsigned c) const;

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


// Filling and copying------------------------------------------------

  //: Set all elements of matrix to specified value.
  // Complexity $O(r.c)$
  void fill(T const&);

  //: Set all diagonal elements of matrix to specified value.
  // Complexity $O(\min(r,c))$
  void fill_diagonal(T const&);

  //: Fill (laminate) this matrix with the given data.
  // We assume that p points to a contiguous rows*cols array, stored rowwise.
  void copy_in(T const *);

  //: Fill (laminate) this matrix with the given data.
  // A synonym for copy_in()
  void set(T const *d) { copy_in(d); }

  //: Fill the given array with this matrix.
  // We assume that p points to a contiguous rows*cols array, stored rowwise.
  // No bounds checking on the array.
  void copy_out(T *) const;


  //: Set all elements to value v
  // Complexity $O(r.c)$
  vnl_matrix<T>& operator=(T const&v) { fill(v); return *this; }

  //: Copies all elements of rhs matrix into lhs matrix.
  // Complexity $O(\min(r,c))$
  vnl_matrix<T>& operator=(vnl_matrix<T> const&);

// Arithmetic ----------------------------------------------------
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

  //: Return transpose
  vnl_matrix<T> transpose() const;

  //: Return conjugate transpose
  vnl_matrix<T> conjugate_transpose() const;

  //: Set values of this matrix to those of M, starting at [top,left]
  vnl_matrix<T>& update(vnl_matrix<T> const&, unsigned top=0, unsigned left=0);

  //: Set the elements of the i'th column to v[j]  (No bounds checking)
  void set_column(unsigned i, T const * v);

  //: Set the elements of the i'th column to value
  void set_column(unsigned i, T value );

  //: Set j-th column to v
  void set_column(unsigned j, vnl_vector<T> const& v);

  //: Set columns to those in M, starting at starting_column
  void set_columns(unsigned starting_column, vnl_matrix<T> const& M);

  //: Set the elements of the i'th row to v[j]  (No bounds checking)
  void set_row(unsigned i, T const * v);

  //: Set the elements of the i'th row to value
  void set_row(unsigned i, T value );

  //: Set the i-th row
  void set_row(unsigned i, vnl_vector<T> const&);

  //: Extract a sub-matrix of size r x c, starting at (top,left)
  //  Thus it contains elements  [top,top+r-1][left,left+c-1]
  vnl_matrix<T> extract(unsigned r, unsigned c,
                        unsigned top=0, unsigned left=0) const;

  //: Get a vector equal to the given row
  vnl_vector<T> get_row(unsigned r) const;

  //: Get a vector equal to the given column
  vnl_vector<T> get_column(unsigned c) const;

  //: Get n rows beginning at rowstart
  vnl_matrix<T> get_n_rows(unsigned rowstart, unsigned n) const;

  //: Get n columns beginning at colstart
  vnl_matrix<T> get_n_columns(unsigned colstart, unsigned n) const;

  // mutators

  //: Set this matrix to an identity matrix
  //  Abort if the matrix is not square
  void set_identity();

  //: Transpose this matrix efficiently
  void inplace_transpose();

  //: Reverse order of rows.
  void flipud();
  //: Reverse order of columns.
  void fliplr();

  //: Normalize each row so it is a unit vector
  //  Zero rows are ignored
  void normalize_rows();

  //: Normalize each column so it is a unit vector
  //  Zero columns are ignored
  void normalize_columns();

  //: Scale elements in given row by a factor of T
  void scale_row(unsigned row, T value);

  //: Scale elements in given column by a factor of T
  void scale_column(unsigned col, T value);

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

  //: Return true if finite
  bool is_finite() const;

  //: Return true if matrix contains NaNs
  bool has_nans() const;

  //: abort if size is not as expected
  // This function does or tests nothing if NDEBUG is defined
  void assert_size(unsigned r, unsigned c) const
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

  //: Read a vnl_matrix from an ascii vcl_istream, automatically determining file size if the input matrix has zero size.
  static vnl_matrix<T> read(vcl_istream& s);

  // : Read a vnl_matrix from an ascii vcl_istream, automatically determining file size if the input matrix has zero size.
  bool read_ascii(vcl_istream& s);

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
  iterator       begin() { return data?data[0]:0; }
  //: Iterator pointing to element beyond end of data
  iterator       end() { return data?data[0]+num_rows*num_cols:0; }

  //: Const iterators
  typedef T const *const_iterator;
  //: Iterator pointing to start of data
  const_iterator begin() const { return data?data[0]:0; }
  //: Iterator pointing to element beyond end of data
  const_iterator end() const { return data?data[0]+num_rows*num_cols:0; }

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
  void print(vcl_ostream& os) const;

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

#if VCL_NEED_FRIEND_FOR_TEMPLATE_OVERLOAD
# define v vnl_vector<T>
# define m vnl_matrix<T>
  friend m operator+         VCL_NULL_TMPL_ARGS (T const&, m const&);
  friend m operator-         VCL_NULL_TMPL_ARGS (T const&, m const&);
  friend m operator*         VCL_NULL_TMPL_ARGS (T const&, m const&);
  friend m element_product   VCL_NULL_TMPL_ARGS (m const&, m const&);
  friend m element_quotient  VCL_NULL_TMPL_ARGS (m const&, m const&);
  friend T dot_product       VCL_NULL_TMPL_ARGS (m const&, m const&);
  friend T inner_product     VCL_NULL_TMPL_ARGS (m const&, m const&);
  friend T cos_angle         VCL_NULL_TMPL_ARGS (m const&, m const&);
  friend vcl_ostream& operator<< VCL_NULL_TMPL_ARGS (vcl_ostream&, m const&);
  friend vcl_istream& operator>> VCL_NULL_TMPL_ARGS (vcl_istream&, m&);
# undef v
# undef m
#endif

  // inline function template instantiation hack for gcc 2.97 -- fsm
  static void inline_function_tickler();
};


// Definitions of inline functions.


//: Returns the value of the element at specified row and column. O(1).
// Checks for valid range of indices.

template<class T>
inline T vnl_matrix<T>::get(unsigned row, unsigned column) const
{
#ifdef ERROR_CHECKING
  if (row >= this->num_rows)                   // If invalid size specified
    vnl_error_matrix_row_index("get", row);    // Raise exception
  if (column >= this->num_cols)                // If invalid size specified
    vnl_error_matrix_col_index("get", column); // Raise exception
#endif
  return this->data[row][column];
}

//: Puts value into element at specified row and column. O(1).
// Checks for valid range of indices.

template<class T>
inline void vnl_matrix<T>::put(unsigned row, unsigned column, T const& value)
{
#ifdef ERROR_CHECKING
  if (row >= this->num_rows)                   // If invalid size specified
    vnl_error_matrix_row_index("put", row);    // Raise exception
  if (column >= this->num_cols)                // If invalid size specified
    vnl_error_matrix_col_index("put", column); // Raise exception
#endif
  this->data[row][column] = value;             // Assign data value
}


// non-member arithmetical operators.

//:
// \relates vnl_matrix
template<class T>
inline vnl_matrix<T> operator*(T const& value, vnl_matrix<T> const& m)
{
  return vnl_matrix<T>(m, value, vnl_tag_mul());
}

//:
// \relates vnl_matrix
template<class T>
inline vnl_matrix<T> operator+(T const& value, vnl_matrix<T> const& m)
{
  return vnl_matrix<T>(m, value, vnl_tag_add());
}

//: Swap two matrices
// \relates vnl_matrix
template<class T>
inline void swap(vnl_matrix<T> &A, vnl_matrix<T> &B) { A.swap(B); }


#endif // vnl_matrix_h_
