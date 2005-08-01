// This is core/vnl/vnl_matrix_fixed_ref.h
#ifndef vnl_matrix_fixed_ref_h_
#define vnl_matrix_fixed_ref_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief Fixed size stack-stored vnl_matrix
//
//  > > From: Amitha Perera [mailto:perera@cs.rpi.edu]
//  > > Sent: Monday, October 07, 2002 3:18 PM
//  > > Subject: vnl_vector_fixed_ref
//  > >
//  > > I'm working on separating vnl_vector and vnl_vector_fixed in the VXL
//  > > tree, as I mailed a while ago to the vxl-maintainers list. I noticed
//  > > that you'd committed a vnl_vector_fixed_ref class which doesn't seem
//  > > to provide any additional functionality over vnl_vector_ref. May I
//  > > remove it, or is there some use for it?
//  > >
//  > > FYI, the author is listed as "Paul P. Smyth, Vicon Motion
//  > > Systems Ltd."
//  > > and the comment is dated 02 May 2001.
//
//  Paul Smyth <paul.smyth@vicon.com> writes:
//  > The rationale behind it was that I had some (fast) algorithms for
//  > matrix/vector operations that made use of compile-time knowledge of the
//  > vector and matrix sizes.
//  > This was typically appropriate when trying to interpret a fixed-size
//  > subvector within a large vector of parameters as e.g. a translation.
//  >
//  > As I saw it, the various types of vector possible were: (with their current
//  > names)
//  > - pointer to memory, plus compile-time knowledge of vector size ( T*, and enum{size}) = vnl_vector_fixed_ref
//  > - ownership of memory, plus compile-time size = vnl_vector_fixed
//  > - pointer to memory, plus run-time only knowledge of size (T* and size()) = vnl_vector_ref
//  > - ownership of memory, variably sized = vnl_vector
//  >
//  > I had a conversation with Andrew Fitzgibbon, where he reckoned that the best
//  > thing to do with vnl vectors etc. was to create entirely separate types, and
//  > routines for conversion between them (possibly implicitly), rather that
//  > trying to establish a class hierarchy, which may add too many burdens in
//  > terms of object size for small vectors/matrices.
//  >
//  > Sorry - I've now found the debate on the maintaners list!
//  >
//  > Anyway, I believe that vector_fixed_ref is very necessary, and that you
//  > should be able to convert from a vector_fixed to a vector_fixed_ref - say
//  > using an as_ref() member on vector_fixed or standalone function.
//  > And I believe that for the restructured classes, vector_fixed_ref and
//  > vector_fixed should not be related by inheritance, as that would place an
//  > excessive burden on the size of vector_fixed.
//  >
//  > ------
//  > Another issue - do you have a mechanism for dealing with const data safely?
//  > {
//  >   template<typename T, int n>
//  >   vnl_vector_fixed_ref(T* i_Data);
//  >
//  >   void MyFunction(const vnl_vector<double> & Input)
//  >   {
//  >     // take a reference to the first 3 elements of Input
//  >     vnl_vector_fixed_ref<double,3> ref(Input.begin());
//  >     // compiler error - as making vector_fixed_ref from const
//  > double *
//  >   }
//  > }
//  >
//  > The options appear to be
//  > 1) Make a separate class vnl_vector_fixed_ref_const
//  > 2) Make vnl_vector_fixed_ref so it can be instantiated with
//  > vnl_vector_fixed_ref<double,n> AND vnl_vector_fixed_ref<const double,n>, and
//  > gives appropriate behaviour - would probably require a to_const function
//  > which generates vnl_vector_fixed_ref<const T,n> from
//  > vnl_vector_fixed_ref<T,n>
//  >
//  > ------
//  > Another note is that a number of routines that use vector_fixed currently
//  > (e.g. cross_3d) should really use vector_fixed_ref as an input, because they
//  > should be able to operate on fixed vector references as well as fixed
//  > vectors.
//  >
//  > While I'm at it, has it been decided that the vnl_vector and vnl_vector_ref
//  > classes are to remain unchanged? Because having vnl_vector as the base, and
//  > vnl_vector_ref derived from it is a real pain in the backside. A vector
//  > which may or may not own its own memory is a more general type than one
//  > which does own it's own memory, and having vnl_vector as the base means that
//  > all sorts of nastinesses can happen. Simply, a vector_ref Is-not a type of
//  > vector.
//  > If anything, it should be the other way round.
//  >
//  > void DoAssign(vnl_vector<double> & RefToMemoryIDontOwn, const vnl_vector<double> & NewContents)
//  > {
//  >   RefToMemoryIDontOwn = NewContents;
//  > }
//  >
//  > void DeleteTwice()
//  > {
//  >   vnl_vector<double> vec1(3, 0); // size 3 - news 3*double
//  >   vnl_vector<double> vec2(4,1); // size 4 news 4 * double
//  >   vnl_vector_ref<double> ref_to_1(3,vec1.begin()); // copies pointer
//  >   DoAssign(ref_to_1, vec2); // deletes memory owned by 1, news 4 * double
//  >   // vec1 now points to deleted memory, and will crash when goes out of scope
//  > }
//  >
//  > Maybe that issue isn't on your agenda - but it's a bit of a disaster. I know
//  > that fixing this might break some code.
//  >
//  > ---------
//  > Sorry for rolling all these things into one - I'd be interested to know what
//  > you think. But please don't kill my vnl_vector_ref!
//  >
//  > Paul.
//
//    vnl_matrix_fixed_ref is a fixed-size vnl_matrix for which the data space
//    has been supplied externally.  This is useful for two main tasks:
//
//    (a) Treating some row-based "C" matrix as a vnl_matrix in order to
//    perform vnl_matrix operations on it.
//
//    (b) Declaring a vnl_matrix that uses entirely stack-based storage for the
//    matrix.
//
//    The big warning is that returning a vnl_matrix_fixed_ref pointer will free
//    non-heap memory if deleted through a vnl_matrix pointer.  This should be
//    very difficult though, as vnl_matrix_fixed_ref objects may not be constructed
//    using operator new.  This in turn is plausible as the point is to avoid
//    such calls.
//
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   04 Aug 96
//
// \verbatim
//  Modifications:
//   27-Nov-1996 Peter Vanroose - added default constructor which allocates matrix storage
//    4-Jul-2003 Paul Smyth - general cleanup and rewrite; interface now as vnl_matrix_fixed
//   15-Aug-2003 Peter Vanroose - removed "duplicate" operator=(vnl_matrix_fixed<T,n> const&)
// \endverbatim
//
//-----------------------------------------------------------------------------

#include <vcl_cassert.h>
#include <vcl_iosfwd.h>
#include <vcl_cstring.h> // memcpy()
#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_vector_fixed_ref.h>
#include <vnl/vnl_c_vector.h>

//: Fixed size stack-stored vnl_matrix
// vnl_matrix_fixed_ref is a fixed-size vnl_matrix for which the data space
// has been supplied externally.  This is useful for two main tasks:
//
// (a) Treating some row-based "C" matrix as a vnl_matrix in order to
// perform vnl_matrix operations on it.
//
// (b) Declaring a vnl_matrix that uses entirely stack-based storage for the
// matrix.
//
// The big warning is that returning a vnl_matrix_fixed_ref pointer will free
// non-heap memory if deleted through a vnl_matrix pointer.  This should be
// very difficult though, as vnl_matrix_fixed_ref objects may not be constructed
// using operator new.  This in turn is plausible as the point is to avoid
// such calls.
//

template <class T, unsigned num_rows, unsigned num_cols>
class vnl_matrix_fixed_ref_const
{
 protected:
  const T* data_;
 public:
  vnl_matrix_fixed_ref_const(const vnl_matrix_fixed<T,num_rows,num_cols>& rhs)
    : data_(rhs.data_block())
  {
  }
  explicit vnl_matrix_fixed_ref_const(const T * dataptr)
    : data_(dataptr)
  {
  }
  vnl_matrix_fixed_ref_const(const vnl_matrix_fixed_ref_const<T,num_rows,num_cols> & rhs)
    : data_(rhs.data_)
  {
  }
  //: Get j-th row
  vnl_vector_fixed<T,num_rows> get_row(unsigned row_index) const
  {
    vnl_vector<T> v(num_cols);
    for (unsigned int j = 0; j < num_cols; j++)    // For each element in row
      v[j] = (*this)(row_index,j);
    return v;
  }

  //: Get j-th column
  vnl_vector_fixed<T,num_cols> get_column(unsigned column_index) const
  {
    vnl_vector<T> v(num_rows);
    for (unsigned int j = 0; j < num_rows; j++)
      v[j] = (*this)(j,column_index);
    return v;
  }
  const T * data_block() const { return data_; }

  //: Const iterators
  typedef T const *const_iterator;
  //: Iterator pointing to start of data
  const_iterator begin() const { return data_; }
  //: Iterator pointing to element beyond end of data
  const_iterator end() const { return begin() + this->size(); }

  //: Type defs for iterators
  typedef const T element_type;
  //: Type defs for iterators
  typedef const T       *iterator;

  T const & operator() (unsigned r, unsigned c) const
  {
#if VNL_CONFIG_CHECK_BOUNDS  && (!defined NDEBUG)
    assert(r<num_rows);   // Check the row index is valid
    assert(c<num_cols);   // Check the column index is valid
#endif
    return *(data_ + num_cols * r + c);
  }

  //: return pointer to given row
  // No boundary checking here.
  T const * operator[] (unsigned r) const { return data_ + num_cols * r; }

  //: Return number of rows
  unsigned rows ()    const { return num_rows; }

  //: Return number of columns
  // A synonym for cols()
  unsigned columns ()  const { return num_cols; }

  //: Return number of columns
  // A synonym for columns()
  unsigned cols ()    const { return num_cols; }

  //: Return number of elements
  // This equals rows() * cols()
  unsigned size ()    const { return num_rows*num_cols; }


  //: Print matrix to os in some hopefully sensible format
  void print(vcl_ostream& os) const;

  void copy_out(T *) const;

  ////--------------------------- Additions ----------------------------

  //: Make a new matrix by applying function to each element.
  vnl_matrix_fixed<T,num_rows,num_cols> apply(T (*f)(T)) const;

  //: Make a new matrix by applying function to each element.
  vnl_matrix_fixed<T,num_rows,num_cols> apply(T (*f)(T const&)) const;

  //: Return transpose
  vnl_matrix_fixed<T,num_cols,num_rows> transpose () const;

  //: Return conjugate transpose
  vnl_matrix_fixed<T,num_cols,num_rows> conjugate_transpose () const;

  //: Extract a sub-matrix of size rows x cols, starting at (top,left)
  //  Thus it contains elements  [top,top+rows-1][left,left+cols-1]
  vnl_matrix<T> extract (unsigned rows,  unsigned cols,
                         unsigned top=0, unsigned left=0) const;

  //: Get n rows beginning at rowstart
  vnl_matrix<T> get_n_rows   (unsigned rowstart, unsigned n) const;

  //: Get n columns beginning at colstart
  vnl_matrix<T> get_n_columns(unsigned colstart, unsigned n) const;

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
  bool empty() const { return num_rows==0 && num_cols==0; }

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
  void assert_size(unsigned rows, unsigned cols) const
  {
#ifndef NDEBUG
    assert_size_internal(rows, cols);
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

  static void add( const T* a, const T* b, T* r ) { vnl_matrix_fixed<T,num_rows,num_cols>::add(a,b,r); }
  static void add( const T* a, T b, T* r ) { vnl_matrix_fixed<T,num_rows,num_cols>::add(a,b,r); }
  static void sub( const T* a, const T* b, T* r ) { vnl_matrix_fixed<T,num_rows,num_cols>::sub(a,b,r); }
  static void sub( const T* a, T b, T* r ) { vnl_matrix_fixed<T,num_rows,num_cols>::sub(a,b,r); }
  static void sub( T a, const T* b, T* r ) { vnl_matrix_fixed<T,num_rows,num_cols>::sub(a,b,r); }
  static void mul( const T* a, const T* b, T* r ) { vnl_matrix_fixed<T,num_rows,num_cols>::mul(a,b,r); }
  static void mul( const T* a, T b, T* r ) { vnl_matrix_fixed<T,num_rows,num_cols>::mul(a,b,r); }
  static void div( const T* a, const T* b, T* r ) { vnl_matrix_fixed<T,num_rows,num_cols>::div(a,b,r); }
  static void div( const T* a, T b, T* r ) { vnl_matrix_fixed<T,num_rows,num_cols>::div(a,b,r); }

  static bool equal( const T* a, const T* b ) { return vnl_matrix_fixed<T,num_rows,num_cols>::equal(a,b); }

 private:
  const vnl_matrix_fixed_ref_const<T,num_rows,num_cols> & operator=(const vnl_matrix_fixed_ref_const<T,num_rows,num_cols>& ) const
  {
    assert(!"Assignment is illegal for a vnl_matrix_fixed_ref_const");
    return *this;
  }

  void assert_finite_internal() const;

  void assert_size_internal(unsigned, unsigned) const;
};


template <class T, unsigned num_rows, unsigned num_cols>
class vnl_matrix_fixed_ref : public vnl_matrix_fixed_ref_const<T,num_rows,num_cols>
{
  typedef vnl_matrix_fixed_ref_const<T,num_rows,num_cols> base;

 public:
  // this is the only point where the const_cast happens
  // the base class is used to store the pointer, so that conversion is not necessary
  T * data_block() const {
    return const_cast<T*>(this->data_);
  }
  vnl_matrix_fixed_ref(vnl_matrix_fixed<T,num_rows,num_cols>& rhs)
    : base(rhs.data_block())
  {
  }
  explicit vnl_matrix_fixed_ref(T * dataptr)
    : base(dataptr)
  {
  }

  //: Copy another vnl_matrix_fixed<T,m,n> into this.
  vnl_matrix_fixed_ref const & operator=(const vnl_matrix_fixed_ref_const<T,num_rows,num_cols>& rhs) const
  {
    vcl_memcpy(data_block(), rhs.data_block(), num_rows*num_cols*sizeof(T));
    return *this;
  }

  // Basic 2D-Array functionality-------------------------------------------

  //: set element
  void put (unsigned r, unsigned c, T const& v) { (*this)(r,c) = v; }

  //: get element
  T    get (unsigned r, unsigned c) const { return (*this)(r,c); }

  //: return pointer to given row
  // No boundary checking here.
  T  * operator[] (unsigned r) const { return data_block() + num_cols * r; }


  //: Access an element for reading or writing
  // There are assert style boundary checks - #define NDEBUG to turn them off.
  T       & operator() (unsigned r, unsigned c) const
  {
#if VNL_CONFIG_CHECK_BOUNDS  && (!defined NDEBUG)
    assert(r<num_rows);   // Check the row index is valid
    assert(c<num_cols);   // Check the column index is valid
#endif
    return *(this->data_block() + num_cols * r + c);
  }


  // Filling and copying------------------------------------------------

  //: Set all elements of matrix to specified value.
  // Complexity $O(r.c)$
  void fill (T) const;

  //: Set all diagonal elements of matrix to specified value.
  // Complexity $O(\min(r,c))$
  void fill_diagonal (T) const;

  //: Fill (laminate) this matrix with the given data.
  // We assume that p points to a contiguous rows*cols array, stored rowwise.
  void copy_in(T const *) const;

  //: Fill (laminate) this matrix with the given data.
  // A synonym for copy_in()
  void set(T const *d) const { copy_in(d); }

  //: Fill the given array with this matrix.
  // We assume that p points to a contiguous rows*cols array, stored rowwise.
  // No bounds checking on the array

  //: Transpose this matrix efficiently, if it is a square matrix
  void inplace_transpose() const;


  // Arithmetic ----------------------------------------------------
  // note that these functions should not pass scalar as a const&.
  // Look what would happen to A /= A(0,0).

  //: Add \a s to each element of lhs matrix in situ
  vnl_matrix_fixed_ref const& operator+= (T s) const
  {
    add( data_block(), s, data_block() ); return *this;
  }

  //: Subtract \a s from each element of lhs matrix in situ
  vnl_matrix_fixed_ref const& operator-= (T s) const
  {
    sub( data_block(), s, data_block() ); return *this;
  }

  //:
  vnl_matrix_fixed_ref const& operator*= (T s) const
  {
    mul( data_block(), s, data_block() ); return *this;
  }

  //:
  vnl_matrix_fixed_ref const& operator/= (T s) const
  {
    div( data_block(), s, data_block() ); return *this;
  }

  //:
  vnl_matrix_fixed_ref const & operator+= (vnl_matrix_fixed_ref_const<T,num_rows,num_cols> const& m) const
  {
    add( data_block(), m.data_block(), data_block() ); return *this;
  }

  //:
  vnl_matrix_fixed_ref const& operator+= (vnl_matrix<T> const& m) const
  {
    assert( m.rows() == num_rows && m.cols() == num_cols );
    add( data_block(), m.data_block(), data_block() ); return *this;
  }

  //:
  vnl_matrix_fixed_ref const& operator-= (vnl_matrix_fixed_ref_const<T,num_rows,num_cols> const& m) const
  {
    sub( data_block(), m.data_block(), data_block() ); return *this;
  }

  //:
  vnl_matrix_fixed_ref const& operator-= (vnl_matrix<T> const& m) const
  {
    assert( m.rows() == num_rows && m.cols() == num_cols );
    sub( data_block(), m.data_block(), data_block() ); return *this;
  }

  //: Negate all elements of matrix
  vnl_matrix_fixed<T,num_rows,num_cols> operator- () const
  {
    vnl_matrix_fixed<T,num_rows,num_cols> r;
    sub( T(0), data_block(), r.data_block() );
    return r;
  }

  //:
  vnl_matrix_fixed_ref const& operator*= (vnl_matrix_fixed_ref_const<T,num_cols,num_cols> const& s) const
  {
    vnl_matrix_fixed<T, num_rows, num_cols> out;
    for (unsigned i = 0; i < num_rows; ++i)
      for (unsigned j = 0; j < num_cols; ++j)
      {
        T accum = this->operator()(i,0) * s(0,j);
        for (unsigned k = 1; k < num_cols; ++k)
          accum += this->operator()(i,k) * s(k,j);
        out(i,j) = accum;
      }
    *this = out;
    return *this;
  }

#ifdef VCL_VC60
  template<unsigned o>
  vnl_matrix_fixed<T,num_rows,o> operator*( vnl_matrix_fixed_fake_base<o,num_cols,T> const& mat ) const
  {
    vnl_matrix_fixed<T,num_cols,o> const& b = static_cast<vnl_matrix_fixed<T,num_cols,o> const&>(mat);
    return vnl_matrix_fixed_mat_mat_mult<T,num_rows,num_cols,o>( *this, b );
  }
  vnl_vector_fixed<T, num_rows> operator*( vnl_vector_fixed_ref_const<T, num_cols> const& b) const
  {
    return vnl_matrix_fixed_mat_vec_mult<T,num_rows,num_cols>(*this,b);
  }
#endif


  //: Set values of this matrix to those of M, starting at [top,left]
  vnl_matrix_fixed_ref const & update (vnl_matrix<T> const&, unsigned top=0, unsigned left=0) const;

  //: Set the elements of the i'th column to v[j]  (No bounds checking)
  void set_column(unsigned i, T const * v) const;

  //: Set the elements of the i'th column to value
  void set_column(unsigned i, T value ) const;

  //: Set j-th column to v
  void set_column(unsigned j, vnl_vector<T> const& v) const;

  //: Set columns to those in M, starting at starting_column
  void set_columns(unsigned starting_column, vnl_matrix<T> const& M) const;

  //: Set the elements of the i'th row to v[j]  (No bounds checking)
  void set_row   (unsigned i, T const * v) const;

  //: Set the elements of the i'th row to value
  void set_row   (unsigned i, T value ) const;

  //: Set the i-th row
  void set_row   (unsigned i, vnl_vector<T> const&) const;


  // mutators

  //: Set this matrix to an identity matrix
  //  Abort if the matrix is not square
  void set_identity() const;

  //: Reverse order of rows.
  void flipud() const;

  //: Reverse order of columns.
  void fliplr() const;

  //: Normalize each row so it is a unit vector
  //  Zero rows are ignored
  void normalize_rows() const;

  //: Normalize each column so it is a unit vector
  //  Zero columns are ignored
  void normalize_columns() const;

  //: Scale elements in given row by a factor of T
  void scale_row   (unsigned row, T value) const;

  //: Scale elements in given column by a factor of T
  void scale_column(unsigned col, T value) const;


  ////----------------------- Input/Output ----------------------------

  // : Read a vnl_matrix from an ascii vcl_istream, automatically determining file size if the input matrix has zero size.
  bool read_ascii(vcl_istream& s) const;


  //----------------------------------------------------------------------
  // Conversion to vnl_matrix_ref.

  // The const version of as_ref should return a const vnl_matrix_ref
  // so that the vnl_matrix_ref::non_const() cannot be used on
  // it. This prevents a const vnl_matrix_fixed from being cast into a
  // non-const vnl_matrix reference, giving a slight increase in type safety.

  //: Explicit conversion to a vnl_matrix_ref.
  // This is a cheap conversion for those functions that have an interface
  // for vnl_matrix_ref but not for vnl_matrix_fixed_ref. There is also a
  // conversion operator that should work most of the time.
  // \sa vnl_matrix_ref::non_const
  vnl_matrix_ref<T> as_ref() { return vnl_matrix_ref<T>( num_rows, num_cols, data_block() ); }

  //: Explicit conversion to a vnl_matrix_ref.
  // This is a cheap conversion for those functions that have an interface
  // for vnl_matrix_ref but not for vnl_matrix_fixed_ref. There is also a
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
  iterator       begin() const { return data_block(); }
  //: Iterator pointing to element beyond end of data
  iterator       end() const { return begin() + this->size(); }
  //--------------------------------------------------------------------------------

  //: Return true if *this == rhs
  bool operator_eq (vnl_matrix_fixed_ref_const<T,num_rows,num_cols> const & rhs) const
  {
    return equal( this->data_block(), rhs.data_block() );
  }

  //: Equality operator
  bool operator==(vnl_matrix_fixed_ref_const<T,num_rows,num_cols> const &that) const { return  this->operator_eq(that); }

  //: Inequality operator
  bool operator!=(vnl_matrix_fixed_ref_const<T,num_rows,num_cols> const &that) const { return !this->operator_eq(that); }

//--------------------------------------------------------------------------------
};

#undef VNL_MATRIX_FIXED_VCL60_WORKAROUND

  // Helper routines for arithmetic. These routines know the size from
  // the template parameters. The vector-vector operations are
  // element-wise.


// Make the operators below inline because (1) they are small and
// (2) we then have less explicit instantiation trouble.


// --- Matrix-scalar -------------------------------------------------------------

template<class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> operator+( const vnl_matrix_fixed_ref_const<T,m,n>& mat1, const vnl_matrix_fixed_ref_const<T,m,n>& mat2 )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::add( mat1.data_block(), mat2.data_block(), r.data_block() );
  return r;
}

template<class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> operator+( const vnl_matrix_fixed_ref_const<T,m,n>& mat, T s )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::add( mat.data_block(), s, r.data_block() );
  return r;
}

template<class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> operator+( T s, const vnl_matrix_fixed_ref_const<T,m,n>& mat )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::add( mat.data_block(), s, r.data_block() );
  return r;
}

template<class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> operator-( const vnl_matrix_fixed_ref_const<T,m,n>& mat1, const vnl_matrix_fixed_ref_const<T,m,n>& mat2 )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::sub( mat1.data_block(), mat2.data_block(), r.data_block() );
  return r;
}

template<class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> operator-( const vnl_matrix_fixed_ref_const<T,m,n>& mat, T s )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::sub( mat.data_block(), s, r.data_block() );
  return r;
}

template<class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> operator-( T s, const vnl_matrix_fixed_ref_const<T,m,n>& mat )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::sub( s, mat.data_block(), r.data_block() );
  return r;
}

template<class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> operator*( const vnl_matrix_fixed_ref_const<T,m,n>& mat, T s )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::mul( mat.data_block(), s, r.data_block() );
  return r;
}

template<class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> operator*( T s, const vnl_matrix_fixed_ref_const<T,m,n>& mat )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::mul( mat.data_block(), s, r.data_block() );
  return r;
}

template<class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> operator/( const vnl_matrix_fixed_ref_const<T,m,n>& mat, T s )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::div( mat.data_block(), s, r.data_block() );
  return r;
}


template<class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> element_product( const vnl_matrix_fixed_ref_const<T,m,n>& mat1,
                                         const vnl_matrix_fixed_ref_const<T,m,n>& mat2 )
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::mul( mat1.data_block(), mat2.data_block(), r.data_block() );
  return r;
}


template<class T, unsigned m, unsigned n>
inline
vnl_matrix_fixed<T,m,n> element_quotient( const vnl_matrix_fixed_ref_const<T,m,n>& mat1,
                                          const vnl_matrix_fixed_ref_const<T,m,n>& mat2)
{
  vnl_matrix_fixed<T,m,n> r;
  vnl_matrix_fixed<T,m,n>::div( mat1.data_block(), mat2.data_block(), r.data_block() );
  return r;
}


// The following two functions are helper functions that keep the
// matrix-matrix and matrix-vector multiplication code in one place,
// so that bug fixes, etc, can be localized.
template <class T, unsigned M, unsigned N>
inline
vnl_vector_fixed<T, M>
vnl_matrix_fixed_mat_vec_mult(const vnl_matrix_fixed_ref_const<T, M, N>& a,
                              const vnl_vector_fixed_ref_const<T, N>& b)
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

// see comment above
template <class T, unsigned M, unsigned N, unsigned O>
inline
vnl_matrix_fixed<T, M, O>
vnl_matrix_fixed_mat_mat_mult(const vnl_matrix_fixed_ref_const<T, M, N>& a,
                              const vnl_matrix_fixed_ref_const<T, N, O>& b)
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

#ifndef VCL_VC60
// The version for correct compilers

//: Multiply  conformant vnl_matrix_fixed (M x N) and vector_fixed (N)
// \relates vnl_vector_fixed
// \relates vnl_matrix_fixed
template <class T, unsigned M, unsigned N>
inline
vnl_vector_fixed<T, M> operator*(const vnl_matrix_fixed_ref_const<T, M, N>& a, const vnl_vector_fixed_ref_const<T, N>& b)
{
  return vnl_matrix_fixed_mat_vec_mult(a,b);
}

//: Multiply two conformant vnl_matrix_fixed (M x N) times (N x O)
// \relates vnl_matrix_fixed
template <class T, unsigned M, unsigned N, unsigned O>
inline
vnl_matrix_fixed<T, M, O> operator*(const vnl_matrix_fixed_ref_const<T, M, N>& a, const vnl_matrix_fixed_ref_const<T, N, O>& b)
{
  return vnl_matrix_fixed_mat_mat_mult(a,b);
}
#endif // ! VCL_VC60


// These overloads for the common case of mixing a fixed with a
// non-fixed. Because the operator* are templated, the fixed will not
// be automatically converted to a non-fixed-ref. These do it for you.

template<class T, unsigned m, unsigned n>
inline vnl_matrix<T> operator+( const vnl_matrix_fixed_ref_const<T,m,n>& a, const vnl_matrix<T>& b )
{
  return a.as_ref() + b;
}

template<class T, unsigned m, unsigned n>
inline vnl_matrix<T> operator+( const vnl_matrix<T>& a, const vnl_matrix_fixed_ref_const<T,m,n>& b )
{
  return a + b.as_ref();
}

template<class T, unsigned m, unsigned n>
inline vnl_matrix<T> operator-( const vnl_matrix_fixed_ref_const<T,m,n>& a, const vnl_matrix<T>& b )
{
  return a.as_ref() - b;
}

template<class T, unsigned m, unsigned n>
inline vnl_matrix<T> operator-( const vnl_matrix<T>& a, const vnl_matrix_fixed_ref_const<T,m,n>& b )
{
  return a - b.as_ref();
}

template<class T, unsigned m, unsigned n>
inline vnl_matrix<T> operator*( const vnl_matrix_fixed_ref_const<T,m,n>& a, const vnl_matrix<T>& b )
{
  return a.as_ref() * b;
}

template<class T, unsigned m, unsigned n>
inline vnl_matrix<T> operator*( const vnl_matrix<T>& a, const vnl_matrix_fixed_ref_const<T,m,n>& b )
{
  return a * b.as_ref();
}

template<class T, unsigned m, unsigned n>
inline vnl_vector<T> operator*( const vnl_matrix_fixed_ref_const<T,m,n>& a, const vnl_vector<T>& b )
{
  return a.as_ref() * b;
}

template<class T, unsigned n>
inline vnl_vector<T> operator*( const vnl_matrix<T>& a, const vnl_vector_fixed_ref_const<T,n>& b )
{
  return a * b.as_ref();
}


// --- I/O operations ------------------------------------------------------------

template<class T, unsigned m, unsigned n>
inline
vcl_ostream& operator<< (vcl_ostream& os, vnl_matrix_fixed_ref_const<T,m,n> const& mat)
{
  mat.print(os);
  return os;
}

template<class T, unsigned m, unsigned n>
inline
vcl_istream& operator>> (vcl_istream& is, vnl_matrix_fixed_ref<T,m,n>& mat)
{
  mat.read_ascii(is);
  return is;
}


#endif // vnl_matrix_fixed_ref_h_
