// This is core/vnl/vnl_vector_fixed.h
#ifndef vnl_vector_fixed_h_
#define vnl_vector_fixed_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Fixed length stack-stored vnl_vector
//
// The operators are inlined because (1) they are small and
// (2) we then have less explicit instantiation trouble.
//
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   04 Aug 96
//
// \verbatim
//  Modifications
//   LSB Manchester 16/3/01 Binary I/O added
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
//   Oct.2002 - Amitha Perera - decoupled vnl_vector and vnl_vector_fixed for
//              space efficiency, removed necessity for vnl_vector_fixed_ref
//   Jun.2003 - Paul Smyth - added as_fixed_ref() to convert to fixed-size ref
//              removed duplicate cross_3d
//   Jun.2003 - Peter Vanroose - added cross_2d
//   Oct.2003 - Peter Vanroose - removed deprecated x(), y(), z(), t()
// \endverbatim

#include <vcl_cstring.h> // memcpy()
#include <vcl_cassert.h>
#include <vcl_iosfwd.h>
#include "vnl_vector.h"
#include "vnl_vector_ref.h"
#include "vnl_c_vector.h"
#include "vnl_matrix.h" // outerproduct

export template <class T, unsigned int n> class vnl_vector_fixed;
export template <class T, unsigned int num_rows, unsigned int num_cols> class vnl_matrix_fixed;

//: Fixed length  stack-stored, space-efficient vector.
// vnl_vector_fixed is a fixed-length, stack storage vector. It has
// the same storage size as a C-style array. It is not related via
// inheritance to vnl_vector. However, it can be converted cheaply to
// a vnl_vector_ref.
//
// In most cases, a vnl_vector_fixed can be used where a vnl_vector is
// expected. There are some situations, however, when the automatic
// conversion cannot be applied. In those cases, you need to call the
// as_ref() method to perform an explicit conversion. This occurs most
// often when the called function is templated, since the user-defined
// conversion operators are then suppressed.
// \code
//    template<class T>
//    void do_something( const vnl_vector<T>& v );
//    ...
//    vnl_vector_fixed<double,4> my_vec;
//
//    do_something( my_vec );
//      // Error: no do_something( vnl_vector_fixed<double,4> ) found
//
//    do_something( my_vec.as_ref() );  // works
// \endcode
//
// Since the conversion operator creates a temporary vnl_vector_ref
// object, the conversion cannot be used directly to a function that
// expects a non-const vnl_vector reference. Use
// vnl_vector_ref::non_const method for this (and only this).
// \code
//    void mutator( vnl_vector<double>& v );
//    ...
//    vnl_vector_fixed<double,4> my_vec;
//    mutator( my_vec.as_ref().non_const() );
// \endcode
// If the mutator only accesses the data, all should be fine. If the
// mutator attempts to resize the vector, you are doomed.
//
// vnl_vector_fixed defines most of the operators defined by
// vnl_vector, and does so efficiently. If you try to mix
// vnl_vector_fixed and vnl_vector, however, you will probably get a
// vnl_vector result, with the corresponding malloc cost.
template <class T, unsigned int n>
class vnl_vector_fixed
{
 public:
  typedef unsigned int size_type;
  // Compile-time accessible attribute to get the dimensionality of the vector.
  enum{ SIZE = n };

 protected:
  T data_[n];

 public:
  // Don't out-of-line the constructors, as extra the function call
  // adds a significant overhead. (memcpy is often implemented with a
  // couple of assembly instructions.)

  //: Construct an uninitialized n-vector
  vnl_vector_fixed() {}

  //: Copy constructor
  //  The dimensions must match.
  vnl_vector_fixed( const vnl_vector_fixed<T,n>& rhs )
  {
    vcl_memcpy( data_, rhs.data_, sizeof data_ );
  }

  //: Construct a fixed-n-vector copy of \a rhs.
  //  The dimensions must match.
  vnl_vector_fixed( const vnl_vector<T>& rhs )
  {
    assert( n == rhs.size() );
    vcl_memcpy( data_, rhs.data_block(), sizeof data_ );
  }

  //: Constructs n-vector with all elements initialised to \a v
  explicit vnl_vector_fixed( const T& v ) { fill( v ); }

  //: Construct an fixed-n-vector initialized from \a datablck
  //  The data *must* have enough data. No checks performed.
  explicit vnl_vector_fixed( const T* datablck )
  {
    vcl_memcpy( data_, datablck, sizeof data_ );
  }

  //: Convenience constructor for 2-D vectors
  // While this constructor is sometimes useful, consider using
  // vnl_double_2 or vnl_float_2 instead.
  vnl_vector_fixed( const T& x0, const T& x1 )
  {
    assert( n == 2 );
    data_[0] = x0; data_[1] = x1;
  }

  //: Convenience constructor for 3-D vectors
  // While this constructor is sometimes useful, consider using
  // vnl_double_3 or vnl_float_3 instead.
  vnl_vector_fixed( const T& x0, const T& x1, const T& x2 )
  {
    assert( n == 3 );
    data_[0] = x0; data_[1] = x1; data_[2] = x2;
  }

  //: Convenience constructor for 4-D vectors
  vnl_vector_fixed( const T& x0, const T& x1, const T& x2, const T& x3 )
  {
    assert( n == 4 );
    data_[0] = x0; data_[1] = x1; data_[2] = x2; data_[3] = x3;
  }

  //: Copy operator
  vnl_vector_fixed<T,n>& operator=( const vnl_vector_fixed<T,n>& rhs ) {
    vcl_memcpy( data_, rhs.data_, sizeof data_ );
    return *this;
  }

  //: Copy data from a dynamic vector
  // The dimensions must match.
  vnl_vector_fixed<T,n>& operator=( const vnl_vector<T>& rhs) {
    assert( n == rhs.size() );
    vcl_memcpy( data_, rhs.data_block(), sizeof data_ );
    return *this;
  }

  //: Length of the vector.
  // This is always \a n.
  unsigned size() const { return n; }

  //: Put value at given position in vector.
  void put (unsigned int i, T const& v) { data_[i] = v; }

  //: Get value at element i
  T get (unsigned int i) const { return data_[i]; }

  //: Set all values to v
  void fill( T const& v )
  {
    for ( size_type i = 0; i < n; ++i )
      data_[i] = v;
  }

  //: Sets elements to ptr[i]
  //  Note: ptr[i] must be valid for i=0..size()-1
  void copy_in( T const * ptr )
  {
    for ( size_type i = 0; i < n; ++i )
      data_[i] = ptr[i];
  }

  //: Copy elements to ptr[i]
  //  Note: ptr[i] must be valid for i=0..size()-1
  void copy_out( T* ptr ) const
  {
    for ( size_type i = 0; i < n; ++i )
      ptr[i] = data_[i];
  }

  //: Sets elements to ptr[i]
  //  Note: ptr[i] must be valid for i=0..size()-1
  void set( T const *ptr ) { copy_in(ptr); }


  //: Return reference to the element at specified index.
  // There are assert style boundary checks - #define NDEBUG to turn them off.
  T       & operator() (unsigned int i)
  {
#if VNL_CONFIG_CHECK_BOUNDS  && (!defined NDEBUG)
    assert(i<n);   // Check the index is valid.
#endif
    return data_[i];
  }

  //: Return reference to the element at specified index.
  // There are assert style boundary checks - #define NDEBUG to turn them off.
  T const & operator() (unsigned int i) const
  {
#if VNL_CONFIG_CHECK_BOUNDS  && (!defined NDEBUG)
    assert(i<n);   // Check the index is valid
#endif
    return data_[i];
  }

  //: Return the i-th element
  T& operator[] ( unsigned int i ) { return data_[i]; }

  //: Return the i-th element
  const T& operator[] ( unsigned int i ) const { return data_[i]; }

  //: Access the contiguous block storing the elements in the vector.
  //  O(1).
  //  data_block()[0] is the first element of the vector
  T const* data_block() const { return data_; }

  //: Access the contiguous block storing the elements in the vector.
  //  O(1).
  //  data_block()[0] is the first element of the vector
  T      * data_block() { return data_; }

  //----------------------------------------------------------------------
  // Conversion to vnl_vector_ref.

  // The const version of as_ref should return a const vnl_vector_ref
  // so that the vnl_vector_ref::non_const() cannot be used on
  // it. This prevents a const vnl_vector_fixed from being cast into a
  // non-const vnl_vector reference, giving a slight increase in type safety.

  //: Explicit conversion to a vnl_vector_ref.
  // This is a cheap conversion for those functions that have an interface
  // for vnl_vector but not for vnl_vector_fixed. There is also a
  // conversion operator that should work most of the time.
  // \sa vnl_vector_ref::non_const
  vnl_vector_ref<T> as_ref() { return vnl_vector_ref<T>( n, data_ ); }

  //: Explicit conversion to a vnl_vector_ref.
  // This is a cheap conversion for those functions that have an interface
  // for vnl_vector but not for vnl_vector_fixed. There is also a
  // conversion operator that should work most of the time.
  // \sa vnl_vector_ref::non_const
  const vnl_vector_ref<T> as_ref() const { return vnl_vector_ref<T>( n, const_cast<T*>(data_) ); }

  //: Cheap conversion to vnl_vector_ref
  // Sometimes, such as with templated functions, the compiler cannot
  // use this user-defined conversion. For those cases, use the
  // explicit as_ref() method instead.
  operator const vnl_vector_ref<T>() const { return vnl_vector_ref<T>( n, const_cast<T*>(data_) ); }

  //----------------------------------------------------------------------

  //: Type defs for iterators
  typedef T element_type;
  //: Type defs for iterators
  typedef T       *iterator;
  //: Iterator pointing to start of data
  iterator begin() { return data_; }

  //: Iterator pointing to element beyond end of data
  iterator end() { return data_+n; }

  //: Const iterator type
  typedef T const *const_iterator;
  //: Iterator pointing to start of data
  const_iterator begin() const { return data_; }
  //: Iterator pointing to element beyond end of data
  const_iterator end() const { return data_+n; }


  //: Apply f to each element.
  // Returns a new vector with the result.
  vnl_vector_fixed<T,n> apply(T (*f)(T));

  //: Apply f to each element.
  // Returns a new vector with the result.
  vnl_vector_fixed<T,n> apply(T (*f)(const T&));

  //:
  vnl_vector_fixed<T,n>& operator+=( T s ) { add( data_, s, data_ ); return *this; }

  //:
  vnl_vector_fixed<T,n>& operator-=( T s ) { sub( data_, s, data_ ); return *this; }

  //:
  vnl_vector_fixed<T,n>& operator*=( T s ) { mul( data_, s, data_ ); return *this; }

  //:
  vnl_vector_fixed<T,n>& operator/=( T s ) { div( data_, s, data_ ); return *this; }

  //:
  vnl_vector_fixed<T,n>& operator+=( const vnl_vector_fixed<T,n>& v ) { add( data_, v.data_block(), data_ ); return *this; }

  //:
  vnl_vector_fixed<T,n>& operator-=( const vnl_vector_fixed<T,n>& v ) { sub( data_, v.data_block(), data_ ); return *this; }

  //:
  vnl_vector_fixed<T,n>& operator+=( const vnl_vector<T>& v )
  {
    assert( v.size() == n );
    add( data_, v.data_block(), data_ ); return *this;
  }

  //:
  vnl_vector_fixed<T,n>& operator-=( const vnl_vector<T>& v )
  {
    assert( v.size() == n );
    sub( data_, v.data_block(), data_ ); return *this;
  }

  //:
  vnl_vector_fixed<T,n> operator-() const
  {
    vnl_vector_fixed<T,n> result;
    sub( (T)0, data_, result.data_ );
    return result;
  }

  //: Returns a subvector specified by the start index and length. O(n).
  vnl_vector<T> extract (unsigned int len, unsigned int start=0) const;

  //: Convert to a vnl_vector.
  vnl_vector<T> as_vector() const { return extract(n); }

  //: Replaces elements with index beginning at start, by values of v. O(n).
  vnl_vector_fixed& update (vnl_vector<T> const&, unsigned int start=0);

  // norms etc
  typedef typename vnl_c_vector<T>::abs_t abs_t;

  //: Return sum of squares of elements
  abs_t squared_magnitude() const { return vnl_c_vector<T>::two_nrm2(begin(), size()); }

  //: Return magnitude (length) of vector
  abs_t magnitude() const { return two_norm(); }

  //: Return sum of absolute values of the elements
  abs_t one_norm() const { return vnl_c_vector<T>::one_norm(begin(), size()); }

  //: Return sqrt of sum of squares of values of elements
  abs_t two_norm() const { return vnl_c_vector<T>::two_norm(begin(), size()); }

  //: Return largest absolute element value
  abs_t inf_norm() const { return vnl_c_vector<T>::inf_norm(begin(), size()); }

  //: Normalise by dividing through by the magnitude
  vnl_vector_fixed<T,n>& normalize() { vnl_c_vector<T>::normalize(begin(), size()); return *this; }

  // These next 6 functions are should really be helper functions since they aren't
  // really proper functions on a vector in a philosophical sense.

  //: Root Mean Squares of values
  abs_t rms     () const { return vnl_c_vector<T>::rms_norm(begin(), size()); }

  //: Smallest value
  T min_value () const { return vnl_c_vector<T>::min_value(begin(), size()); }

  //: Largest value
  T max_value () const { return vnl_c_vector<T>::max_value(begin(), size()); }

  //: Mean of values in vector
  T mean() const { return vnl_c_vector<T>::mean(begin(), size()); }

  //: Sum of values in a vector
  T sum() const { return vnl_c_vector<T>::sum(begin(), size()); }

  //: Reverse the order of the elements
  //  Element i swaps with element size()-1-i
  void flip();

  //: Check that size()==sz if not, abort();
  // This function does or tests nothing if NDEBUG is defined
  void assert_size( unsigned sz ) const { assert( sz == n ); }

  //: Check that this is finite if not, abort();
  // This function does or tests nothing if NDEBUG is defined
  void assert_finite() const
  {
#ifndef NDEBUG
    assert_finite_internal();
#endif
  }

  //: Return true if its finite
  bool is_finite() const;

  //: Return true iff all the entries are zero.
  bool is_zero() const;

  //: Return true iff the size is zero.
  bool empty() const { return n==0; }

  //: Return true if *this == v
  bool operator_eq (vnl_vector_fixed<T,n> const& v) const
  {
    for ( size_type i = 0; i < n; ++i )
      if ( (*this)[i] != v[i] )
        return false;
    return true;
  }

  //: Return true if *this == v
  bool operator_eq (vnl_vector<T> const& v) const
  {
    assert( v.size() == n );
    for ( size_type i = 0; i < n; ++i )
      if ( (*this)[i] != v[i] )
        return false;
    return true;
  }


  //: Read from text stream
  bool read_ascii(vcl_istream& s);

  //: Display the vector
  // Output each element separated by a single space.
  void print( vcl_ostream& s ) const;

 public:
  // Helper routines for arithmetic. n is the size, and is the
  // template parameter.

  inline static void add( const T* a, const T* b, T* r )
  {
    for ( unsigned int i=0; i < n; ++i,++r,++a,++b )
      *r = *a + *b;
  }

  inline static void add( const T* a, T b, T* r )
  {
    for ( unsigned int i=0; i < n; ++i,++r,++a )
      *r = *a + b;
  }

  inline static void sub( const T* a, const T* b, T* r )
  {
    for ( unsigned int i=0; i < n; ++i,++r,++a,++b )
      *r = *a - *b;
  }

  inline static void sub( const T* a, T b, T* r )
  {
    for ( unsigned int i=0; i < n; ++i,++r,++a )
      *r = *a - b;
  }

  inline static void sub( T a, const T* b, T* r )
  {
    for ( unsigned int i=0; i < n; ++i,++r,++b )
      *r = a - *b;
  }

  inline static void mul( const T* a, const T* b, T* r )
  {
    for ( unsigned int i=0; i < n; ++i,++r,++a,++b )
      *r = *a * *b;
  }

  inline static void mul( const T* a, T b, T* r )
  {
    for ( unsigned int i=0; i < n; ++i,++r,++a )
      *r = *a * b;
  }

  inline static void div( const T* a, const T* b, T* r )
  {
    for ( unsigned int i=0; i < n; ++i,++r,++a,++b )
      *r = *a / *b;
  }

  inline static void div( const T* a, T b, T* r )
  {
    for ( unsigned int i=0; i < n; ++i,++r,++a )
      *r = *a / b;
  }

 private:
  //: See assert_finite().
  void assert_finite_internal() const;
};


// --- Vector-scalar operators ----------------------------------------

//:
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> operator+( const vnl_vector_fixed<T,n>& v, T s )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::add( v.data_block(), s, r.data_block() );
  return r;
}

//:
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> operator+( const T& s,
                                        const vnl_vector_fixed<T,n>& v )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::add( v.data_block(), s, r.data_block() );
  return r;
}

//:
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> operator-( const vnl_vector_fixed<T,n>& v, T s )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::sub( v.data_block(), s, r.data_block() );
  return r;
}

//:
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> operator-( const T& s,
                                        const vnl_vector_fixed<T,n>& v )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::sub( s, v.data_block(), r.data_block() );
  return r;
}

//:
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> operator*( const vnl_vector_fixed<T,n>& v, T s )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::mul( v.data_block(), s, r.data_block() );
  return r;
}

//:
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> operator*( const T& s,
                                        const vnl_vector_fixed<T,n>& v )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::mul( v.data_block(), s, r.data_block() );
  return r;
}

//:
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> operator/( const vnl_vector_fixed<T,n>& v, T s )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::div( v.data_block(), s, r.data_block() );
  return r;
}


// --- Vector-vector operators ----------------------------------------
//
// Includes overloads for the common case of mixing a fixed with a
// non-fixed. Because the operators are templated, the fixed will not
// be automatically converted to a non-fixed-ref. These do it for you.

//:
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> operator+( const vnl_vector_fixed<T,n>& a, const vnl_vector_fixed<T,n>& b )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::add( a.data_block(), b.data_block(), r.data_block() );
  return r;
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector<T> operator+( const vnl_vector_fixed<T,n>& a, const vnl_vector<T>& b )
{
  return a.as_ref() + b;
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector<T> operator+( const vnl_vector<T>& a, const vnl_vector_fixed<T,n>& b )
{
  return a + b.as_ref();
}

//:
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> operator-( const vnl_vector_fixed<T,n>& a, const vnl_vector_fixed<T,n>& b )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::sub( a.data_block(), b.data_block(), r.data_block() );
  return r;
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector<T> operator-( const vnl_vector_fixed<T,n>& a, const vnl_vector<T>& b )
{
  return a.as_ref() - b;
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector<T> operator-( const vnl_vector<T>& a, const vnl_vector_fixed<T,n>& b )
{
  return a - b.as_ref();
}

//:
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> element_product( const vnl_vector_fixed<T,n>& a, const vnl_vector_fixed<T,n>& b )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::mul( a.data_block(), b.data_block(), r.data_block() );
  return r;
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector<T> element_product( const vnl_vector_fixed<T,n>& a, const vnl_vector<T>& b )
{
  assert( b.size() == n );
  vnl_vector<T> r(n);
  vnl_vector_fixed<T,n>::mul( a.data_block(), b.data_block(), r.data_block() );
  return r;
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector<T> element_product( const vnl_vector<T>& a, const vnl_vector_fixed<T,n>& b )
{
  assert( a.size() == n );
  vnl_vector<T> r(n);
  vnl_vector_fixed<T,n>::mul( a.data_block(), b.data_block(), r.data_block() );
  return r;
}

//:
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> element_quotient( const vnl_vector_fixed<T,n>& a, const vnl_vector_fixed<T,n>& b )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::div( a.data_block(), b.data_block(), r.data_block() );
  return r;
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector<T> element_quotient( const vnl_vector_fixed<T,n>& a, const vnl_vector<T>& b )
{
  assert( b.size() == n );
  vnl_vector<T> r(n);
  vnl_vector_fixed<T,n>::div( a.data_block(), b.data_block(), r.data_block() );
  return r;
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector<T> element_quotient( const vnl_vector<T>& a, const vnl_vector_fixed<T,n>& b )
{
  assert( a.size() == n );
  vnl_vector<T> r(n);
  vnl_vector_fixed<T,n>::div( a.data_block(), b.data_block(), r.data_block() );
  return r;
}

//:
// \relates vnl_vector_fixed
template<class T, unsigned n>
inline T dot_product( const vnl_vector_fixed<T,n>& a, const vnl_vector_fixed<T,n>& b )
{
  return dot_product( a.as_ref(), b.as_ref() );
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned n>
inline T dot_product( const vnl_vector_fixed<T,n>& a, const vnl_vector<T>& b )
{
  return dot_product( a.as_ref(), b );
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned n>
inline T dot_product( const vnl_vector<T>& a, const vnl_vector_fixed<T,n>& b )
{
  return dot_product( a, b.as_ref() );
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_matrix<T> outer_product( const vnl_vector<T>& a, const vnl_vector_fixed<T,n>& b )
{
  return outer_product( a, b.as_ref());
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_matrix<T> outer_product( const vnl_vector_fixed<T,n>& a, const vnl_vector<T>& b )
{
  return outer_product( a.as_ref(), b);
}

//:
// \relates vnl_vector_fixed
template<class T, unsigned n>
inline T angle( const vnl_vector_fixed<T,n>& a, const vnl_vector_fixed<T,n>& b )
{
  return angle( a.as_ref(), b.as_ref() );
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned n>
inline T angle( const vnl_vector_fixed<T,n>& a, const vnl_vector<T>& b )
{
  return angle( a.as_ref(), b );
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned n>
inline T angle( const vnl_vector<T>& a, const vnl_vector_fixed<T,n>& b )
{
  return angle( a, b.as_ref() );
}


//:
// \relates vnl_vector_fixed
template<class T, unsigned n>
inline T vnl_vector_ssd( const vnl_vector_fixed<T,n>& a, const vnl_vector_fixed<T,n>& b )
{
  return vnl_vector_ssd( a.as_ref(), b.as_ref() );
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned n>
inline T vnl_vector_ssd( const vnl_vector_fixed<T,n>& a, const vnl_vector<T>& b )
{
  return vnl_vector_ssd( a.as_ref(), b );
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned n>
inline T vnl_vector_ssd( const vnl_vector<T>& a, const vnl_vector_fixed<T,n>& b )
{
  return vnl_vector_ssd( a, b.as_ref() );
}


//:
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline bool operator==( const vnl_vector_fixed<T,n>& a, const vnl_vector_fixed<T,n>& b )
{
  return a.operator_eq(b);
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline bool operator==( vnl_vector_fixed<T,n> const& a, vnl_vector<T> const& b )
{
  return a.operator_eq(b);
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline bool operator==( vnl_vector<T> const& a, vnl_vector_fixed<T,n> const& b )
{
  return b.operator_eq(a);
}

//:
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline bool operator!=( const vnl_vector_fixed<T,n>& a, const vnl_vector_fixed<T,n>& b )
{
  return ! a.operator_eq(b);
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline bool operator!=( vnl_vector_fixed<T,n> const& a, vnl_vector<T> const& b )
{
  return ! a.operator_eq(b);
}

//:
// \relates vnl_vector
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline bool operator!=( vnl_vector<T> const& a, vnl_vector_fixed<T,n> const& b )
{
  return ! b.operator_eq(a);
}


// --- I/O operators -------------------------------------------------


//:
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline
vcl_ostream& operator<< ( vcl_ostream& ostr, const vnl_vector_fixed<T,n>& v )
{
  v.print( ostr );
  return ostr;
}

//:
// \relates vnl_vector_fixed
template<class T, unsigned int n>
inline
vcl_istream& operator>> ( vcl_istream& ostr, vnl_vector_fixed<T,n>& v )
{
  v.read_ascii( ostr );
  return ostr;
}

#endif // vnl_vector_fixed_h_
