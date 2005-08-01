// This is core/vnl/vnl_vector_fixed_ref.h
#ifndef vnl_vector_fixed_ref_h_
#define vnl_vector_fixed_ref_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Fixed size vnl_vector using user-supplied storage
// See vnl_matrix_fixed_ref for rationale.
// See also vnl_vector_ref, vnl_vector_fixed
//
// \author Paul P. Smyth, Vicon Motion Systems Ltd.
// \date 02 May 2001
//
// \verbatim
//  Modifications
//   4-Jul-2003 Paul Smyth - general cleanup and rewrite; interface now as vnl_vector_fixed
// \endverbatim

#include <vcl_cassert.h>
#include <vnl/vnl_vector_fixed.h>
#include <vcl_iosfwd.h>


template <class T, unsigned int n>
class vnl_vector_fixed_ref_const
{
 public:
  typedef unsigned int size_type;

 protected:
  const T* data_;

 public:
  vnl_vector_fixed_ref_const(vnl_vector_fixed<T,n> const& rhs) : data_(rhs.data_block()) {}

  explicit vnl_vector_fixed_ref_const(const T * dataptr) : data_(dataptr) {}

  vnl_vector_fixed_ref_const(const vnl_vector_fixed_ref_const<T,n> & rhs) : data_(rhs.data_block()) {}

  const T * data_block() const { return data_; }

 public:
  // Don't out-of-line the constructors, as the extra function call
  // adds a significant overhead. (memcpy is often implemented with a
  // couple of assembly instructions.)


  //: Length of the vector.
  // This is always \a n.
  unsigned size() const { return n; }

  //: Get value at element i
  T get (unsigned int i) const { return data_[i]; }

  //: Copy elements to ptr[i]
  //  Note: ptr[i] must be valid for i=0..size()-1
  void copy_out( T* ptr ) const {
    for ( size_type i = 0; i < n; ++i )
      ptr[i] = data_[i];
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
  const T& operator[] ( unsigned int i ) const { return data_[i]; }


  //----------------------------------------------------------------------
  // Conversion to vnl_vector_ref.

  // The const version of as_ref should return a const vnl_vector_ref
  // so that the vnl_vector_ref::non_const() cannot be used on
  // it. This prevents a vnl_vector_fixed_ref_const from being cast into a
  // non-const vnl_vector reference, giving a slight increase in type safety.

  //: Explicit conversion to a vnl_vector_ref.
  // This is a cheap conversion for those functions that have an interface
  // for vnl_vector_ref but not for vnl_vector_fixed_ref. There is also a
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
  typedef T const *iterator;

  //: Const iterator type
  typedef T const *const_iterator;
  //: Iterator pointing to start of data
  const_iterator begin() const { return data_; }
  //: Iterator pointing to element beyond end of data
  const_iterator end() const { return data_+n; }


  //: Apply f to each element.
  // Returns a new vector with the result.
  vnl_vector_fixed<T,n> apply(T (*f)(T)) const;

  //: Apply f to each element.
  // Returns a new vector with the result.
  vnl_vector_fixed<T,n> apply(T (*f)(const T&)) const;

  //:
  vnl_vector_fixed<T,n> operator-() const {
    vnl_vector_fixed<T,n> result;
    sub( (T)0, data_, result.data_block() );
    return result;
  }

  //: Returns a subvector specified by the start index and length. O(n).
  vnl_vector<T> extract (unsigned int len, unsigned int start=0) const;

  //: Convert to a vnl_vector.
  vnl_vector<T> as_vector() const { return extract(n); }


  // norms etc
  typedef typename vnl_c_vector<T>::abs_t abs_t;

  //: Return sum of squares of elements
  abs_t squared_magnitude() const { return vnl_c_vector<T>::two_nrm2(begin(), n); }

  //: Return magnitude (length) of vector
  abs_t magnitude() const { return two_norm(); }

  //: Return sum of absolute values of the elements
  abs_t one_norm() const { return vnl_c_vector<T>::one_norm(begin(), n); }

  //: Return sqrt of sum of squares of values of elements
  abs_t two_norm() const { return vnl_c_vector<T>::two_norm(begin(), n); }

  //: Return largest absolute element value
  abs_t inf_norm() const { return vnl_c_vector<T>::inf_norm(begin(), n); }


  // These next 6 functions are should really be helper functions since they aren't
  // really proper functions on a vector in a philosophical sense.

  //: Root Mean Squares of values
  abs_t rms     () const { return vnl_c_vector<T>::rms_norm(begin(), n); }

  //: Smallest value
  T min_value () const { return vnl_c_vector<T>::min_value(begin(), n); }

  //: Largest value
  T max_value () const { return vnl_c_vector<T>::max_value(begin(), n); }

  //: Mean of values in vector
  T mean() const { return vnl_c_vector<T>::mean(begin(), n); }

  //: Sum of values in a vector
  T sum() const { return vnl_c_vector<T>::sum(begin(), n); }


  //: Check that size()==sz if not, abort();
  // This function does or tests nothing if NDEBUG is defined
  void assert_size( unsigned sz ) const { assert( sz == n ); }

  //: Check that this is finite if not, abort();
  // This function does or tests nothing if NDEBUG is defined
  void assert_finite() const {
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
  bool operator_eq (vnl_vector_fixed_ref_const<T,n> const& v) const {
    for ( size_type i = 0; i < n; ++i )
      if ( (*this)[i] != v[i] )
        return false;
    return true;
  }

  //: Return true if *this == v
  bool operator_eq (vnl_vector<T> const& v) const {
    assert( v.size() == n );
    for ( size_type i = 0; i < n; ++i )
      if ( (*this)[i] != v[i] )
        return false;
    return true;
  }


  //: Display the vector
  // Output each element separated by a single space.
  void print( vcl_ostream& s ) const;

 public:
  // Helper routines for arithmetic. n is the size, and is the
  // template parameter.

  inline static void add( const T* a, const T* b, T* r ) {
    for ( unsigned int i=0; i < n; ++i,++r,++a,++b )
      *r = *a + *b;
  }

  inline static void add( const T* a, T b, T* r ) {
    for ( unsigned int i=0; i < n; ++i,++r,++a )
      *r = *a + b;
  }

  inline static void sub( const T* a, const T* b, T* r ) {
    for ( unsigned int i=0; i < n; ++i,++r,++a,++b )
      *r = *a - *b;
  }

  inline static void sub( const T* a, T b, T* r ) {
    for ( unsigned int i=0; i < n; ++i,++r,++a )
      *r = *a - b;
  }

  inline static void sub( T a, const T* b, T* r ) {
    for ( unsigned int i=0; i < n; ++i,++r,++b )
      *r = a - *b;
  }

  inline static void mul( const T* a, const T* b, T* r ) {
    for ( unsigned int i=0; i < n; ++i,++r,++a,++b )
      *r = *a * *b;
  }

  inline static void mul( const T* a, T b, T* r ) {
    for ( unsigned int i=0; i < n; ++i,++r,++a )
      *r = *a * b;
  }

  inline static void div( const T* a, const T* b, T* r ) {
    for ( unsigned int i=0; i < n; ++i,++r,++a,++b )
      *r = *a / *b;
  }

  inline static void div( const T* a, T b, T* r ) {
    for ( unsigned int i=0; i < n; ++i,++r,++a )
      *r = *a / b;
  }


  //: Equality operator
  bool operator==(vnl_vector_fixed_ref_const<T,n> const &that) const { return  this->operator_eq(that); }

  //: Inequality operator
  bool operator!=(vnl_vector_fixed_ref_const<T,n> const &that) const { return !this->operator_eq(that); }

 private:
  //: See assert_finite().
  const vnl_vector_fixed_ref_const<T,n> & operator=(const vnl_vector_fixed<T,n> & ) const
  {
    assert(!"Assignment is illegal for a vnl_vector_fixed_ref_const");
    return *this;
  }
  const vnl_vector_fixed_ref_const<T,n> & operator=(const vnl_vector_fixed_ref_const<T,n> & ) const
  {
    assert(!"Assignment is illegal for a vnl_vector_fixed_ref_const");
    return *this;
  }
  void assert_finite_internal() const;
};

// Non const vector fixed reference

template <class T, unsigned n>
class vnl_vector_fixed_ref : public vnl_vector_fixed_ref_const<T,n>
{
  typedef vnl_vector_fixed_ref_const<T,n> base;

 public:
  typedef unsigned int size_type;

  // this is the only point where the const_cast happens
  // the base class is used to store the pointer, so that conversion is not necessary
  T * data_block() const { return const_cast<T*>(this->data_); }

  vnl_vector_fixed_ref(vnl_vector_fixed<T,n>& rhs) : base(rhs.data_block()) {}

  explicit vnl_vector_fixed_ref(T * dataptr) : base(dataptr) {}

  //: Copy operator
  vnl_vector_fixed_ref<T,n> const & operator=( const vnl_vector_fixed<T,n>& rhs ) const {
    vcl_memcpy( data_block(), rhs.data_block(), n * sizeof(T) );
    return *this;
  }

  //: Copy operator
  vnl_vector_fixed_ref<T,n> const& operator=( const vnl_vector_fixed_ref_const<T,n>& rhs ) const {
    vcl_memcpy( data_block(), rhs.data_block(), n * sizeof(T) );
    return *this;
  }


  //: Put value at given position in vector.
  void put (unsigned int i, T const& v) const { data_block()[i] = v; }

  //: Set all values to v
  void fill( T const& v ) { for ( size_type i = 0; i < n; ++i ) data_block()[i] = v; }

  //: Sets elements to ptr[i]
  //  Note: ptr[i] must be valid for i=0..size()-1
  void copy_in( T const * ptr ) const { for ( size_type i = 0; i < n; ++i ) data_block()[i] = ptr[i]; }

  //: Sets elements to ptr[i]
  //  Note: ptr[i] must be valid for i=0..size()-1
  void set( T const *ptr ) const { copy_in(ptr); }


  //: Return reference to the element at specified index.
  // There are assert style boundary checks - #define NDEBUG to turn them off.
  T       & operator() (unsigned int i) const
  {
#if VNL_CONFIG_CHECK_BOUNDS  && (!defined NDEBUG)
    assert(i<n);   // Check the index is valid.
#endif
    return data_block()[i];
  }

  //: Return the i-th element
  T& operator[] ( unsigned int i ) const { return data_block()[i]; }

  // \sa vnl_vector_ref::non_const
  vnl_vector_ref<T> as_ref() { return vnl_vector_ref<T>( n, data_block() ); }

  typedef T       *iterator;
  //: Iterator pointing to start of data
  iterator begin() const { return data_block(); }

  //: Iterator pointing to element beyond end of data
  iterator end() const { return begin()+n; }

  //: Replaces elements with index beginning at start, by values of v. O(n).
  vnl_vector_fixed_ref const& update (vnl_vector<T> const&, unsigned int start=0) const;

  //: Read from text stream
  bool read_ascii(vcl_istream& s) const;

  void flip() const;

  //:
  vnl_vector_fixed_ref<T,n> const & operator+=( T s ) const {
    add( data_block(), s, data_block() ); return *this;
  }

  //:
  vnl_vector_fixed_ref<T,n> const & operator-=( T s ) const {
    sub( data_block(), s, data_block() ); return *this;
  }

  //:
  vnl_vector_fixed_ref<T,n> const & operator*=( T s ) const {
    mul( data_block(), s, data_block() ); return *this;
  }

  //:
  vnl_vector_fixed_ref<T,n> const & operator/=( T s ) const {
    div( data_block(), s, data_block() ); return *this;
  }

  //:
  vnl_vector_fixed_ref<T,n> const & operator+=( const vnl_vector_fixed<T,n>& v ) const {
    add( data_block(), v.data_block(), data_block() ); return *this;
  }

  //:
  vnl_vector_fixed_ref<T,n> const & operator-=( const vnl_vector_fixed<T,n>& v ) const {
    sub( data_block(), v.data_block(), data_block() ); return *this;
  }

  //:
  vnl_vector_fixed_ref<T,n> const & operator+=( const vnl_vector<T>& v ) const {
    assert( v.size() == n );
    add( data_block(), v.data_block(), data_block() ); return *this;
  }

  //:
  vnl_vector_fixed_ref<T,n> const & operator-=( const vnl_vector<T>& v ) const {
    assert( v.size() == n );
    sub( data_block(), v.data_block(), data_block() ); return *this;
  }
};


// Make the operators below inline because (1) they are small and
// (2) we then have less explicit instantiation trouble.


// --- Vector-scalar operators ----------------------------------------


//: \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> operator+( const vnl_vector_fixed_ref_const<T,n>& v, T s )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::add( v.data_block(), s, r.data_block() );
  return r;
}

//: \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> operator+( T s, const vnl_vector_fixed_ref_const<T,n>& v )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::add( v.data_block(), s, r.data_block() );
  return r;
}

//: \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> operator-( const vnl_vector_fixed_ref_const<T,n>& v, T s )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::sub( v.data_block(), s, r.data_block() );
  return r;
}

//: \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> operator-( T s, const vnl_vector_fixed_ref_const<T,n>& v )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::sub( s, v.data_block(), r.data_block() );
  return r;
}

//: \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> operator*( const vnl_vector_fixed_ref_const<T,n>& v, T s )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::mul( v.data_block(), s, r.data_block() );
  return r;
}

//: \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> operator*( T s, const vnl_vector_fixed_ref_const<T,n>& v )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::mul( v.data_block(), s, r.data_block() );
  return r;
}

//: \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> operator/( const vnl_vector_fixed_ref_const<T,n>& v, T s )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::div( v.data_block(), s, r.data_block() );
  return r;
}


// --- Vector-vector operators ----------------------------------------


//: \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> operator+( const vnl_vector_fixed_ref_const<T,n>& a, const vnl_vector_fixed_ref_const<T,n>& b )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::add( a.data_block(), b.data_block(), r.data_block() );
  return r;
}

//: \relates vnl_vector_fixed
template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> operator-( const vnl_vector_fixed_ref_const<T,n>& a, const vnl_vector_fixed_ref_const<T,n>& b )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::sub( a.data_block(), b.data_block(), r.data_block() );
  return r;
}

template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> element_product( const vnl_vector_fixed_ref_const<T,n>& a, const vnl_vector_fixed_ref_const<T,n>& b )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::mul( a.data_block(), b.data_block(), r.data_block() );
  return r;
}

template<class T, unsigned int n>
inline vnl_vector_fixed<T,n> element_quotient( const vnl_vector_fixed_ref_const<T,n>& a, const vnl_vector_fixed_ref_const<T,n>& b )
{
  vnl_vector_fixed<T,n> r;
  vnl_vector_fixed<T,n>::div( a.data_block(), b.data_block(), r.data_block() );
  return r;
}

template<class T>
vnl_vector_fixed<T,3> vnl_cross_3d(vnl_vector_fixed_ref_const<T,3> const& v1, vnl_vector_fixed_ref_const<T,3> const& v2)
{
  vnl_vector_fixed<T,3> result;

  result[0] = v1[1] * v2[2] - v1[2] * v2[1];
  result[1] = v1[2] * v2[0] - v1[0] * v2[2];
  result[2] = v1[0] * v2[1] - v1[1] * v2[0];
  return result;
}

// These overloads for the common case of mixing a fixed with a
// non-fixed. Because the operator* are templated, the fixed will not
// be automatically converted to a non-fixed-ref. These do it for you.

template<class T, unsigned int n>
inline vnl_vector<T> operator+( const vnl_vector_fixed_ref_const<T,n>& a, const vnl_vector<T>& b )
{
  return a.as_ref() + b;
}

template<class T, unsigned int n>
inline vnl_vector<T> operator+( const vnl_vector<T>& a, const vnl_vector_fixed_ref_const<T,n>& b )
{
  return a + b.as_ref();
}

template<class T, unsigned int n>
inline vnl_vector<T> operator-( const vnl_vector_fixed_ref_const<T,n>& a, const vnl_vector<T>& b )
{
  return a.as_ref() - b;
}

template<class T, unsigned int n>
inline vnl_vector<T> operator-( const vnl_vector<T>& a, const vnl_vector_fixed_ref_const<T,n>& b )
{
  return a - b.as_ref();
}


template<class T, unsigned n>
inline T dot_product( const vnl_vector_fixed_ref_const<T,n>& a, const vnl_vector_fixed_ref_const<T,n>& b )
{
  return dot_product( a.as_ref(), b.as_ref() );
}

template<class T, unsigned n>
inline T dot_product( const vnl_vector_fixed_ref_const<T,n>& a, const vnl_vector<T>& b )
{
  return dot_product( a.as_ref(), b );
}

template<class T, unsigned n>
inline T dot_product( const vnl_vector<T>& a, const vnl_vector_fixed_ref_const<T,n>& b )
{
  return dot_product( a, b.as_ref() );
}

template<class T, unsigned int m, unsigned int n>
inline vnl_matrix_fixed<T,m,n> outer_product( const vnl_vector_fixed_ref_const<T,m>& a, const vnl_vector_fixed_ref_const<T,n>& b )
{
  vnl_matrix_fixed<T,m,n> out; // = a.column() * b.row()
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      out[i][j] = a[i] * b[j];
  return out;
}

template<class T,unsigned int n>
  inline vnl_vector_fixed<T,n> vnl_cross_3d( const vnl_vector_fixed_ref_const<T,n>& a, const vnl_vector<T>& b ) {
  return vnl_cross_3d( a.as_ref(), b);
}

template<class T,unsigned int n>
  inline vnl_vector_fixed<T,n> vnl_cross_3d( const vnl_vector<T>& a, const vnl_vector_fixed_ref_const<T,n>& b ) {
  return vnl_cross_3d( a, b.as_ref());
}

template<class T, unsigned int n>
inline vnl_matrix<T> outer_product( const vnl_vector<T>& a, const vnl_vector_fixed_ref_const<T,n>& b )
{
  return outer_product( a, b.as_ref());
}

template<class T, unsigned int n>
inline vnl_matrix<T> outer_product( const vnl_vector_fixed_ref_const<T,n>& a, const vnl_vector<T>& b )
{
  return outer_product( a.as_ref(), b);
}

template<class T, unsigned n>
inline T angle( const vnl_vector_fixed_ref_const<T,n>& a, const vnl_vector_fixed_ref_const<T,n>& b )
{
  return angle( a.as_ref(), b.as_ref() );
}

template<class T, unsigned n>
inline T angle( const vnl_vector_fixed_ref_const<T,n>& a, const vnl_vector<T>& b )
{
  return angle( a.as_ref(), b );
}

template<class T, unsigned n>
inline T angle( const vnl_vector<T>& a, const vnl_vector_fixed_ref_const<T,n>& b )
{
  return angle( a, b.as_ref() );
}


template<class T, unsigned n>
inline T vnl_vector_ssd( const vnl_vector_fixed_ref_const<T,n>& a, const vnl_vector_fixed_ref_const<T,n>& b )
{
  return vnl_vector_ssd( a.as_ref(), b.as_ref() );
}

template<class T, unsigned n>
inline T vnl_vector_ssd( const vnl_vector_fixed_ref_const<T,n>& a, const vnl_vector<T>& b )
{
  return vnl_vector_ssd( a.as_ref(), b );
}

template<class T, unsigned n>
inline T vnl_vector_ssd( const vnl_vector<T>& a, const vnl_vector_fixed_ref_const<T,n>& b )
{
  return vnl_vector_ssd( a, b.as_ref() );
}


// --- I/O operators -------------------------------------------------


//: \relates vnl_vector_fixed
template<class T, unsigned int n>
inline
vcl_ostream& operator<<(vcl_ostream& o,const vnl_vector_fixed_ref_const<T,n>& v)
{
  v.print(o);
  return o;
}

//: \relates vnl_vector_fixed
template<class T, unsigned int n>
inline
vcl_istream& operator>>(vcl_istream& i, const vnl_vector_fixed_ref<T,n>& v)
{
  v.read_ascii(i);
  return i;
}


#endif // vnl_vector_fixed_ref_h_
