// This is core/vnl/vnl_vector.h
#ifndef vnl_vector_h_
#define vnl_vector_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \author Andrew W. Fitzgibbon
//
// \verbatim
// Modifications
// Comments re-written by Tim Cootes, for his sins.
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
//   Mar.2004 - Peter Vanroose - deprecated fixed-size constructors now compile only when VNL_CONFIG_LEGACY_METHODS==1
// \endverbatim

#include <vcl_iosfwd.h>
#include <vnl/vnl_tag.h>
#include <vnl/vnl_c_vector.h>
#include <vnl/vnl_config.h>
#ifndef NDEBUG
# include <vnl/vnl_error.h>
# if VNL_CONFIG_CHECK_BOUNDS
#  include <vcl_cassert.h>
# endif
#else
# undef VNL_CONFIG_CHECK_BOUNDS
# define VNL_CONFIG_CHECK_BOUNDS 0
# undef ERROR_CHECKING
#endif
#if VNL_CONFIG_LEGACY_METHODS
# include <vcl_deprecated.h>
#endif

export template <class T> class vnl_vector;
export template <class T> class vnl_matrix;

//----------------------------------------------------------------------

#define v vnl_vector<T>
#define m vnl_matrix<T>
template <class T> T      dot_product(v const&, v const&);
template <class T> T      inner_product(v const&, v const&);
template <class T> T      bracket(v const &, m const &, v const &);
template <class T> T      cos_angle(v const&, v const& );
template <class T> double angle(v const&, v const&);
template <class T> m      outer_product(v const&, v const&);
template <class T> v      operator+(T, v const&);
template <class T> v      operator-(T, v const&);
template <class T> v      operator*(T, v const&);
// also exists as method: template <class T> v      operator*(m const&, v const&);
template <class T> v      operator*(v const&, m const&);
template <class T> v      element_product(v const&,v const&);
template <class T> v      element_quotient(v const&,v const&);
template <class T> T      vnl_vector_ssd(v const&, v const&);
template <class T> void   swap(v &, v &);
#undef v
#undef m

//----------------------------------------------------------------------

//: Mathematical vector class, templated by type of element.
// The vnl_vector<T> class implements one-dimensional arithmetic
// vectors to be used with the vnl_matrix<T> class. vnl_vector<T>
// has size fixed by constructor time or changed by assignment
// operator.
// For faster, non-mallocing vectors with size known at compile
// time, use vnl_vector_fixed* or vnl_T_n (e.g. vnl_double_3).
//
// NOTE: Vectors are indexed from zero!  Thus valid elements are [0,size()-1].
template<class T>
class vnl_vector
{
 public:
  friend class vnl_matrix<T>;

  //: Creates an empty vector. O(1).
  vnl_vector() : num_elmts(0) , data(0) {}

  //: Creates vector containing n elements.
  // Elements are not initialized.
  explicit vnl_vector(unsigned len);

  //: Creates vector of len elements, all set to v0
  vnl_vector(unsigned len, T const& v0);

  //: Creates a vector of specified length and initialize first n elements with values. O(n).
  vnl_vector(unsigned len, int n, T const values[]);

#if VNL_CONFIG_LEGACY_METHODS // these constructors are deprecated and should not be used
  //: Creates a vector of length 2 and initializes with the arguments, px,py.
  //  Requires that len==2.
  //  Consider using vnl_vector_fixed<T,2> instead!
  // \deprecated
  vnl_vector(unsigned len, T const& px, T const& py);

  //: Creates a vector of length 3 and initializes with the arguments, px,py,pz.
  //  Requires that len==3.
  //  Consider using vnl_vector_fixed<T,3> instead!
  // \deprecated
  vnl_vector(unsigned len, T const& px, T const& py, T const& pz);

  //: Creates a vector of length 4 and initializes with the arguments.
  //  Requires that len==4.
  //  Consider using vnl_vector_fixed<T,4> instead!
  // \deprecated
  vnl_vector(unsigned len, T const& px, T const& py, T const& pz, T const& pw);
#endif

  //: Create n element vector and copy data from data_block
  vnl_vector(T const* data_block,unsigned int n);

  //: Copy constructor
  vnl_vector(vnl_vector<T> const&);

#ifndef VXL_DOXYGEN_SHOULD_SKIP_THIS
// <internal>
  // These constructors are here so that operator* etc can take
  // advantage of the C++ return value optimization.
  vnl_vector(vnl_vector<T> const &, vnl_vector<T> const &, vnl_tag_add); // v + v
  vnl_vector(vnl_vector<T> const &, vnl_vector<T> const &, vnl_tag_sub); // v - v
  vnl_vector(vnl_vector<T> const &, T,                     vnl_tag_mul); // v * s
  vnl_vector(vnl_vector<T> const &, T,                     vnl_tag_div); // v / s
  vnl_vector(vnl_vector<T> const &, T,                     vnl_tag_add); // v + s
  vnl_vector(vnl_vector<T> const &, T,                     vnl_tag_sub); // v - s
  vnl_vector(vnl_matrix<T> const &, vnl_vector<T> const &, vnl_tag_mul); // M * v
  vnl_vector(vnl_vector<T> const &, vnl_matrix<T> const &, vnl_tag_mul); // v * M
  vnl_vector(vnl_vector<T> &that, vnl_tag_grab)
    : num_elmts(that.num_elmts), data(that.data)
  { that.num_elmts=0; that.data=0; } // "*this" now uses "that"'s data.
// </internal>
#endif

  //: Destructor
  ~vnl_vector();

  //: Return the length, number of elements, dimension of this vector.
  unsigned size() const { return num_elmts; }

  //: Put value at given position in vector.
  inline void put(unsigned int i, T const&);

  //: Get value at element i
  inline T get(unsigned int i) const;

  //: Set all values to v
  void fill(T const& v);

  //: Sets elements to ptr[i]
  //  Note: ptr[i] must be valid for i=0..size()-1
  void copy_in(T const * ptr);

  //: Copy elements to ptr[i]
  //  Note: ptr[i] must be valid for i=0..size()-1
  void copy_out(T *) const; // from vector to array[].


  //: Sets elements to ptr[i]
  //  Note: ptr[i] must be valid for i=0..size()-1
  void set(T const *ptr) { copy_in(ptr); }

  //: Return reference to the element at specified index.
  // There are assert style boundary checks - #define NDEBUG to turn them off.
  T       & operator()(unsigned int i)
  {
#if VNL_CONFIG_CHECK_BOUNDS
    assert(i<size());   // Check the index is valid.
#endif
    return data[i];
  }
  //: Return reference to the element at specified index. No range checking.
  // There are assert style boundary checks - #define NDEBUG to turn them off.
  T const & operator()(unsigned int i) const
  {
#if VNL_CONFIG_CHECK_BOUNDS
    assert(i<size());   // Check the index is valid
#endif
    return data[i];
  }

  //: Return reference to the element at specified index. No range checking.
  T       & operator[](unsigned int i) { return data[i]; }
  //: Return reference to the element at specified index. No range checking.
  T const & operator[](unsigned int i) const { return data[i]; }

  //: Set all elements to value v
  vnl_vector<T>& operator=(T const&v) { fill(v); return *this; }

  //: Copy operator
  vnl_vector<T>& operator=(vnl_vector<T> const& rhs);

  //: Add scalar value to all elements
  vnl_vector<T>& operator+=(T );

  //: Subtract scalar value from all elements
  vnl_vector<T>& operator-=(T value) { return *this += (-value); }

  //: Multiply all elements by scalar
  vnl_vector<T>& operator*=(T );

  //: Divide all elements by scalar
  vnl_vector<T>& operator/=(T );

  //: Add rhs to this and return *this
  vnl_vector<T>& operator+=(vnl_vector<T> const& rhs);

  //: Subtract rhs from this and return *this
  vnl_vector<T>& operator-=(vnl_vector<T> const& rhs);

  //: *this = M*(*this) where M is a suitable matrix.
  //  this is treated as a column vector
  vnl_vector<T>& pre_multiply(vnl_matrix<T> const& M);

  //: *this = (*this)*M where M is a suitable matrix.
  //  this is treated as a row vector
  vnl_vector<T>& post_multiply(vnl_matrix<T> const& M);

  //: *this = (*this)*M where M is a suitable matrix.
  //  this is treated as a row vector
  vnl_vector<T>& operator*=(vnl_matrix<T> const& m) { return this->post_multiply(m); }

  //: Unary plus operator
  // Return new vector = (*this)
  vnl_vector<T> operator+() const { return *this; }

  //: Unary minus operator
  // Return new vector = -1*(*this)
  vnl_vector<T> operator-() const;

  vnl_vector<T> operator+(T v) const { return vnl_vector<T>(*this, v, vnl_tag_add()); }
  vnl_vector<T> operator-(T v) const { return vnl_vector<T>(*this, v, vnl_tag_sub()); }
  vnl_vector<T> operator*(T v) const { return vnl_vector<T>(*this, v, vnl_tag_mul()); }
  vnl_vector<T> operator/(T v) const { return vnl_vector<T>(*this, v, vnl_tag_div()); }

  vnl_vector<T> operator+(vnl_vector<T> const& v) const { return vnl_vector<T>(*this, v, vnl_tag_add()); }
  vnl_vector<T> operator-(vnl_vector<T> const& v) const { return vnl_vector<T>(*this, v, vnl_tag_sub()); }
  vnl_vector<T> operator*(vnl_matrix<T> const& M) const { return vnl_vector<T>(*this, M, vnl_tag_mul()); }

  //--------------------------------------------------------------------------------

  //: Access the contiguous block storing the elements in the vector. O(1).
  //  data_block()[0] is the first element of the vector
  T const* data_block() const { return data; }

  //: Access the contiguous block storing the elements in the vector. O(1).
  //  data_block()[0] is the first element of the vector
  T      * data_block() { return data; }

  //: Type defs for iterators
  typedef T element_type;
  //: Type defs for iterators
  typedef T       *iterator;
  //: Iterator pointing to start of data
  iterator begin() { return data; }

  //: Iterator pointing to element beyond end of data
  iterator end() { return data+num_elmts; }

  //: Const iterator type
  typedef T const *const_iterator;
  //: Iterator pointing to start of data
  const_iterator begin() const { return data; }
  //: Iterator pointing to element beyond end of data
  const_iterator end() const { return data+num_elmts; }

  //: Return a reference to this.
  // Useful in code which would prefer not to know if its argument
  // is a vector, vector_ref or a vector_fixed.  Note that it doesn't
  // return a vector_ref, so it's only useful in templates or macros.
  vnl_vector<T> const& as_ref() const { return *this; }

  //: Return a reference to this.
  vnl_vector<T>&       as_ref()       { return *this; }

  //: Applies function to elements
  vnl_vector<T> apply(T (*f)(T)) const;
  //: Applies function to elements
  vnl_vector<T> apply(T (*f)(T const&)) const;

  //: Returns a subvector specified by the start index and length. O(n).
  vnl_vector<T> extract(unsigned int len, unsigned int start=0) const;

  //: Replaces elements with index beginning at start, by values of v. O(n).
  vnl_vector<T>& update(vnl_vector<T> const&, unsigned int start=0);

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
  vnl_vector<T>& normalize() { vnl_c_vector<T>::normalize(begin(), size()); return *this; }

  // These next 6 functions are should really be helper functions since they aren't
  // really proper functions on a vector in a philosophial sense.

  //: Root Mean Squares of values
  abs_t rms() const { return vnl_c_vector<T>::rms_norm(begin(), size()); }

  //: Smallest value
  T min_value() const { return vnl_c_vector<T>::min_value(begin(), size()); }

  //: Largest value
  T max_value() const { return vnl_c_vector<T>::max_value(begin(), size()); }

  //: Mean of values in vector
  T mean() const { return vnl_c_vector<T>::mean(begin(), size()); }

  //: Sum of values in a vector
  T sum() const { return vnl_c_vector<T>::sum(begin(), size()); }

  //: Reverse the order of the elements
  //  Element i swaps with element size()-1-i
  void flip();

  //: Set this to that and that to this
  void swap(vnl_vector<T> & that);

#if VNL_CONFIG_LEGACY_METHODS // these methods are deprecated and should not be used
  //: Return first element of vector
  // \deprecated
  T& x() const { VXL_DEPRECATED("vnl_vector<T>::x()"); return data[0]; }
  //: Return second element of vector
  // \deprecated
  T& y() const { VXL_DEPRECATED("vnl_vector<T>::y()"); return data[1]; }
  //: Return third element of vector
  // \deprecated
  T& z() const { VXL_DEPRECATED("vnl_vector<T>::z()"); return data[2]; }
  //: Return fourth element of vector
  // \deprecated
  T& t() const { VXL_DEPRECATED("vnl_vector<T>::t()"); return data[3]; }
  //: Set the first element (with bound checking)
  // \deprecated
  void set_x(T const&xx) { VXL_DEPRECATED("vnl_vector<T>::set_x()"); if (size() >= 1) data[0] = xx; }
  //: Set the second element (with bound checking)
  // \deprecated
  void set_y(T const&yy) { VXL_DEPRECATED("vnl_vector<T>::set_y()"); if (size() >= 2) data[1] = yy; }
  //: Set the third element (with bound checking)
  // \deprecated
  void set_z(T const&zz) { VXL_DEPRECATED("vnl_vector<T>::set_z()"); if (size() >= 3) data[2] = zz; }
  //: Set the fourth element (with bound checking)
  // \deprecated
  void set_t(T const&tt) { VXL_DEPRECATED("vnl_vector<T>::set_t()"); if (size() >= 4) data[3] = tt; }
#endif // VNL_CONFIG_LEGACY_METHODS

  //: Check that size()==sz if not, abort();
  // This function does or tests nothing if NDEBUG is defined
  void assert_size(unsigned sz) const {
#ifndef NDEBUG
    assert_size_internal(sz);
#endif
  }

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
  bool empty() const { return !data || !num_elmts; }

  //: Return true if *this == v
  bool operator_eq(vnl_vector<T> const& v) const;

  //: Equality test
  bool operator==(vnl_vector<T> const &that) const { return  this->operator_eq(that); }

  //: Inequality test
  bool operator!=(vnl_vector<T> const &that) const { return !this->operator_eq(that); }

  //: Resize to n elements.
  // This is a destructive resize, in that the old data is lost if size() != \a n before the call.
  // If size() is already \a n, this is a null operation.
  bool set_size(unsigned n);

  //: Make the vector as if it had been default-constructed.
  void clear();


  //: Read from text stream
  bool read_ascii(vcl_istream& s);

  //: Read from text stream
  static vnl_vector<T> read(vcl_istream& s);

 protected:
  unsigned num_elmts;           // Number of elements (length)
  T* data;                      // Pointer to the actual data

#if VCL_HAS_SLICED_DESTRUCTOR_BUG
  // Since this bug exists, we need a flag that can be set during
  // construction to tell our destructor whether we own data.
  char vnl_vector_own_data;
#endif

  void assert_size_internal(unsigned sz) const;
  void assert_finite_internal() const;

  void destroy();

#if VCL_NEED_FRIEND_FOR_TEMPLATE_OVERLOAD
#ifndef DOXYGEN_SHOULD_SKIP_THIS
# define v vnl_vector<T>
# define m vnl_matrix<T>
#endif // DOXYGEN_SHOULD_SKIP_THIS
  friend T      dot_product      VCL_NULL_TMPL_ARGS (v const&, v const&);
  friend T      inner_product    VCL_NULL_TMPL_ARGS (v const&, v const&);
  friend T      bracket          VCL_NULL_TMPL_ARGS (v const&, m const&, v const&);
  friend T      cos_angle        VCL_NULL_TMPL_ARGS (v const&, v const&);
  friend double angle            VCL_NULL_TMPL_ARGS (v const&, v const&);
  friend m      outer_product    VCL_NULL_TMPL_ARGS (v const&, v const&);
  friend v      operator+        VCL_NULL_TMPL_ARGS (T const,  v const&);
  friend v      operator-        VCL_NULL_TMPL_ARGS (T const,  v const&);
  friend v      operator*        VCL_NULL_TMPL_ARGS (T const,  v const&);
  friend v      operator*        VCL_NULL_TMPL_ARGS (m const&, v const&);
  friend v      element_product  VCL_NULL_TMPL_ARGS (v const&, v const&);
  friend v      element_quotient VCL_NULL_TMPL_ARGS (v const&, v const&);
# undef v
# undef m
#endif

  // inline function template instantiation hack for gcc 2.97 -- fsm
  static void inline_function_tickler();
};


// Definitions of inline functions


//: Gets the element at specified index and return its value. O(1).
// Range check is performed.

template <class T>
inline T vnl_vector<T>::get(unsigned int index) const
{
#ifdef ERROR_CHECKING
  if (index >= this->num_elmts)     // If invalid index specified
    vnl_error_vector_index("get", index);  // Raise exception
#endif
  return this->data[index];
}

//: Puts the value at specified index. O(1).
// Range check is performed.

template <class T>
inline void vnl_vector<T>::put(unsigned int index, T const& value)
{
#ifdef ERROR_CHECKING
  if (index >= this->num_elmts)     // If invalid index specified
    vnl_error_vector_index("put", index); // Raise exception
#endif
  this->data[index] = value;    // Assign data value
}

//: multiply matrix and (column) vector. O(m*n).
// \relates vnl_vector
// \relates vnl_matrix
template<class T>
inline vnl_vector<T> operator*(vnl_matrix<T> const& m, vnl_vector<T> const& v)
{
  return vnl_vector<T>(m, v, vnl_tag_mul());
}

//: add scalar and vector. O(n).
// \relates vnl_vector
template<class T>
inline vnl_vector<T> operator+(T s, vnl_vector<T> const& v)
{
  return vnl_vector<T>(v, s, vnl_tag_add());
}

//: subtract vector from scalar. O(n).
// \relates vnl_vector
template<class T>
inline vnl_vector<T> operator-(T s, vnl_vector<T> const& v)
{
  return vnl_vector<T>(-v, s, vnl_tag_add());
}

//: multiply scalar and vector. O(n).
// \relates vnl_vector
template<class T>
inline vnl_vector<T> operator*(T s, vnl_vector<T> const& v)
{
  return vnl_vector<T>(v, s, vnl_tag_mul());
}

//: Interchange the two vectors
// \relates vnl_vector
template<class T>
inline void swap(vnl_vector<T> &a, vnl_vector<T> &b) { a.swap(b); }

//: Euclidean Distance between two vectors.
// Sum of Differences squared.
// \relates vnl_vector
template<class T>
inline T vnl_vector_ssd(vnl_vector<T> const& v1, vnl_vector<T> const& v2)
{
#ifndef NDEBUG
  if (v1.size() != v2.size())
    vnl_error_vector_dimension("vnl_vector_ssd", v1.size(), v2.size());
#endif
  return vnl_c_vector<T>::euclid_dist_sq(v1.begin(), v2.begin(), v1.size());
}

// Non-vector functions which are nevertheless very useful.

//: Write vector to a vcl_ostream
// \relates vnl_vector
export template <class T> vcl_ostream& operator<<(vcl_ostream &, vnl_vector<T> const&);
//: Read vector from a vcl_istream
// \relates vnl_vector
export template <class T> vcl_istream& operator>>(vcl_istream &, vnl_vector<T>      &);

#endif // vnl_vector_h_
