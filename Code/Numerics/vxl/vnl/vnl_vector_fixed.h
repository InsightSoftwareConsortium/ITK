#ifndef vnl_vector_fixed_h_
#define vnl_vector_fixed_h_
#ifdef __GNUC__
#pragma interface
#endif

#include <vcl_cstring.h> // memcpy()
#include <vnl/vnl_vector_ref.h>
#include <vnl/vnl_c_vector.h>

// .NAME	vnl_vector_fixed - Fixed length stack-stored vnl_vector
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_vector_fixed.h
// .FILE	vnl_vector_fixed.txx
//
// .SECTION Description
//    vnl_vector_fixed is a fixed-length, stack storage vnl_vector.
//    See the docs for vnl_matrix_ref
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 04 Aug 96

//: fixed length  stack-stored vnl_vector.
template <class T, int n>
class vnl_vector_fixed : public vnl_vector_ref<T> {
  typedef vnl_vector_ref<T> Base;
public:
  // -- Construct an uninitialized n-vector
  vnl_vector_fixed():Base(n, space) {}

  // -- Construct an n-vector copy of rhs.  Does not check that rhs
  // is the right size.
  vnl_vector_fixed(vnl_vector<T> const& rhs):Base(n, space) {
    if (rhs.size() != n)
      vnl_error_vector_dimension ("vnl_vector_fixed(const vnl_vector&) ", n, rhs.size());
    memcpy(space, rhs.data_block(), sizeof space);
  }

  // GCC generates (and calls) this even though above should do...
  vnl_vector_fixed(vnl_vector_fixed<T,n> const& rhs):Base(n, space) {
    memcpy(space, rhs.space, sizeof space);
  }

  vnl_vector_fixed (T const& v): Base(n,space) {
    for(int i = 0; i < n; ++i)
      data[i] = v;
  }

  vnl_vector_fixed (T const& px, T const& py, T const& pz): Base(n,space) { // 3D vector (px,py,pz)
    if (n != 3) vnl_error_vector_dimension ("constructor (x,y,z): n != 3", n, 3);
    data[0] = px;
    data[1] = py;
    data[2] = pz;
  }

  vnl_vector_fixed (T const& px, T const& py): Base(n,space) { // 2D vector (px,py)
    if (n != 2) vnl_error_vector_dimension ("constructor (x,y): n != 2", n, 2);
    data[0] = px;
    data[1] = py;
  }

  vnl_vector_fixed<T,n>& operator=(vnl_vector_fixed<T,n> const& rhs) {
    memcpy(space, rhs.space, sizeof space);
    return *this;
  }

  vnl_vector_fixed<T,n>& operator=(vnl_vector<T> const& rhs) {
    if (rhs.size() != n)
      vnl_error_vector_dimension ("operator=", n, rhs.size());
    memcpy(space, rhs.data_block(), sizeof space);
    return *this;
  }

  vnl_vector_fixed<T,n>& operator= (T const& t)
    { vnl_vector<T>::operator=  (t); return *this; }
  vnl_vector_fixed<T,n>& operator+= (T const t)
    { vnl_vector<T>::operator+= (t); return *this; }
  vnl_vector_fixed<T,n>& operator-= (T const t)
    { vnl_vector<T>::operator-= (t); return *this; }
  vnl_vector_fixed<T,n>& operator*= (T const t)
    { vnl_vector<T>::operator*= (t); return *this; }
  vnl_vector_fixed<T,n>& operator/= (T const t)
    { vnl_vector<T>::operator/= (t); return *this; }

  vnl_vector_fixed<T,n>& operator+= (vnl_vector<T> const& rhs)
    { vnl_vector<T>::operator+= (rhs); return *this; }
  vnl_vector_fixed<T,n>& operator-= (vnl_vector<T> const& rhs)
    { vnl_vector<T>::operator-= (rhs); return *this; }

  vnl_vector_fixed<T,n> operator- () const
    { return  (vnl_vector_fixed<T,n> (*this) *= -1); }
  vnl_vector_fixed<T,n> operator+ (T const t) const
    { return  (vnl_vector_fixed<T,n> (*this) += t); }
  vnl_vector_fixed<T,n> operator- (T const t) const
    { return  (vnl_vector_fixed<T,n> (*this) -= t); }
  vnl_vector_fixed<T,n> operator* (T const t) const
    { return  (vnl_vector_fixed<T,n> (*this) *= t); }
  vnl_vector_fixed<T,n> operator/ (T const t) const
    { return  (vnl_vector_fixed<T,n> (*this) /= t); }

  vnl_vector_fixed<T,n> operator+ (vnl_vector<T> const& rhs) const
    { return  (vnl_vector_fixed<T,n> (*this) += rhs); }
  vnl_vector_fixed<T,n> operator- (vnl_vector<T> const& rhs) const
    { return  (vnl_vector_fixed<T,n> (*this) -= rhs); }

  vnl_vector_fixed<T,n> apply(T (*f)(T)) {
    vnl_vector_fixed<T,n> ret;
    vnl_c_vector<T>::apply(this->data, num_elmts, f, ret.data);
    return ret;
  }
  vnl_vector_fixed<T,n> apply(T (*f)(T const&)) {
    vnl_vector_fixed<T,n> ret;
    vnl_c_vector<T>::apply(this->data, num_elmts, f, ret.data);
    return ret;
  }
  // do not specialize operations with vnl_matrix, since the size of the result
  // vector is not known at compile time
  //  vnl_vector_fixed<T,n> operator* (vnl_matrix<T> const& m) const;
  //friend vnl_vector_fixed<T,n> operator* (vnl_matrix<T> const& m, vnl_vector_fixed<T,n> const& v);
  // do not specialize extract for the same reason as above
  //  vnl_vector_fixed<T,n> extract (unsigned int len, unsigned int start=0) const; // subvector
  vnl_vector_fixed<T,n>& update (vnl_vector<T> const& v, unsigned int start=0)
    { return (vnl_vector_fixed<T,n>&) vnl_vector<T>::update (v, start); }

  vnl_vector_fixed<T,n>& normalize()	 // v /= sqrt(dot(v,v))
    { return (vnl_vector_fixed<T,n>&) vnl_vector<T>::normalize(); }

  friend vnl_vector_fixed<T,n> element_product VCL_NULL_TMPL_ARGS (vnl_vector_fixed<T,n> const&,
								       vnl_vector_fixed<T,n> const&);
  friend vnl_vector_fixed<T,n> element_quotient VCL_NULL_TMPL_ARGS (vnl_vector_fixed<T,n> const&,
									vnl_vector_fixed<T,n> const&);

public:
  // void these methods on vnl_vector_fixed, since they deallocate the underlying
  // storage
  vnl_vector<T>& pre_multiply (vnl_matrix<T> const&); // v = m * v
  vnl_vector<T>& post_multiply (vnl_matrix<T> const&); // v = v * m
  vnl_vector<T>& operator*= (vnl_matrix<T> const&);

private:
  T space[n];
};

#ifndef VCL_SUNPRO_CC_50 // does not allow funtions templated over non-types.
// define inline friends.
template <class T, int n>
inline vnl_vector_fixed<T,n> operator+(T const t, vnl_vector_fixed<T,n> const & rhs)
{ return  (vnl_vector_fixed<T,n> (rhs) += t); }

template <class T, int n>
inline vnl_vector_fixed<T,n> operator-(T const t, vnl_vector_fixed<T,n> const & rhs)
{ return  (( - vnl_vector_fixed<T,n> (rhs)) += t); }

template <class T, int n>
inline vnl_vector_fixed<T,n> operator*(T const t, vnl_vector_fixed<T,n> const& rhs)
{ return  (vnl_vector_fixed<T,n> (rhs) *= t); }

template <class T, int n>
inline vnl_vector_fixed<T,n> element_product (vnl_vector_fixed<T,n> const& a,
				       vnl_vector_fixed<T,n> const& b)
{
  vnl_vector_fixed<T,n> ret (a);
  for (int i=0; i<n; i++) ret[i] *= b[i];
  return ret;
}

template <class T, int n>
inline vnl_vector_fixed<T,n> element_quotient (vnl_vector_fixed<T,n> const& a,
					vnl_vector_fixed<T,n> const& b)
{
  vnl_vector_fixed<T,n> ret (a);
  for (int i=0; i<n; i++) ret[i] /= b[i];
  return ret;
}
#endif

#if defined(VCL_GCC_27) || defined(VCL_SGI_CC_7)
template <class T, int n>
inline
vcl_ostream &operator<<(vcl_ostream &os, vnl_vector_fixed<T, n> const &v) {
  return os << (vnl_vector<T>const&)v;
}
#endif

//                                        what's this?
#if !defined (VCL_SUNPRO_CC) || ! defined (_ODI_OSSG_)
vnl_vector_fixed<double,3> cross_3d (vnl_vector_fixed<double,3> const& vect1,
				     vnl_vector_fixed<double,3> const& vect2);
vnl_vector_fixed<float,3> cross_3d (vnl_vector_fixed<float,3> const& vect1,
				    vnl_vector_fixed<float,3> const& vect2);
vnl_vector_fixed<int,3> cross_3d (vnl_vector_fixed<int,3> const& vect1,
				  vnl_vector_fixed<int,3> const& vect2);
#endif

#endif // vnl_vector_fixed_h_
