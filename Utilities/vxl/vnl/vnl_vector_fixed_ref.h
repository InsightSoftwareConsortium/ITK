// This is vxl/vnl/vnl_vector_fixed_ref.h
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

#include <vcl_cassert.h>
#include <vnl/vnl_vector_ref.h>

template <class T, unsigned n>
class vnl_vector_fixed_ref : public vnl_vector_ref<T>
{
 public:
  vnl_vector_fixed_ref(vnl_vector<T>& rhs, unsigned int offset)
    : vnl_vector_ref<T>(n, rhs.data_block() + offset)
  {
    assert(& rhs != this);
    assert(rhs.size() >= offset + n);
  }

  explicit vnl_vector_fixed_ref(T *space) : vnl_vector_ref<T>(n, space)
  {
  }

  vnl_vector_fixed_ref<T,n>& operator= (T const& t)
    { vnl_vector<T>::operator=  (t); return *this; }
  vnl_vector_fixed_ref<T,n>& operator+= (T const t)
    { vnl_vector<T>::operator+= (t); return *this; }
  vnl_vector_fixed_ref<T,n>& operator-= (T const t)
    { vnl_vector<T>::operator-= (t); return *this; }
  vnl_vector_fixed_ref<T,n>& operator*= (T const t)
    { vnl_vector<T>::operator*= (t); return *this; }
  vnl_vector_fixed_ref<T,n>& operator/= (T const t)
    { vnl_vector<T>::operator/= (t); return *this; }

  vnl_vector_fixed_ref<T,n>& operator+= (vnl_vector<T> const& rhs)
    { vnl_vector<T>::operator+= (rhs); return *this; }
  vnl_vector_fixed_ref<T,n>& operator-= (vnl_vector<T> const& rhs)
    { vnl_vector<T>::operator-= (rhs); return *this; }


  // do not specialize operations with vnl_matrix, since the size of the result
  // vector is not known at compile time
  //  vnl_vector_fixed<T,n> operator* (vnl_matrix<T> const& m) const;
  //friend vnl_vector_fixed<T,n> operator* (vnl_matrix<T> const& m, vnl_vector_fixed<T,n> const& v);
  // do not specialize extract for the same reason as above
  //  vnl_vector_fixed<T,n> extract (unsigned int len, unsigned int start=0) const; // subvector
  vnl_vector_fixed_ref<T,n>& update (vnl_vector<T> const& v, unsigned int start=0)
    { return (vnl_vector_fixed_ref<T,n>&) vnl_vector<T>::update (v, start); }

  vnl_vector_fixed_ref<T,n>& normalize()  // v /= sqrt(dot(v,v))
    { return (vnl_vector_fixed_ref<T,n>&) vnl_vector<T>::normalize(); }


 public:
  // void these methods on vnl_vector_fixed, since they deallocate the underlying
  // storage
  vnl_vector<T>& pre_multiply (vnl_matrix<T> const&); // v = m * v
  vnl_vector<T>& post_multiply (vnl_matrix<T> const&); // v = v * m
  vnl_vector<T>& operator*= (vnl_matrix<T> const&);
};


#endif // vnl_vector_fixed_ref_h_
