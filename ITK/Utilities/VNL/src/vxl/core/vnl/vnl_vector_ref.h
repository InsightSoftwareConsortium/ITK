// This is core/vnl/vnl_vector_ref.h
#ifndef vnl_vector_ref_h_
#define vnl_vector_ref_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief vnl_vector using user-supplied storage
//  \author Andrew W. Fitzgibbon, Oxford RRG
//  \date   04 Aug 96
//
// \verbatim
//  Modifications
//   LSB (Manchester) 19/03/2001: Tidied up the documentation
//   Peter Vanroose   27-Jun-2003 Removed .txx as all methods are inlined
// \endverbatim
//-----------------------------------------------------------------------------

#include <vnl/vnl_vector.h>

//: vnl_vector using user-supplied storage
//   vnl_vector for which the data space has
//   been supplied externally.
export template <class T>
class vnl_vector_ref : public vnl_vector<T>
{
 public:
  typedef vnl_vector<T> Base;

  //: Constructor
  // Do *not* call anything else than the default constructor of vnl_vector<T>
  vnl_vector_ref(unsigned n, T *space) : vnl_vector<T>() {
    Base::data = space;
    Base::num_elmts = n;
#if VCL_HAS_SLICED_DESTRUCTOR_BUG
    this->vnl_vector_own_data = 0;
#endif
  }

  //: Copy constructor
  // Do *not* call anything else than the default constructor of vnl_vector<T>
  // (That is why the default copy constructor is *not* good.)
  vnl_vector_ref(vnl_vector_ref<T> const& v) : vnl_vector<T>() {
    Base::data = const_cast<T*>(v.data_block()); // const incorrect!
    Base::num_elmts = v.size();
#if VCL_HAS_SLICED_DESTRUCTOR_BUG
    this->vnl_vector_own_data = 0;
#endif
  }

  //: Destructor
  // Prevents base destructor from releasing memory we don't own
  ~vnl_vector_ref() {
    Base::data = 0;
  }

  //: Reference to self to make non-const temporaries.
  // This is intended for passing vnl_vector_fixed objects to
  // functions that expect non-const vnl_vector references:
  // \code
  //   void mutator( vnl_vector<double>& );
  //   ...
  //   vnl_vector_fixed<double,4> my_v;
  //   mutator( v );          // Both these fail because the temporary vnl_vector_ref
  //   mutator( v.as_ref() ); // cannot be bound to the non-const reference
  //   mutator( v.as_ref().non_const() ); // works
  // \endcode
  // \attention Use this only to pass the reference to a
  // function. Otherwise, the underlying object will be destructed and
  // you'll be left with undefined behaviour.
  vnl_vector_ref& non_const() { return *this; }

 private:

  //: Copy constructor from vnl_vector<T> is disallowed:
  vnl_vector_ref(vnl_vector<T> const&) {}

#if 0 // NOW COMMENTED OUT - PVR, may 97
  // Private operator new because deleting a pointer to
  // one of these through a baseclass pointer will attempt
  // to free the referenced memory.
  // Therefore disallow newing of these -- if you're paying for
  // one malloc, you can afford two.
  void* operator new(vcl_size_t) { return 0; }

 public:
  // Privatizing other new means we must offer placement new for STL
  void* operator new(vcl_size_t, void* space) { return space; }
#endif
};

//: Create a reference vector with part of an existing vector.
template <class T>
inline const vnl_vector_ref<T> vnl_vector_ref_extract(const vnl_vector <T> &v, unsigned start, unsigned len)
{
  return vnl_vector_ref<T>(len, const_cast<T *>(v.data_block()+start));
}

//: Create a reference vector with part of an existing vector.
template <class T>
inline vnl_vector_ref<T> vnl_vector_ref_extract(vnl_vector <T> &v, unsigned start, unsigned len)
{
  return vnl_vector_ref<T>(len, v.data_block()+start);
}


#endif // vnl_vector_ref_h_
