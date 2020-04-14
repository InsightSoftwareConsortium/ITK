// This is core/vnl/vnl_vector_ref.h
#ifndef vnl_vector_ref_h_
#define vnl_vector_ref_h_
//:
//  \file
//  \brief vnl_vector using user-supplied storage
//  \author Andrew W. Fitzgibbon, Oxford RRG
//  \date   04 Aug 96
//
// \verbatim
//  Modifications
//   LSB (Manchester) 19/03/2001: Tidied up the documentation
//   Peter Vanroose   27-Jun-2003 Removed .hxx as all methods are inlined
// \endverbatim
//-----------------------------------------------------------------------------

#include "vnl_vector.h"
#include "vnl/vnl_export.h"

//: vnl_vector using user-supplied storage
//   vnl_vector for which the data space has
//   been supplied externally.
template <class T>
class VNL_EXPORT vnl_vector_ref : public vnl_vector<T>
{
 public:
  using Base = vnl_vector<T>;

  //: Constructor
  // Do \e not call anything else than the default constructor of vnl_vector<T>
  // NOTE:  The vnl_vector_ref always has write access to the data!
  //        and can only be used on data in a read/write senses.
  //        There is no way to pass 'const T *' in a way that vnl_vector_ref
  //        can preserve memory access.
  vnl_vector_ref(size_t n, T *space);

  //: Copy constructor
  // Do \e not call anything else than the default constructor of vnl_vector<T>
  // (That is why the default copy constructor is \e not good.)
  // NOTE: This interface breaks const correctness,
  vnl_vector_ref(const vnl_vector_ref<T>& v);

  //: Destructor
  // Prevents base destructor from releasing memory we don't own
  ~vnl_vector_ref() override = default;

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
  vnl_vector_ref& non_const();

  //: Copy and move constructor from vnl_matrix_ref<T> is disallowed by default
  // due to other constructor definitions.
  //: assignment and move-assignment is disallowed
  //  because it does not define external memory to be managed.
  vnl_vector_ref & operator=( vnl_vector_ref<T> const& ) = delete;
  vnl_vector_ref & operator=( vnl_vector_ref<T> && ) = delete;

  //: Explicit conversion to a vnl_vector_ref or vnl_vector.
  // This is a cheap conversion for those functions that have an interface
  // for vnl_vector but not for vnl_vector_fixed. There is also a
  // conversion operator that should work most of the time.
  // \sa vnl_vector_ref::non_const
  vnl_vector_ref<T> as_ref() { return *this; }
  const vnl_vector_ref<T> as_ref() const { return *this; }
  vnl_vector<T> as_vector() const { return vnl_vector<T>(this->data_block(), this->size()); }
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
