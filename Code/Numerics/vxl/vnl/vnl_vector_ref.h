#ifndef vnl_vector_ref_h_
#define vnl_vector_ref_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_vector_ref.h

//: 
//  \file
//  \brief vnl_vector using user-supplied storage
//  \author Andrew W. Fitzgibbon, Oxford RRG, 04 Aug 96 

//
// Modifications
// LSB (Manchester) 19/03/2001: Tidied up the documentation
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_vector.h>

//: vnl_vector using user-supplied storage
//   vnl_vector for which the data space has
//   been supplied externally.
export template <class T>
class vnl_vector_ref : public vnl_vector<T> {
public:
  typedef vnl_vector<T> Base;

  //: Constructor
  // Do *not* call anything else than the default constructor of vnl_vector<T>
  vnl_vector_ref(int n, T *space) : vnl_vector<T>() {
    Base::data = space;
    Base::num_elmts = n;
  }

  //: Destructor
  // Prevents base destructor from releasing memory we don't own
  ~vnl_vector_ref() {
    
    Base::data = 0;
  }

  //private:
  // Private operator new because deleting a pointer to
  // one of these through a baseclass pointer will attempt
  // to free the referenced memory.
  // Therefore disallow newing of these -- if you're paying for
  // one malloc, you can afford two.
  // NOW COMMENTED OUT - PVR, may 97
  //void* operator new(size_t) { return 0; }

  //public:
  // Privatizing other new means we must offer placement new for STL
  //void* operator new(size_t, void* space) { return space; }
};

#endif // vnl_vector_ref_h_
