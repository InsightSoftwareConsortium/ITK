#ifndef vnl_vector_ref_h_
#define vnl_vector_ref_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_vector_ref - vnl_vector using user-supplied storage
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_vector_ref.h
// .FILE	vnl_vector_ref.txx
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 04 Aug 96
//
// .SECTION See also
//    vnl_matrix_ref
//
// .SECTION Modifications
//     <none yet>
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_vector.h>

template <class T>
class vnl_vector_ref : public vnl_vector<T> {
public:
  typedef vnl_vector<T> Base;

  // Do *not* call anything else than the default constructor of vnl_vector<T>
  vnl_vector_ref(int n, T *space) : vnl_vector<T>() {
    Base::data = space;
    Base::num_elmts = n;
  }

  ~vnl_vector_ref() {
    // Prevent base dtor from releasing memory we don't own
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
