// This is core/vnl/vnl_matrix_ref.h
#ifndef vnl_matrix_ref_h_
#define vnl_matrix_ref_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief vnl_matrix reference to user-supplied storage.
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   04 Aug 96
//
// \verbatim
//  Modifications
//   Documentation updated by Ian Scott 12 Mar 2000
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
// \endverbatim
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix.h>

//: vnl_matrix reference to user-supplied storage
//    vnl_matrix_ref is a vnl_matrix for which the data space has been
//    supplied externally.  This is useful for two main tasks:
//    (a) Treating some row-based "C" matrix as a vnl_matrix in order to
//    perform vnl_matrix operations on it.
//
//    This is a dangerous class.  I believe that I've covered all the bases, but
//    it's really only intended for interfacing with the Fortran routines.
//
//    The big warning is that returning a vnl_matrix_ref pointer will free non-heap
//    memory if deleted through a vnl_matrix pointer.  This should be
//    very difficult though, as vnl_matrix_ref objects may not be constructed using
//    operator new, and are therefore unlikely to be the unwitting subject
//    of an operator delete.
template <class T>
class vnl_matrix_ref : public vnl_matrix<T>
{
  typedef vnl_matrix<T> Base;

 public:
  // Constructors/Destructors--------------------------------------------------
  vnl_matrix_ref(unsigned int m, unsigned int n, T *datablck) {
    Base::data = vnl_c_vector<T>::allocate_Tptr(m);
    for (unsigned int i = 0; i < m; ++i)
      Base::data[i] = datablck + i * n;
    Base::num_rows = m;
    Base::num_cols = n;
#if VCL_HAS_SLICED_DESTRUCTOR_BUG
    vnl_matrix_own_data = 0;
#endif
  }

  vnl_matrix_ref(vnl_matrix_ref<T> const & other) : vnl_matrix<T>() {
    Base::data = vnl_c_vector<T>::allocate_Tptr(other.rows());
    for (unsigned int i = 0; i < other.rows(); ++i)
      Base::data[i] = const_cast<T*>(other.data_block()) + i * other.cols();
    Base::num_rows = other.rows();
    Base::num_cols = other.cols();
#if VCL_HAS_SLICED_DESTRUCTOR_BUG
    vnl_matrix_own_data = 0;
#endif
  }

  ~vnl_matrix_ref() {
    Base::data[0] = 0; // Prevent base dtor from releasing our memory
  }

  //: Reference to self to make non-const temporaries.
  // This is intended for passing vnl_matrix_fixed objects to
  // functions that expect non-const vnl_matrix references:
  // \code
  //   void mutator( vnl_matrix<double>& );
  //   ...
  //   vnl_matrix_fixed<double,5,3> my_m;
  //   mutator( m );        // Both these fail because the temporary vnl_matrix_ref
  //   mutator( m.as_ref() );  // cannot be bound to the non-const reference
  //   mutator( m.as_ref().non_const() ); // works
  // \endcode
  // \attention Use this only to pass the reference to a
  // function. Otherwise, the underlying object will be destructed and
  // you'll be left with undefined behaviour.
  vnl_matrix_ref& non_const() { return *this; }

#if 0
 private:
  // Private operator new because deleting a pointer to
  // one of these through a baseclass pointer will attempt
  // to free this in-class memory.
  // Therefore disallow newing of these -- if you're paying for
  // one malloc, you can afford three.
   // fsm: This was wrong for two reasons:
   //  1. operator new may not return a null pointer.
   //  2. it should be enabled for compilers that need it,
   //     not disabled for compilers that don't need it.
#include <vcl_new.h>
  void* operator new(vcl_size_t) { return 0; }
#endif

 private:
  //: Resizing is disallowed
  bool resize (unsigned int, unsigned int) { return false; }
  //: Resizing is disallowed
  bool make_size (unsigned int, unsigned int) { return false; }
  //: Resizing is disallowed
  bool set_size (unsigned int, unsigned int) { return false; }

  //: Copy constructor from vnl_matrix<T> is disallowed
  // (because it would create a non-const alias to the matrix)
  vnl_matrix_ref(vnl_matrix<T> const &) {}
};

#endif // vnl_matrix_ref_h_
