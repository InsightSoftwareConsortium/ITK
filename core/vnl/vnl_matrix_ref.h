// This is core/vnl/vnl_matrix_ref.h
#ifndef vnl_matrix_ref_h_
#define vnl_matrix_ref_h_
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

#include "vnl_matrix.h"
#include "vnl/vnl_export.h"

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
class VNL_EXPORT vnl_matrix_ref : public vnl_matrix<T>
{
  typedef vnl_matrix<T> Base;

 public:

  // Constructors/Destructors--------------------------------------------------
  vnl_matrix_ref(unsigned int row, unsigned int col, const T *datablck)
    : vnl_matrix<T>(row, col, const_cast<T *>(datablck), false)
  { }

  vnl_matrix_ref(const vnl_matrix_ref<T> & other)
  : vnl_matrix<T>(other.rows(), other.cols(),
      const_cast<T *>(other.data_block()), false)
  { }

  ~vnl_matrix_ref() override = default;

  //: Copy and move constructor from vnl_matrix_ref<T> is disallowed by default
  // due to other constructor definitions.
  //: assignment and move-assignment is disallowed
  //  because it does not define external memory to be managed.
  vnl_matrix_ref & operator=( vnl_matrix_ref<T> const& ) = delete;
  vnl_matrix_ref & operator=( vnl_matrix_ref<T> && ) = delete;

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

 private:
  //: Resizing is disallowed
  bool resize (unsigned int, unsigned int) { return false; }
  //: Resizing is disallowed
  bool make_size (unsigned int, unsigned int) { return false; }
  //: Resizing is disallowed
  bool set_size (unsigned int, unsigned int) { return false; }

};

#endif // vnl_matrix_ref_h_
