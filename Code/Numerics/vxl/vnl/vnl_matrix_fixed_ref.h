#ifndef vnl_matrix_fixed_ref_h_
#define vnl_matrix_fixed_ref_h_
// This is vxl/vnl/vnl_matrix_fixed_ref.h

//
//: \file
//  \brief Fixed size stack-stored vnl_matrix
// \author Andrew W. Fitzgibbon, Oxford RRG, 04 Aug 96
//    vnl_matrix_fixed_ref is a fixed-size vnl_matrix for which the data space
//    has been supplied externally.  This is useful for two main tasks:
//
//    (a) Treating some row-based "C" matrix as a vnl_matrix in order to
//    perform vnl_matrix operations on it.
//
//    (b) Declaring a vnl_matrix that uses entirely stack-based storage for the
//    matrix.
//
//    The big warning is that returning a vnl_matrix_fixed_ref pointer will free
//    non-heap memory if deleted through a vnl_matrix pointer.  This should be
//    very difficult though, as vnl_matrix_fixed_ref objects may not be constructed
//    using operator new.  This in turn is plausible as the point is to avoid
//    such calls.
//

// .Modifications:
//     Peter Vanroose, 27 nov 1996:  added default constructor, which does
//            itself allocate the matrix storage.  Necessary because otherwise
//            the compiler will itself generate a default constructor.
//  4/4/01 LSB (Manchester) Tidied documentation
//
//-----------------------------------------------------------------------------

#include <vcl_cassert.h>
#include <vcl_cstring.h> // memcpy()
#include <vnl/vnl_matrix.h>

template <class T, int m, int n>
class vnl_matrix_fixed_ref : public vnl_matrix<T> {
  typedef vnl_matrix<T> Base;
  T* rowspace[m];
public:

//: Construct a fixed size matrix which points to the row-stored
// data space supplied.  The space must remain valid for the lifetime
// of the vnl_matrix_fixed_ref.  Alterations to the locations pointed to by
// space will be (obviously) visible to users of the vnl_matrix_fixed_ref
// and vice versa.
  vnl_matrix_fixed_ref(T *space = (T*)0) {
    Base::data = rowspace;  // thf. can't derive this from matrixref
    if (!space) space = vnl_c_vector<T>::allocate_T(m*n);
    for(int i = 0; i < m; ++i)
      Base::data[i] = space + i * n;
    Base::num_rows = m;
    Base::num_cols = n;
  }

//: Destroy this vnl_matrix_fixed_ref after detaching from the space
// supplied to the constructor.
  ~vnl_matrix_fixed_ref() {
    // Prevent base dtor from releasing our memory
    Base::data[0] = 0;
    Base::data = 0;
  }

//: Copy a vnl_matrix into our space.  Will cause an assertion
// failure (i.e. abort) if the rhs is not exactly the same size.
  vnl_matrix_fixed_ref<T, m, n>& operator=(const vnl_matrix<T>& rhs) {
    assert(rhs.rows() == m && rhs.columns() == n);
    memcpy(data[0], rhs.data_block(), m*n*sizeof(T));
    return *this;
  }

//: Resizing a vnl_matrix_ref fails.
  bool resize (unsigned int, unsigned int) { return 0; }

private:
  // Private operator new because deleting a pointer to
  // one of these through a baseclass pointer will attempt
  // to free this in-class memory.
  // Therefore disallow newing of these -- if you're paying for
  // one malloc, you can afford three.

  // New operator restored to avoid problems constructing STL containers
  // - capes Nov 99

  // You can't assign one of these from a matrix, cos' you don't have any space
  vnl_matrix_fixed_ref(const vnl_matrix<T>&): vnl_matrix<T>() {}
  vnl_matrix_fixed_ref(const vnl_matrix_fixed_ref<T,m,n>&): vnl_matrix<T>() {}
};

#endif // vnl_matrix_fixed_ref_h_
