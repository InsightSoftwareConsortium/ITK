// This is core/vnl/vnl_matrix_fixed_ref.h
#ifndef vnl_matrix_fixed_ref_h_
#define vnl_matrix_fixed_ref_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief Fixed size stack-stored vnl_matrix
//
//  > > From: Amitha Perera [mailto:perera@cs.rpi.edu]
//  > > Sent: Monday, October 07, 2002 3:18 PM
//  > > Subject: vnl_vector_fixed_ref
//  > >
//  > > Hi there
//  > >
//  > > I'm working on separating vnl_vector and vnl_vector_fixed in the VXL
//  > > tree, as I mailed a while ago to the vxl-maintainers list. I noticed
//  > > that you'd committed a vnl_vector_fixed_ref class which doesn't seem
//  > > to provide any additional functionality over vnl_vector_ref. May I
//  > > remove it, or is there some use for it?
//  > >
//  > > FYI, the author is listed as "Paul P. Smyth, Vicon Motion
//  > > Systems Ltd."
//  > > and the comment is dated 02 May 2001.
//  > >
//  > > Thanks,
//  > > Amitha.
//  > >
//
//  Paul Smyth <paul.smyth@vicon.com> writes:
//  > Hi,
//  >
//  > The rationale behind it was that I had some (fast) algorithms for
//  > matrix/vector operations that made use of compile-time knowledge of the
//  > vector and matrix sizes.
//  > This was typically appropriate when trying to interpret a fixed-size
//  > subvector within a large vector of parameters as e.g. a translation.
//  >
//  > As I saw it, the various types of vector possible were: (with their current
//  > names)
//  > - pointer to memory, plus compile-time knowledge of vector size ( T*, and enum{size}) = vnl_vector_fixed_ref
//  > - ownership of memory, plus compile-time size = vnl_vector_fixed
//  > - pointer to memory, plus run-time only knowledge of size (T* and size()) = vnl_vector_ref
//  > - ownership of memory, variably sized = vnl_vector
//  >
//  > I had a conversation with Andrew Fitzgibbon, where he reckoned that the best
//  > thing to do with vnl vectors etc. was to create entirely separate types, and
//  > routines for conversion between them (possibly implicitly), rather that
//  > trying to establish a class heirarchy, which may add too many burdens in
//  > terms of object size for small vectors/matrices.
//  >
//  > Sorry - I've now found the debate on the maintaners list!
//  >
//  > Anyway, I believe that vector_fixed_ref is very necessary, and that you
//  > should be able to convert from a vector_fixed to a vector_fixed_ref - say
//  > using an as_ref() member on vector_fixed or standalone function.
//  > And I believe that for the restructured classes, vector_fixed_ref and
//  > vector_fixed should not be related by inheritance, as that would place an
//  > excessive burden on the size of vector_fixed.
//  >
//  > ------
//  > Another issue - do you have a mechanism for dealing with const data safely?
//  > {
//  >   template<typename T, int n>
//  >   vnl_vector_fixed_ref(T* i_Data);
//  >
//  >   void MyFunction(const vnl_vector<double> & Input)
//  >   {
//  >     // take a reference to the first 3 elements of Input
//  >     vnl_vector_fixed_ref<double,3> ref(Input.begin());
//  >     // compiler error - as making vector_fixed_ref from const
//  > double *
//  >   }
//  > }
//  >
//  > The options appear to be
//  > 1) Make a separate class vnl_vector_fixed_ref_const
//  > 2) Make vnl_vector_fixed_ref so it can be instantiated with
//  > vnl_vector_fixed_ref<double,n> AND vnl_vector_fixed_ref<const double,n>, and
//  > gives appropriate behaviour - would probably require a to_const function
//  > which generates vnl_vector_fixed_ref<const T,n> from
//  > vnl_vector_fixed_ref<T,n>
//  >
//  > ------
//  > Another note is that a number of routines that use vector_fixed currently
//  > (e.g. cross_3d) should really use vector_fixed_ref as an input, because they
//  > should be able to operate on fixed vector references as well as fixed
//  > vectors.
//  >
//  > While I'm at it, has it been decided that the vnl_vector and vnl_vector_ref
//  > classes are to remain unchanged? Because having vnl_vector as the base, and
//  > vnl_vector_ref derived from it is a real pain in the backside. A vector
//  > which may or may not own its own memory is a more general type than one
//  > which does own it's own memory, and having vnl_vector as the base means that
//  > all sorts of nastinesses can happen. Simply, a vector_ref Is-not a type of
//  > vector.
//  > If anything, it should be the other way round.
//  >
//  > void DoAssign(vnl_vector<double> & RefToMemoryIDontOwn, const vnl_vector<double> & NewContents)
//  > {
//  >   RefToMemoryIDontOwn = NewContents;
//  > }
//  >
//  > void DeleteTwice()
//  > {
//  >   vnl_vector<double> vec1(3, 0); // size 3 - news 3*double
//  >   vnl_vector<double> vec2(4,1); // size 4 news 4 * double
//  >   vnl_vector_ref<double> ref_to_1(3,vec1.begin()); // copies pointer
//  >   DoAssign(ref_to_1, vec2); // deletes memory owned by 1, news 4 * double
//  >   // vec1 now points to deleted memory, and will crash when goes out of scope
//  > }
//  >
//  > Maybe that issue isn't on your agenda - but it's a bit of a disaster. I know
//  > that fixing this might break some code.
//  >
//  > ---------
//  > Sorry for rolling all these things into one - I'd be interested to know what
//  > you think. But please don't kill my vnl_vector_ref!
//  >
//  > Paul.
//
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
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   04 Aug 96
//
// \verbatim
// Modifications:
//  Peter Vanroose, 27 nov 1996:  added default constructor, which does
//            itself allocate the matrix storage.  Necessary because otherwise
//            the compiler will itself generate a default constructor.
//  4/4/01 LSB (Manchester) Tidied documentation
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
// \endverbatim
//
//-----------------------------------------------------------------------------

#include <vcl_cassert.h>
#include <vcl_cstring.h> // memcpy()
#include <vnl/vnl_matrix.h>

//: Fixed size stack-stored vnl_matrix
// vnl_matrix_fixed_ref is a fixed-size vnl_matrix for which the data space
// has been supplied externally.  This is useful for two main tasks:
//
// (a) Treating some row-based "C" matrix as a vnl_matrix in order to
// perform vnl_matrix operations on it.
//
// (b) Declaring a vnl_matrix that uses entirely stack-based storage for the
// matrix.
//
// The big warning is that returning a vnl_matrix_fixed_ref pointer will free
// non-heap memory if deleted through a vnl_matrix pointer.  This should be
// very difficult though, as vnl_matrix_fixed_ref objects may not be constructed
// using operator new.  This in turn is plausible as the point is to avoid
// such calls.
//
template <class T, unsigned m, unsigned n>
class vnl_matrix_fixed_ref : public vnl_matrix<T>
{
  typedef vnl_matrix<T> Base;
  T* rowspace[m];
 public:

  //: Construct a fixed size matrix which points to the row-stored data space supplied.
  // The space must remain valid for the lifetime of the vnl_matrix_fixed_ref.
  // Alterations to the locations pointed to by space will be (obviously) visible
  // to users of the vnl_matrix_fixed_ref and vice versa.
  vnl_matrix_fixed_ref(T *space = (T*)0) {
    Base::data = rowspace;  // thf. can't derive this from matrixref
    if (!space) space = vnl_c_vector<T>::allocate_T(m*n);
    for (int i = 0; i < m; ++i)
      Base::data[i] = space + i * n;
    Base::num_rows = m;
    Base::num_cols = n;
  }

  //: Destroy this vnl_matrix_fixed_ref after detaching from the space supplied to the constructor.
  ~vnl_matrix_fixed_ref() {
    // Prevent base dtor from releasing our memory
    Base::data[0] = 0;
    Base::data = 0;
  }

  //: Copy a vnl_matrix into our space.
  //  Will cause an assertion failure (i.e. abort) if the rhs is not exactly the same size.
  vnl_matrix_fixed_ref<T, m, n>& operator=(const vnl_matrix<T>& rhs) {
    assert(rhs.rows() == m && rhs.columns() == n);
    vcl_memcpy(data[0], rhs.data_block(), m*n*sizeof(T));
    return *this;
  }

  // Need the assignment operator below because the compiler generated
  // one causes weird behaviour: some how, this.data[0] is set to
  // point to rhs.data[0].

  //: Copy a vnl_matrix into our space.
  //  Will cause an assertion failure (i.e. abort) if the rhs is not exactly the same size.
  vnl_matrix_fixed_ref<T, m, n>& operator=(const vnl_matrix_fixed_ref& rhs) {
    vcl_memcpy(data[0], rhs.data_block(), m*n*sizeof(T));
    return *this;
  }

  //: Resizing a vnl_matrix_ref fails.
  bool resize (unsigned int, unsigned int) { return 0; }

 private:
  // You can't assign one of these from a matrix, cos' you don't have any space
  vnl_matrix_fixed_ref(const vnl_matrix<T>&) {}
  vnl_matrix_fixed_ref(const vnl_matrix_fixed_ref<T,m,n>&) {}

  // Private operator new because deleting a pointer to
  // one of these through a baseclass pointer will attempt
  // to free this in-class memory.
  // Therefore disallow newing of these -- if you're paying for
  // one malloc, you can afford three.
  //
  // New operator restored to avoid problems constructing STL containers
  // - capes Nov 99
};

#endif // vnl_matrix_fixed_ref_h_
