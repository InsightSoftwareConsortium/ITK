// This is vxl/vnl/vnl_scalar_join_iterator.h
#ifndef vnl_scalar_join_iterator_h_
#define vnl_scalar_join_iterator_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief  Database join on matrix columns
//
//    vnl_scalar_join_iterator implements a fast database join on columns
//    of matrices of scalars.  "Scalar" here really means that the
//    objects have comparison operators.  The cost is O(n log n) where
//    n is the number of rows, all for the two sorts in the ctor.
//
//    CAVEAT: The current implementation fudges multiple occurrences
//    of the same key in the source column.  For example,
//  \verbatim
//    join  1 3 and  3 5 on columns 2 and 1 respectively
//          2 3      3 6
//  \endverbatim
//    should give
//  \verbatim
//          1 3 3 5
//          1 3 3 6
//          2 3 3 5
//          2 3 3 6
//  \endverbatim
//    and it doesn't.  Contact awf if you need this to work.
//
//  \author Andrew W. Fitzgibbon, Oxford RRG
//  \date   27 Dec 96
//
// \verbatim
// Modifications:
// LSB (Manchester) Documentation Tidied
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
// \endverbatim
//
//-----------------------------------------------------------------------------

#include <vcl_list.h>
#include <vnl/vnl_matrix.h>

template <class T>
class vnl_scalar_join_iterator_indexed_pair;

//: Database join on matrix columns.
//  vnl_scalar_join_iterator implements a fast database join on columns
//  of matrices of scalars.  "Scalar" here really means that the
//  objects have comparison operators.  The cost is O(n log n) where
//  n is the number of rows, all for the two sorts in the ctor.
//
//  CAVEAT: The current implementation fudges multiple occurrences
//  of the same key in the source column.  For example,
// \verbatim
//  join  1 3 and  3 5 on columns 2 and 1 respectively
//        2 3      3 6
// \endverbatim
//  should give
// \verbatim
//        1 3 3 5
//        1 3 3 6
//        2 3 3 5
//        2 3 3 6
// \endverbatim
//  and it doesn't.  Contact awf if you need this to work.

template <class T>
class vnl_scalar_join_iterator
{
 protected:
  unsigned n1;
  unsigned n2;
  vcl_list<vnl_scalar_join_iterator_indexed_pair<T> >* pI1;
  vcl_list<vnl_scalar_join_iterator_indexed_pair<T> >* pI2;
  vcl_list<vnl_scalar_join_iterator_indexed_pair<T> >& I1;
  vcl_list<vnl_scalar_join_iterator_indexed_pair<T> >& I2;
  typename vcl_list<vnl_scalar_join_iterator_indexed_pair<T> >::iterator index1;
  typename vcl_list<vnl_scalar_join_iterator_indexed_pair<T> >::iterator index2;

 public:

  //: Initialize this iterator to the join of relation1(:,column1) and relation2(:,column2).
  // The algorithm sorts an array of pointers to each row and
  // traversal of the iterator runs through these to produce the join.
  // After construction the row1() and row2() methods indicate the first pair.
  vnl_scalar_join_iterator(const vnl_matrix<T>& relation1, unsigned column1,
                           const vnl_matrix<T>& relation2, unsigned column2);

  ~vnl_scalar_join_iterator();


  //: Return true if all pairs have been seen.
  operator bool () { return !done(); }

  //: Advance to the next pair.  This is prefix ++.
  inline vnl_scalar_join_iterator<T>& operator ++ () { next(); return *this; }

  bool done();
  void next();

  //: Return the indices of the current rows in the first and second relations.
  unsigned row1();
  //: Return the indices of the current rows in the first and second relations.
  unsigned row2();

 private:
  // Postfix ++ is private as it would be costly to implement.
  vnl_scalar_join_iterator<T>& operator ++ (int);

#if 0
  T object1() { return *I1[index1].object; }
  T object2() { return *I2[index2].object; }
#endif
};

//: Helper class to hold the sorted arrays of indices.
template <class T>
class vnl_scalar_join_iterator_indexed_pair
{
 public:
  const T* object;
  int original_index;

  vnl_scalar_join_iterator_indexed_pair() {}
  vnl_scalar_join_iterator_indexed_pair(const T* object_, int original_index_):object(object_), original_index(original_index_) {}

  bool operator == (const vnl_scalar_join_iterator_indexed_pair<T>& that) const;
  bool operator <  (const vnl_scalar_join_iterator_indexed_pair<T>& that) const;
};

#endif // vnl_scalar_join_iterator_h_
