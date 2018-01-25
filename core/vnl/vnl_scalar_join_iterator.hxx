// This is core/vnl/vnl_scalar_join_iterator.hxx
#ifndef vnl_scalar_join_iterator_hxx_
#define vnl_scalar_join_iterator_hxx_
//:
// \file
//
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   27 Dec 96
//
//-----------------------------------------------------------------------------

#include <list>
#include <iostream>
#include "vnl_scalar_join_iterator.h"
#include <vcl_compiler.h>

#define VNL_SCALAR_JOIN_ITERATOR_INSTANTIATE(T) \
template class VNL_EXPORT vnl_scalar_join_iterator_indexed_pair<T >;\
template class VNL_EXPORT vnl_scalar_join_iterator<T >; \
template VNL_EXPORT std::ostream& operator<<(std::ostream& s, const vnl_scalar_join_iterator_indexed_pair<T >& p);\

#include <vcl_cassert.h>
#include <vnl/vnl_matrix.h>

// Helper class to hold the sorted arrays of indices.

template <class T>
bool vnl_scalar_join_iterator_indexed_pair<T>::operator ==
    (const vnl_scalar_join_iterator_indexed_pair<T>& that) const
{
  return (*that.object) == (*object);
}

template <class T>
bool vnl_scalar_join_iterator_indexed_pair<T>::operator <
    (const vnl_scalar_join_iterator_indexed_pair<T>& that) const
{
  return (*object) < (*that.object);
}

template <class T>
std::ostream& operator<<(std::ostream& s,
                        const vnl_scalar_join_iterator_indexed_pair<T>& p)
{
  return s << p.original_index << ' ' << *(p.object) << '\n';
}

template <class T>
vnl_scalar_join_iterator<T>::vnl_scalar_join_iterator
    (const vnl_matrix<T>& relation1, unsigned column1,
     const vnl_matrix<T>& relation2, unsigned column2):
  n1(relation1.rows()),
  n2(relation2.rows()),
  pI1(new std::list<vnl_scalar_join_iterator_indexed_pair<T > >(n1)),
  pI2(new std::list<vnl_scalar_join_iterator_indexed_pair<T > >(n2)),
  I1(*pI1),
  I2(*pI2)
{
  // Sort on appropriate columns
  {
    for (unsigned i = 0; i < n1; ++i)
      I1.push_back(vnl_scalar_join_iterator_indexed_pair<T>(&relation1(i, column1), i));
    I1.sort();
  }
  {
    for (unsigned i = 0; i < n2; ++i)
      I2.push_back(vnl_scalar_join_iterator_indexed_pair<T>(&relation2(i, column2), i));
    I2.sort();
  }

  // Initialize for iteration
  index1 = I1.begin();
  index2 = I2.begin();

  // Loop to first
  for (;;) {
    T star1 = *(*index1).object;
    T star2 = *(*index2).object;
    if (star1 == star2)
      return;

    if (star1 > star2)
      ++index2;
    else
      ++index1;
  }
}

//: Destructor
template <class T>
vnl_scalar_join_iterator<T>::~vnl_scalar_join_iterator()
{
  delete pI1;
  delete pI2;
}

template <class T>
bool vnl_scalar_join_iterator<T>::done() const
{
  return (index1 == I1.end()) || (index2 == I2.end());
}

//: Increment the iterator to point to the next pair of rows.
template <class T>
void vnl_scalar_join_iterator<T>::next()
{
  T obj1 = *(*index1).object;
  // increment i2, check if still valid/same
  if (++index2 == I2.end()) return;

  T nextobj2 = *(*index2).object;
  if (obj1 == nextobj2)
    return; // Found another match

  // nextobj2 must not be < obj1
  assert(!(nextobj2 < obj1));

  // So, objects are different (in fact, obj1 > obj2 right now), lockstep until
  // they match or we're done.
  while (!done()) {
    T obj1 = *(*index1).object;
    T obj2 = *(*index2).object;

    if (obj1 == obj2) {
      // If they're equal, hack back along obj2's array to find the start of the
      // stretch of equal ones.  This allows join
      //      1 3     3 5
      //      2 3     3 6
      // to return the kronecker product of the sets by iteration.
      // No that's going to be a hack.  Will be fixed RSN.
      return;
    }

    if (obj1 > obj2)
      ++index2;
    else
      ++index1;
  }
}

template <class T>
unsigned vnl_scalar_join_iterator<T>::row1() const
{
  return (*index1).original_index;
}

template <class T>
unsigned vnl_scalar_join_iterator<T>::row2() const
{
  return (*index2).original_index;
}

//: Postfix ++ should not be used. Only present for instantiation purposes.
template <class T>
vnl_scalar_join_iterator<T> vnl_scalar_join_iterator<T>::operator++(int)
{
  std::cerr << "This should not happen! postfix ++ called\n";
  return *this;
}

#endif // vnl_scalar_join_iterator_hxx_
