/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef itkFEMPArray_h
#define itkFEMPArray_h

#include "itkFEMP.h"
#include "itkFEMException.h"
#include <vector>

namespace itk
{
namespace fem
{
/**
 * \class FEMPArray
 * \brief Array for FEMP objects
 *
 * Array class that holds special pointers to objects. Every object must contain
 * a variable member of type int called GN (global number). You can search for
 * an object with specific GN within an array.
 * \ingroup ITKFEM
 */
template <typename T>
class FEMPArray : public std::vector<FEMP<T>>
{
public:
  /**
   * Standard Self typedef
   */
  using Self = FEMPArray;

  /**
   * Standard Superclass typedef
   */
  using Superclass = std::vector<FEMP<T>>;

  /**
   * Dumb pointer type alias support
   */
  using Pointer = Self *;
  using ConstPointer = const Self *;

  /**
   * Easy access to the base class of objects inside the array.
   */
  using ClassType = T;
  using ClassTypePointer = typename ClassType::Pointer;
  using ClassTypeConstPointer = typename ClassType::ConstPointer;

  /**
   * Finds and returns a pointer to the object with specific global number
   */
  ClassTypePointer
  Find(int gn);

  ClassTypeConstPointer
  Find(int gn) const;

  /**
   * Returns a pointer to i-th object stored in an array (not a pointer to FEMP of that object).
   */
  ClassTypePointer
  operator()(int i)
  {
    return &(*this->operator[](i));
  }

  /**
   * Returns a pointer to i-th object stored in an array (not a pointer to FEMP of that object).
   * This function works on the const arrays.
   */
  ClassTypeConstPointer
  operator()(int i) const
  {
    return &(*this->operator[](i));
  }

  /**
   * Applies new numbers to objects in array so that they are in order (0,1,2,...).
   * This speeds up finding object by global number a lot. The function returns
   * total number of objects in an array.
   */
  int
  Renumber();
};

/**
 * Find function for for non-const objects
 */
template <typename T>
typename FEMPArray<T>::ClassTypePointer
FEMPArray<T>::Find(int gn)
{
  auto it = this->begin();
  auto iend = this->end();
  while (it != iend)
  {
    if ((*it)->GetGlobalNumber() == gn)
    {
      break;
    }
    ++it;
  }

  if (it == this->end())
  {
    /**
     * We din't find an object with that GN...
     */
    throw FEMExceptionObjectNotFound(__FILE__, __LINE__, "FEMPArray::Find() const", typeid(T).name(), gn);
  }

  /**
   * Return a pointer to the found object.
   */
  return &(*(*it));
}

/**
 * Find function for for const objects
 */
template <typename T>
typename FEMPArray<T>::ClassTypeConstPointer
FEMPArray<T>::Find(int gn) const
{
  using ConstIterator = typename Superclass::const_iterator;

  ConstIterator it = this->begin();
  ConstIterator iend = this->end();
  while (it != iend)
  {
    if ((*it)->GetGlobalNumber() == gn)
    {
      break;
    }
    ++it;
  }

  if (it == this->end())
  {
    /**
     * We din't find an object with that GN...
     */
    throw FEMExceptionObjectNotFound(__FILE__, __LINE__, "FEMPArray::Find() const", typeid(T).name(), gn);
  }

  /**
   * Return a pointer to the found object.
   */
  return &(*(*it));
}

template <typename T>
int
FEMPArray<T>::Renumber()
{
  typename Superclass::iterator i;
  int                           j = 0;
  for (i = this->begin(); i != this->end(); ++i)
  {
    (*i)->SetGlobalNumber(j);
    ++j;
  }

  return j;
}

} // end namespace fem
} // end namespace itk

#endif // itkFEMPArray_h
