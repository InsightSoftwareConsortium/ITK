/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkFEMP_h
#define itkFEMP_h

#include <iostream>
#include "itkMacro.h"

namespace itk
{
namespace fem
{
/**
 * \class FEMP
 * \brief Pointer used to store polymorphic elements in STL arrays.
 *
 * FEMP holds a pointer to objects of typename T and its derived classes. it
 * behaves like a special kind of pointer. Special pointers to object can
 * be used to store polymorphic arrays in STL. The basic idea of the special
 * pointer is: whatever you do to the pointer (object of class FEMP), is
 * also reflected on the object within (pointed to by m_Data member). For
 * example: if you copy the special pointer, an object within is also copied.
 *
 * Class T should have a member Clone() which produces a copy of an object.
 * This is important in polymorphic classes, where object of the derived
 * class should be created when copying an existing object.
 *
 * Class T should also include typedefs T::Pointer and T::ConstPointer that
 * define standard pointers to the class. Note that these could be
 * SmartPointer classes.
 * \ingroup ITKFEM
 */
template <typename T>
class FEMP
{
public:

  /**
   * Default constructor makes sure that m_Data is 0,
   * to prevent problems when deleting m_Data object
   * on destruction.
   */
  FEMP() : m_Data(0)
  {
  }

  /**
   * Copy constructor. Clone() method is called
   * to duplicate the existing object.
   */
  FEMP(const FEMP & x)
  {
    if( x.m_Data )
      {
#ifdef USE_FEM_CLONE
      m_Data = static_cast<T *>( x.m_Data->Clone().GetPointer() );
#else
      std::cout << "Create Another" << std::endl;
      m_Data = static_cast<T *>( x.m_Data->CreateAnother().GetPointer() );
#endif
      }
    else
      {
      m_Data = ITK_NULLPTR;
      }
  }

  /**
   * Conversion constructor from T::Pointer to FEMP<T>.
   * The object T* must exist and we take ownership of object T*.
   * If you want to create a copy of object and take ownership of that,
   * use: FEMP(x->Clone()) instead of FEMP(x).
   */
  explicit FEMP(typename T::Pointer x) : m_Data(x)
  {
  }

  /**
   * Destructor of a special pointer class also destroys the actual object.
   */
  ~FEMP()
  {
    m_Data = ITK_NULLPTR;
  }

  /**
   * Asignment operator
   */
  const FEMP<T> & operator=(const FEMP<T> & rhs);

  /**
   * Easy access to members of stored object
   */
  typename T::Pointer operator->() const
  {
    return m_Data;
  }

  /**
   * Dereferencing operator provides automatic conversion from
   * special to standard pointer to object
   */
  operator T *() const
    {
    return m_Data;
    }

  /**
   * Return true if special pointer actually points
   * to a valid object and false otherwise.
   */
  bool IsNULL() const
  {
    return m_Data == 0;
  }

private:

  /**
   * Pointer to actual object. Note that this could be a SmartPointer.
   */
  typename T::Pointer m_Data;
};

template <typename T>
const FEMP<T> & FEMP<T>::operator=(const FEMP<T> & rhs)
{
  /** Self assignments don't make sense. */
  if( &rhs != this )
    {
    /**
     * First destroy the existing object on the left hand side
     */
    m_Data = ITK_NULLPTR;

    /**
     * Then clone the one on the right hand side
     * of the expression (if not ITK_NULLPTR).
     */
    if( rhs.m_Data )
      {
#ifdef USE_FEM_CLONE
      m_Data = static_cast<T *>( rhs.m_Data->Clone().GetPointer() );
#else
      m_Data = static_cast<T *>( rhs.m_Data->CreateAnother().GetPointer() );
#endif

      }
    else
      {
      m_Data = ITK_NULLPTR;
      }
    }
  return *this;
}

}
}  // end namespace itk::fem

#endif // #ifndef itkFEMP_h
