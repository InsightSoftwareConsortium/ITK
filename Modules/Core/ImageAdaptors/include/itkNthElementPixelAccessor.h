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
#ifndef __itkNthElementPixelAccessor_h
#define __itkNthElementPixelAccessor_h

#include "itkMacro.h"

namespace itk
{
/** \class NthElementPixelAccessor
 * \brief Give access to the N-th of a Container type
 *
 * This class is intended to be used as parameter of
 * an ImageAdaptor to make a  Container appears as being
 * of scalar type T, showing only the N-th component.
 *
 * This class is templated over the container type.
 * Any container type that provides a method:
 * operator[]( unsigned int ) can be used here,
 * for example: itkPoint, itkVector, itkVectorContainer,
 *              and std::vector.
 *
 * For performance, no bound checking is performed during
 * access to the n-th element.
 *
 * \sa ImageAdaptor
 * \sa PixelAccessor
 *
 * \ingroup ImageAdaptors
 * \ingroup ITK-ImageAdaptors
 */

template< class T, class TContainer >
class ITK_EXPORT NthElementPixelAccessor
{
public:
  /** Standard class typedefs. */
  typedef   NthElementPixelAccessor Self;

  /** that this class will exhibit */
  typedef T ExternalType;

  /** Internal typedef. It defines the internal real
   * representation of data */
  typedef   TContainer InternalType;

  /** Write access to the NthElement component */
  inline void Set(InternalType & output, const ExternalType & input) const
  { output[m_ElementNumber] =  input; }

  /** Read access to the NthElement component */
  inline ExternalType Get(const InternalType & input) const
  { return static_cast< ExternalType >( input[m_ElementNumber] ); }

  /** Get the element number to access in the container */
  unsigned int GetElementNumber(void) const
  { return m_ElementNumber; }

  /** Set the element number to access in the container */
  void SetElementNumber(unsigned int nth)
  { m_ElementNumber = nth; }

  /** operator!=. This is needed to convert a pixel accessor to a functor.
   * \sa AdaptImageFilter */
  bool operator!=(const Self & accessor) const
  {
    return ( m_ElementNumber != accessor.m_ElementNumber );
  }

  /** Assignment operator */
  NthElementPixelAccessor & operator=(const NthElementPixelAccessor & accessor)
  {
    m_ElementNumber = accessor.m_ElementNumber;
    return *this;
  }

  /** Constructor */
  NthElementPixelAccessor()
  {
    m_ElementNumber = 0;
  }

private:
  // Identifier of the N-th element to be accessed
  unsigned int m_ElementNumber;
};
}  // end namespace itk

#endif
