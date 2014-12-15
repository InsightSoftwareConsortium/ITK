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
#ifndef itkNthElementPixelAccessor_h
#define itkNthElementPixelAccessor_h

#include "itkMacro.h"
#include "itkDefaultConvertPixelTraits.h"
#include "itkVariableLengthVector.h"
#include "itkDefaultVectorPixelAccessor.h"

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
 * \ingroup ITKImageAdaptors
 */

template< typename T, typename TContainer >
class NthElementPixelAccessor
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
  { DefaultConvertPixelTraits<InternalType>::SetNthComponent(m_ElementNumber, output, input); }

  /** Read access to the NthElement component */
  inline ExternalType Get(const InternalType & input) const
  { return static_cast<ExternalType>( DefaultConvertPixelTraits<InternalType>::GetNthComponent( m_ElementNumber, input ) ); }

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


template< typename TOutputPixelType, typename TPixelType >
class NthElementPixelAccessor< TOutputPixelType, itk::VariableLengthVector<TPixelType> >
  : private DefaultVectorPixelAccessor< TPixelType >
{
public:
  /** Standard class typedefs. */
  typedef   NthElementPixelAccessor Self;

  typedef unsigned int VectorLengthType;

  /** External typedef. It defines the external aspect
   * that this class will exhibit. */
  typedef  TOutputPixelType ExternalType;

  /** Internal typedef used by the ImageAdaptor for the buffer pointer */
  typedef TPixelType InternalType;

  typedef VariableLengthVector< TPixelType > ActualPixelType;

  inline void Set(ActualPixelType &output, const ExternalType & input) const
  {
    output[m_ElementNumber] = input;
  }

  inline void Set(InternalType &output, const ExternalType & input,
                  const unsigned long offset) const
  {
    // note: v is a reference to the internal buffer, this method of
    // access relies on return value optimization to work
    ActualPixelType v = Superclass::Get( output, offset );

    return Set( v, input );
  }

  inline ExternalType Get(const ActualPixelType & input) const
  {
    ExternalType output;

    output = static_cast< ExternalType >( input[m_ElementNumber] );
    return output;
  }

  inline ExternalType Get(const InternalType &input, const SizeValueType offset) const
  {
    return Get( Superclass::Get(input, offset) );
  }


  /** Get the element number to access in the container */
  unsigned int GetElementNumber(void) const
  { return m_ElementNumber; }

  /** Set the element number to access in the container */
  void SetElementNumber(unsigned int nth)
  { m_ElementNumber = nth; }

  /** Set the length of each vector in the VectorImage */
  void SetVectorLength(VectorLengthType l)
  {
    Superclass::SetVectorLength( l );
  }

  /** Get Vector lengths */
  VectorLengthType GetVectorLength() const { return Superclass::GetVectorLength(); }

  NthElementPixelAccessor( unsigned int length = 1)
    :m_ElementNumber(0)
    {
    Superclass::SetVectorLength( length );
    }

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
    this->SetVectorLength( accessor.GetVectorLength() );
    return *this;
  }

protected:
  typedef DefaultVectorPixelAccessor< TPixelType > Superclass;

private:
  VectorLengthType m_ElementNumber;
};

}  // end namespace itk

#endif
