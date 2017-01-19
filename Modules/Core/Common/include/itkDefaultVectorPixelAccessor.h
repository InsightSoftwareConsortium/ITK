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
#ifndef itkDefaultVectorPixelAccessor_h
#define itkDefaultVectorPixelAccessor_h

#include "itkMacro.h"
#include "itkVariableLengthVector.h"
#include "itkIntTypes.h"

namespace itk
{
/** \class DefaultVectorPixelAccessor
 * \brief Give access to partial aspects of a type
 *
 * DefaultVectorPixelAccessor is specifically meant to provide VectorImage
 * with the same \c DefaultPixelAccessor interface that
 * DefaultPixelAccessor provides to Image.
 *
 * The template parameters is the type that is contained in by the elements of
 * a vector.
 *
 * The class also contains a m_VectorLength parameter, set with the SetVectorLength
 * method to set the length of the vectors. This must be set before the accessor
 * can be used. This is the length of each of the vector containers.
 *
 * \note
 * This work is part of the National Alliance for Medical Image Computing
 * (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
 * for Medical Research, Grant U54 EB005149.
 *
 * \ingroup ImageAdaptors
 * \ingroup ITKCommon
 */
template< typename TType >
class ITK_TEMPLATE_EXPORT DefaultVectorPixelAccessor
{
public:

  typedef unsigned int VectorLengthType;

  /** External typedef. It defines the external aspect
   * that this class will exhibit. Here it is an VariableLengthVector. The container does not
   * manage the memory. In other words it is an array reference with the contents
   * pointing to the actual data in the image. */
  typedef VariableLengthVector< TType > ExternalType;

  /** Internal typedef. It defines the internal real representation of data. */
  typedef TType InternalType;

  /** Set output using the value in input */
  inline void Set(InternalType & output, const ExternalType & input,
                  const unsigned long offset) const
  {
    InternalType *truePixel = ( &output ) + offset * m_OffsetMultiplier;

    for ( VectorLengthType i = 0; i < m_VectorLength; i++ )
      {
      truePixel[i] = input[i];
      }
  }

  /** Get the value from input */
  inline ExternalType Get(const InternalType & input, const SizeValueType offset) const
  {
    // Do not create a local for this method, to use return value
    // optimization.
    return ExternalType( ( &input ) + ( offset * m_OffsetMultiplier ), m_VectorLength );
  }

  /** Set the length of each vector in the VectorImage */
  void SetVectorLength(VectorLengthType l)
  {
    m_VectorLength = l;
    m_OffsetMultiplier = ( l - 1 );
  }

  /** Get Vector lengths */
  VectorLengthType GetVectorLength() const { return m_VectorLength; }

  DefaultVectorPixelAccessor() : m_VectorLength(0), m_OffsetMultiplier(0) {}

  /** Constructor to initialize VectorLength at construction time */
  DefaultVectorPixelAccessor(VectorLengthType l)
  {
    m_VectorLength = l;
    m_OffsetMultiplier = l - 1;
  }

  virtual ~DefaultVectorPixelAccessor() {}

private:
  VectorLengthType m_VectorLength;
  VectorLengthType m_OffsetMultiplier;
};
} // end namespace itk

#endif
