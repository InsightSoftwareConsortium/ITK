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
#ifndef itkVectorImageNeighborhoodAccessorFunctor_h
#define itkVectorImageNeighborhoodAccessorFunctor_h

#include "itkVariableLengthVector.h"
#include "itkImageBoundaryCondition.h"
#include "itkImageBase.h"

namespace itk
{
/** \class VectorImageNeighborhoodAccessorFunctor
 * \brief Provides accessor interfaces to Access pixels and is meant to be
 * used on pointers to pixels held by the Neighborhood class.
 *
 * A typical user should not need to use this class. The class is internally
 * used by the neighborhood iterators.
 *
 * \note
 * This work is part of the National Alliance for Medical Image Computing
 * (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
 * for Medical Research, Grant U54 EB005149.
  * \ingroup ITKCommon
 */
template< typename TImage >
class VectorImageNeighborhoodAccessorFunctor
{
public:
  typedef TImage                                ImageType;
  typedef typename ImageType::PixelType         PixelType;
  typedef typename ImageType::InternalPixelType InternalPixelType;
  typedef unsigned int                          VectorLengthType;
  typedef typename ImageType::OffsetType        OffsetType;

  typedef Neighborhood< InternalPixelType *,
                         TImage ::ImageDimension > NeighborhoodType;

  typedef ImageBoundaryCondition< ImageType > const
  *ImageBoundaryConditionConstPointerType;

  VectorImageNeighborhoodAccessorFunctor(VectorLengthType length):
    m_VectorLength(length), m_OffsetMultiplier(length - 1), m_Begin(ITK_NULLPTR) {}
  VectorImageNeighborhoodAccessorFunctor():
    m_VectorLength(0), m_OffsetMultiplier(0), m_Begin(ITK_NULLPTR) {}

  /** Set the pointer index to the start of the buffer.
   * This must be set by the iterators to the starting location of the buffer.
   * Typically a neighborhood iterator iterating on a neighborhood of an Image,
   * say \c image will set this in its constructor. For instance:
   *
   * \code
   * ConstNeighborhoodIterator( radius, image, )
   *   {
   *   ...
   *   m_NeighborhoodAccessorFunctor.SetBegin( image->GetBufferPointer() );
   *   }
   * \endcode
   */
  inline void SetBegin(const InternalPixelType *begin)
  { this->m_Begin = const_cast< InternalPixelType * >( begin ); }

  /** Method to dereference a pixel pointer. This is used from the
   * ConstNeighborhoodIterator as the equivalent operation to (*it).
   * This method should be preferred over the former (*it) notation.
   * The reason is that dereferencing a pointer to a location of
   * VectorImage pixel involves a different operation that simply
   * dereferencing the pointer. Here a PixelType (array of InternalPixelType s)
   * is created on the stack and returned.  */
  inline PixelType Get(const InternalPixelType *pixelPointer) const
  {
    return PixelType(pixelPointer + ( pixelPointer - m_Begin ) * m_OffsetMultiplier, m_VectorLength);
  }

  /** Method to set the pixel value at a certain pixel pointer */
  inline void Set(InternalPixelType * & pixelPointer, const PixelType & p) const
  {
    InternalPixelType *truePixelPointer =
      pixelPointer + ( pixelPointer - m_Begin ) * m_OffsetMultiplier;

    for ( VectorLengthType i = 0; i < m_VectorLength; i++ )
      {
      truePixelPointer[i] = p[i];
      }
  }

  inline PixelType BoundaryCondition(
    const OffsetType & point_index,
    const OffsetType & boundary_offset,
    const NeighborhoodType *data,
    const ImageBoundaryConditionConstPointerType boundaryCondition) const
  {
    return boundaryCondition->operator()(point_index, boundary_offset, data, *this);
  }

  /** Methods to Set/Get vector length. This should be the length of a block of
   * pixels in the VectorImage. */
  void SetVectorLength(VectorLengthType length)
  {
    m_VectorLength = length;
    m_OffsetMultiplier = length - 1;
  }

  /** Methods to Set/Get vector length. This should be the length of a block of
   * pixels in the VectorImage. */
  VectorLengthType GetVectorLength()
  {
    return m_VectorLength;
  }

private:
  VectorLengthType m_VectorLength;
  VectorLengthType m_OffsetMultiplier; // m_OffsetMultiplier = m_VectorLength-1
                                       // (precomputed for speedup).
  InternalPixelType *m_Begin;          // Begin of the buffer.
};
} // end namespace itk
#endif
