/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkNeighborhoodAccessorFunctor_h
#define itkNeighborhoodAccessorFunctor_h

#include "itkImageBoundaryCondition.h"
#include "itkImageBase.h"

namespace itk
{
/** \class NeighborhoodAccessorFunctor
 * \brief Provides accessor interfaces to Get pixels and is meant to be
 * used on pointers contained within Neighborhoods. A typical user should
 * not need to use this class directly. This class is used by the
 * neighborhood iterators to get pixels from pixel pointers or assign
 * a pixel to an address.
 *
 *
 * \note
 * This work is part of the National Alliance for Medical Image Computing
 * (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
 * for Medical Research, Grant U54 EB005149.
 * \ingroup ITKCommon
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT NeighborhoodAccessorFunctor final
{
public:
  using Self = NeighborhoodAccessorFunctor;
  using ImageType = TImage;
  using PixelType = typename ImageType::PixelType;
  using InternalPixelType = typename ImageType::InternalPixelType;
  using VectorLengthType = unsigned int;
  using OffsetType = typename ImageType::OffsetType;

  static constexpr unsigned int ImageDimension = TImage::ImageDimension;
  using NeighborhoodType = Neighborhood<InternalPixelType *, Self::ImageDimension>;

  template <typename TOutput = ImageType>
  using ImageBoundaryConditionType = ImageBoundaryCondition<ImageType, TOutput>;

  /** Set the pointer index to the start of the buffer. */
  inline void
  SetBegin(const InternalPixelType *)
  {}

  /** Method to dereference a pixel pointer. This is used from the
   * ConstNeighborhoodIterator as the equivalent operation to (*it).
   * This method should be preferred over the former (*it) notation.
   * The reason is that dereferencing a pointer to a location of
   * VectorImage pixel involves a different operation that simply
   * dereferencing the pointer.  */
  inline PixelType
  Get(const InternalPixelType * pixelPointer) const
  {
    return (*pixelPointer);
  }

  /** Method to set the pixel value at a certain pixel pointer */
  inline void
  Set(InternalPixelType * const pixelPointer, const PixelType & p) const
  {
    *pixelPointer = p;
  }

  template <typename TOutput>
  inline typename ImageBoundaryConditionType<TOutput>::OutputPixelType
  BoundaryCondition(const OffsetType &                          point_index,
                    const OffsetType &                          boundary_offset,
                    const NeighborhoodType *                    data,
                    const ImageBoundaryConditionType<TOutput> * boundaryCondition) const
  {
    return boundaryCondition->operator()(point_index, boundary_offset, data);
  }

  void SetVectorLength(VectorLengthType) {}
  VectorLengthType
  SetVectorLength()
  {
    return 0;
  }
};
} // end namespace itk

// template< typename TImage > const unsigned int
// itk::NeighborhoodAccessorFunctor<TImage>::ImageDimension;

#endif
