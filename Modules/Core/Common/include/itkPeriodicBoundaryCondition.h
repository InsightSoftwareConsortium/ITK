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
#ifndef itkPeriodicBoundaryCondition_h
#define itkPeriodicBoundaryCondition_h
#include "itkImageBoundaryCondition.h"

namespace itk
{
/** \class PeriodicBoundaryCondition
 * \brief
 * A function object that determines values outside of image boundaries
 * according to periodic (wrap-around) conditions.
 *
 * The input to this function object is a neighborhood iterator.  This boundary
 * condition object is designed to be given as a template argument to a
 * NeighborhoodIterator or any of the NeighborhoodIterator subclasses.
 *
 * \ingroup DataRepresentation
 * \ingroup ImageObjects
 * \ingroup ITKCommon
 */
template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT PeriodicBoundaryCondition : public ImageBoundaryCondition<TInputImage, TOutputImage>
{
public:
  /** Standard class type aliases. */
  using Self = PeriodicBoundaryCondition;
  using Superclass = ImageBoundaryCondition<TInputImage, TOutputImage>;

  /** Runtime information support. */
  itkTypeMacro(PeriodicBoundaryCondition, ImageBoundaryCondition);

  /** Extract information from the image type. */
  using typename Superclass::PixelType;
  using typename Superclass::PixelPointerType;
  using typename Superclass::OutputPixelType;
  using typename Superclass::RegionType;
  using typename Superclass::IndexType;
  using typename Superclass::SizeType;
  using typename Superclass::OffsetType;
  using typename Superclass::NeighborhoodType;

  using typename Superclass::NeighborhoodAccessorFunctorType;

  /** Extract information from the image type. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Default constructor. */
  PeriodicBoundaryCondition() = default;

  /** Computes and returns a neighborhood of appropriate values from
   * neighborhood iterator data.. */
  OutputPixelType
  operator()(const OffsetType &       point_index,
             const OffsetType &       boundary_offset,
             const NeighborhoodType * data) const override;

  /** Computes and returns the appropriate pixel value from
   * neighborhood iterator data, using the functor. */
  OutputPixelType
  operator()(const OffsetType &                      point_index,
             const OffsetType &                      boundary_offset,
             const NeighborhoodType *                data,
             const NeighborhoodAccessorFunctorType & neighborhoodAccessorFunctor) const override;

  /** Determines the necessary input region for the output region.
   * For this boundary condition, the output region is mapped into the
   * input image index space. If the mapped region crosses an image
   * boundary in some dimension, then the entire size of the image in
   * that dimension is requested. For this reason, it is most memory
   * efficient to request regions that map to regions that do cross
   * image boundaries.
   *
   * \param inputLargestPossibleRegion Largest possible region of the input image.
   * \param outputRequestedRegion The output requested region.
   * \return The necessary input region required to determine the
   * pixel values in the outputRequestedRegion.
   */
  RegionType
  GetInputRequestedRegion(const RegionType & inputLargestPossibleRegion,
                          const RegionType & outputRequestedRegion) const override;

  /** Returns a value for a given pixel at an index. If the index is inside the
   * bounds of the input image, then the pixel value is obtained from
   * the input image. Otherwise, the pixel at the desired index (modulo
   * the size of the image) is returned.
   *
   * \param index The index of the desired pixel.
   * \param image The image from which pixel values should be determined.
   */
  OutputPixelType
  GetPixel(const IndexType & index, const TInputImage * image) const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPeriodicBoundaryCondition.hxx"
#endif

#endif
