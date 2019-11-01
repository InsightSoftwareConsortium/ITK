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
#ifndef itkNeighborhoodAlgorithm_h
#define itkNeighborhoodAlgorithm_h

#include <list>
#include "itkImage.h"
#include "itkNeighborhoodOperator.h"
#include "itkNeighborhoodIterator.h"

namespace itk
{
namespace NeighborhoodAlgorithm
{
/** \class ImageBoundaryFacesCalculator
 *   \brief Splits an image into a main region and several "face" regions
 *          which are used to handle computations on the boundary of an image.
 *
 * Splitting the image into the necessary regions is an easy task when you use
 * the ImageBoundaryFacesCalculator.  The face
 * calculator is so named because it returns a list of the "faces" of the ND
 * dataset.  Faces are those regions whose pixels all lie within a distance \f$d\f$
 * from the boundary, where \f$d\f$ is the radius of the neighborhood stencil used
 * for the numerical calculations. In other words, faces are those regions
 * where a neighborhood iterator of radius \f$d\f$ will always overlap the boundary
 * of the image. The face calculator also returns the single \em inner
 * region, in which out-of-bounds values are never required and bounds checking
 * is not necessary.
 *
 * \image html FaceBoundaryCalculator.png "Example regions produced by the calculator."
 *
 * First we find center (non-boundary) region 0.
 * then find the face on the lower side of the 0th dimension (Region 1).
 * Next we find the face opposite to that face (Region 2).
 * Then we find the face between region 1 and region 2 on
 * the lower side of the 1th dimension.(region 3).
 * Finally we find the face opposite to face 3 (region 4).
 *
 * \note The first region contained in faceList should be the
 * non-boundary region, if there is one. The existence of a
 * non-boundary region depends on the relative location of
 * regionToProcess and bufferedRegion. The non-boundary regions (if
 * any) are the remaining faces in faceList.
 *
 * \ingroup ITKCommon
 */
template <typename TImage>
struct ImageBoundaryFacesCalculator
{
  using RadiusType = typename NeighborhoodIterator<TImage>::RadiusType;
  using RegionType = typename TImage::RegionType;
  using IndexType = typename TImage::IndexType;
  using SizeType = typename TImage::SizeType;
  using FaceListType = std::list<RegionType>;
  static constexpr unsigned int ImageDimension = TImage::ImageDimension;

  /** \class Result
   *
   * Represents the result of ImageBoundaryFacesCalculator::Compute
   *
   * \ingroup ITKCommon
   */
  class Result
  {
  public:
    /** Returns the center (non-boundary) region. */
    RegionType
    GetNonBoundaryRegion() const
    {
      return m_NonBoundaryRegion;
    }

    /** Returns the boundary faces (the regions at the boundary of the image). */
    const FaceListType &
    GetBoundaryFaces() const
    {
      return m_BoundaryFaces;
    }

    /** Tells whether Result objects `lhs` and `rhs` are equal. */
    friend bool
    operator==(const Result & lhs, const Result & rhs)
    {
      return (lhs.m_NonBoundaryRegion == rhs.m_NonBoundaryRegion) && (lhs.m_BoundaryFaces == rhs.m_BoundaryFaces);
    }

    /** Tells whether Result objects `lhs` and `rhs` are unequal. */
    friend bool
    operator!=(const Result & lhs, const Result & rhs)
    {
      return !(lhs == rhs);
    }

  private:
    friend struct ImageBoundaryFacesCalculator;

    RegionType   m_NonBoundaryRegion;
    FaceListType m_BoundaryFaces;
  };

  /** Splits the specified region into a non-boundary region and a list of
   * boundary faces, and returns the result. */
  static Result
  Compute(const TImage &, RegionType, RadiusType);

  FaceListType
  operator()(const TImage *, RegionType, RadiusType);
};

/** \class CalculateOutputWrapOffsetModifiers
 *  \brief Sets up itkNeighborhoodIterator output buffers.
 *
 * Helper class for setting up itkNeighborhoodIterator output
 * buffers. Calculates the necessary modifiers to synchronize input and output
 * iteration between images with equal RequestedRegion sizes but unequal
 * BufferedRegion sizes.
 * \ingroup ITKCommon
 */
template <typename TImage>
struct CalculateOutputWrapOffsetModifiers
{
  using OffsetType = Offset<TImage::ImageDimension>;
  OffsetType
  operator()(TImage *, TImage *) const;
};
} // end namespace NeighborhoodAlgorithm
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkNeighborhoodAlgorithm.hxx"
#endif

#endif
