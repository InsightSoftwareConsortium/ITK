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
#ifndef itkZeroFluxNeumannBoundaryCondition_h
#define itkZeroFluxNeumannBoundaryCondition_h
#include "itkImageBoundaryCondition.h"

namespace itk
{
/** \class ZeroFluxNeumannBoundaryCondition
 * \brief
 * A function object that determines a neighborhood of values at an
 * image boundary according to a Neumann boundary condition where first,
 * upwind derivatives on the boundary are zero.  This is a useful condition
 * in solving some classes of differential equations.
 *
 * For example, invoking this function object on a 7x5 iterator that masks
 * a region at an image corner (iterator is centered on the 2):
 * \code
 *               * * * * * * *
 *               * * * * * * *
 *               * * 1 2 3 4 5  (where * denotes pixels that lie
 *               * * 3 3 5 5 6          outside of the image boundary)
 *               * * 4 4 6 7 8
 * \endcode
 * returns the following neighborhood of values:
 * \code
 *               1 1 1 2 3 4 5
 *               1 1 1 2 3 4 5
 *               1 1 1 2 3 4 5
 *               3 3 3 3 5 5 6   (note the corner values)
 *               4 4 4 4 6 7 8
 * \endcode
 * The input to this function object is a neighborhood iterator.  This boundary
 * condition object is designed to be given as a template argument to a
 * NeighborhoodIterator or any of the NeighborhoodIterator
 * subclasses.
 *
 * \ingroup DataRepresentation
 * \ingroup ImageObjects
 * \ingroup ITKCommon
 */
template< typename TInputImage, typename TOutputImage = TInputImage >
class ITK_TEMPLATE_EXPORT ZeroFluxNeumannBoundaryCondition:
    public ImageBoundaryCondition< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ZeroFluxNeumannBoundaryCondition                    Self;
  typedef ImageBoundaryCondition< TInputImage, TOutputImage > Superclass;

  /** Extract information from the image type. */
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::PixelPointerType PixelPointerType;
  typedef typename Superclass::OutputPixelType  OutputPixelType;
  typedef typename Superclass::RegionType       RegionType;
  typedef typename Superclass::IndexType        IndexType;
  typedef typename Superclass::SizeType         SizeType;
  typedef typename Superclass::OffsetType       OffsetType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;

  typedef typename Superclass::NeighborhoodAccessorFunctorType
  NeighborhoodAccessorFunctorType;

  /** Extract information from the image type. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** Default constructor. */
  ZeroFluxNeumannBoundaryCondition() {}

  /** Runtime information support. */
  virtual const char * GetNameOfClass() const
  {
    return "itkZeroFluxNeumannBoundaryCondition";
  }

  /** Computes and returns a neighborhood of appropriate values from
   * neighborhood iterator data.. */
  virtual OutputPixelType operator()(const OffsetType & point_index,
                                     const OffsetType & boundary_offset,
                                     const NeighborhoodType *data) const;

  /** Computes and returns the appropriate pixel value from
   * neighborhood iterator data, using the functor. */
  virtual OutputPixelType operator()(
    const OffsetType & point_index,
    const OffsetType & boundary_offset,
    const NeighborhoodType *data,
    const NeighborhoodAccessorFunctorType & neighborhoodAccessorFunctor) const;

  /** Determines the necessary input region for the output region.
   * For this boundary condition, only the intersection of the largest
   * possible image region and the output requested region is
   * needed. If the intersection is empty, then a one-pixel layer of
   * the image from the side closest to the output requested region is needed.
   *
   * \param inputLargestPossibleRegion Largest possible region of the input image.
   * \param outputRequestedRegion The output requested region.
   * \return The necessary input region required to determine the
   * pixel values in the outputRequestedRegion.
   */
  virtual RegionType GetInputRequestedRegion( const RegionType & inputLargestPossibleRegion,
                                              const RegionType & outputRequestedRegion ) const;

  /** Returns a value for a given pixel at an index. If the index is inside the
   * bounds of the input image, then the pixel value is obtained from
   * the input image. Otherwise, the nearest pixel value is returned.
   *
   * \param index The index of the desired pixel.
   * \param image The image from which pixel values should be determined.
   */
  OutputPixelType GetPixel( const IndexType & index, const TInputImage * image ) const;

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkZeroFluxNeumannBoundaryCondition.hxx"
#endif

/*
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkZeroFluxNeumannBoundaryCondition.hxx"
#endif
*/

#endif
