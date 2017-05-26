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
#ifndef itkImageBoundaryCondition_h
#define itkImageBoundaryCondition_h

#include "itkIndex.h"
#include "itkNeighborhood.h"
#include "itkImageRegion.h"

namespace itk
{
/**
 *\class ImageBoundaryCondition
 * \brief A virtual base object that defines an interface to a class of
 * boundary condition objects for use by neighborhood iterators.
 *
 * A boundary condition object supplies a phantom pixel value when
 * given a neighborhood of (pointers to) image values, the (ND) index of
 * the phantom pixel, and its (ND) offset from the boundary.  The index
 * of the phantom pixel is relative to the "upper left-hand corner" of
 * the neighborhood (as opposed to its center).
 *
 *
 * Associated Types                 Description
 * ----------------                 -----------
 * PixelType                         The data type of the return value.
 * PixelPointerType                  A pointer to PixelType.
 * PixelPointerTypeNeighborhood      A neighborhood of PixelPointerTypes
 *                                   that points to the pixel values in
 *                                   an image neighborhood.
 *
 * \ingroup DataRepresentation
 * \ingroup ImageObjects
 * \ingroup ITKCommon
 */
template< typename TInputImage, typename TOutputImage = TInputImage >
class ITK_TEMPLATE_EXPORT ImageBoundaryCondition
{
public:
  /** Extract information from the image type */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Standard typedefs. */
  typedef ImageBoundaryCondition Self;

  /** Extract information from the image type */
  typedef typename TInputImage::PixelType                       PixelType;
  typedef typename TInputImage::InternalPixelType *             PixelPointerType;
  typedef typename TOutputImage::PixelType                      OutputPixelType;
  typedef Index< itkGetStaticConstMacro(ImageDimension) >       IndexType;
  typedef Size< itkGetStaticConstMacro(ImageDimension) >        SizeType;
  typedef Offset< itkGetStaticConstMacro(ImageDimension) >      OffsetType;
  typedef ImageRegion< itkGetStaticConstMacro(ImageDimension) > RegionType;

  /** Type of the data container passed to this function object. */
  typedef Neighborhood< PixelPointerType,
                        itkGetStaticConstMacro(ImageDimension) > NeighborhoodType;

  /** Functor used to access pixels from a neighborhood of pixel pointers */
  typedef typename TInputImage::NeighborhoodAccessorFunctorType
  NeighborhoodAccessorFunctorType;

  /** Default constructor. */
  ImageBoundaryCondition() {}

  /** Runtime information support. */
  virtual const char * GetNameOfClass() const
  {
    return "itkImageBoundaryCondition";
  }

  /** Utility for printing the boundary condition. */
  virtual void Print( std::ostream & os, Indent i = 0 ) const
  {
    os << i << this->GetNameOfClass() << " (" << this << ")" << std::endl;
  }

  /** Returns a value for a given out-of-bounds pixel.  The arguments are the
   * phantom pixel (ND) index within the neighborhood, the pixel's offset from
   * the nearest image border pixel, and a neighborhood of pointers to pixel
   * values in the image.  */
  virtual OutputPixelType operator()(const OffsetType & point_index,
                                     const OffsetType & boundary_offset,
                                     const NeighborhoodType *data) const = 0;

  /** Computes and returns the appropriate pixel value from
   * neighborhood iterator data, using the functor. */
  virtual OutputPixelType operator()(
    const OffsetType & point_index,
    const OffsetType & boundary_offset,
    const NeighborhoodType *data,
    const NeighborhoodAccessorFunctorType & neighborhoodAccessorFunctor) const = 0;

  virtual ~ImageBoundaryCondition() {}

  /** Tell if the boundary condition can index to any location within
    * the associated iterator's neighborhood or if it has some limited
    * subset (such as none) that it relies upon.
    * Subclasses should override this method if they can safely limit
    * indexes to active pixels (or no pixels).
    */
  virtual bool RequiresCompleteNeighborhood() { return true; }

  /** Determines the necessary input region for an output region given
   * the largest possible region of the input image. Subclasses should
   * override this method to efficiently support streaming.
   *
   * \param inputLargestPossibleRegion Largest possible region of the input image.
   * \param outputRequestedRegion The output requested region.
   * \return The necessary input region required to determine the
   * pixel values in the outputRequestedRegion.
   */
  virtual RegionType GetInputRequestedRegion( const RegionType & inputLargestPossibleRegion,
                                              const RegionType & outputRequestedRegion ) const
  {
    (void) outputRequestedRegion;
    return inputLargestPossibleRegion;
  }

  /** Returns a value for a given pixel at an index. If the index is inside the
   * bounds of the input image, then the pixel value is obtained from
   * the input image. Otherwise, the pixel value is determined
   * according to the boundary condition type.
   *
   * \param index The index of the desired pixel.
   * \param image The image from which pixel values should be determined.
   */
  virtual OutputPixelType GetPixel( const IndexType & index,
                                    const TInputImage * image ) const = 0;

};
} // end namespace itk

#endif
