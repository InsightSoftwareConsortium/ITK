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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkInterpolateImagePointsFilter_h
#define itkInterpolateImagePointsFilter_h

#include "itkBSplineInterpolateImageFunction.h"
#include "itkImageToImageFilter.h"
#include "itkImageRegionIterator.h"

namespace itk
{
/** \class InterpolateImagePointsFilter
 *  \brief Resamples an image at the coordinates specified by the user.
 *
 *  This class may be templated over the Interpolate Function but defaults
 *    to the BSplineInterpolateImageFunction for cubic interpolation.
 *    The user must set the image using the SetInputImage function and they
 *    must set the coordinates (one coordinate "image" for each dimension) using
 *    SetInterpolationCoordinate().  The coordinates may be any number of points and can
 *    be randomly organized.  The interpolated output will correspond to the
 *    ordering of the coordinate points.  The coordinates must be of type
 *    ContinuousIndexType and not of PointType.
 *
 *  This function is different from the resampleImageFilter class in that the
 *    resampleImageFilter applies a transform to the original input image
 *    coordinates.  This function takes an arbitrary set of point coordinates
 *    and applies the transform at these locations.
 *
 * Limitations:  The coordinates must be in an image of the same dimension as the
 *       input image.  There is no reason why this should be the case and future
 *       revisions should look at eliminating this limitation.
 *    Currently TCoordType must be the same as the input pixel type (TInputImage).
 *       Again future revisions should look at eliminating this limitation.
 *    Though the output generation may be streamed the entire input image,
 *       must be supplied. The coordinates may be streamed in smaller blocks.
 *    The coordinates are specified as separate images for each dimension.
 *    The coordinates are treated as Continuous Indices. If coordinates are
 *    supplied as Points then they must be converted to an Index before passing
 *       to this class.
 *
 * \sa BSplineInterpolateImageFunction
 * \sa ResampleImageFilter
 *
 * \ingroup GeometricTransformationFilters
 * \ingroup MultiThreaded
 * \ingroup CanBeStreamed
 *
 * \ingroup ITKImageGrid
 */

template <typename TInputImage,
          typename TOutputImage,
          typename TCoordType = typename TInputImage::PixelType,
          typename InterpolatorType = BSplineInterpolateImageFunction<TInputImage, TCoordType>>
class ITK_TEMPLATE_EXPORT InterpolateImagePointsFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(InterpolateImagePointsFilter);

  /** Standard class type aliases. */
  using Self = InterpolateImagePointsFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(InterpolateImagePointsFilter, ImageToImageFilter);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** ImageDimension enumeration. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Typedefs from the Superclass */
  using InputImageType = typename Superclass::InputImageType;
  using OutputImageType = typename Superclass::OutputImageType;
  using InputImagePointer = typename Superclass::InputImagePointer;

  /** Typedefs to describe and access output image. */
  using OutputImagePointer = typename TOutputImage::Pointer;
  using OutputImageIterator = ImageRegionIterator<InputImageType>;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  /** Image pixel value type alias. */
  using PixelType = typename TOutputImage::PixelType;

  /** Typedefs to describe and access Interpolator */
  using InterpolatorPointer = typename InterpolatorType::Pointer;
  using ContinuousIndexType = typename InterpolatorType::ContinuousIndexType;

  /** Typedefs to describe and access coordinate images */
  using CoordImageType = Image<TCoordType, Self::ImageDimension>;

  /** Typedef for region copier */
  using RegionCopierType = ImageToImageFilterDetail::ImageRegionCopier<Self::ImageDimension, Self::ImageDimension>;

  /** SetInputImage is used to set the image to be interpolated.
   * Note that this should be used instead of the direct setInput
   * as multiple inputs are needed and this class keeps track of
   * the ordering. */
  void
  SetInputImage(const TInputImage * inputImage);

  /** SetInterpolationCoordinate must be called for each dimension.  The variable setDimension
   * is used to identify the dimension being set, i.e. 0,1,2...N */
  void
  SetInterpolationCoordinate(const CoordImageType * coordinate, unsigned int setDimension);

  /** Set the pixel value when a transformed pixel is outside of the image */
  itkSetMacro(DefaultPixelValue, PixelType);

  /** Get the pixel value when a transformed pixel is outside of the image */
  itkGetConstMacro(DefaultPixelValue, PixelType);

  /** Returns a pointer to the  interpolator. */
  InterpolatorPointer
  GetInterpolator()
  {
    return m_Interpolator;
  }

  /** Overloaded to ensure that output is sized the same as the coordinate inputs
   * and not the size of the input image. */
  void
  GenerateOutputInformation() override;

  /**  Overloaded to set the input image to the largest possible region */
  void
  GenerateInputRequestedRegion() override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<typename TInputImage::PixelType>));
  // End concept checking
#endif

protected:
  InterpolateImagePointsFilter();
  // ~InterpolateImagePointsFilter(){} default implementation ok

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Main function for calculating interpolated values at each coordinate
   *  set.  Access is through the Update() call. */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


  /** Override VerifyInputInformation() since this filter's inputs do
   * not need to occupy the same physical space.
   *
   * \sa ProcessObject::VerifyInputInformation
   */
  void
  VerifyInputInformation() ITKv5_CONST override
  {}

private:
  /** Typedefs to describe and access coordinate images */
  using CoordImageTypePointer = typename CoordImageType::Pointer;
  using CoordImageIterator = ImageRegionConstIterator<CoordImageType>;
  using CoordImageRegionType = typename CoordImageType::RegionType;

  InterpolatorPointer m_Interpolator;
  PixelType           m_DefaultPixelValue; // default pixel value if the
                                           // point is outside the image
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkInterpolateImagePointsFilter.hxx"
#endif

#endif
