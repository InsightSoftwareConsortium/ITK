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
#ifndef itkBSplineDownsampleImageFilter_h
#define itkBSplineDownsampleImageFilter_h

// include .h for each ResamplerType
#include "itkBSplineL2ResampleImageFilterBase.h"
#include "itkBSplineCenteredL2ResampleImageFilterBase.h"

namespace itk
{
/**
 *\class BSplineDownsampleImageFilter
 * \brief Down-samples an image by a factor of 2 using B-Spline filter interpolation.
 *
 * This class is the public interface for spline down-sampling as defined by the
 *   ResamplerType.
 * Requires the use of a resampler type.  If in doubt, the basic itkBSplineResampleImageFilterBase
 *   should work fine for most applications.
 *
 *  This class may also be used to create a smoother by combining it with the upSampler
 *  as in the following example:
 *      using ResamplerType = itk::BSplineResampleImageFilterBase<ImageType2D, ImageType2D>;
 *      using DownsamplerType2D = itk::BSplineDownsampleImageFilter<ImageType2D,ImageType2D,ResamplerType>;
 *      using UpsamplerType2D = itk::BSplineUpsampleImageFilter<ImageType2D,ImageType2D,ResamplerType>;
 *
 *      DownsamplerType2D::Pointer downSampler = DownsamplerType2D::New();
 *      UpsamplerType2D::Pointer   upSampler =   UpsamplerType2D::New();
 *      int splineOrder = 3;
 *      downSampler->SetSplineOrder(splineOrder);
 *      upSampler->SetSplineOrder(splineOrder);
 *
 *      downSampler->SetInput(image);
 *      downSampler->Update();
 *
 *      upSampler->SetInput( downSampler->GetOutput() );   // output of downSampler is input to upSampler
 *      upSampler->Update();
 *
 *      ImageTypePtr2D outImage2 = upSampler->GetOutput();  // outImage2 is the smoothed imaged
 *
 * Limitations:  This class requires specification of a resampler type which may
 *                      be one of:
 *                        itkBSplineResampleImageFilterBase,
 *                        itkBSplineL2ResampleImageFilterBase
 *                        itkBSplineSplineCenteredResampleImageFilterBase,
 *                        itkBSplineCenteredL2ResampleImageFilterBase
 *               The limitations of these resampler types will apply to this filter.
 *               Downsamples only by a factor of 2.
 *
 * \sa itkBSplineDownsampleImageFilter
 * \sa itkBSplineL2ResampleImageFilter
 * \sa itkBSplineResampleImageFilterBase
 * \sa itkBSplineCenteredResampleImageFilterBase
 * \sa itkBSplineCenteredL2ResampleImageFilterBase
 *
 * \ingroup GeometricTransformationFilters
 * \ingroup SingleThreaded
 * \ingroup CannotBeStreamed
 * \ingroup ITKImageGrid
 */

//= typename BSplineResampleImageFilterBase<TInputImage, TOutputImage>
template <typename TInputImage,
          typename TOutputImage,
          typename ResamplerType = BSplineResampleImageFilterBase<TInputImage, TOutputImage>>
class ITK_TEMPLATE_EXPORT BSplineDownsampleImageFilter : public ResamplerType
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineDownsampleImageFilter);

  /** Standard class type aliases. */
  using Self = BSplineDownsampleImageFilter;
  using Superclass = ResamplerType;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineDownsampleImageFilter, ResamplerType);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** InputImageType type alias support */
  using InputImageType = typename Superclass::InputImageType;

  /** InputImagePointer type alias support */
  using InputImagePointer = typename Superclass::InputImagePointer;

  /** OutputImagePointer type alias support */
  using OutputImagePointer = typename Superclass::OutputImagePointer;

  /** OutputImageIterator type alias support */
  using OutputImageIterator = typename Superclass::OutputImageIterator;

  /** Creates an image half the size of the input image with spacing twice the
   * input image. */
  void
  GenerateOutputInformation() override;

  /** This filter requires all of the input image */
  void
  GenerateInputRequestedRegion() override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(DoubleConvertibleToOutputCheck, (Concept::Convertible<double, typename TOutputImage::PixelType>));
  // End concept checking
#endif

protected:
  void
  GenerateData() override;

  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  BSplineDownsampleImageFilter() = default;
  ~BSplineDownsampleImageFilter() override = default;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBSplineDownsampleImageFilter.hxx"
#endif

#endif
