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
#ifndef itkGradientImageFilter_h
#define itkGradientImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkCovariantVector.h"
#include "itkImageRegionIterator.h"

namespace itk
{


template <typename TPixelType, unsigned int VImageDimension>
class VectorImage;


/**
 *\class GradientImageFilter
 * \brief Computes the gradient of an image using directional derivatives.
 *
 * Computes the gradient of an image using directional derivatives.
 * The directional derivative at each pixel location is computed by
 * convolution with a first-order derivative operator.
 *
 * The second template parameter defines the value type used in the
 * derivative operator (defaults to float).  The third template
 * parameter defines the value type used for output image (defaults to
 * float).  The output image is defined as a covariant vector image
 * whose value type is specified as this third template parameter.
 *
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 *
 * \ingroup GradientFilters
 * \ingroup ITKImageGradient
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageGradient/GradientOfVectorImage,Gradient Of Vector Image}
 * \sphinxexample{Filtering/ImageGradient/ComputeAndDisplayGradient,Compute And Display Gradient Of Image}
 * \endsphinx
 */
template <typename TInputImage,
          typename TOperatorValueType = float,
          typename TOutputValueType = float,
          typename TOutputImageType =
            Image<CovariantVector<TOutputValueType, TInputImage::ImageDimension>, TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT GradientImageFilter : public ImageToImageFilter<TInputImage, TOutputImageType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GradientImageFilter);

  /** Extract dimension from input image. */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImageType::ImageDimension;

  /** Convenient type alias for simplifying declarations. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using OutputImageType = TOutputImageType;
  using OutputImagePointer = typename OutputImageType::Pointer;

  /** Standard class type aliases. */
  using Self = GradientImageFilter;
  using Superclass = ImageToImageFilter<InputImageType, OutputImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GradientImageFilter, ImageToImageFilter);

  /** Image type alias support */
  using InputPixelType = typename InputImageType::PixelType;
  using OperatorValueType = TOperatorValueType;
  using OutputValueType = TOutputValueType;
  using OutputPixelType = typename OutputImageType::PixelType;
  using CovariantVectorType = CovariantVector<OutputValueType, Self::OutputImageDimension>;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  /** GradientImageFilter needs a larger input requested region than
   * the output requested region.  As such, GradientImageFilter needs
   * to provide an implementation for GenerateInputRequestedRegion()
   * in order to inform the pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

  /** Set/Get whether or not the filter will use the spacing of the input
   * image in its calculations. Use On to take the image spacing information
   * into account and to compute the derivatives in physical space; use Off to
   * ignore the image spacing and to compute the derivatives in isotropic
   * voxel space. Default is On. */
  itkSetMacro(UseImageSpacing, bool);
  itkGetConstMacro(UseImageSpacing, bool);
  itkBooleanMacro(UseImageSpacing);

#if !defined(ITK_FUTURE_LEGACY_REMOVE)
  /** Use the image spacing information in calculations. Use this option if you
      want derivatives in physical space. Default is UseImageSpacingOn.
     \deprecated Use GradientImageFilter::UseImageSpacingOn instead. */
  void
  SetUseImageSpacingOn()
  {
    this->SetUseImageSpacing(true);
  }

  /** Ignore the image spacing. Use this option if you want derivatives in
      isotropic pixel space.  Default is UseImageSpacingOn.
      \deprecated Use GradientImageFilter::UseImageSpacingOff instead. */
  void
  SetUseImageSpacingOff()
  {
    this->SetUseImageSpacing(false);
  }
#endif

  /** Allows to change the default boundary condition */
  void
  OverrideBoundaryCondition(ImageBoundaryCondition<TInputImage> * boundaryCondition);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputPixelType, OutputValueType>));
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<OutputValueType>));
  // End concept checking
#endif

  /** The UseImageDirection flag determines whether image derivatives are
   * computed with respect to the image grid or with respect to the physical
   * space. When this flag is ON the derivatives are computed with respect to
   * the coordinate system of physical space. The difference is whether we take
   * into account the image Direction or not. The flag ON will take into
   * account the image direction and will result in an extra matrix
   * multiplication compared to the amount of computation performed when the
   * flag is OFF.
   * The default value of this flag is On.
   */
  itkSetMacro(UseImageDirection, bool);
  itkGetConstMacro(UseImageDirection, bool);
  itkBooleanMacro(UseImageDirection);

protected:
  GradientImageFilter();
  ~GradientImageFilter() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** GradientImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a DynamicThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling DynamicThreadedGenerateData().  DynamicThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


private:
  void
  GenerateOutputInformation() override;

  // An overloaded method which may transform the gradient to a
  // physical vector and converts to the correct output pixel type.
  template <typename TValue>
  void
  SetOutputPixel(ImageRegionIterator<VectorImage<TValue, OutputImageDimension>> & it, CovariantVectorType & gradient)
  {
    if (this->m_UseImageDirection)
    {
      CovariantVectorType physicalGradient;
      it.GetImage()->TransformLocalVectorToPhysicalVector(gradient, physicalGradient);
      it.Set(OutputPixelType(physicalGradient.GetDataPointer(), InputImageDimension, false));
    }
    else
    {
      it.Set(OutputPixelType(gradient.GetDataPointer(), InputImageDimension, false));
    }
  }

  template <typename T>
  void
  SetOutputPixel(ImageRegionIterator<T> & it, CovariantVectorType & gradient)
  {
    // This uses the more efficient set by reference method
    if (this->m_UseImageDirection)
    {
      it.GetImage()->TransformLocalVectorToPhysicalVector(gradient, it.Value());
    }
    else
    {
      it.Value() = gradient;
    }
  }


  bool m_UseImageSpacing;

  // flag to take or not the image direction into account
  // when computing the derivatives.
  bool m_UseImageDirection;

  // allow setting the the m_BoundaryCondition
  ImageBoundaryCondition<TInputImage, TInputImage> * m_BoundaryCondition;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGradientImageFilter.hxx"
#endif

#endif
