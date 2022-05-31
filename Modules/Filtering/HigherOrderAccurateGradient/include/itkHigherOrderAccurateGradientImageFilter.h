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
#ifndef itkHigherOrderAccurateGradientImageFilter_h
#define itkHigherOrderAccurateGradientImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkCovariantVector.h"

namespace itk
{

/** \class HigherOrderAccurateGradientImageFilter
 *
 * \brief Calculate the image gradient from a higher order accurate
 * central-difference derivative kernel.
 *
 * Based on the work here:
 *
 * Khan, IR and Ohba, Ryoji.  "Closed-form expressions for the finite difference
 * approximations of first and higher derivatives based on Taylor series."
 * Journal of Computational and Applied Mathematics.  vol 107.  p. 179-193.
 * 1999.
 *
 * Khan, IR and Ohba, Ryoji.  "Taylor series based finite difference
 * approximations of higher-degree derivatives."  Journal of Computational and
 * Applied Mathematics.  vol 154.  p. 115-124.  2003.
 *
 * To specify the order of accuracy, use SetOrderOfAccuracy().  The
 * approximation will be accurate to two times the OrderOfAccuracy in terms of
 * Taylor series terms.
 *
 * \sa HigherOrderAccurateDerivativeOperator
 * \sa HigherOrderAccurateDerivativeImageFilter
 *
 * \ingroup GradientFilters
 * \ingroup HigherOrderAccurateGradient
 */
template <typename TInputImage, typename TOperatorValueType = float, class TOutputValueType = float>
class HigherOrderAccurateGradientImageFilter
  : public ImageToImageFilter<
      TInputImage,
      Image<CovariantVector<TOutputValueType, TInputImage::ImageDimension>, TInputImage::ImageDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(HigherOrderAccurateGradientImageFilter);

  /** Extract dimension from input image. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Standard class type alias. */
  using Self = HigherOrderAccurateGradientImageFilter;

  /** Convenient type alias for simplifying declarations. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using OutputImageType = Image<CovariantVector<TOutputValueType, itkGetStaticConstMacro(OutputImageDimension)>,
                                itkGetStaticConstMacro(OutputImageDimension)>;
  using OutputImagePointer = typename OutputImageType::Pointer;

  /** Standard class type alias. */
  using Superclass = ImageToImageFilter<InputImageType, OutputImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(HigherOrderAccurateGradientImageFilter, ImageToImageFilter);

  /** Image type alias support. */
  using InputPixelType = typename InputImageType::PixelType;
  using OperatorValueType = TOperatorValueType;
  using OutputValueType = TOutputValueType;
  using OutputPixelType = CovariantVector<OutputValueType, itkGetStaticConstMacro(OutputImageDimension)>;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  /** Set/Get whether or not the filter will use the spacing of the input
      image in its calculations */
  itkSetMacro(UseImageSpacing, bool);
  itkGetConstMacro(UseImageSpacing, bool);
  itkBooleanMacro(UseImageSpacing);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputPixelType, OutputValueType>));
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<OutputValueType>));
  /** End concept checking */
#endif

  /** The UseImageDirection flag determines whether image derivatives are
   * computed with respect to the image grid or with respect to the physical
   * space. When this flag is ON the derivatives are computed with respect to
   * the coodinate system of physical space. The difference is whether we take
   * into account the image Direction or not. The flag ON will take into
   * account the image direction and will result in an extra matrix
   * multiplication compared to the amount of computation performed when the
   * flag is OFF.
   * The default value of this flag is On.
   */
  itkSetMacro(UseImageDirection, bool);
  itkGetConstMacro(UseImageDirection, bool);
  itkBooleanMacro(UseImageDirection);

  /** Set/Get the order of accuracy of the derivative operator.  For more
   * information, see HigherOrderAccurateDerivativeOperator. */
  itkSetMacro(OrderOfAccuracy, unsigned int);
  itkGetConstMacro(OrderOfAccuracy, unsigned int);

protected:
  HigherOrderAccurateGradientImageFilter();
  ~HigherOrderAccurateGradientImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** GradientImageFilter needs a larger input requested region than
   * the output requested region.  As such, GradientImageFilter needs
   * to provide an implementation for GenerateInputRequestedRegion()
   * in order to inform the pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

  /** GradientImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

private:
  bool m_UseImageSpacing{ true };

  // flag to take or not the image direction into account
  // when computing the derivatives.
  bool m_UseImageDirection{ true };

  unsigned int m_OrderOfAccuracy{ 2 };
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkHigherOrderAccurateGradientImageFilter.hxx"
#endif

#endif
