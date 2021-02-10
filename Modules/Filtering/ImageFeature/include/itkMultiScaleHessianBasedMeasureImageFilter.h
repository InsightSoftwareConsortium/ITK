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
#ifndef itkMultiScaleHessianBasedMeasureImageFilter_h
#define itkMultiScaleHessianBasedMeasureImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkHessianRecursiveGaussianImageFilter.h"
#include "ITKImageFeatureExport.h"

namespace itk
{
/**\class MultiScaleHessianBasedMeasureImageFilterEnums
 * \brief This class contains all enum classes used by MultiScaleHessianBasedMeasureImageFilter class.
 * \ingroup ITKImageFeature
 */
class MultiScaleHessianBasedMeasureImageFilterEnums
{
public:
  /**\class SigmaStepMethod
   * \ingroup ITKImageFeature
   * \ingroup IntensityImageFilters
   * Sigma step method type
   * */
  enum class SigmaStepMethod : uint8_t
  {
    EquispacedSigmaSteps = 0,
    LogarithmicSigmaSteps = 1
  };
};
// Define how to print enumeration
extern ITKImageFeature_EXPORT std::ostream &
                              operator<<(std::ostream & out, const MultiScaleHessianBasedMeasureImageFilterEnums::SigmaStepMethod value);

/**\class MultiScaleHessianBasedMeasureImageFilter
 * \brief A filter to enhance structures using Hessian eigensystem-based
 * measures in a multiscale framework
 *
 * The filter evaluates a Hessian-based enhancement measure, such as vesselness
 * or objectness, at different scale levels. The Hessian-based measure is computed
 * from the Hessian image at each scale level and the best response is selected.
 *
 * Minimum and maximum sigma value can be set using SetMinSigma and SetMaxSigma
 * methods respectively. The number of scale levels is set using
 * SetNumberOfSigmaSteps method. Exponentially distributed scale levels are
 * computed within the bound set by the minimum and maximum sigma values
 *
 * The filter computes a second output image (accessed by the GetScalesOutput method)
 * containing the scales at which each pixel gave the best response.
 *
 *
 * This code was contributed in the Insight Journal paper:
 * "Generalizing vesselness with respect to dimensionality and shape"
 * by Antiga L.
 * https://www.insight-journal.org/browse/publication/175
 *
 *
 * \author Luca Antiga Ph.D.  Medical Imaging Unit,
 *                            Bioengineering Department, Mario Negri Institute, Italy.
 *
 * \sa HessianToObjectnessMeasureImageFilter
 * \sa Hessian3DToVesselnessMeasureImageFilter
 * \sa HessianSmoothed3DToVesselnessMeasureImageFilter
 * \sa HessianRecursiveGaussianImageFilter
 * \sa SymmetricEigenAnalysisImageFilter
 * \sa SymmetricSecondRankTensor
 *
 * \ingroup IntensityImageFilters TensorObjects
 *
 * \ingroup ITKImageFeature
 */
template <typename TInputImage, typename THessianImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT MultiScaleHessianBasedMeasureImageFilter
  : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MultiScaleHessianBasedMeasureImageFilter);

  /** Standard class type aliases. */
  using Self = MultiScaleHessianBasedMeasureImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using HessianImageType = THessianImage;

  using HessianToMeasureFilterType = ImageToImageFilter<HessianImageType, OutputImageType>;

  using InputPixelType = typename TInputImage::PixelType;
  using OutputPixelType = typename TOutputImage::PixelType;
  using OutputRegionType = typename TOutputImage::RegionType;

  /** Image dimension. */
  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;

  /** Types for Scales image */
  using ScalesPixelType = float;
  using ScalesImageType = Image<ScalesPixelType, Self::ImageDimension>;

  /** Hessian computation filter. */
  using HessianFilterType = HessianRecursiveGaussianImageFilter<InputImageType, HessianImageType>;

  /** Update image buffer that holds the best objectness response. This is not redundant from
   the output image because the latter may not be of float type, which is required for the comparisons
   between responses at different scales. */
  using UpdateBufferType = Image<double, Self::ImageDimension>;
  using BufferValueType = typename UpdateBufferType::ValueType;

  using DataObjectPointer = typename Superclass::DataObjectPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MultiScaleHessianBasedMeasureImageFilter, ImageToImageFilter);

  /** Set/Get macros for SigmaMin */
  itkSetMacro(SigmaMinimum, double);
  itkGetConstMacro(SigmaMinimum, double);

  /** Set/Get macros for SigmaMax */
  itkSetMacro(SigmaMaximum, double);
  itkGetConstMacro(SigmaMaximum, double);

  /** Set/Get macros for Number of Scales */
  itkSetMacro(NumberOfSigmaSteps, unsigned int);
  itkGetConstMacro(NumberOfSigmaSteps, unsigned int);

  /** Set/Get HessianToMeasureFilter. This will be a filter that takes
   Hessian input image and produces enhanced output scalar image. The filter must derive from
   itk::ImageToImage filter */
  itkSetObjectMacro(HessianToMeasureFilter, HessianToMeasureFilterType);
  itkGetModifiableObjectMacro(HessianToMeasureFilter, HessianToMeasureFilterType);

  /** Methods to turn on/off flag to inform the filter that the Hessian-based measure
   is non-negative (classical measures like Sato's and Frangi's are), hence it has a minimum
   at zero. In this case, the update buffer is initialized at zero, and the output scale and Hessian
   are zero in case the Hessian-based measure returns zero for all scales. Otherwise, the minimum
   output scale and Hessian are the ones obtained at scale SigmaMinimum. On by default.
   */
  itkSetMacro(NonNegativeHessianBasedMeasure, bool);
  itkGetConstMacro(NonNegativeHessianBasedMeasure, bool);
  itkBooleanMacro(NonNegativeHessianBasedMeasure);

  using SigmaStepMethodEnum = MultiScaleHessianBasedMeasureImageFilterEnums::SigmaStepMethod;
#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  static constexpr SigmaStepMethodEnum EquispacedSigmaSteps = SigmaStepMethodEnum::EquispacedSigmaSteps;
  static constexpr SigmaStepMethodEnum LogarithmicSigmaSteps = SigmaStepMethodEnum::LogarithmicSigmaSteps;
#endif

  /** Set/Get the method used to generate scale sequence (Equispaced
   * or Logarithmic) */
  itkSetEnumMacro(SigmaStepMethod, SigmaStepMethodEnum);
  itkGetConstMacro(SigmaStepMethod, SigmaStepMethodEnum);

  /**Set equispaced sigma step method */
  void
  SetSigmaStepMethodToEquispaced();

  /**Set logarithmic sigma step method */
  void
  SetSigmaStepMethodToLogarithmic();

  /** Get the image containing the Hessian computed at the best
   * response scale */
  const HessianImageType *
  GetHessianOutput() const;

  /** Get the image containing the scales at which each pixel gave the
   * best response */
  const ScalesImageType *
  GetScalesOutput() const;

  /** Methods to turn on/off flag to generate an image with scale values at
   *  each pixel for the best vesselness response */
  itkSetMacro(GenerateScalesOutput, bool);
  itkGetConstMacro(GenerateScalesOutput, bool);
  itkBooleanMacro(GenerateScalesOutput);

  /** Methods to turn on/off flag to generate an image with hessian values at
   *  each pixel for the best vesselness response */
  itkSetMacro(GenerateHessianOutput, bool);
  itkGetConstMacro(GenerateHessianOutput, bool);
  itkBooleanMacro(GenerateHessianOutput);

  /** This is overloaded to create the Scales and Hessian output images */
  using DataObjectPointerArraySizeType = ProcessObject::DataObjectPointerArraySizeType;

protected:
  MultiScaleHessianBasedMeasureImageFilter();
  ~MultiScaleHessianBasedMeasureImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Generate Data */
  void
  GenerateData() override;

  void
  EnlargeOutputRequestedRegion(DataObject *) override;

  using Superclass::MakeOutput;
  DataObjectPointer
  MakeOutput(DataObjectPointerArraySizeType idx) override;

private:
  void
  UpdateMaximumResponse(double sigma);

  double
  ComputeSigmaValue(int scaleLevel);

  void
  AllocateUpdateBuffer();

  bool m_NonNegativeHessianBasedMeasure;

  double m_SigmaMinimum;
  double m_SigmaMaximum;

  unsigned int        m_NumberOfSigmaSteps;
  SigmaStepMethodEnum m_SigmaStepMethod;

  typename HessianToMeasureFilterType::Pointer m_HessianToMeasureFilter;

  typename HessianFilterType::Pointer m_HessianFilter;

  typename UpdateBufferType::Pointer m_UpdateBuffer;

  bool m_GenerateScalesOutput;
  bool m_GenerateHessianOutput;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMultiScaleHessianBasedMeasureImageFilter.hxx"
#endif

#endif
