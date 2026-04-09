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
#ifndef itkStructuralSimilarityImageFilter_h
#define itkStructuralSimilarityImageFilter_h
#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"
#include "itkArray.h"
namespace itk
{
/**
 * \class StructuralSimilarityImageFilter
 * \brief Computes the Structural Similarity Index Measure (SSIM) between two images.
 *
 * This filter computes the local SSIM map and an overall scalar SSIM value
 * between two input images. The SSIM is defined as:
 *
 * \f[
 *   \text{SSIM}(x,y) = [l(x,y)]^{\alpha} \cdot [c(x,y)]^{\beta} \cdot [s(x,y)]^{\gamma}
 * \f]
 *
 * where
 * \f[
 *   l(x,y) = \frac{2\mu_x\mu_y + C_1}{\mu_x^2 + \mu_y^2 + C_1}, \quad
 *   c(x,y) = \frac{2\sigma_x\sigma_y + C_2}{\sigma_x^2 + \sigma_y^2 + C_2}, \quad
 *   s(x,y) = \frac{\sigma_{xy} + C_3}{\sigma_x\sigma_y + C_3}
 * \f]
 *
 * with \f$C_1 = (K_1 L)^2\f$, \f$C_2 = (K_2 L)^2\f$, \f$C_3 = C_2 / 2\f$.
 * \f$L\f$ is the dynamic range (defaults to 255), and \f$K_1\f$ and \f$K_2\f$
 * are small stability constants (defaults 0.01 and 0.03 respectively).
 *
 * The exponents \f$\alpha\f$, \f$\beta\f$, and \f$\gamma\f$ control the
 * relative importance of luminance, contrast, and structure. With the default
 * values of 1.0 the formula simplifies to the standard SSIM:
 *
 * \f[
 *   \text{SSIM}(x,y) = \frac{(2\mu_x\mu_y + C_1)(2\sigma_{xy} + C_2)}{(\mu_x^2 + \mu_y^2 + C_1)(\sigma_x^2 + \sigma_y^2 + C_2)}
 * \f]
 *
 * The output image contains the per-pixel SSIM map. The scalar mean SSIM
 * across all pixels can be retrieved with GetSSIM().
 *
 * The filter is N-dimensional and multi-threaded.
 *
 * \par Parameters
 * - Radius: neighborhood radius for local statistics (default: 1 in each dimension, giving a 3x3 window in 2D).
 * - LuminanceWeight (\f$\alpha\f$), ContrastWeight (\f$\beta\f$), StructureWeight (\f$\gamma\f$):
 *   exponents for the three components (all default to 1.0).
 * - DynamicRange (\f$L\f$): range of pixel values (default: 255.0).
 * - K1, K2: stabilization constants (defaults: 0.01, 0.03).
 *
 * \sa SimilarityIndexImageFilter
 *
 * \ingroup ImageCompare
 * \ingroup ITKImageCompare
 */
template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT StructuralSimilarityImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(StructuralSimilarityImageFilter);
  /** Standard class type aliases. */
  using Self = StructuralSimilarityImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(StructuralSimilarityImageFilter);
  /** Image type aliases. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using InputPixelType = typename InputImageType::PixelType;
  using OutputPixelType = typename OutputImageType::PixelType;
  using InputImageRegionType = typename InputImageType::RegionType;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using SizeType = typename InputImageType::SizeType;
  using IndexType = typename InputImageType::IndexType;
  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;
  /** Floating point type for computations. */
  using RealType = typename NumericTraits<InputPixelType>::RealType;
  /** Neighborhood radius type. */
  using RadiusType = SizeType;
  using RadiusValueType = typename RadiusType::SizeValueType;
  /** Set/Get the neighborhood radius for local statistics computation. */
  virtual void
  SetRadius(const RadiusType & radius);
  virtual void
  SetRadius(RadiusValueType radius);
  itkGetConstReferenceMacro(Radius, RadiusType);
  /** Set the first input image. */
  void
  SetInput1(const InputImageType * image)
  {
    this->SetInput(image);
  }
  /** Set the second input image. */
  void
  SetInput2(const InputImageType * image);
  /** Get the first input image. */
  const InputImageType *
  GetInput1() const
  {
    return this->GetInput(0);
  }
  /** Get the second input image. */
  const InputImageType *
  GetInput2() const;
  /** Set/Get the luminance exponent (alpha). Default: 1.0. */
  itkSetMacro(LuminanceWeight, RealType);
  itkGetConstMacro(LuminanceWeight, RealType);
  /** Set/Get the contrast exponent (beta). Default: 1.0. */
  itkSetMacro(ContrastWeight, RealType);
  itkGetConstMacro(ContrastWeight, RealType);
  /** Set/Get the structure exponent (gamma). Default: 1.0. */
  itkSetMacro(StructureWeight, RealType);
  itkGetConstMacro(StructureWeight, RealType);
  /** Set/Get the dynamic range L of the pixel values. Default: 255.0. */
  itkSetMacro(DynamicRange, RealType);
  itkGetConstMacro(DynamicRange, RealType);
  /** Set/Get the K1 stabilization constant. Default: 0.01. */
  itkSetMacro(K1, RealType);
  itkGetConstMacro(K1, RealType);
  /** Set/Get the K2 stabilization constant. Default: 0.03. */
  itkSetMacro(K2, RealType);
  itkGetConstMacro(K2, RealType);
  /** Get the computed mean SSIM value (available after Update()). */
  itkGetConstMacro(SSIM, RealType);
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputPixelType>));
protected:
  StructuralSimilarityImageFilter();
  ~StructuralSimilarityImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
  void
  GenerateInputRequestedRegion() override;
  void
  BeforeThreadedGenerateData() override;
  void
  AfterThreadedGenerateData() override;
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;
private:
  RadiusType m_Radius{};
  RealType   m_LuminanceWeight{ 1.0 };
  RealType   m_ContrastWeight{ 1.0 };
  RealType   m_StructureWeight{ 1.0 };
  RealType   m_DynamicRange{ 255.0 };
  RealType   m_K1{ 0.01 };
  RealType   m_K2{ 0.03 };
  RealType   m_SSIM{};
  /** Thread-local accumulators for computing the global mean SSIM. */
  std::mutex         m_Mutex{};
  RealType           m_AccumulatedSSIM{};
  SizeValueType      m_PixelCount{};
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkStructuralSimilarityImageFilter.hxx"
#endif
#endif
