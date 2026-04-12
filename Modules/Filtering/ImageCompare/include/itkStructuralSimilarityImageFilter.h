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

#include "itkArray.h"
#include "itkImage.h"
#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"

#include <type_traits>

namespace itk
{
/**
 * \class StructuralSimilarityImageFilter
 * \brief Computes the Structural Similarity Index Measure (SSIM) between two images.
 *
 * This filter computes the Structural Similarity Index Measure
 * \cite wang2004image between two input images of identical geometry.  The
 * output image stores the per-pixel SSIM map.  The scalar mean SSIM over the
 * valid (non-boundary) region is available via GetMeanSSIM() after Update().
 *
 * \par Algorithm
 * For two images \f$x\f$ and \f$y\f$, local statistics are computed by
 * convolving with a discrete Gaussian kernel of standard deviation
 * \f$\sigma\f$ (default 1.5):
 * \f[
 *   \mu_x = G_\sigma * x, \quad \mu_y = G_\sigma * y,
 * \f]
 * \f[
 *   \sigma_x^2 = G_\sigma * x^2 - \mu_x^2,\quad
 *   \sigma_y^2 = G_\sigma * y^2 - \mu_y^2,\quad
 *   \sigma_{xy} = G_\sigma * (xy) - \mu_x \mu_y .
 * \f]
 *
 * The three SSIM components are
 * \f[
 *   l(x,y) = \frac{2\mu_x\mu_y + C_1}{\mu_x^2 + \mu_y^2 + C_1}, \qquad
 *   c(x,y) = \frac{2\sigma_x\sigma_y + C_2}{\sigma_x^2 + \sigma_y^2 + C_2}, \qquad
 *   s(x,y) = \frac{\sigma_{xy} + C_3}{\sigma_x\sigma_y + C_3}
 * \f]
 * with \f$C_1 = (K_1 L)^2\f$, \f$C_2 = (K_2 L)^2\f$, \f$C_3 = C_2/2\f$,
 * and \f$L\f$ the dynamic range of the pixel values.
 *
 * The combined SSIM is
 * \f[
 *   \mathrm{SSIM}(x,y) = [l(x,y)]^{\alpha}\,[c(x,y)]^{\beta}\,[s(x,y)]^{\gamma}.
 * \f]
 *
 * With the default exponents \f$\alpha = \beta = \gamma = 1\f$ and the
 * convention \f$C_3 = C_2/2\f$, this collapses to the simplified form
 * \f[
 *   \mathrm{SSIM}(x,y) =
 *     \frac{(2\mu_x\mu_y + C_1)\,(2\sigma_{xy} + C_2)}
 *          {(\mu_x^2 + \mu_y^2 + C_1)\,(\sigma_x^2 + \sigma_y^2 + C_2)}
 * \f]
 * which matches the reference implementation distributed by Wang et al.
 * and the default behavior of \c skimage.metrics.structural_similarity .
 *
 * \par Properties
 * - For identical images, the per-pixel SSIM is exactly 1 and the mean SSIM
 *   is exactly 1 (subject to floating-point precision).
 * - The SSIM index is symmetric: \f$\mathrm{SSIM}(x,y) = \mathrm{SSIM}(y,x)\f$.
 * - The SSIM index is bounded above by 1.  In typical cases it is
 *   non-negative; values can be slightly negative for anti-correlated
 *   regions.
 *
 * \par Parameters
 * - \c GaussianSigma: standard deviation of the Gaussian window
 *   (default 1.5, matching Wang et al.).
 * - \c MaximumKernelWidth: hard limit on the discrete Gaussian kernel width
 *   (default 11, matching the canonical 11x11 window).
 * - \c K1, \c K2: stability constants (defaults 0.01 and 0.03).
 * - \c DynamicRange: \f$L\f$ in the formulas above; defaults to the dynamic
 *   range of the input pixel type via NumericTraits (e.g. 255 for
 *   \c unsigned char, 1.0 for \c float / \c double).  For arbitrary
 *   floating-point images, set this explicitly to the actual data range.
 * - \c LuminanceExponent (\f$\alpha\f$), \c ContrastExponent (\f$\beta\f$),
 *   \c StructureExponent (\f$\gamma\f$): defaults all 1.0.
 * - \c ScaleWeights: array of per-scale weights for multi-scale SSIM
 *   (MS-SSIM, \cite wang2003multiscale).  When the array contains a single
 *   element (the default), the filter computes ordinary single-scale SSIM.
 *   Multi-scale evaluation with more than one scale is not yet implemented
 *   and will raise an exception in BeforeGenerate.
 *
 * The filter is N-dimensional, multi-threaded, and templated over the input
 * and output image types.  The output pixel type defaults to \c float.
 *
 * \sa SimilarityIndexImageFilter
 * \sa DiscreteGaussianImageFilter
 *
 * \ingroup MultiThreaded
 * \ingroup ITKImageCompare
 */
template <typename TInputImage, typename TOutputImage = Image<float, TInputImage::ImageDimension>>
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

  /** Floating-point type used for all SSIM computations. */
  using RealType = typename NumericTraits<InputPixelType>::RealType;

  /** Type used for the user-specified array of multi-scale weights. */
  using ScaleWeightsType = Array<RealType>;

  /** Set/Get the first input image. */
  /** @ITKStartGrouping */
  void
  SetInput1(const InputImageType * image)
  {
    this->SetInput(image);
  }
  const InputImageType *
  GetInput1() const
  {
    return this->GetInput(0);
  }
  /** @ITKEndGrouping */

  /** Set/Get the second input image. */
  /** @ITKStartGrouping */
  void
  SetInput2(const InputImageType * image);
  const InputImageType *
  GetInput2() const;
  /** @ITKEndGrouping */

  /** Standard deviation \f$\sigma\f$ of the Gaussian window used to compute
   *  local statistics.  Default 1.5 (matching Wang et al. 2004). */
  /** @ITKStartGrouping */
  itkSetMacro(GaussianSigma, double);
  itkGetConstMacro(GaussianSigma, double);
  /** @ITKEndGrouping */

  /** Maximum width (per dimension) of the discrete Gaussian kernel.
   *  Default 11, giving an 11x11 window in 2D when sigma=1.5. */
  /** @ITKStartGrouping */
  itkSetMacro(MaximumKernelWidth, unsigned int);
  itkGetConstMacro(MaximumKernelWidth, unsigned int);
  /** @ITKEndGrouping */

  /** \f$K_1\f$ stability constant.  Default 0.01. */
  /** @ITKStartGrouping */
  itkSetMacro(K1, double);
  itkGetConstMacro(K1, double);
  /** @ITKEndGrouping */

  /** \f$K_2\f$ stability constant.  Default 0.03. */
  /** @ITKStartGrouping */
  itkSetMacro(K2, double);
  itkGetConstMacro(K2, double);
  /** @ITKEndGrouping */

  /** Dynamic range \f$L\f$ of the pixel values used to compute
   *  \f$C_1 = (K_1 L)^2\f$ and \f$C_2 = (K_2 L)^2\f$.  Default depends on
   *  the input pixel type: 255 for \c unsigned \c char, 65535 for
   *  \c unsigned \c short, 1.0 for \c float / \c double, etc. */
  /** @ITKStartGrouping */
  itkSetMacro(DynamicRange, double);
  itkGetConstMacro(DynamicRange, double);
  /** @ITKEndGrouping */

  /** Exponent \f$\alpha\f$ on the luminance term.  Default 1.0. */
  /** @ITKStartGrouping */
  itkSetMacro(LuminanceExponent, double);
  itkGetConstMacro(LuminanceExponent, double);
  /** @ITKEndGrouping */

  /** Exponent \f$\beta\f$ on the contrast term.  Default 1.0. */
  /** @ITKStartGrouping */
  itkSetMacro(ContrastExponent, double);
  itkGetConstMacro(ContrastExponent, double);
  /** @ITKEndGrouping */

  /** Exponent \f$\gamma\f$ on the structure term.  Default 1.0. */
  /** @ITKStartGrouping */
  itkSetMacro(StructureExponent, double);
  itkGetConstMacro(StructureExponent, double);
  /** @ITKEndGrouping */

  /** Per-scale weights for multi-scale SSIM (MS-SSIM).  An array of size 1
   *  (the default) requests ordinary single-scale SSIM and is the only
   *  configuration currently supported.  Setting an array of length greater
   *  than 1 will currently raise an exception in BeforeGenerate. */
  /** @ITKStartGrouping */
  void
  SetScaleWeights(const ScaleWeightsType & weights);
  itkGetConstReferenceMacro(ScaleWeights, ScaleWeightsType);
  /** @ITKEndGrouping */

  /** Mean SSIM over the valid (non-Gaussian-padded) region.  Available
   *  after Update(). */
  itkGetConstMacro(MeanSSIM, double);

  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputPixelType>));

protected:
  StructuralSimilarityImageFilter();
  ~StructuralSimilarityImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Verify that parameters are valid and both inputs are set with matching regions. */
  void
  VerifyPreconditions() const override;

  /** This filter requires its full input to compute correct mean SSIM. */
  void
  GenerateInputRequestedRegion() override;

  /** This filter computes the full output. */
  void
  EnlargeOutputRequestedRegion(DataObject * data) override;

  /** Composite-filter-style: drives the internal sub-pipeline (5 Gaussian
   *  convolutions plus a parallelized SSIM combination). */
  void
  GenerateData() override;

private:
  double       m_GaussianSigma{ 1.5 };
  unsigned int m_MaximumKernelWidth{ 11 };
  double       m_K1{ 0.01 };
  double       m_K2{ 0.03 };

  /** Default dynamic range: 1.0 for floating-point pixels (assume normalized
   *  data), and \c NumericTraits::max() - \c NumericTraits::min() for integer
   *  pixels (e.g. 255 for \c unsigned \c char). */
  static constexpr double
  DefaultDynamicRange()
  {
    if constexpr (std::is_floating_point_v<InputPixelType>)
    {
      return 1.0;
    }
    else
    {
      return static_cast<double>(NumericTraits<InputPixelType>::max()) -
             static_cast<double>(NumericTraits<InputPixelType>::min());
    }
  }
  double m_DynamicRange{ DefaultDynamicRange() };
  double m_LuminanceExponent{ 1.0 };
  double m_ContrastExponent{ 1.0 };
  double m_StructureExponent{ 1.0 };

  ScaleWeightsType m_ScaleWeights{};

  double m_MeanSSIM{ 0.0 };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkStructuralSimilarityImageFilter.hxx"
#endif

#endif
