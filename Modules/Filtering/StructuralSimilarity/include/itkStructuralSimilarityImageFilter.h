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
 * \brief Computes the (multi-scale) Structural Similarity Index Measure between two images.
 *
 * This filter computes the Structural Similarity Index Measure (SSIM)
 * \cite wang2004image and its multi-scale extension (MS-SSIM)
 * \cite wang2003multiscale between two input images of identical geometry.
 * The output image stores the per-pixel SSIM map at the original resolution.
 * The scalar (MS-)SSIM is available via GetMeanSSIM() after Update().
 *
 * \par Local statistics
 * With \f$G_\sigma\f$ the DiscreteGaussianImageFilter kernel of standard
 * deviation \f$\sigma\f$ (in physical units), \f$\mu_x = G_\sigma * x\f$,
 * \f$\sigma_x^2 = G_\sigma * x^2 - \mu_x^2\f$ (likewise for \f$y\f$) and
 * \f$\sigma_{xy} = G_\sigma * (xy) - \mu_x\mu_y\f$, the luminance and
 * contrast-structure terms are
 * \f[
 *   l = \frac{2\mu_x\mu_y + C_1}{\mu_x^2 + \mu_y^2 + C_1}, \qquad
 *   cs = \frac{2\sigma_{xy} + C_2}{\sigma_x^2 + \sigma_y^2 + C_2}, \qquad
 *   \mathrm{ssim} = l \cdot cs,
 * \f]
 * with \f$C_1 = (K_1 L)^2\f$, \f$C_2 = (K_2 L)^2\f$ and \f$L\f$ the dynamic
 * range.  If any of \c LuminanceExponent (\f$\alpha\f$), \c ContrastExponent
 * (\f$\beta\f$) or \c StructureExponent (\f$\gamma\f$) differs from 1,
 * \f$cs = c^\beta s^\gamma\f$ and \f$\mathrm{ssim} = l^\alpha c^\beta s^\gamma\f$
 * with the separate contrast and structure terms of \cite wang2004image and
 * \f$C_3 = C_2/2\f$.
 *
 * \par Multi-scale combination
 * Scale \f$j = 0, \dots, M-1\f$ uses the inputs downsampled \f$j\f$ times
 * by BinShrinkImageFilter (averaging of 2x2...2 blocks) and a window of
 * \f$\sigma_j = 2^j \sigma\f$, i.e. the same number of pixels at every scale.
 * Each map is averaged over the whole image at its scale, giving
 * \f$\overline{cs}_j\f$ and \f$\overline{\mathrm{ssim}}_j\f$.  With weights
 * \f$w_j\f$ (\c ScaleWeights),
 * \f[
 *   \mathrm{MS\mbox{-}SSIM}(x,y) =
 *     \max(\overline{\mathrm{ssim}}_{M-1}, \epsilon)^{w_{M-1}}
 *     \prod_{j=0}^{M-2} \max(\overline{cs}_j, \epsilon)^{w_j},
 *     \qquad \epsilon = 10^{-6}.
 * \f]
 * The floor keeps the product finite and away from zero when a per-scale
 * mean is not positive.  The default weights are
 * WangEtAl2003ScaleWeights().  With a single scale (\f$M = 1\f$) the result
 * is the plain, unfloored mean SSIM \f$\overline{\mathrm{ssim}}_0\f$, which
 * can be negative, and the weight value is not used.  Weights must be finite
 * and non-negative.  Each image dimension must survive \f$M-1\f$
 * BinShrinkImageFilter halvings, i.e. have at least \f$2^{M-1}\f$ pixels
 * (more when the region's start index is not a multiple of \f$2^{M-1}\f$).
 *
 * The filter is N-dimensional and multi-threaded.  The output pixel type
 * defaults to \c float.
 *
 * \sa SimilarityIndexImageFilter
 * \sa DiscreteGaussianImageFilter
 * \sa BinShrinkImageFilter
 *
 * \ingroup MultiThreaded
 * \ingroup StructuralSimilarity
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

  /** Type used for the multi-scale weights and the per-scale results. */
  using ScaleWeightsType = Array<RealType>;
  using ScaleValuesType = Array<RealType>;

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

  /** Standard deviation \f$\sigma\f$ of the Gaussian window at the finest
   *  scale, in physical units.  Default 1.5. */
  /** @ITKStartGrouping */
  itkSetMacro(GaussianSigma, double);
  itkGetConstMacro(GaussianSigma, double);
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

  /** Per-scale exponents \f$w_j\f$; the number of elements is the number of
   *  scales.  A single element requests single-scale SSIM.
   *  Default WangEtAl2003ScaleWeights(). */
  /** @ITKStartGrouping */
  void
  SetScaleWeights(const ScaleWeightsType & weights);
  itkGetConstReferenceMacro(ScaleWeights, ScaleWeightsType);
  /** @ITKEndGrouping */

  /** Returns the 5-scale MS-SSIM weights of Wang et al. 2003
   *  \cite wang2003multiscale
   *  \f$(0.0448,\ 0.2856,\ 0.3001,\ 0.2363,\ 0.1333)\f$. */
  static ScaleWeightsType
  WangEtAl2003ScaleWeights();

  /** Returns \c size samples of a Gaussian of standard deviation \c sigma,
   *  centered on the middle element and normalized to sum to 1.
   *  Throws if \c sigma is not finite and strictly positive. */
  static ScaleWeightsType
  GaussianScaleWeights(unsigned int size, double sigma);

  /** Mean (MS-)SSIM.  Available after Update(). */
  itkGetConstMacro(MeanSSIM, double);

  /** Per-scale means \f$\overline{\mathrm{ssim}}_j\f$ and \f$\overline{cs}_j\f$,
   *  finest scale first, before flooring.  Available after Update(). */
  /** @ITKStartGrouping */
  itkGetConstReferenceMacro(SSIMPerScale, ScaleValuesType);
  itkGetConstReferenceMacro(ContrastStructurePerScale, ScaleValuesType);
  /** @ITKEndGrouping */

  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputPixelType>));

protected:
  StructuralSimilarityImageFilter();
  ~StructuralSimilarityImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Verify that parameters are valid and both inputs are set with matching regions. */
  void
  VerifyPreconditions() const override;

  /** This filter computes the full output. */
  void
  EnlargeOutputRequestedRegion(DataObject * data) override;

  void
  GenerateData() override;

private:
  double m_GaussianSigma{ 1.5 };
  double m_K1{ 0.01 };
  double m_K2{ 0.03 };

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

  ScaleWeightsType m_ScaleWeights{ WangEtAl2003ScaleWeights() };

  double          m_MeanSSIM{ 0.0 };
  ScaleValuesType m_SSIMPerScale{};
  ScaleValuesType m_ContrastStructurePerScale{};
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkStructuralSimilarityImageFilter.hxx"
#endif

#endif
