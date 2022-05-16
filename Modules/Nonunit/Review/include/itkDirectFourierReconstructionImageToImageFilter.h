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
#ifndef itkDirectFourierReconstructionImageToImageFilter_h
#define itkDirectFourierReconstructionImageToImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"

#include "itkVnlForwardFFTImageFilter.h"
#include "itkVnlInverseFFTImageFilter.h"

#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageSliceConstIteratorWithIndex.h"

#include "itkComplexBSplineInterpolateImageFunction.h"

#include <cmath>

namespace itk
{
/**
 * \class DirectFourierReconstructionImageToImageFilter
 * \brief Direct fourier reconstruction filter of a tomographic volume.
 *
 * The algorithm is detailed in the Insight Journal publication on
 * "Direct Fourier Tomographic Reconstruction Image-to-Image Filter"
 * by D. Zosso, M. Bach Cuadra and J. Thiran, August 2007
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/585
 *
 * \ingroup ImageFilters
 * \ingroup ITKReview
 */
template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT DirectFourierReconstructionImageToImageFilter
  : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DirectFourierReconstructionImageToImageFilter);

  /** Standard Self type alias */
  using Self = DirectFourierReconstructionImageToImageFilter;

  using InputImageType = TInputImage;
  using InputPixelType = typename InputImageType::PixelType;
  using OutputImageType = TOutputImage;
  using OutputPixelType = typename OutputImageType::PixelType;

  /** Standard Superclass type alias */
  using Superclass = ImageToImageFilter<InputImageType, OutputImageType>;

  /** Standard Pointer type alias */
  using Pointer = SmartPointer<Self>;
  /** Standard ConstPointer type alias */
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);
  itkTypeMacro(DirectFourierReconstructionImageToImageFilter, ImageToImageFilter);

  /** Class RegionType */
  using RegionType = typename InputImageType::RegionType;
  /** Class IndexType */
  using IndexType = typename InputImageType::IndexType;
  /** Class SizeType */
  using SizeType = typename InputImageType::SizeType;
  /** Class PointType */
  using PointType = typename InputImageType::PointType;
  /** Class SpacingType */
  using SpacingType = typename InputImageType::SpacingType;

  /** Standard (const) InputImagePointer */
  using ConstInputImagePointer = typename InputImageType::ConstPointer;
  /** Special (non-const) InputImagePointer */
  using InputImagePointer = typename InputImageType::Pointer;
  /** OutputImagePointer */
  using OutputImagePointer = typename OutputImageType::Pointer;

  itkSetMacro(ZeroPadding, unsigned short);
  itkGetConstMacro(ZeroPadding, unsigned short);

  itkSetMacro(OverSampling, unsigned short);
  itkGetConstMacro(OverSampling, unsigned short);

  itkSetMacro(Cutoff, double);
  itkGetConstMacro(Cutoff, double);

  itkSetMacro(AlphaRange, double);
  itkGetConstMacro(AlphaRange, double);

  itkSetMacro(AlphaDirection, unsigned short);
  itkGetConstMacro(AlphaDirection, unsigned short);

  itkSetMacro(ZDirection, unsigned short);
  itkGetConstMacro(ZDirection, unsigned short);

  itkSetMacro(RDirection, unsigned short);
  itkGetConstMacro(RDirection, unsigned short);

  itkSetMacro(RadialSplineOrder, unsigned short);
  itkGetConstMacro(RadialSplineOrder, unsigned short);

protected:
  /** Constructor */
  DirectFourierReconstructionImageToImageFilter();
  /** Destructor */
  ~DirectFourierReconstructionImageToImageFilter() override = default;

  /** Output class information */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Generate metadata for output image */
  void
  GenerateOutputInformation() override;

  /** Calculate the required input region */
  void
  GenerateInputRequestedRegion() override;

  /** Actual filter computation */
  void
  GenerateData() override;

private:
  /** Const slice iterator type of the input image */
  using InputSliceIteratorType = ImageSliceConstIteratorWithIndex<InputImageType>;

  /** 1D FFT filter type */
  using LineImageType = Image<double, 1>;
  using FFTLineFilterType = VnlForwardFFTImageFilter<LineImageType>;
  /** Derived 1D FFT image type */
  using FFTLineType = FFTLineFilterType::OutputImageType;
  /** Derived 1D input image type */
  using ProjectionLineType = FFTLineFilterType::InputImageType;
  /** 1D FFT line iterator */
  using FFTLineIteratorType = ImageRegionIteratorWithIndex<FFTLineType>;
  /** 1D FFT line B-Spline interpolator */
  using FFTLineInterpolatorType = ComplexBSplineInterpolateImageFunction<FFTLineType, double, double>;

  /** 2D inverse FFT filter type */
  using IFFTImageType = Image<std::complex<double>, 2>;
  using IFFTSliceFilterType = VnlInverseFFTImageFilter<IFFTImageType>;
  /** Derived 2D FFT image type */
  using FFTSliceType = IFFTSliceFilterType::InputImageType;
  /** Derived 2D output slice type */
  using OutputSliceType = IFFTSliceFilterType::OutputImageType;
  /** 2D FFT slice iterator */
  using FFTSliceIteratorType = ImageRegionIteratorWithIndex<FFTSliceType>;
  /** 2D output slice iterator */
  using OutputSliceIteratorType = ImageRegionIteratorWithIndex<OutputSliceType>;

  unsigned short m_ZeroPadding;  /**< n-fold zero-padding */
  unsigned short m_OverSampling; /**< n-fold oversampling */

  double m_Cutoff;     /**< Radial lowpass cut-off frequency
                        */
  double m_AlphaRange; /**< Covered angular range */

  unsigned short m_ZDirection;        /**< Axial index in the input image */
  unsigned short m_AlphaDirection;    /**< Angular index in the input image
                                       */
  unsigned short m_RDirection;        /**< Radial index in the input image
                                       */
  unsigned short m_RadialSplineOrder; /**< Spline order for the radial
                                            BSpline interpolation  */

  double m_PI; /**< The constant pi....  */

  RegionType m_InputRequestedRegion; /**< The region requested from* the input
                                       image   */
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDirectFourierReconstructionImageToImageFilter.hxx"
#endif

#endif /* itkDirectFourierReconstructionImageToImageFilter_h */
