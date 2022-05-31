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
#ifndef itkPhaseSymmetryImageFilter_h
#define itkPhaseSymmetryImageFilter_h

#include "itkFFTShiftImageFilter.h"
#include "itkArray2D.h"
#include "itkImageToImageFilter.h"
#include "itkConceptChecking.h"
#include "itkMultiplyImageFilter.h"
#include "itkDivideImageFilter.h"
#include "itkAddImageFilter.h"
#include "itkSqrtImageFilter.h"
#include "itkSquareImageFilter.h"
#include "itkMaximumImageFilter.h"
#include "itkExpImageFilter.h"
#include "itkBoundedReciprocalImageFilter.h"
#include "itkAtan2ImageFilter.h"
#include "itkAcosImageFilter.h"
#include "itkLogGaborFreqImageSource.h"
#include "itkSteerableFilterFreqImageSource.h"
#include "itkButterworthFilterFreqImageSource.h"
#include "itkComposeImageFilter.h"
#include "itkMagnitudeAndPhaseToComplexImageFilter.h"
#include "itkImageAdaptor.h"
#include "itkForwardFFTImageFilter.h"
#include "itkComplexToComplexFFTImageFilter.h"
#include "itkComplexToRealImageFilter.h"
#include "itkComplexToPhaseImageFilter.h"
#include "itkComplexToImaginaryImageFilter.h"
#include "itkComplexToImaginaryImageFilter.h"
#include "itkComplexToModulusImageFilter.h"
#include "itkShiftScaleImageFilter.h"
#include "itkAbsImageFilter.h"

#include <vector>
#include <complex>

namespace itk
{

/**
 * \class PhaseSymmetryImageFilter
 *
 * Phase congruency and feature type templated for an ndimensional image
 * See Peter Kovesi's site for details on the filter
 *
 * \ingroup PhaseSymmetry
 */
template <typename TInputImage, typename TOutputImage>
class PhaseSymmetryImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PhaseSymmetryImageFilter);

  /** Standard class type alias. */
  using Self = PhaseSymmetryImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PhaseSymmetryImageFilter, ImageToImageFilter);

  itkStaticConstMacro(InputImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, TOutputImage::ImageDimension);

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  using ComplexPixelComponentType = OutputImagePixelType;
  using ImagePixelType = OutputImagePixelType;
  using ComplexPixelType = std::complex<ComplexPixelComponentType>;

  using ArrayType = FixedArray<double, InputImageDimension>;
  using DimMinusOneDoubleArrayType = FixedArray<double, InputImageDimension - 1>;
  using MatrixType = Array2D<double>;

  using FloatImageType = Image<ImagePixelType, InputImageDimension>;

  itkSetMacro(Wavelengths, MatrixType);
  itkSetMacro(Orientations, MatrixType);
  itkSetMacro(AngleBandwidth, double);
  itkSetMacro(Sigma, double);
  itkSetMacro(NoiseThreshold, double);
  itkSetMacro(Polarity, int);


  void
  Initialize();
  /** Input and output images must be the same dimension, or the output's
  dimension must be one less than that of the input. */
#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(ImageDimensionCheck, (Concept::SameDimension<InputImageDimension, OutputImageDimension>));
  /** End concept checking */
#endif


protected:
  PhaseSymmetryImageFilter();
  ~PhaseSymmetryImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Apply changes to the output image information. */
  void
  GenerateOutputInformation() override;

  /** Apply changes to the input image requested region. */
  void
  GenerateInputRequestedRegion() override;

  void
  GenerateData() override;

  static const int FFT_FORWARD = -1;
  static const int FFT_BACKWARD = 1;

  using FFTFilterType = ForwardFFTImageFilter<InputImageType>;
  using ComplexImageType = typename FFTFilterType::OutputImageType;
  using IFFTFilterType = ComplexToComplexFFTImageFilter<ComplexImageType>;


  using FloatImageStack = std::vector<typename FloatImageType::Pointer>;
  using FloatImageBank = std::vector<FloatImageStack>;

  using MultiplyImageFilterType = MultiplyImageFilter<FloatImageType, FloatImageType>;
  using ComplexMultiplyImageFilterType = MultiplyImageFilter<ComplexImageType, ComplexImageType>;
  using DivideImageFilterType = DivideImageFilter<FloatImageType, FloatImageType, FloatImageType>;
  using AddImageFilterType = AddImageFilter<FloatImageType, FloatImageType>;
  using SqrtImageFilterType = SqrtImageFilter<FloatImageType, FloatImageType>;
  using SquareImageFilterType = SquareImageFilter<FloatImageType, FloatImageType>;
  using MaxImageFilterType = MaximumImageFilter<FloatImageType, FloatImageType>;
  using ExpImageFilterType = ExpImageFilter<FloatImageType, FloatImageType>;
  using BoundedReciprocalImageFilterType = BoundedReciprocalImageFilter<FloatImageType, FloatImageType>;
  using Atan2ImageFilterType = Atan2ImageFilter<FloatImageType, FloatImageType, FloatImageType>;
  using AcosImageFilterType = AcosImageFilter<FloatImageType, FloatImageType>;


  using LogGaborFreqImageSourceType = LogGaborFreqImageSource<FloatImageType>;
  using SteerableFiltersFreqImageSourceType = SteerableFilterFreqImageSource<FloatImageType>;
  using ButterworthKernelFreqImageSourceType = ButterworthFilterFreqImageSource<FloatImageType>;

  using ShiftScaleImageFilterType = ShiftScaleImageFilter<FloatImageType, FloatImageType>;
  using ComplexToRealFilterType = ComplexToRealImageFilter<ComplexImageType, FloatImageType>;
  using ComplexToImaginaryFilterType = ComplexToImaginaryImageFilter<ComplexImageType, FloatImageType>;
  using ComplexToModulusFilterType = ComplexToModulusImageFilter<ComplexImageType, FloatImageType>;
  using ComplexToPhaseFilterType = ComplexToPhaseImageFilter<ComplexImageType, FloatImageType>;
  using RealAndImaginaryToComplexFilterType = ComposeImageFilter<FloatImageType, ComplexImageType>;
  using MagnitudeAndPhaseToComplexFilterType =
    MagnitudeAndPhaseToComplexImageFilter<InputImageType, InputImageType, ComplexImageType>;
  using ComplexFFTShiftImageFilterType = FFTShiftImageFilter<ComplexImageType, ComplexImageType>;
  using DoubleFFTShiftImageFilterType = FFTShiftImageFilter<FloatImageType, FloatImageType>;
  using AbsImageFilterType = AbsImageFilter<FloatImageType, FloatImageType>;

private:
  MatrixType m_Wavelengths;
  MatrixType m_Orientations;

  double m_AngleBandwidth;
  double m_Sigma;
  double m_NoiseThreshold;
  int    m_Polarity;

  typename MultiplyImageFilterType::Pointer m_MultiplyImageFilter;
  typename DivideImageFilterType::Pointer   m_DivideImageFilter;
  typename AddImageFilterType::Pointer      m_AddImageFilter;
  typename AddImageFilterType::Pointer      m_AddImageFilter2;
  typename MaxImageFilterType::Pointer      m_MaxImageFilter;
  typename Atan2ImageFilterType::Pointer    m_AtanImageFilter;
  typename AcosImageFilterType::Pointer     m_AcosImageFilter;

  typename FFTFilterType::Pointer  m_FFTFilter;
  typename IFFTFilterType::Pointer m_IFFTFilter;

  typename ShiftScaleImageFilterType::Pointer            m_ShiftScaleFilter;
  typename ShiftScaleImageFilterType::Pointer            m_NegateFilter;
  typename ShiftScaleImageFilterType::Pointer            m_NegateFilter2;
  typename ComplexToRealFilterType::Pointer              m_C2RFilter;
  typename ComplexToImaginaryFilterType::Pointer         m_C2IFilter;
  typename ComplexToModulusFilterType::Pointer           m_C2MFilter;
  typename ComplexToPhaseFilterType::Pointer             m_C2AFilter;
  typename AbsImageFilterType::Pointer                   m_AbsImageFilter;
  typename AbsImageFilterType::Pointer                   m_AbsImageFilter2;
  typename MagnitudeAndPhaseToComplexFilterType::Pointer m_MP2CFilter;

  typename FloatImageType::Pointer m_PhaseSymmetry;

  FloatImageBank m_FilterBank;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPhaseSymmetryImageFilter.hxx"
#endif

#endif
