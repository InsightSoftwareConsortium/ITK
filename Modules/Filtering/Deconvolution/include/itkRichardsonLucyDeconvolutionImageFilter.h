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
#ifndef itkRichardsonLucyDeconvolutionImageFilter_h
#define itkRichardsonLucyDeconvolutionImageFilter_h

#include "itkIterativeDeconvolutionImageFilter.h"

#include "itkComplexConjugateImageAdaptor.h"
#include "itkDivideOrZeroOutImageFilter.h"
#include "itkMultiplyImageFilter.h"

namespace itk
{
/**
 *\class RichardsonLucyDeconvolutionImageFilter
 * \brief Deconvolve an image using the Richardson-Lucy deconvolution
 * algorithm.
 *
 * This filter implements the Richardson-Lucy deconvolution algorithm
 * as defined in Bertero M and Boccacci P, "Introduction to Inverse
 * Problems in Imaging", 1998. The algorithm assumes that the input
 * image has been formed by a linear shift-invariant system with a
 * known kernel.
 *
 * The Richardson-Lucy algorithm assumes that noise in the image
 * follows a Poisson distribution and that the distribution for each
 * pixel is independent of the other pixels.
 *
 * This code was adapted from the Insight Journal contribution:
 *
 * "Deconvolution: infrastructure and reference algorithms"
 * by Gaetan Lehmann
 * https://www.insight-journal.org/browse/publication/753
 *
 * \author Gaetan Lehmann, Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France
 * \author Cory Quammen, The University of North Carolina at Chapel Hill
 *
 * \ingroup ITKDeconvolution
 * \sa IterativeDeconvolutionImageFilter
 * \sa LandweberDeconvolutionImageFilter
 * \sa ProjectedLandweberDeconvolutionImageFilter
 *
 */
template <typename TInputImage,
          typename TKernelImage = TInputImage,
          typename TOutputImage = TInputImage,
          typename TInternalPrecision = double>
class ITK_TEMPLATE_EXPORT RichardsonLucyDeconvolutionImageFilter
  : public IterativeDeconvolutionImageFilter<TInputImage, TKernelImage, TOutputImage, TInternalPrecision>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RichardsonLucyDeconvolutionImageFilter);

  /** Standard type alias. */
  using Self = RichardsonLucyDeconvolutionImageFilter;
  using Superclass = IterativeDeconvolutionImageFilter<TInputImage, TKernelImage, TOutputImage, TInternalPrecision>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Other useful type alias. */
  using InputImageType = TInputImage;
  using KernelImageType = TKernelImage;
  using OutputImageType = TOutputImage;

  /** Internal types used by the FFT filters. */
  using InternalImageType = typename Superclass::InternalImageType;
  using InternalImagePointerType = typename Superclass::InternalImagePointerType;
  using InternalComplexType = typename Superclass::InternalComplexType;
  using InternalComplexImageType = typename Superclass::InternalComplexImageType;
  using InternalComplexImagePointerType = typename Superclass::InternalComplexImagePointerType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(RichardsonLucyDeconvolutionImageFilter, IterativeDeconvolutionImageFilter);

protected:
  RichardsonLucyDeconvolutionImageFilter();
  ~RichardsonLucyDeconvolutionImageFilter() override;

  void
  Initialize(ProgressAccumulator * progress, float progressWeight, float iterationProgressWeight) override;

  void
  Iteration(ProgressAccumulator * progress, float iterationProgressWeight) override;

  void
  Finish(ProgressAccumulator * progress, float progressWeight) override;

  using FFTFilterType = typename Superclass::FFTFilterType;
  using IFFTFilterType = typename Superclass::IFFTFilterType;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Filters to compute each iterative update step. */
  using MultiplyFilterType = MultiplyImageFilter<InternalImageType>;
  using ComplexMultiplyType = MultiplyImageFilter<InternalComplexImageType>;
  using DivideFilterType = DivideOrZeroOutImageFilter<InternalImageType>;
  using ConjugateAdaptorType = ComplexConjugateImageAdaptor<InternalComplexImageType>;
  using ComplexConjugateMultiplyType =
    MultiplyImageFilter<InternalComplexImageType, ConjugateAdaptorType, InternalComplexImageType>;

  InternalImagePointerType m_PaddedInput;

  typename ComplexMultiplyType::Pointer          m_ComplexMultiplyFilter1;
  typename IFFTFilterType::Pointer               m_IFFTFilter1;
  typename DivideFilterType::Pointer             m_DivideFilter;
  typename FFTFilterType::Pointer                m_FFTFilter;
  typename ConjugateAdaptorType::Pointer         m_ConjugateAdaptor;
  typename ComplexConjugateMultiplyType::Pointer m_ComplexMultiplyFilter2;
  typename IFFTFilterType::Pointer               m_IFFTFilter2;
  typename MultiplyFilterType::Pointer           m_MultiplyFilter;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRichardsonLucyDeconvolutionImageFilter.hxx"
#endif

#endif
