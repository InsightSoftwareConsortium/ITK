/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
/** \class RichardsonLucyDeconvolutionImageFilter
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
 * https://hdl.handle.net/10380/3207
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
template< typename TInputImage, typename TKernelImage=TInputImage, typename TOutputImage=TInputImage, typename TInternalPrecision=double >
class ITK_TEMPLATE_EXPORT RichardsonLucyDeconvolutionImageFilter :
    public IterativeDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
{
public:
  /** Standard typedefs. */
  typedef RichardsonLucyDeconvolutionImageFilter                  Self;
  typedef IterativeDeconvolutionImageFilter< TInputImage,
                                             TKernelImage,
                                             TOutputImage,
                                             TInternalPrecision > Superclass;
  typedef SmartPointer< Self >                                    Pointer;
  typedef SmartPointer< const Self >                              ConstPointer;

  /** Other useful typedefs. */
  typedef TInputImage  InputImageType;
  typedef TKernelImage KernelImageType;
  typedef TOutputImage OutputImageType;

  /** Internal types used by the FFT filters. */
  typedef typename Superclass::InternalImageType               InternalImageType;
  typedef typename Superclass::InternalImagePointerType        InternalImagePointerType;
  typedef typename Superclass::InternalComplexType             InternalComplexType;
  typedef typename Superclass::InternalComplexImageType        InternalComplexImageType;
  typedef typename Superclass::InternalComplexImagePointerType InternalComplexImagePointerType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(RichardsonLucyDeconvolutionImageFilter,
               IterativeDeconvolutionImageFilter);

protected:
  RichardsonLucyDeconvolutionImageFilter();
  virtual ~RichardsonLucyDeconvolutionImageFilter() ITK_OVERRIDE;

  virtual void Initialize(ProgressAccumulator * progress,
                          float progressWeight,
                          float iterationProgressWeight) ITK_OVERRIDE;

  virtual void Iteration(ProgressAccumulator * progress,
                         float iterationProgressWeight) ITK_OVERRIDE;

  virtual void Finish(ProgressAccumulator *progress, float progressWeight) ITK_OVERRIDE;

  typedef typename Superclass::FFTFilterType  FFTFilterType;
  typedef typename Superclass::IFFTFilterType IFFTFilterType;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RichardsonLucyDeconvolutionImageFilter);

  /** Filters to compute each iterative update step. */
  typedef MultiplyImageFilter< InternalImageType >                 MultiplyFilterType;
  typedef MultiplyImageFilter< InternalComplexImageType >          ComplexMultiplyType;
  typedef DivideOrZeroOutImageFilter< InternalImageType >          DivideFilterType;
  typedef ComplexConjugateImageAdaptor< InternalComplexImageType > ConjugateAdaptorType;
  typedef MultiplyImageFilter< InternalComplexImageType,
                               ConjugateAdaptorType,
                               InternalComplexImageType >          ComplexConjugateMultiplyType;

  InternalImagePointerType m_PaddedInput;

  typename ComplexMultiplyType::Pointer           m_ComplexMultiplyFilter1;
  typename IFFTFilterType::Pointer                m_IFFTFilter1;
  typename DivideFilterType::Pointer              m_DivideFilter;
  typename FFTFilterType::Pointer                 m_FFTFilter;
  typename ConjugateAdaptorType::Pointer          m_ConjugateAdaptor;
  typename ComplexConjugateMultiplyType::Pointer  m_ComplexMultiplyFilter2;
  typename IFFTFilterType::Pointer                m_IFFTFilter2;
  typename MultiplyFilterType::Pointer            m_MultiplyFilter;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRichardsonLucyDeconvolutionImageFilter.hxx"
#endif

#endif
