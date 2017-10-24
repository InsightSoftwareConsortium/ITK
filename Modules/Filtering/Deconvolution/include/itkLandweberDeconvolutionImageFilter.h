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
#ifndef itkLandweberDeconvolutionImageFilter_h
#define itkLandweberDeconvolutionImageFilter_h

#include "itkIterativeDeconvolutionImageFilter.h"

#include "itkComplexConjugateImageAdaptor.h"
#include "itkTernaryFunctorImageFilter.h"

namespace itk
{
namespace Functor
{
/** \class LandweberMethod
 * \brief Functor class for computing a Landweber iteration.
 * \ingroup ITKDeconvolution
 */
template< typename TInput1, typename TInput2, typename TInput3, typename TOutput >
class ITK_TEMPLATE_EXPORT LandweberMethod
{
public:
  LandweberMethod() {}
  ~LandweberMethod() {}

  bool operator!=(const LandweberMethod &) const
  {
    return false;
  }

  bool operator==(const LandweberMethod & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput1 & estimateFT,
                            const TInput2 & kernelFT,
                            const TInput2 & inputFT) const
  {
    return m_Alpha * std::conj( kernelFT ) * inputFT +
      ( NumericTraits< typename TInput1::value_type >::OneValue() - m_Alpha * std::norm( kernelFT ) ) * estimateFT;
  }

  typename TInput1::value_type m_Alpha;

};
} // end namespace Functor

/** \class LandweberDeconvolutionImageFilter
 * \brief Deconvolve an image using the Landweber deconvolution
 * algorithm.
 *
 * This filter implements the Landweber deconvolution algorthm as
 * defined in Bertero M and Boccacci P, "Introduction to Inverse
 * Problems in Imaging", 1998. The algorithm assumes that the input
 * image has been formed by a linear shift-invariant system with a
 * known kernel.
 *
 * The Landweber algorithm converges to a solution that minimizes the
 * sum of squared errors \f$||f \otimes h - g||\f$ where \f$f\f$ is
 * the estimate of the unblurred image, \f$\otimes\f$ is the
 * convolution operator, \f$h\f$ is the blurring kernel, and \f$g\f$ is
 * the blurred input image. As such, it is best suited for images that
 * have zero-mean Gaussian white noise.
 *
 * This is the base implementation of the Landweber algorithm. It may
 * produce results with negative values. For a version of this
 * algorithm that enforces a positivity constraint on each
 * intermediate solution, see ProjectedLandweberDeconvolutionImageFilter.
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
 * \sa RichardsonLucyDeconvolutionImageFilter
 * \sa ProjectedLandweberDeconvolutionImageFilter
 */
template< typename TInputImage, typename TKernelImage=TInputImage, typename TOutputImage=TInputImage, typename TInternalPrecision=double >
class ITK_TEMPLATE_EXPORT LandweberDeconvolutionImageFilter :
  public IterativeDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
{
public:
  /** Standard typedefs. */
  typedef LandweberDeconvolutionImageFilter                       Self;
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
  itkTypeMacro(LandweberDeconvolutionImageFilter,
               IterativeDeconvolutionImageFilter);

  /** Set/get relaxation factor. */
  itkSetMacro(Alpha, double);
  itkGetMacro(Alpha, double);

protected:
  LandweberDeconvolutionImageFilter();
  virtual ~LandweberDeconvolutionImageFilter() ITK_OVERRIDE;

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
  ITK_DISALLOW_COPY_AND_ASSIGN(LandweberDeconvolutionImageFilter);

  double m_Alpha;

  InternalComplexImagePointerType m_TransformedInput;

  typedef Functor::LandweberMethod< InternalComplexType,
                                    InternalComplexType,
                                    InternalComplexType,
                                    InternalComplexType > LandweberFunctor;
  typedef TernaryFunctorImageFilter< InternalComplexImageType,
                                     InternalComplexImageType,
                                     InternalComplexImageType,
                                     InternalComplexImageType,
                                     LandweberFunctor >   LandweberFilterType;

  typename LandweberFilterType::Pointer  m_LandweberFilter;
  typename IFFTFilterType::Pointer       m_IFFTFilter;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLandweberDeconvolutionImageFilter.hxx"
#endif

#endif
