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
#ifndef itkParametricBlindLeastSquaresDeconvolutionImageFilter_h
#define itkParametricBlindLeastSquaresDeconvolutionImageFilter_h

#include "itkIterativeDeconvolutionImageFilter.h"

#include "itkBinaryFunctorImageFilter.h"
#include "itkParametricImageSource.h"
#include "itkTernaryFunctorImageFilter.h"

namespace itk
{
/** \class ParametricBlindLeastSquaresDeconvolutionImageFilter
 *
 * \brief Least-squares blind deconvolution filter that also estimates
 * the parameters of a user-supplied parametric point-spread function.
 *
 * This filter takes a parametric kernel image source instead of a
 * static kernel image. During the deconvolution iterations, a new
 * estimate of the restored image is produced, along with a new
 * estimate of the kernel parameters. The parameters are available
 * through the kernel image sources GetParameters() method after the
 * filter has executed.
 *
 * Both the image estimate and the kernel parameter estimate are
 * produced through gradient descent on a sum-of-squared differences
 * objective function, making this method suitable for zero-mean
 * Gaussian white noise.
 *
 * This filter produces output in two forms: a deconvolved image and the
 * parameters of the input kernel image source.
 *
 * \warning The method SetKernelImage(), inherited from the superclass
 * of this filter, is publicly available. However, this algorithm does
 * not use the static kernel image set through this method. Instead,
 * it uses the output of the parametric kernel source you specify.
 *
 * \author Cory Quammen, The University of North Carolina at Chapel Hill
 *
 * \ingroup ITKDeconvolution
 */
template< typename TInputImage, typename TKernelSource, typename TOutputImage=TInputImage >
class ITK_TEMPLATE_EXPORT ParametricBlindLeastSquaresDeconvolutionImageFilter :
    public IterativeDeconvolutionImageFilter< TInputImage, typename TKernelSource::OutputImageType, TOutputImage >
{
public:
  /** Standard typedefs. */
  typedef ParametricBlindLeastSquaresDeconvolutionImageFilter Self;
  typedef IterativeDeconvolutionImageFilter< TInputImage,
                                             typename TKernelSource::OutputImageType,
                                             TOutputImage >   Superclass;
  typedef SmartPointer< Self >                                Pointer;
  typedef SmartPointer< const Self >                          ConstPointer;

  /** Other useful typedefs. */
  typedef TInputImage   InputImageType;
  typedef TOutputImage  OutputImageType;

  /** Internal types used by the FFT filters. */
  typedef typename Superclass::InternalImageType               InternalImageType;
  typedef typename Superclass::InternalImagePointerType        InternalImagePointerType;
  typedef typename Superclass::InternalComplexType             InternalComplexType;
  typedef typename Superclass::InternalComplexImageType        InternalComplexImageType;
  typedef typename Superclass::InternalComplexImagePointerType InternalComplexImagePointerType;

  /** Type for the parametric kernel source. */
  typedef TKernelSource                      KernelSourceType;
  typedef typename KernelSourceType::Pointer KernelSourcePointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ParametricBlindLeastSquaresDeconvolutionImageFilter,
               IterativeDeconvolutionImageFilter);

  /** Set/get the parametric kernel source. */
  void SetKernelSource(KernelSourceType * kernelSource);
  itkGetModifiableObjectMacro(KernelSource, KernelSourceType);

  /** Set/get the scale factor (also known as learning rate) for the
   * image intensity gradient descent. */
  itkSetMacro(Alpha, double);
  itkGetMacro(Alpha, double);

  /** Set/get the scale factor (also known as learning rate) for the
   * parameter gradient descent. */
  itkSetMacro(Beta, double);
  itkGetMacro(Beta, double);

protected:
  ParametricBlindLeastSquaresDeconvolutionImageFilter();
  virtual ~ParametricBlindLeastSquaresDeconvolutionImageFilter() ITK_OVERRIDE;

  virtual void Initialize(ProgressAccumulator * progress,
                          float progressWeight,
                          float iterationProgressWeight) ITK_OVERRIDE;

  virtual void Iteration(ProgressAccumulator * progress,
                         float iterationProgressWeight) ITK_OVERRIDE;

  virtual void Finish(ProgressAccumulator *progress, float progressWeight) ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ParametricBlindLeastSquaresDeconvolutionImageFilter);

  template< typename TPixel >
    class ITK_TEMPLATE_EXPORT ParametricBlindLeastSquaresDeconvolutionDifference
  {
  public:
    ParametricBlindLeastSquaresDeconvolutionDifference() {}
    ~ParametricBlindLeastSquaresDeconvolutionDifference() {}

    bool operator!=(const ParametricBlindLeastSquaresDeconvolutionDifference &) const
    {
      return false;
    }

    bool operator==(const ParametricBlindLeastSquaresDeconvolutionDifference & other) const
    {
      return !( *this != other );
    }

    inline TPixel operator()(const TPixel & estimateFT,
                             const TPixel & kernelEstimateFT,
                             const TPixel & inputFT)
    {
      return estimateFT * kernelEstimateFT - inputFT;
    }
  };

  template< typename TPixel >
    class ITK_TEMPLATE_EXPORT ParametricBlindLeastSquaresDeconvolutionImageUpdate
  {
  public:
    ParametricBlindLeastSquaresDeconvolutionImageUpdate() : m_Alpha(0.01) {}
    ~ParametricBlindLeastSquaresDeconvolutionImageUpdate() {}

    bool operator!=(const ParametricBlindLeastSquaresDeconvolutionImageUpdate &) const
    {
      return false;
    }

    bool operator==(const ParametricBlindLeastSquaresDeconvolutionImageUpdate & other) const
    {
      return !( *this != other );
    }

    inline TPixel operator()(const TPixel & estimateFT,
                             const TPixel & differenceFT,
                             const TPixel & kernelFT)
    {
      // Because of the linearity of the Fourier transform, we can
      // perform the update step in the Fourier domain
      return estimateFT - m_Alpha * ( differenceFT * std::conj( kernelFT ) );
    }

    void SetAlpha(double alpha)
    {
      m_Alpha = alpha;
    }
    double GetAlpha() const
    {
      return m_Alpha;
    }

  private:
    double m_Alpha;
  };

  KernelSourcePointer             m_KernelSource;

  /** Step sizes for the gradient descent of the image and the
   * kernel parameters. These are very different spaces, so they
   * deserve different step size parameters. */
  double m_Alpha;
  double m_Beta;

  /** Temporary images. */
  InternalComplexImagePointerType m_TransformedInput;
  InternalComplexImagePointerType m_TransformedCurrentEstimate;

  /** These are the internal filters that perform the updating of the
   * image estimate. */
  typedef ParametricBlindLeastSquaresDeconvolutionDifference< InternalComplexType >
    DifferenceFunctorType;
  typedef TernaryFunctorImageFilter< InternalComplexImageType,
    InternalComplexImageType,
    InternalComplexImageType,
    InternalComplexImageType,
    DifferenceFunctorType >
    DifferenceFilterType;
  typename DifferenceFilterType::Pointer m_DifferenceFilter;

  typedef ParametricBlindLeastSquaresDeconvolutionImageUpdate< InternalComplexType >
    ImageUpdateFunctorType;
  typedef TernaryFunctorImageFilter< InternalComplexImageType,
    InternalComplexImageType,
    InternalComplexImageType,
    InternalComplexImageType,
    ImageUpdateFunctorType >
    ImageUpdateFilterType;
  typename ImageUpdateFilterType::Pointer m_ImageUpdateFilter;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkParametricBlindLeastSquaresDeconvolutionImageFilter.hxx"
#endif


#endif
