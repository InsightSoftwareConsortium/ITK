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
#ifndef itkWienerDeconvolutionImageFilter_h
#define itkWienerDeconvolutionImageFilter_h

#include "itkInverseDeconvolutionImageFilter.h"

namespace itk
{
/** \class WienerDeconvolutionImageFilter
 * \brief The Wiener deconvolution image filter is designed to restore an
 * image convolved with a blurring kernel while keeping noise
 * enhancement to a minimum.
 *
 * The Wiener filter aims to minimize noise enhancement induced by
 * frequencies with low signal-to-noise ratio. The Wiener filter
 * kernel is defined in the frequency domain as \f$W(\omega) =
 * H^*(\omega) / (|H(\omega)|^2 + (1 / SNR(\omega)))\f$ where
 * \f$H(\omega)\f$ is the Fourier transform of the blurring kernel
 * with which the original image was convolved and the signal-to-noise
 * ratio \f$SNR(\omega)\f$. \f$SNR(\omega)\f$ is defined by
 * \f$P_f(\omega) / P_n(\omega)\f$ where \f$P_f(\omega)\f$ is the
 * power spectral density of the uncorrupted signal and
 * \f$P_n(\omega)\f$ is the power spectral density of the noise. When
 * applied to the input blurred image, this filter produces an
 * estimate \f$\hat{f}(x)\f$ of the true underlying signal \f$f(x)\f$
 * that minimizes the expected error between \f$\hat{f}(x)\f$ and
 * \f$f(x)\f$.
 *
 * This filter requires two inputs, the image to be deconvolved and
 * the blurring kernel. These two inputs can be set using the methods
 * SetInput() and SetKernelImage(), respectively.
 *
 * The power spectral densities of the signal and noise are typically
 * unavailable for a given problem. In particular, \f$P_f(\omega)\f$
 * cannot be computed from \f$f(x)\f$ because this unknown signal is
 * precisely the signal that this filter aims to
 * recover. Nevertheless, it is common for the noise to have a power
 * spectral density that is flat or decreasing significantly more
 * slowly than the power spectral density of a typical image as the
 * frequency \f$\omega\f$ increases. Hence, \f$P_n(\omega)\f$ can
 * typically be approximated with a constant, and this filter makes
 * this assumption (see the NoiseVariance member
 * variable). \f$P_f(\omega)\f$, on the other hand, will vary with
 * input. This filter computes the power spectral density of the input
 * blurred image, subtracts the power spectral density of the noise,
 * and uses the result as the estimate of \f$P_f(\omega)\f$.
 *
 * For further information on the Wiener deconvolution filter, please see
 * "Digital Signal Processing" by Kenneth R. Castleman, Prentice Hall, 1995
 *
 * \author Gaetan Lehmann, Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France
 * \author Chris Mullins, The University of North Carolina at Chapel Hill
 * \author Cory Quammen, The University of North Carolina at Chapel Hill
 *
 * \ingroup ITKDeconvolution
 *
 */
template< typename TInputImage, typename TKernelImage = TInputImage, typename TOutputImage = TInputImage, typename TInternalPrecision=double >
class ITK_TEMPLATE_EXPORT WienerDeconvolutionImageFilter :
  public InverseDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
{
public:
  typedef WienerDeconvolutionImageFilter                         Self;
  typedef InverseDeconvolutionImageFilter< TInputImage,
                                           TKernelImage,
                                           TOutputImage,
                                           TInternalPrecision >  Superclass;
  typedef SmartPointer< Self >                                   Pointer;
  typedef SmartPointer< const Self >                             ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information ( and related methods ) */
  itkTypeMacro(WienerDeconvolutionImageFilter, InverseDeconvolutionImageFilter);

  /** Dimensionality of input and output data is assumed to be the same. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  typedef TInputImage                           InputImageType;
  typedef TOutputImage                          OutputImageType;
  typedef TKernelImage                          KernelImageType;
  typedef typename Superclass::InputPixelType   InputPixelType;
  typedef typename Superclass::OutputPixelType  OutputPixelType;
  typedef typename Superclass::KernelPixelType  KernelPixelType;
  typedef typename Superclass::InputIndexType   InputIndexType;
  typedef typename Superclass::OutputIndexType  OutputIndexType;
  typedef typename Superclass::KernelIndexType  KernelIndexType;
  typedef typename Superclass::InputSizeType    InputSizeType;
  typedef typename Superclass::OutputSizeType   OutputSizeType;
  typedef typename Superclass::KernelSizeType   KernelSizeType;
  typedef typename Superclass::SizeValueType    SizeValueType;
  typedef typename Superclass::InputRegionType  InputRegionType;
  typedef typename Superclass::OutputRegionType OutputRegionType;
  typedef typename Superclass::KernelRegionType KernelRegionType;

  /** Internal image types. */
  typedef typename Superclass::InternalImageType               InternalImageType;
  typedef typename Superclass::InternalImagePointerType        InternalImagePointerType;
  typedef typename Superclass::InternalComplexType             InternalComplexType;
  typedef typename Superclass::InternalComplexImageType        InternalComplexImageType;
  typedef typename Superclass::InternalComplexImagePointerType InternalComplexImagePointerType;

  /** Set/get the variance of the zero-mean Gaussian white noise
   * assumed to be added to the input. */
  itkSetMacro(NoiseVariance, double);
  itkGetConstMacro(NoiseVariance, double);

protected:
  WienerDeconvolutionImageFilter();
  ~WienerDeconvolutionImageFilter() ITK_OVERRIDE {}

  /** This filter uses a minipipeline to compute the output. */
  void GenerateData() ITK_OVERRIDE;

  virtual void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(WienerDeconvolutionImageFilter);

  double m_NoiseVariance;
};

namespace Functor
{
template< typename TPixel >
class ITK_TEMPLATE_EXPORT WienerDeconvolutionFunctor
{
public:
  WienerDeconvolutionFunctor() { m_KernelZeroMagnitudeThreshold = 0.0; }
  ~WienerDeconvolutionFunctor() {}

  bool operator!=( const WienerDeconvolutionFunctor & ) const
  {
    return false;
  }
  bool operator==( const WienerDeconvolutionFunctor & other) const
  {
    return !(*this != other);
  }
  inline TPixel operator()(const TPixel & I, const TPixel & H) const
  {
    TPixel Pn = m_NoisePowerSpectralDensityConstant;

    // We estimate the power spectral density of the output image to
    // be the same as the power spectral density of the blurred input
    // minus the power spectral density of the noise.
    TPixel Pf = std::norm( I );

    TPixel denominator = std::norm( H ) + ( Pn / (Pf - Pn) );
    TPixel value = NumericTraits< TPixel >::ZeroValue();
    if ( std::abs( denominator ) >= m_KernelZeroMagnitudeThreshold )
      {
      value = I * ( std::conj( H ) / denominator );
      }

    return value;
  }

  /** Set/get the constant defining the noise power spectral density
  * constant. */
  void SetNoisePowerSpectralDensityConstant(double constant)
  {
    m_NoisePowerSpectralDensityConstant = constant;
  }
  double GetNoisePowerSpectralDensityConstant() const
  {
    return m_NoisePowerSpectralDensityConstant;
  }

  /** Set/get the threshold value below which complex magnitudes are considered
   * to be zero. */
  void SetKernelZeroMagnitudeThreshold(double mu)
  {
    m_KernelZeroMagnitudeThreshold = mu;
  }
  double GetKernelZeroMagnitudeThreshold() const
  {
    return m_KernelZeroMagnitudeThreshold;
  }

private:
  double m_NoisePowerSpectralDensityConstant;
  double m_KernelZeroMagnitudeThreshold;
};
} //namespace Functor

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWienerDeconvolutionImageFilter.hxx"
#endif

#endif
