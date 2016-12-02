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
#ifndef itkFFTWRealToHalfHermitianForwardFFTImageFilter_h
#define itkFFTWRealToHalfHermitianForwardFFTImageFilter_h

#include "itkRealToHalfHermitianForwardFFTImageFilter.h"

#include "itkFFTWCommon.h"

namespace itk
{
/** \class FFTWRealToHalfHermitianForwardFFTImageFilter
 *
 * \brief FFTW-based forward Fast Fourier Transform.
 *
 * This filter computes the forward Fourier transform of an image. The
 * implementation is based on the FFTW library.
 *
 * This filter is multithreaded and supports input images of any size.
 *
 * In order to use this class, ITK_USE_FFTWF must be set to ON in the CMake
 * configuration to support float images, and ITK_USE_FFTWD must set to ON to
 * support double images.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/10380/3154
 * or http://insight-journal.com/browse/publication/717
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup FourierTransform
 * \ingroup MultiThreaded
 * \ingroup ITKFFT
 *
 * \sa FFTWGlobalConfiguration
 * \sa RealToHalfHermitianForwardFFTImageFilter
 */
template< typename TInputImage, typename TOutputImage=Image< std::complex<typename TInputImage::PixelType>, TInputImage::ImageDimension> >
class ITK_TEMPLATE_EXPORT FFTWRealToHalfHermitianForwardFFTImageFilter:
  public RealToHalfHermitianForwardFFTImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef TInputImage                          InputImageType;
  typedef typename InputImageType::PixelType   InputPixelType;
  typedef typename InputImageType::SizeType    InputSizeType;
  typedef TOutputImage                         OutputImageType;
  typedef typename OutputImageType::PixelType  OutputPixelType;
  typedef typename OutputImageType::SizeType   OutputSizeType;

  typedef FFTWRealToHalfHermitianForwardFFTImageFilter                          Self;
  typedef RealToHalfHermitianForwardFFTImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                                Pointer;
  typedef SmartPointer< const Self >                                          ConstPointer;

  /** The proxy type is a wrapper for the FFTW API. Because the proxy
   * is defined only for double and float, trying to use any other
   * pixel type is unsupported, as is trying to use double if only the
   * float FFTW version is configured in, or float if only double is
   * configured. */
  typedef typename fftw::Proxy< InputPixelType > FFTWProxyType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTWRealToHalfHermitianForwardFFTImageFilter,
               RealToHalfHermitianForwardFFTImageFilter);

  /** Define the image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, InputImageType::ImageDimension);

  /** Set/Get the behavior of wisdom plan creation. The default is
   * provided by FFTWGlobalConfiguration::GetPlanRigor().
   *
   * The parameter is one of the FFTW planner rigor flags FFTW_ESTIMATE, FFTW_MEASURE,
   * FFTW_PATIENT, FFTW_EXHAUSTIVE provided by FFTWGlobalConfiguration.
   *
   * /sa FFTWGlobalConfiguration
   */
  virtual void SetPlanRigor( const int & value )
  {
    // Use that method to check the value
    FFTWGlobalConfiguration::GetPlanRigorName( value );
    if( m_PlanRigor != value )
      {
      m_PlanRigor = value;
      this->Modified();
      }
  }
  itkGetConstReferenceMacro( PlanRigor, int );

  SizeValueType GetSizeGreatestPrimeFactor() const ITK_OVERRIDE;

protected:
  FFTWRealToHalfHermitianForwardFFTImageFilter();
  ~FFTWRealToHalfHermitianForwardFFTImageFilter() {}

  virtual void GenerateData() ITK_OVERRIDE;

  virtual void UpdateOutputData(DataObject *output) ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FFTWRealToHalfHermitianForwardFFTImageFilter);

  bool m_CanUseDestructiveAlgorithm;

  int m_PlanRigor;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFFTWRealToHalfHermitianForwardFFTImageFilter.hxx"
#endif

#endif //itkFFTWRealToHalfHermitianForwardFFTImageFilter_h
