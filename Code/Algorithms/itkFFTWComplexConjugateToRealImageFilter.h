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
#ifndef __itkFFTWComplexConjugateToRealImageFilter_h
#define __itkFFTWComplexConjugateToRealImageFilter_h

#include "itkFFTComplexConjugateToRealImageFilter.h"
#include "itkFFTWCommon.h"


namespace itk
{
/** \class FFTWComplexConjugateToRealImageFilter
 *
 * \brief FFTW based reverse Fast Fourier Transform
 *
 * This filter computes the reverse Fourier transform of an image. The implementation is
 * based on the FFTW library.
 * This filter is multithreaded and supports input images with sizes which are not
 * a power of two.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/10380/3154
 * or http://insight-journal.com/browse/publication/717
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup FourierTransform, Multithreaded
 * \sa FFTWLock
 */
template< typename TPixel, unsigned int VDimension >
class ITK_EXPORT FFTWComplexConjugateToRealImageFilter:
  public FFTComplexConjugateToRealImageFilter< TPixel, VDimension >
{
public:
  typedef FFTWComplexConjugateToRealImageFilter                      Self;
  typedef FFTComplexConjugateToRealImageFilter< TPixel, VDimension > Superclass;
  typedef SmartPointer< Self >                                       Pointer;
  typedef SmartPointer< const Self >                                 ConstPointer;
  //
  // the proxy type is a wrapper for the fftw API
  // since the proxy is only defined over double and float,
  // trying to use any other pixel type is inoperative, as
  // is trying to use double if only the float FFTW version is
  // configured in, or float if only double is configured.
  //
  typedef typename fftw::Proxy< TPixel > FFTWProxyType;

  /** Standard class typedefs. */
  typedef typename Superclass::TInputImageType  TInputImageType;
  typedef typename Superclass::TOutputImageType TOutputImageType;
  typedef typename TOutputImageType::RegionType OutputImageRegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTWComplexConjugateToRealImageFilter,
               FFTComplexConjugateToRealImageFilter);

  /** Image type typedef support. */
  typedef TInputImageType              ImageType;
  typedef typename ImageType::SizeType ImageSizeType;

  //
  // these should be defined in every FFT filter class
  virtual bool FullMatrix();
protected:
  FFTWComplexConjugateToRealImageFilter()
    {
    }
  virtual ~FFTWComplexConjugateToRealImageFilter()
    {
    }

  virtual void UpdateOutputData(DataObject *output);

  virtual void BeforeThreadedGenerateData();
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread, int threadId );

private:
  FFTWComplexConjugateToRealImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  bool m_CanUseDestructiveAlgorithm;

};


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFFTWComplexConjugateToRealImageFilter.txx"
#endif

#endif //__itkFFTWComplexConjugateToRealImageFilter_h
