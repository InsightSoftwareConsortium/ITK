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
#ifndef __itkVnlFFTComplexConjugateToRealImageFilter_h
#define __itkVnlFFTComplexConjugateToRealImageFilter_h

#include "itkFFTComplexConjugateToRealImageFilter.h"

#include "itkImage.h"

namespace itk
{
/** \class VnlFFTComplexConjugateToRealImageFilter
 *
 * \brief VNL based reverse Fast Fourier Transform.
 *
 * The input image size must be a multiple of combinations of 2s, 3s,
 * and/or 5s in all dimensions.
 *
 * \ingroup FourierTransform
 *
 * \sa FFTComplexConjugateToRealImageFilter
 * \ingroup ITKFFT
 *
 * \wiki
 * \wikiexample{SpectralAnalysis/CrossCorrelationInFourierDomain,Compute the cross-correlation of two images in the Fourier domain}
 * \endwiki
 */
template< class TInputImage, class TOutputImage=Image< typename TInputImage::PixelType::value_type, TInputImage::ImageDimension> >
class VnlFFTComplexConjugateToRealImageFilter:
  public FFTComplexConjugateToRealImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef TInputImage                            InputImageType;
  typedef typename InputImageType::PixelType     InputPixelType;
  typedef typename InputImageType::SizeType      InputSizeType;
  typedef typename InputImageType::SizeValueType InputSizeValueType;
  typedef TOutputImage                           OutputImageType;
  typedef typename OutputImageType::PixelType    OutputPixelType;
  typedef typename OutputImageType::SizeType     OutputSizeType;

  typedef VnlFFTComplexConjugateToRealImageFilter                           Self;
  typedef FFTComplexConjugateToRealImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                              Pointer;
  typedef SmartPointer< const Self >                                        ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VnlFFTComplexConjugateToRealImageFilter,
               FFTComplexConjugateToRealImageFilter);

  /** Extract the dimensionality of the images. They must be the
   * same. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** These should be defined in every FFT filter class. */
  virtual void GenerateData();  // generates output from input

  virtual bool FullMatrix();

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( PixelUnsignedIntDivisionOperatorsCheck,
                   ( Concept::DivisionOperators< OutputPixelType, unsigned int > ) );
  itkConceptMacro( ImageDimensionsMatchCheck,
                   ( Concept::SameDimension< InputImageDimension, OutputImageDimension > ) );
  /** End concept checking */
#endif

protected:
  VnlFFTComplexConjugateToRealImageFilter()  {}
  virtual ~VnlFFTComplexConjugateToRealImageFilter(){}

  /** Method to check if an array dimension is legal for the prime
   * factor FFT algorithm. */
  bool IsDimensionSizeLegal(InputSizeValueType n);

private:
  // compile time choice of fft solver instead of runtime
  template <unsigned VDim> struct DimDiscriminator { };
  typedef vnl_vector< InputPixelType  > SignalVectorType;
  /** call proper vnl_fft transform function */
  void FFTND_transform(SignalVectorType &signal, const OutputSizeType &inputSize, DimDiscriminator<1> *);
  void FFTND_transform(SignalVectorType &signal, const OutputSizeType &inputSize, DimDiscriminator<2> *);
  void FFTND_transform(SignalVectorType &signal, const OutputSizeType &inputSize, DimDiscriminator<3> *);

  VnlFFTComplexConjugateToRealImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);                          //purposely not implemented
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVnlFFTComplexConjugateToRealImageFilter.hxx"
#endif

#endif
