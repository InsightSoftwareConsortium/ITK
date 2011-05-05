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
 * Input image must be a power of two in all directions.
 *
 * \ingroup FourierTransform
 *
 * \sa ConstantPadImageFilter
 * \ingroup ITK-FFT
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
  typedef TInputImage                          InputImageType;
  typedef typename InputImageType::PixelType   InputPixelType;
  typedef TOutputImage                         OutputImageType;
  typedef typename OutputImageType::PixelType  OutputPixelType;

  typedef VnlFFTComplexConjugateToRealImageFilter                           Self;
  typedef FFTComplexConjugateToRealImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                              Pointer;
  typedef SmartPointer< const Self >                                        ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VnlFFTComplexConjugateToRealImageFilter,
               FFTComplexConjugateToRealImageFilter);

  /** Image type typedef support. */
  typedef typename InputImageType::SizeType ImageSizeType;

  //
  // these should be defined in every FFT filter class
  virtual void GenerateData();  // generates output from input

  virtual bool FullMatrix();

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( PixelUnsignedIntDivisionOperatorsCheck,
                   ( Concept::DivisionOperators< OutputPixelType, unsigned int > ) );
  /** End concept checking */
#endif
protected:
  VnlFFTComplexConjugateToRealImageFilter()  {}
  virtual ~VnlFFTComplexConjugateToRealImageFilter(){}
private:

  VnlFFTComplexConjugateToRealImageFilter(const Self &); //purposely not
                                                         // implemented
  void operator=(const Self &);                          //purposely not
                                                         // implemented
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVnlFFTComplexConjugateToRealImageFilter.txx"
#endif

#endif
