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
#ifndef itkHalfHermitianToRealInverseFFTImageFilter_h
#define itkHalfHermitianToRealInverseFFTImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class HalfHermitianToRealInverseFFTImageFilter
 *
 * \brief Base class for specialized complex-to-real inverse Fast Fourier Transform.
 *
 * This is a base class for the "inverse" or "reverse" Discrete
 * Fourier Transform.  This is an abstract base class: the actual
 * implementation is provided by the best child class available on the
 * system when the object is created via the object factory system.
 *
 * The input to this filter is assumed to have the same format as the
 * output of the RealToHalfHermitianForwardFFTImageFilter. That is, the
 * input is assumed to consist of roughly half the full complex image
 * resulting from a real-to-complex discrete Fourier transform. This
 * half is expected to be the first half of the image in the
 * X-dimension. Because this filter assumes that the input stores only
 * about half of the non-redundant complex pixels, the output is
 * larger in the X-dimension than it is in the input. To determine the
 * actual size of the output image, this filter needs additional
 * information in the form of a flag indicating whether the output
 * image has an odd size in the X-dimension. Use
 * SetActualXDimensionIsOdd() to set this flag.
 *
 * \ingroup FourierTransform
 *
 * \sa ForwardFFTImageFilter, HalfHermitianToRealInverseFFTImageFilter
 * \ingroup ITKFFT
 */
template< typename TInputImage, typename TOutputImage=Image< typename TInputImage::PixelType::value_type, TInputImage::ImageDimension> >
class HalfHermitianToRealInverseFFTImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >

{
public:
  /** Standard class typedefs. */
  typedef TInputImage                          InputImageType;
  typedef typename InputImageType::PixelType   InputPixelType;
  typedef TOutputImage                         OutputImageType;
  typedef typename OutputImageType::PixelType  OutputPixelType;

  typedef HalfHermitianToRealInverseFFTImageFilter              Self;
  typedef ImageToImageFilter< InputImageType, OutputImageType > Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;

  itkStaticConstMacro(ImageDimension, unsigned int,
                      InputImageType::ImageDimension);

  /** Customized object creation methods that support configuration-based
  * selection of FFT implementation.
  *
  * Default implementation is VnlFFT. */
  static Pointer New();

  /** Was the original truncated dimension size odd? */
  void SetActualXDimensionIsOdd(bool isodd)
  {
    m_ActualXDimensionIsOdd = isodd;
  }
  void SetActualXDimensionIsOddOn()
  {
    this->SetActualXDimensionIsOdd(true);
  }
  void SetActualXDimensionIsOddOff()
  {
    this->SetActualXDimensionIsOdd(false);
  }
  bool GetActualXDimensionIsOdd()
  {
    return m_ActualXDimensionIsOdd;
  }

  /* Return the prefered greatest prime factor supported for the input image
   * size. Defaults to 2 as many implementations work only for sizes that are
   * power of 2.
   */
  virtual SizeValueType GetSizeGreatestPrimeFactor() const;

protected:
  HalfHermitianToRealInverseFFTImageFilter():m_ActualXDimensionIsOdd(false) {}
  virtual ~HalfHermitianToRealInverseFFTImageFilter(){}

  /** The output may be a different size from the input if complex conjugate
   * symmetry is implicit. */
  virtual void GenerateOutputInformation();

  /** This class requires the entire input. */
  virtual void GenerateInputRequestedRegion();

  /** Sets the output requested region to the largest possible output
   * region. */
  void EnlargeOutputRequestedRegion( DataObject *itkNotUsed(output) );

private:
  HalfHermitianToRealInverseFFTImageFilter(const Self &); // purposely not implemented
  void operator=(const Self &);        // purposely not implemented

  bool m_ActualXDimensionIsOdd;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#ifndef itkVnlHalfHermitianToRealInverseFFTImageFilter_h
#ifndef itkVnlHalfHermitianToRealInverseFFTImageFilter_hxx
#ifndef itkFFTWHalfHermitianToRealInverseFFTImageFilter_h
#ifndef itkFFTWHalfHermitianToRealInverseFFTImageFilter_hxx
#include "itkHalfHermitianToRealInverseFFTImageFilter.hxx"
#endif
#endif
#endif
#endif
#endif

#endif
