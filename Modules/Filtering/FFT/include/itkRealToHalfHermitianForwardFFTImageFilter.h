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
#ifndef itkRealToHalfHermitianForwardFFTImageFilter_h
#define itkRealToHalfHermitianForwardFFTImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
/** \class RealToHalfHermitianForwardFFTImageFilter
 *
 * \brief Base class for specialized real-to-complex forward Fast Fourier Transform.
 *
 * This is a base class for the "forward" or "direct" discrete Fourier
 * Transform.  This is an abstract base class: the actual implementation is
 * provided by the best child class available on the system when the object is
 * created via the object factory system.
 *
 * This class transforms a real input image into its complex Fourier
 * transform.  The Fourier transform of a real input image has
 * Hermitian symmetry: \f$ f(\mathbf{x}) = f^*(-\mathbf{x}) \f$. That
 * is, when the result of the transform is split in half along the
 * X-dimension, the values in the second half of the transform are the
 * complex conjugates of values in the first half reflected about the
 * center of the image in each dimension. This filter takes advantage
 * of the Hermitian symmetry property and reduces the size of the
 * output in the first dimension to N/2+1, where N is the size of the
 * input image in that dimension and the division by 2 is rounded
 * down.
 *
 * \ingroup FourierTransform
 *
 * \sa HalfHermitianToRealInverseFFTImageFilter
 * \sa ForwardFFTImageFilter
 * \ingroup ITKFFT
 */
template< typename TInputImage, typename TOutputImage=Image< std::complex<typename TInputImage::PixelType>, TInputImage::ImageDimension> >
class ITK_TEMPLATE_EXPORT RealToHalfHermitianForwardFFTImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef TInputImage                          InputImageType;
  typedef typename InputImageType::PixelType   InputPixelType;
  typedef typename InputImageType::IndexType   InputIndexType;
  typedef typename InputImageType::SizeType    InputSizeType;
  typedef TOutputImage                         OutputImageType;
  typedef typename OutputImageType::PixelType  OutputPixelType;
  typedef typename OutputImageType::IndexType  OutputIndexType;
  typedef typename OutputIndexType::SizeType   OutputSizeType;

  typedef RealToHalfHermitianForwardFFTImageFilter              Self;
  typedef ImageToImageFilter< InputImageType, OutputImageType > Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;

  /** Customized object creation methods that support configuration-based
    * selection of FFT implementation.
    *
    * Default implementation is VnlFFT. */
  static Pointer New();

  /* Return the prefered greatest prime factor supported for the input image
   * size. Defaults to 2 as many implementations work only for sizes that are
   * power of 2.
   */
  virtual SizeValueType GetSizeGreatestPrimeFactor() const;

  /** Get whether the actual X dimension of the image is odd or not in the full
   * representation */
  itkGetDecoratedOutputMacro(ActualXDimensionIsOdd, bool);

protected:
  RealToHalfHermitianForwardFFTImageFilter();
  virtual ~RealToHalfHermitianForwardFFTImageFilter() {}

  /** The output is a different size from the input because of
   * Hermitian symmetry. */
  virtual void GenerateOutputInformation();

  /** This class requires the entire input. */
  virtual void GenerateInputRequestedRegion();

  /** This class produces the entire output. */
  virtual void EnlargeOutputRequestedRegion(DataObject *output);

  itkSetDecoratedOutputMacro(ActualXDimensionIsOdd, bool);

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RealToHalfHermitianForwardFFTImageFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#ifndef itkVnlRealToHalfHermitianForwardFFTImageFilter_h
#ifndef itkVnlRealToHalfHermitianForwardFFTImageFilter_hxx
#ifndef itkFFTWRealToHalfHermitianForwardFFTImageFilter_h
#ifndef itkFFTWRealToHalfHermitianForwardFFTImageFilter_hxx
#include "itkRealToHalfHermitianForwardFFTImageFilter.hxx"
#endif
#endif
#endif
#endif
#endif

#endif
