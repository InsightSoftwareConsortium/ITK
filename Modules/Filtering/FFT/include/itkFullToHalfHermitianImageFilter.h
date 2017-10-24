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
#ifndef itkFullToHalfHermitianImageFilter_h
#define itkFullToHalfHermitianImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
/** \class FullToHalfHermitianImageFilter
 *
 * \brief Reduces the size of a full complex image produced from a
 * forward discrete Fourier transform of a real image to only the
 * non-redundant half of the image.
 *
 * In particular, this filter reduces the size of the image in the
 * first dimension to \f$\lfloor N/2 \rfloor + 1 \f$.
 *
 * \ingroup FourierTransform
 *
 * \sa HalfToFullHermitianImageFilter
 * \sa ForwardFFTImageFilter
 * \sa InverseFFTImageFilter
 * \sa RealToHalfHermitianForwardFFTImageFilter
 * \sa HalfHermitianToRealInverseFFTImageFilter
 * \ingroup ITKFFT
 */
template< typename TInputImage >
class ITK_TEMPLATE_EXPORT FullToHalfHermitianImageFilter :
  public ImageToImageFilter< TInputImage, TInputImage >
{
public:
  /** Standard class typedefs. */
  typedef TInputImage                              InputImageType;
  typedef typename InputImageType::PixelType       InputImagePixelType;
  typedef typename InputImageType::IndexType       InputImageIndexType;
  typedef typename InputImageType::IndexValueType  InputImageIndexValueType;
  typedef typename InputImageType::SizeType        InputImageSizeType;
  typedef typename InputImageType::SizeValueType   InputImageSizeValueType;
  typedef typename InputImageType::RegionType      InputImageRegionType;
  typedef TInputImage                              OutputImageType;
  typedef typename OutputImageType::PixelType      OutputImagePixelType;
  typedef typename OutputImageType::IndexType      OutputImageIndexType;
  typedef typename OutputImageType::IndexValueType OutputImageIndexValueType;
  typedef typename OutputImageType::SizeType       OutputImageSizeType;
  typedef typename OutputImageType::SizeValueType  OutputImageSizeValueType;
  typedef typename OutputImageType::RegionType     OutputImageRegionType;

  typedef FullToHalfHermitianImageFilter                 Self;
  typedef ImageToImageFilter< TInputImage, TInputImage > Superclass;
  typedef SmartPointer< Self >                           Pointer;
  typedef SmartPointer< const Self >                     ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FullToHalfHermitianImageFilter,
               ImageToImageFilter);

  /** Extract the dimensionality of the input and output images. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Get whether the actual X dimension of the image is odd or not in the full
   * representation */
  itkGetDecoratedOutputMacro(ActualXDimensionIsOdd, bool);

protected:
  FullToHalfHermitianImageFilter();
  ~FullToHalfHermitianImageFilter() ITK_OVERRIDE {}

  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            ThreadIdType threadId) ITK_OVERRIDE;

  /** The output is a different size from the input. */
  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  /** This class requires the entire input. */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  itkSetDecoratedOutputMacro(ActualXDimensionIsOdd, bool);

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FullToHalfHermitianImageFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFullToHalfHermitianImageFilter.hxx"
#endif

#endif // itkFullToHalfHermitianImageFilter_h
