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
#ifndef itkHalfToFullHermitianImageFilter_h
#define itkHalfToFullHermitianImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
/** \class HalfToFullHermitianImageFilter
 *
 * \brief Expands a half image produced from a real-to-complex
 * discrete Fourier transform (DFT) to the full complex image.
 *
 * The subclasses of RealToHalfHermitianForwardFFTImageFilter produce only
 * the non-redundant half of the image resulting from a
 * real-to-complex DFT. This filter takes the non-redundant half image
 * and generates the full complex image that includes the redundant
 * half. It requires additional information about the output image
 * size, namely, whether the size in the first dimension of the output
 * image is odd.
 *
 * \ingroup FourierTransform
 *
 * \sa RealToHalfHermitianForwardFFTImageFilter
 * \ingroup ITKFFT
 */
template< typename TInputImage >
class ITK_TEMPLATE_EXPORT HalfToFullHermitianImageFilter :
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

  typedef HalfToFullHermitianImageFilter                 Self;
  typedef ImageToImageFilter< TInputImage, TInputImage > Superclass;
  typedef SmartPointer< Self >                           Pointer;
  typedef SmartPointer< const Self >                     ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(HalfToFullHermitianImageFilter,
               ImageToImageFilter);

  /** Extract the dimensionality of the input and output images. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Was the original truncated dimension size in the x-dimension odd? */
  itkSetGetDecoratedInputMacro(ActualXDimensionIsOdd, bool);
  itkBooleanMacro(ActualXDimensionIsOdd);

protected:
  HalfToFullHermitianImageFilter();
  ~HalfToFullHermitianImageFilter() ITK_OVERRIDE {}

  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            ThreadIdType threadId) ITK_OVERRIDE;

  /** The output is a different size from the input. */
  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  /** This class requires the entire input. */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(HalfToFullHermitianImageFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHalfToFullHermitianImageFilter.hxx"
#endif

#endif // itkHalfToFullHermitianImageFilter_h
