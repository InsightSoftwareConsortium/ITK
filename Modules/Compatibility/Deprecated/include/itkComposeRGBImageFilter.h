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
#ifndef itkComposeRGBImageFilter_h
#define itkComposeRGBImageFilter_h
#if !defined( ITK_LEGACY_REMOVE )

#include "itkTernaryFunctorImageFilter.h"
#include "itkRGBPixel.h"

namespace itk
{
namespace Functor
{
template< typename TInput >
class ComposeRGB
{
public:
  typedef RGBPixel< TInput > OutputType;
  ComposeRGB() {}
  ~ComposeRGB() {}
  bool operator!=(const ComposeRGB &) const
  {
    return false;
  }

  bool operator==(const ComposeRGB & other) const
  {
    return !( *this != other );
  }

  inline OutputType operator()(const TInput & R,
                               const TInput & G,
                               const TInput & B) const
  {
    OutputType rgbPixel;

    rgbPixel.Set(R, G, B);
    return rgbPixel;
  }
};
}

/** \class ComposeRGBImageFilter
 * \brief Implements pixel-wise composition of an RGB pixel from three scalar images.
 *
 * This filter receives three scalar images as input. Each image containing
 * one of the RGB components of a color image. The filter produces as output an
 * RGB image in which the three components have been unified. The Component
 * type is preserved from the PixelType of the input images.
 *
 * \deprecated
 * \ingroup ITKDeprecated
 */

template< typename TInputImage,
          typename TOutputImage =
            Image< RGBPixel< typename TInputImage::PixelType >,
                   TInputImage::ImageDimension > >
class ComposeRGBImageFilter:
  public
  TernaryFunctorImageFilter< TInputImage, TInputImage,
                             TInputImage, TOutputImage,
                             Functor::ComposeRGB< typename TInputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef ComposeRGBImageFilter           Self;
  typedef TernaryFunctorImageFilter<
    TInputImage, TInputImage,
    TInputImage, TOutputImage,
    Functor::ComposeRGB<
      typename TInputImage::PixelType > > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef typename Superclass::OutputImageType OutputImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ComposeRGBImageFilter,
               TernaryFunctorImageFilter);

protected:
  ComposeRGBImageFilter() {}
  virtual ~ComposeRGBImageFilter() {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ComposeRGBImageFilter);
};
} // end namespace itk

#endif //#if !defined( ITK_LEGACY_REMOVE )
#endif
