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
#ifndef __itkComposeRGBAImageFilter_h
#define __itkComposeRGBAImageFilter_h

#include "itkNaryFunctorImageFilter.h"
#include "itkNumericTraitsRGBAPixel.h"

namespace itk
{
namespace Functor
{
template< class TInput >
class ComposeRGBA
{
public:
  typedef RGBAPixel< TInput > OutputType;
  ComposeRGBA() {}
  ~ComposeRGBA() {}
  bool operator!=(const ComposeRGBA &) const
  {
    return false;
  }

  bool operator==(const ComposeRGBA & other) const
  {
    return !( *this != other );
  }

  inline OutputType operator()(const std::vector< TInput > & in) const
  {
    OutputType pixel;

    pixel.Set(in[0], in[1], in[2], in[3]);
    return pixel;
  }
};
}

/** \class ComposeRGBAImageFilter
 * \brief Implements pixel-wise composition of an RGBA pixel from four scalar images.
 *
 * This filter receives four scalar images as input. Each image containing
 * one of the RGBA components of a color image. The filter produces as output an
 * RGBA image in which the four components have been unified. The Component
 * type is preserved from the PixelType of the input images.
 *
 *  \author Dan Mueller, Queensland University of Technology, Brisbane, Australia
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/153
 *
 * \ingroup IntensityImageFilters
 * \ingroup ITK-Review
 */

template< typename TInputImage,
          typename TOutputImage =
            Image< RGBAPixel< ITK_TYPENAME TInputImage::PixelType >,
                   ::itk::GetImageDimension< TInputImage >::ImageDimension > >
class ITK_EXPORT ComposeRGBAImageFilter:
  public
  NaryFunctorImageFilter< TInputImage, TOutputImage,
                          Functor::ComposeRGBA< ITK_TYPENAME TInputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef ComposeRGBAImageFilter Self;
  typedef NaryFunctorImageFilter< TInputImage, TOutputImage,
                                  Functor::ComposeRGBA< ITK_TYPENAME TInputImage::PixelType > >  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef typename Superclass::OutputImageType OutputImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ComposeRGBAImageFilter, NaryFunctorImageFilter);
protected:
  ComposeRGBAImageFilter() {}
  virtual ~ComposeRGBAImageFilter() {}
private:
  ComposeRGBAImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);         //purposely not implemented
};
} // end namespace itk

#endif
