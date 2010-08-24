/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkComposeRGBAImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkComposeRGBAImageFilter_h
#define __itkComposeRGBAImageFilter_h

#include "itkNaryFunctorImageFilter.h"
#include "itkRGBAPixel.h"
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
