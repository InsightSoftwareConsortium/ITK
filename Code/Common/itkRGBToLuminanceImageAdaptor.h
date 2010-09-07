/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBToLuminanceImageAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRGBToLuminanceImageAdaptor_h
#define __itkRGBToLuminanceImageAdaptor_h

#include <itkImageAdaptor.h>
#include "vnl/vnl_math.h"

namespace itk
{
namespace Accessor
{
/** \class RGBToLuminancePixelAccessor
 * \brief Give access to Luminance of a color pixel type.
 *
 * RGBToLuminancePixelAccessor is templated over an internal type and an
 * external type representation. This class cast the input applies the function
 * to it and cast the result according to the types defined as template
 * parameters. The input pixel type must support the GetLuminance() method.
 * This is the case of the RGBPixel class for example.
 *
 * \ingroup ImageAdaptors
 */
template< class TInternalType, class TExternalType >
class ITK_EXPORT RGBToLuminancePixelAccessor
{
public:
  /** External typedef. It defines the external aspect
   * that this class will exhibit. */
  typedef TExternalType ExternalType;

  /** Internal typedef. It defines the internal real
   * representation of data. */
  typedef TInternalType InternalType;

  static inline void Set(TInternalType & output, const TExternalType & input)
  { output = static_cast< TInternalType >( input.GetLuminance() ); }

  static inline TExternalType Get(const TInternalType & input)
  { return static_cast< TExternalType >( input.GetLuminance() ); }
};
} // end namespace Accessor

/** \class RGBToLuminanceImageAdaptor
 * \brief Presents a color image as being composed of the Luminance of its pixels.
 *
 * Additional casting is performed according to the input and output image
 * types following C++ default casting rules. The input color pixel type must
 * provide a GetLuminance() method.
 *
 * \ingroup ImageAdaptors
 */
template< class TImage, class TOutputPixelType >
class ITK_EXPORT RGBToLuminanceImageAdaptor:public
  ImageAdaptor< TImage,
                Accessor::RGBToLuminancePixelAccessor<
                  typename TImage::PixelType,
                  TOutputPixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef RGBToLuminanceImageAdaptor Self;
  typedef ImageAdaptor< TImage, Accessor::RGBToLuminancePixelAccessor<
                          typename TImage::PixelType,
                          TOutputPixelType > >  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RGBToLuminanceImageAdaptor, ImageAdaptor);
protected:
  RGBToLuminanceImageAdaptor() {}
  virtual ~RGBToLuminanceImageAdaptor() {}
private:
  RGBToLuminanceImageAdaptor(const Self &); //purposely not implemented
  void operator=(const Self &);             //purposely not implemented
};
} // end namespace itk

#endif
