/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAtanImageAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAtanImageAdaptor_h
#define __itkAtanImageAdaptor_h

#include <itkImageAdaptor.h>
#include "vnl/vnl_math.h"

namespace itk
{
 
namespace Accessor {
/**
 * \class AtanPixelAccessor
 * \brief Give access to the atan() function of a value
 *
 * AtanPixelAccessor is templated over an internal type and an
 * external type representation. This class cast the input
 * applies the funtion to it and cast the result according 
 * to the types defined as template parameters
 * \ingroup ImageAdaptors
 *
 */

template <class TInternalType, class TExternalType >
class ITK_EXPORT AtanPixelAccessor  
{
public:

  /** External typedef. It defines the external aspect
   *  that this class will exhibit. */
  typedef TExternalType ExternalType;

  /** Internal typedef. It defines the internal real
   * representation of data. */
  typedef TInternalType InternalType;

  static inline void Set(TInternalType & output, const TExternalType & input) 
    {output = (TInternalType)atan((double)input);}

  static inline TExternalType Get( const TInternalType & input ) 
    {return (TExternalType)atan((double)input);}
};

  
} // end namespace Accessor
 
/**
 * \class AtanImageAdaptor
 * \brief Presents an image as being composed of the atan() of its pixels
 *
 * Additional casting is performed according to the input and output image
 * types following C++ default casting rules.
 *
 * \ingroup ImageAdaptors
 *
 */
template <class TImage, class TOutputPixelType>
class ITK_EXPORT AtanImageAdaptor : public
      ImageAdaptor<TImage,
                   Accessor::AtanPixelAccessor<
                                      typename TImage::PixelType,
                                      TOutputPixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef AtanImageAdaptor                                  Self;
  typedef ImageAdaptor<TImage,Accessor::AtanPixelAccessor<
                                       typename TImage::PixelType,
                                       TOutputPixelType> >
                                                            Superclass;
  typedef SmartPointer<Self>                                Pointer;
  typedef SmartPointer<const Self>                          ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( AtanImageAdaptor, ImageAdaptor );

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

protected:
  AtanImageAdaptor() {}
  virtual ~AtanImageAdaptor() {}
  
private:
  AtanImageAdaptor(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#endif
