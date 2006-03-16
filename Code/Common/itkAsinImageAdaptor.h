/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAsinImageAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAsinImageAdaptor_h
#define __itkAsinImageAdaptor_h

#include <itkImageAdaptor.h>
#include "vnl/vnl_math.h"

namespace itk
{
 
namespace Accessor {
/**
 * \class AsinPixelAccessor
 * \brief Give access to the asin() function of a value
 *
 * AsinPixelAccessor is templated over an internal type and an
 * external type representation. This class cast the input
 * applies the funtion to it and cast the result according 
 * to the types defined as template parameters
 * 
 * \ingroup ImageAdaptors */
template <class TInternalType, class TExternalType >
class ITK_EXPORT AsinPixelAccessor  
{
public:

  /** External typedef. It defines the external aspect
   *  that this class will exhibit. */
  typedef TExternalType ExternalType;

  /** Internal typedef. It defines the internal real
   * representation of data. */
  typedef TInternalType InternalType;

  static inline void Set(TInternalType & output, const TExternalType & input) 
    {output = (TInternalType)asin((double)input);}

  static inline TExternalType Get( const TInternalType & input ) 
    {return (TExternalType)asin((double)input);}
};

  
} // end namespace Accessor

/**
 * \class AsinImageAdaptor
 * \brief Presents an image as being composed of the asin() of its pixels
 *
 * Additional casting is performed according to the input and output image
 * types following C++ default casting rules.
 *
 * \ingroup ImageAdaptors */
template <class TImage, class TOutputPixelType>
class ITK_EXPORT AsinImageAdaptor : public
      ImageAdaptor<TImage,
                   Accessor::AsinPixelAccessor<
                                      typename TImage::PixelType,
                                      TOutputPixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef AsinImageAdaptor                                    Self;
  typedef ImageAdaptor<TImage,Accessor::AsinPixelAccessor<
                                       typename TImage::PixelType,
                                       TOutputPixelType> >    Superclass;
  typedef SmartPointer<Self>                                  Pointer;
  typedef SmartPointer<const Self>                            ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( AsinImageAdaptor, ImageAdaptor );

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

protected:
  AsinImageAdaptor() {}
  virtual ~AsinImageAdaptor() {}
  
private:
  AsinImageAdaptor(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#endif
