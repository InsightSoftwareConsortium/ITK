/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorToRGBImageAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVectorToRGBImageAdaptor_h
#define __itkVectorToRGBImageAdaptor_h

#include "itkImageAdaptor.h"
#include "itkVectorToRGBPixelAccessor.h"

namespace itk
{
 

/** \class VectorToRGBImageAdaptor
 * \brief Presents an image of pixel type Vector as being and image of
 * RGBPixel type.
 *
 * \ingroup ImageAdaptors
 *
 */
template <class TImage>
class ITK_EXPORT VectorToRGBImageAdaptor : public
      ImageAdaptor<TImage,
        Accessor::VectorToRGBPixelAccessor<
           typename TImage::PixelType::ValueType
                                      > >
{
public:
  /** Standard class typedefs. */
  typedef VectorToRGBImageAdaptor  Self;
  typedef ImageAdaptor<TImage,
              Accessor::VectorToRGBPixelAccessor<
                 typename TImage::PixelType::ValueType
                                       >  > Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro( VectorToRGBImageAdaptor, ImageAdaptor );

 protected:
  VectorToRGBImageAdaptor() {}
  virtual ~VectorToRGBImageAdaptor() {}
  
 private:
  VectorToRGBImageAdaptor(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#endif
