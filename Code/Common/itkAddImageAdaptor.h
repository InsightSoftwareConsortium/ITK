/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAddImageAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAddImageAdaptor_h
#define __itkAddImageAdaptor_h

#include "itkImageAdaptor.h"
#include "itkAddPixelAccessor.h"

namespace itk
{
 
/** \class AddImageAdaptor
 * \brief Presents an image as being composed of the log() of its pixels
 *
 * Additional casting is performed according to the input and output image
 * types following C++ default casting rules.
 *
 * \ingroup ImageAdaptors
 */
template <class TImage>
class ITK_EXPORT AddImageAdaptor : public
      ImageAdaptor<TImage,
                   Accessor::AddPixelAccessor< typename TImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef AddImageAdaptor  Self;
  typedef ImageAdaptor<TImage,
                       Accessor::AddPixelAccessor<
                                 typename TImage::PixelType > >  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  typedef typename TImage::PixelType      PixelType;

  /** Run-time type information (and related methods). */
  itkTypeMacro( AddImageAdaptor, ImageAdaptor );

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Set the value to be added to image pixels */
  void SetValue( const PixelType newvalue )
    { this->GetPixelAccessor().SetValue( newvalue ); }
  
  /** Get the value to be added to image pixels */
  PixelType GetValue() const 
    { return this->GetPixelAccessor().GetValue(); }
  
protected:
  AddImageAdaptor() {}
  virtual ~AddImageAdaptor() {}
  
private:
  AddImageAdaptor(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#endif
