/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageIOFactory_h
#define __itkImageIOFactory_h

#include "itkObject.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class ImageIOFactory
 * \brief Create instances of ImageIO objects using an object factory.
 */
class ITK_EXPORT ImageIOFactory : public Object
{
public:  
  /** Standard class typedefs. */
  typedef ImageIOFactory   Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Class Methods used to interface with the registered factories */
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageIOFactory, Object);

  /** Convenient typedefs. */
  typedef ::itk::ImageIOBase::Pointer ImageIOBasePointer;

  /** Mode in which the files is intended to be used */
  typedef enum { ReadMode, WriteMode } FileModeType;
  
  /** Create the appropriate ImageIO depending on the particulars of the file. */
  static ImageIOBasePointer CreateImageIO(const char* path, FileModeType mode);

  /** Register Built-in factories */
  static void RegisterBuiltInFactories();

protected:
  ImageIOFactory();
  ~ImageIOFactory();

private:
  ImageIOFactory(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};
  
  
} // end namespace itk

#endif
