/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageDuplicator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageDuplicator_h
#define __itkImageDuplicator_h

#include "itkObject.h"
#include "itkImage.h"

namespace itk
{

/** This helper class create an image which is perfect copy of the input image
 */
template <class TInputImage>            
class ITK_EXPORT ImageDuplicator : public Object 
{
public:
  /** Standard class typedefs. */
  typedef ImageDuplicator Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Self, Object);

  /** Type definitions for the input image. */
  typedef TInputImage  ImageType;
  typedef typename TInputImage::Pointer  ImagePointer;
  typedef typename TInputImage::ConstPointer ImageConstPointer;
  typedef typename TInputImage::PixelType PixelType;
  typedef typename TInputImage::IndexType IndexType;

  itkStaticConstMacro(ImageDimension, unsigned int,
                      ImageType::ImageDimension);

  /** Set the input image. */
  itkSetConstObjectMacro(InputImage,ImageType);
  
  /** Get the output image. */
  itkGetObjectMacro(Output,ImageType);

  /** Compute of the input image. */
  void Update(void);

protected:
  ImageDuplicator();
  virtual ~ImageDuplicator() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  ImageDuplicator(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  ImageConstPointer       m_InputImage;
  ImagePointer            m_Output;
  unsigned long           m_InternalImageTime;
  
};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageDuplicator.txx"
#endif

#endif /* __itkImageDuplicator_h */
