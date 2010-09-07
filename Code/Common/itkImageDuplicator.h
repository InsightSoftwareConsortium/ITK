/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageDuplicator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
/** \class ImageDuplicator
 * \brief This helper class create an image which is perfect copy of the input image.
 *
 * This class is NOT a filter. Although it has an API similar to a filter, this class
 * is not intended to be used in a pipeline. Instead, the typical use will be like
 * it is illustrated in the following code:
 *
 * \code
 *     medianFilter->Update();
 *     ImageType::Pointer image = medianFilter->GetOutput();
 *     typedef ImageDuplicator< ImageType > DuplicatorType;
 *     DuplicatorType::Pointer duplicator = DuplicatorType::New();
 *     duplicator->SetInput();
 *     duplicator->Update();
 *     ImageType::Pointer clonedImage = duplicator->GetOutput();
 * \endcode
 *
 * Note that the Update() method must be called explicitly in the filter
 * that provides the input to the ImageDuplicator object. This is needed
 * because the ImageDuplicator is not a pipeline filter.
 *
 */
template< class TInputImage >
class ITK_EXPORT ImageDuplicator:public Object
{
public:
  /** Standard class typedefs. */
  typedef ImageDuplicator            Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageDuplicator, Object);

  /** Type definitions for the input image. */
  typedef TInputImage                        ImageType;
  typedef typename TInputImage::Pointer      ImagePointer;
  typedef typename TInputImage::ConstPointer ImageConstPointer;
  typedef typename TInputImage::PixelType    PixelType;
  typedef typename TInputImage::IndexType    IndexType;

  itkStaticConstMacro(ImageDimension, unsigned int, ImageType::ImageDimension);

  /** Set the input image. */
  itkSetConstObjectMacro(InputImage, ImageType);

  /** Get the output image. */
  itkGetObjectMacro(Output, ImageType);

  /** Compute of the input image. */
  void Update(void);

protected:
  ImageDuplicator();
  virtual ~ImageDuplicator() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  ImageDuplicator(const Self &); //purposely not implemented
  void operator=(const Self &);  //purposely not implemented

  ImageConstPointer m_InputImage;
  ImagePointer      m_Output;
  unsigned long     m_InternalImageTime;
};
} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_ImageDuplicator(_, EXPORT, TypeX, TypeY)               \
  namespace itk                                                             \
  {                                                                         \
  _( 1 ( class EXPORT ImageDuplicator< ITK_TEMPLATE_1 TypeX > ) )           \
  namespace Templates                                                       \
  {                                                                         \
  typedef ImageDuplicator< ITK_TEMPLATE_1 TypeX > ImageDuplicator##TypeY; \
  }                                                                         \
  }

#if ITK_TEMPLATE_EXPLICIT
#include "Templates/itkImageDuplicator+-.h"
#endif

#if ITK_TEMPLATE_TXX
#include "itkImageDuplicator.txx"
#endif

#endif /* __itkImageDuplicator_h */
