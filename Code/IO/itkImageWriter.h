/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageWriter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageWriter_h
#define __itkImageWriter_h

#include "itkWriter.h"

namespace itk
{

/** \class ImageWriter
 * \brief Base class for all writers that write images.
 *
 * ImageWriter is the base class for writers that write images.
 * \ingroup IOFilters Deprecated
 */
template <class TInputImage>
class ITK_EXPORT ImageWriter : public Writer
{
public:
  /** Standard class typedefs. */
  typedef ImageWriter          Self;
  typedef Writer   Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Some convenience typedefs. */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::Pointer InputImagePointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageWriter,Writer);

  /** Set the input image of this writer.  */
  void SetInput(const TInputImage *input);

  /** Get the input image of this writer. */
  const InputImageType * GetInput(void);

protected:
  ImageWriter() {}
  ~ImageWriter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  ImageWriter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageWriter.txx"
#endif

#endif
  
