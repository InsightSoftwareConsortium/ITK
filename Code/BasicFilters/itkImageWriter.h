/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageWriter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

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
 */
template <class TInputImage>
class ITK_EXPORT ImageWriter : public Writer
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImageWriter          Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Writer   Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Some typedefs
   */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::Pointer InputImagePointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImageWriter,Writer);

  /** 
   * Set the input image of this writer. 
   */
  void SetInput(TInputImage *input);

  /** 
   * Get the input image of this writer.
   */
  InputImagePointer GetInput();

protected:
  ImageWriter() {}
  ~ImageWriter() {}
  ImageWriter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);

private:
};

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageWriter.txx"
#endif

#endif
  
