/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWriteImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkImageWriter is the base class for writers that write images.
 */
#ifndef __itkImageWriter_h
#define __itkImageWriter_h

#include "itkWriter.h"

template <class TInputImage>
class ITK_EXPORT itkImageWriter : public itkWriter
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef itkSmartPointer< itkImageWriter<TInputImage> > Pointer;

  /** 
   * Set the input image of this writer. 
   */
  void SetInput(TInputImage *input);

  /** 
   * Get the input image of this writer.
   */
  TInputImage *GetInput();

protected:
  itkImageWriter() {};
  ~itkImageWriter() {};
  itkImageWriter(const itkImageWriter&) {};
  void operator=(const itkImageWriter&) {};
  void PrintSelf(std::ostream& os, itkIndent indent);

private:

  
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageWriter.cxx"
#endif

#endif





