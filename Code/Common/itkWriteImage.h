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
#ifndef __itkVTKImageWriter_h
#define __itkVTKImageWriter_h

#include "itkWriter.h"

template <class TInputImage>
class ITK_EXPORT itkVTKImageWriter : public itkImageWriter
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef itkSmartPointer< itkVTKImageWriter<TInputImage> > Pointer;

  /** 
   * Create the image writer.
   */
  static Pointer New();

  /** 
   * Set the input image of this writer. 
   */
  void SetInput(TInputImage *input);

  /** 
   * Get the input image of this writer.
   */
  TInputImage *GetInput();

protected:
  itkVTKImageWriter() {};
  ~itkVTKImageWriter() {};
  itkVTKImageWriter(const itkVTKImageWriter&) {};
  void operator=(const itkVTKImageWriter&) {};
  void PrintSelf(std::ostream& os, itkIndent indent);

private:

  
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageWriter.cxx"
#endif

#endif





