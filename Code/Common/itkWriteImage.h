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
 * itkWriteImage is the base class for writers that write images.
 */
#ifndef __itkWriteImage_h
#define __itkWriteImage_h

#include "itkWriter.h"

template <class TInputImage>
class ITK_EXPORT itkWriteImage : public itkWriter
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef itkSmartPointer< itkWriteImage<TInputImage> > Pointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(itkWriteImage,itkWriter);

  /** 
   * Set the input image of this writer. 
   */
  void SetInput(TInputImage *input);

  /** 
   * Get the input image of this writer.
   */
  TInputImage *GetInput();

protected:
  itkWriteImage() {};
  ~itkWriteImage() {};
  itkWriteImage(const itkWriteImage&) {};
  void operator=(const itkWriteImage&) {};
  void PrintSelf(std::ostream& os, itkIndent indent);

private:

  
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWriteImage.cxx"
#endif

#endif





