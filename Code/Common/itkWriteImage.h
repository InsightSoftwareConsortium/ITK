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
 * WriteImage is the base class for writers that write images.
 */
#ifndef __itkWriteImage_h
#define __itkWriteImage_h

#include "itkWriter.h"

namespace itk
{

template <class TInputImage>
class ITK_EXPORT WriteImage : public Writer
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef WriteImage          Self;
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(WriteImage,Writer);

  /** 
   * Set the input image of this writer. 
   */
  void SetInput(TInputImage *input);

  /** 
   * Get the input image of this writer.
   */
  TInputImage *GetInput();

protected:
  WriteImage() {}
  ~WriteImage() {}
  WriteImage(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);

private:
};

} // namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWriteImage.txx"
#endif

#endif
  
