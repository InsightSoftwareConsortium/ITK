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
#ifndef __itkWriteImage_h
#define __itkWriteImage_h

#include "itkWriter.h"

ITK_NAMESPACE_BEGIN

/** \class WriteImage
 * \brief Base class for all writers that write images.
 *
 * WriteImage is the base class for writers that write images.
 */
template <class TInputImage>
class ITK_EXPORT WriteImage : public Writer
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef WriteImage          Self;

  /** 
   * Smart pointer typedef support.
   */
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

ITK_NAMESPACE_END
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWriteImage.txx"
#endif

#endif
  
