/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageToImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkFilterImageToImage is the base class for all process objects that output
 * image data, and require image data as input.
 */
#ifndef __itkFilterImageToImage_h
#define __itkFilterImageToImage_h

#include "itkImageSource.h"

template <class TInputImage, class TOutputImage>
class ITK_EXPORT itkFilterImageToImage : public itkImageSource<TOutputImage>
{
public:
  /** 
   * Smart pointer typedef support 
   */
  typedef itkSmartPointer< itkFilterImageToImage<TInputImage,TOutputImage> >
    Pointer;

  /** 
   * Create the source with one output initially 
   */
  static Pointer New();

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(itkFilterImageToImage,itkImageSource);

  /** 
   * Set the image input of this process object. 
   */
  void SetInput(TInputImage *input);

  /** 
   * Get the image input of this process object. 
   */
  TInputImage *GetInput();
  TInputImage *GetInput(unsigned int idx);

protected:
  itkFilterImageToImage();
  ~itkFilterImageToImage() {};
  itkFilterImageToImage(const itkFilterImageToImage&) {};
  void operator=(const itkFilterImageToImage&) {};
  void PrintSelf(std::ostream& os, itkIndent indent);
  
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterImageToImage.txx"
#endif

#endif





