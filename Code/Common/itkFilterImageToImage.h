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
 * FilterImageToImage is the base class for all process objects that output
 * image data, and require image data as input.
 */
#ifndef __itkFilterImageToImage_h
#define __itkFilterImageToImage_h

#include "itkImageSource.h"

namespace itk
{

template <class TInputImage, class TOutputImage>
class ITK_EXPORT FilterImageToImage : public ImageSource<TOutputImage>
{
public:
  /** 
   * Smart pointer typedef support 
   */
  typedef FilterImageToImage  Self;
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(FilterImageToImage,ImageSource);

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
  FilterImageToImage();
  ~FilterImageToImage() {};
  FilterImageToImage(const FilterImageToImage&) {};
  void operator=(const FilterImageToImage&) {};
  void PrintSelf(std::ostream& os, Indent indent);
  
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterImageToImage.txx"
#endif

#endif
