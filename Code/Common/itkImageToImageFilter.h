/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkImageToImageFilter is the base class for all process objects that output
 * image data, and require image data as input.
 */
#ifndef __itkImageToImageFilter_h
#define __itkImageToImageFilter_h

#include "itkImageSource.h"

template <class TInputImage, class TOutputImage>
class ITK_EXPORT itkImageToImageFilter : public itkImageSource<TOutputImage>
{
public:
  /** 
   * Smart pointer typedef support 
   */
  typedef itkSmartPointer< itkImageToImageFilter<TInputImage,TOutputImage> >
    Pointer;

  /** 
   * Create the source with one output initially 
   */
  static Pointer New();

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(itkImageToImageFilter,itkImageSource);

  /** 
   * Set the image input of this process object. 
   */
  void SetInput(TInputImage *input);

  /** 
   * Get the image input of this process object. 
   */
  TInputImage *GetInput();
  TInputImage *GetInput(int idx)
    {return (TInputImage *) this->itkProcessObject::GetInput(idx);}

protected:
  itkImageToImageFilter();
  ~itkImageToImageFilter() {};
  itkImageToImageFilter(const itkImageToImageFilter&) {};
  void operator=(const itkImageToImageFilter&) {};
  void PrintSelf(std::ostream& os, itkIndent indent);
  
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageFilter.cxx"
#endif

#endif





