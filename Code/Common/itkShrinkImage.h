/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShrinkImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkShrinkImage reduces the size of an image by an integer factor.
 */
#ifndef __itkShrinkImage_h
#define __itkShrinkImage_h

#include "itkImageToImageFilter.h"

template <class TInputImage, class TOutputImage>
class ITK_EXPORT itkShrinkImage : 
  public itkImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef itkSmartPointer< itkShrinkImage<TInputImage,TOutputImage> > Pointer;

  /** 
   * Create the source with one output initially.
   */
  static Pointer New();

  /**
   *
   */
  static const char *GetClassName() 
    {return "itkShrinkImage";}

  /** 
   * Set the shrink factor. The default value is 1.
   */
  void SetShrinkFactor(unsigned int f) {itkSetMacro(m_ShrinkFactor,f);}
  
  /** 
   * Get the shrink factor.
   */
  unsigned int GetShrinkFactor() {itkGetMacro(m_ShrinkFactor);}
                 
protected:
  itkShrinkImage();
  ~itkShrinkImage() {};
  itkShrinkImage(const itkShrinkImage&) {};
  void operator=(const itkShrinkImage&) {};
  void PrintSelf(std::ostream& os, itkIndent indent);
  
  void Execute();

private:
  unsigned int m_ShrinkFactor;
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShrinkImage.cxx"
#endif

#endif
