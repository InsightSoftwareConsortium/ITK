/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkRandomImage generates an image of the specified size consisting
 * of random values. All the components of the output image are set to
 * random values.
 */
#ifndef __itkRandomImage_h
#define __itkRandomImage_h

#include "itkImageSource.h"

template <class TOutputImage>
class ITK_EXPORT itkRandomImage : public itkImageSource<TOutputImage>
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef itkSmartPointer< itkRandomImage<TOutputImage> > Pointer;

  /** 
   * Create the source with one output initially.
   */
  static Pointer New();

protected:
  itkRandomImage();
  ~itkRandomImage() {};
  itkRandomImage(const itkRandomImage&) {};
  void operator=(const itkRandomImage&) {};
  void PrintSelf(std::ostream& os, itkIndent indent);
  
  void Execute();

private:

};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRandomImage.cxx"
#endif

#endif
