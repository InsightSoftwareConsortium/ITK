/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomImageSource.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkRandomImageSource generates an image of random scalar values.
 * The output image may be of any dimension. The scalar values are
 * inserted into the image via a scalar iterator (i.e., the pixel type
 * must support GetScalar()/SetScalar()).
 */
#ifndef __itkRandomImageSource_h
#define __itkRandomImageSource_h

#include "itkImageSource.h"

template <class TOutputImage>
class ITK_EXPORT itkRandomImageSource : public itkImageSource<TOutputImage>
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef typename itkSmartPointer< itkRandomImageSource<TOutputImage> > Pointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(itkRandomImageSource,itkImageSource);

  /** 
   * Create the source with one output initially.
   */
  static Pointer New();

protected:
  itkRandomImageSource();
  ~itkRandomImageSource() {};
  itkRandomImageSource(const itkRandomImageSource&) {};
  void operator=(const itkRandomImageSource&) {};
  void PrintSelf(std::ostream& os, itkIndent indent);
  
  void Execute();

private:

};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRandomImageSource.cxx"
#endif

#endif
