/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
///templated image class
/**
 * itkImage is the templated image class.
 */

#ifndef __itkImage_h
#define __itkImage_h

#include "itkImageBase.h"

template <class T>
class ITK_EXPORT itkImage : public itkImageBase
{
public:
  /** Smart pointer typedef support */
  typedef itkSmartPointer< itkImage<T> > Pointer;

  /** Create an empty image. */
  static itkImage<T>::Pointer New();

private:
  
};

#endif

