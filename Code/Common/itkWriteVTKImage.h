/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWriteVTKImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkWriteVTKImage writes 1-3D images in VTK file format. You can specify
 * binary or ASCII output types.
 */
#ifndef __itkWriteVTKImage_h
#define __itkWriteVTKImage_h

#include "itkWriteImage.h"

template <class TInputImage>
class ITK_EXPORT itkWriteVTKImage : public itkWriteImage<TInputImage>
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef itkSmartPointer< itkWriteVTKImage<TInputImage> > Pointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(itkWriteVTKImage,itkImageWriter);

  /** 
   * Create the VTK image writer.
   */
  static Pointer New();

protected:
  itkWriteVTKImage() {};
  ~itkWriteVTKImage() {};
  itkWriteVTKImage(const itkWriteVTKImage&) {};
  void operator=(const itkWriteVTKImage&) {};
  void PrintSelf(std::ostream& os, itkIndent indent);

  void WriteData();
  
private:
  
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWriteVTKImage.cxx"
#endif

#endif





