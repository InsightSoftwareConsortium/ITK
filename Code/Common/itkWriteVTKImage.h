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
 * itkVTKImageWriter writes 1-3D images in VTK file format. You can specify
 * binary or ASCII output types.
 */
#ifndef __itkVTKImageWriter_h
#define __itkVTKImageWriter_h

#include "itkImageWriter.h"

template <class TInputImage>
class ITK_EXPORT itkVTKImageWriter : public itkImageWriter<TInputImage>
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef itkSmartPointer< itkVTKImageWriter<TInputImage> > Pointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(itkVTKImageWriter,itkImageWriter);

  /** 
   * Create the VTK image writer.
   */
  static Pointer New();

protected:
  itkVTKImageWriter() {};
  ~itkVTKImageWriter() {};
  itkVTKImageWriter(const itkVTKImageWriter&) {};
  void operator=(const itkVTKImageWriter&) {};
  void PrintSelf(std::ostream& os, itkIndent indent);

  void WriteData();
  
private:
  
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKImageWriter.cxx"
#endif

#endif





