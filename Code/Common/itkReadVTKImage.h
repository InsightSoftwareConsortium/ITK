/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReadVTKImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkReadVTKImage reads VTK-formatted data files. This class requires
 * that the VTK dataset type is STRUCTURED_POINTS, and only the scalar
 * point data is read (1-4 components).
 */
#ifndef __itkReadVTKImage_h
#define __itkReadVTKImage_h

#include "itkImageSource.h"

template <class TOutputImage>
class ITK_EXPORT itkReadVTKImage : public itkImageSource<TOutputImage>
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef itkSmartPointer< itkReadVTKImage<TOutputImage> > Pointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(itkReadVTKImage,itkImageSource);

  /** 
   * Create the source with one output initially.
   */
  static Pointer New();

  /** 
   * Specify the name of the input file.
   */
  itkSetStringMacro(FileName);
  
  /** 
   * Get the name of the input file.
   */
  itkGetStringMacro(FileName);
  
protected:
  itkReadVTKImage();
  ~itkReadVTKImage() {};
  itkReadVTKImage(const itkReadVTKImage&) {};
  void operator=(const itkReadVTKImage&) {};
  void PrintSelf(std::ostream& os, itkIndent indent);
  
  void Execute();

private:
  std::string m_FileName;

};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkReadVTKImage.txx"
#endif

#endif
