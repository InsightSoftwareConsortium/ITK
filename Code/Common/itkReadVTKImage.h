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
 * itkVTKImageReader reads VTK-formatted data files. This class requires
 * that the VTK dataset type is STRUCTURED_POINTS, and only the scalar
 * point data is read (1-4 components).
 */
#ifndef __itkVTKImageReader_h
#define __itkVTKImageReader_h

#include "itkImageSource.h"

template <class TOutputImage>
class ITK_EXPORT itkVTKImageReader : public itkImageSource<TOutputImage>
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef itkSmartPointer< itkVTKImageReader<TOutputImage> > Pointer;

  /** 
   * Create the source with one output initially.
   */
  static Pointer New();

  /** 
   * Specify the name of the input file.
   */
  void SetFileName(const char *str) 
    {itkSetStringMacro(m_FileName,str);}
  
  /** 
   * Get the name of the input file.
   */
  const char *GetFileName() const
    {itkGetStringMacro(m_FileName);}
  
protected:
  itkVTKImageReader();
  ~itkVTKImageReader() {};
  itkVTKImageReader(const itkVTKImageReader&) {};
  void operator=(const itkVTKImageReader&) {};
  void PrintSelf(std::ostream& os, itkIndent indent);
  
  void Execute();

private:
  std::string m_FileName;

};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKImageReader.cxx"
#endif

#endif
