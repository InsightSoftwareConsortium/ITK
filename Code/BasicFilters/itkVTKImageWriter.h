/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKImageWriter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkVTKImageWriter_h
#define __itkVTKImageWriter_h

#include "itkImageWriter.h"
#include <vector>

namespace itk
{

/** \class VTKImageWriter
 * \brief Write an image (dimension 1-3D) in VTK format.
 *
 * VTKImageWriter writes 1-3D images in VTK file format. You can specify
 * binary or ASCII output types. The class is limited in the type of
 * scalars that are supported: all native types with up to four components.
 */
template <class TInputImage>
class ITK_EXPORT VTKImageWriter : public ImageWriter<TInputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef VTKImageWriter       Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageWriter<TInputImage>  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(VTKImageWriter,ImageWriter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
protected:
  VTKImageWriter();
  ~VTKImageWriter() {}
  VTKImageWriter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);

  void WriteData();
  
private:
  bool               m_WriteToOutputString;
  std::vector<char>  m_OutputBuffer;
  
  std::ostream *OpenVTKFile();
  bool WriteVTKHeader(std::ostream *fp);
  bool VTKImageWriterData(std::ostream *fp, TInputImage *input);
  void CloseVTKFile(std::ostream *fp);  
};

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKImageWriter.txx"
#endif

#endif
