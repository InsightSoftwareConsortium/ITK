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
#include <vector>

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

  /**
   * Enums used to specify VTK file types.
   */
  typedef  enum {VTK_ASCII,VTK_BINARY} VTKFileType;
  
  /** 
   * Set the VTK file type. The default is VTK_ASCII.
   */
  itkSetMacro(FileType,VTKFileType);
  
  /** 
   * Get the VTK file type.
   */
  itkGetMacro(FileType,VTKFileType);
                 
  /** 
   * Specify the output file type as ASCII (the default).
   */
  void SetFileTypeToASCII() 
    {this->SetFileType(itkWriteVTKImage::VTK_ASCII);}

  /** 
   * Specify the output file type to binary.
   */
  void SetFileTypeToBinary() 
    {this->SetFileType(itkWriteVTKImage::VTK_BINARY);}

protected:
  itkWriteVTKImage();
  ~itkWriteVTKImage() {};
  itkWriteVTKImage(const itkWriteVTKImage&) {};
  void operator=(const itkWriteVTKImage&) {};
  void PrintSelf(std::ostream& os, itkIndent indent);

  void WriteData();
  
private:
  bool               m_WriteToOutputString;
  std::vector<char>  m_OutputBuffer;
  VTKFileType        m_FileType;
  
  std::ostream *OpenVTKFile();
  bool WriteVTKHeader(std::ostream *fp);
  bool WriteVTKImageData(std::ostream *fp, TInputImage *input);
  void CloseVTKFile(std::ostream *fp);
  
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWriteVTKImage.txx"
#endif

#endif
