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
#ifndef __itkWriteVTKImage_h
#define __itkWriteVTKImage_h

#include "itkWriteImage.h"
#include <vector>

ITK_NAMESPACE_BEGIN

/** \class WriteVTKImage
 * \brief Write an image (dimension 1-3D) in VTK format.
 *
 * WriteVTKImage writes 1-3D images in VTK file format. You can specify
 * binary or ASCII output types.
 */
template <class TInputImage>
class ITK_EXPORT WriteVTKImage : public WriteImage<TInputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef WriteVTKImage       Self;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(WriteVTKImage,ImageWriter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
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
    {this->SetFileType(WriteVTKImage::VTK_ASCII);}

  /** 
   * Specify the output file type to binary.
   */
  void SetFileTypeToBinary() 
    {this->SetFileType(WriteVTKImage::VTK_BINARY);}

protected:
  WriteVTKImage();
  ~WriteVTKImage() {}
  WriteVTKImage(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);

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

ITK_NAMESPACE_END
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWriteVTKImage.txx"
#endif

#endif
