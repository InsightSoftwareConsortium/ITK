/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWriter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkWriter is the base class for all Insight data writers. You can specify
 * binary or ASCII output types, as well as the output file name.
 */
#ifndef __itkWriter_h
#define __itkWriter_h

#include "itkProcessObject.h"

#define ITK_ASCII 0
#define ITK_BINARY 1

class ITK_EXPORT itkWriter : public itkProcessObject
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef itkSmartPointer<itkWriter> Pointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(itkWriter,itkProcessObject);

  /** 
   * Specify the name of the output file.
   */
  void SetFileName(const char *str) 
    {itkSetStringMacro(m_FileName,str);}
  
  /** 
   * Get the name of the output file.
   */
  const char *GetFileName() const
    {itkGetStringMacro(m_FileName);}
  
  /** 
   * Specify the output file type as either ASCII or binary.
   */
  void SetFileType(int type) 
    {itkSetClampMacro(m_FileType,type,ITK_ASCII,ITK_BINARY);}
  
  /** 
   * Get the file type.
   */
  int GetFileType() const
    {itkGetMacro(m_FileType);}
  
  /** 
   * Specify the output file type as ASCII (the default).
   */
  void SetFileTypeToASCII() 
    {this->SetFileType(ITK_ASCII);}

  /** 
   * Specify the output file type to binary.
   */
  void SetFileTypeToBinary() 
    {this->SetFileType(ITK_BINARY);}

  /** 
   * A special version of the Update() method for writers.
   * It insures the pipeline is up-to-date and invokes the
   * WriteData() method.
   */
  virtual void Write();

protected:
  itkWriter();
  ~itkWriter();
  itkWriter(const itkWriter&) {};
  void operator=(const itkWriter&) {};
  void PrintSelf(std::ostream& os, itkIndent indent);

  // All writers must respond to WriteData().
  virtual void WriteData() = 0;

  void Execute() {this->WriteData();}
  
private:
  std::string m_FileName;
  int         m_FileType;
  
};

#endif





