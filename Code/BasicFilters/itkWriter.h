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
#ifndef __itkWriter_h
#define __itkWriter_h

#include "itkProcessObject.h"

namespace itk
{

/** \class Writer
 * \brief The base class for all data writers.
 *
 * Writer is the base class for all Insight data writers. You can specify
 * binary or ASCII output types, as well as the output file name.
 */
class ITK_EXPORT Writer : public ProcessObject
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Writer              Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ProcessObject  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(Writer,ProcessObject);

  /** 
   * Specify the name of the output file.
   */
  itkSetStringMacro(FileName);
  
  /** 
   * Get the name of the output file.
   */
  itkGetStringMacro(FileName);
  
  /**
   * Enums used to specify VTK type: binary or ASCII.
   */
  typedef  enum {ASCII,Binary} FileType;
  
  /** 
   * Set the file type. The default is ASCII.
   */
  itkSetMacro(FileType,FileType);
  
  /** 
   * Get the file type.
   */
  itkGetMacro(FileType,FileType);
                 
  /** 
   * Specify the output file type as ASCII (the default).
   */
  void SetFileTypeToASCII() 
    {this->SetFileType(Writer::ASCII);}

  /** 
   * Specify the output file type to binary.
   */
  void SetFileTypeToBinary() 
    {this->SetFileType(Writer::Binary);}

  /** 
   * A special version of the Update() method for writers.
   * It invokes start and end methods and handles releasing data.
   */
  virtual void Write();

protected:
  Writer();
  ~Writer();
  Writer(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);

  /**
   * All writers must respond to WriteData(). The WriteData() method
   * is responsible for updating the pipeline, and may request pieces
   * of the data (e.g., stream) if necessary to write out the entire 
   * input dataset.
   */
  virtual void WriteData() = 0;

  void GenerateData() 
    {this->WriteData();}
  
private:
  std::string        m_FileName;
  FileType           m_FileType;
  
};

  
} // end namespace itk
  
#endif
  
