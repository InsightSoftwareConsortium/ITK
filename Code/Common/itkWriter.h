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
  itkSetStringMacro(FileName);
  
  /** 
   * Get the name of the output file.
   */
  itkGetStringMacro(FileName);
  
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
  
};

#endif





