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

#define ITK_ASCII 0
#define ITK_BINARY 1

namespace itk
{

/** \class ProcessObject
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
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

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
   * A special version of the Update() method for writers.
   * It insures the pipeline is up-to-date and invokes the
   * WriteData() method.
   */
  virtual void Write();

protected:
  Writer();
  ~Writer();
  Writer(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);

  // All writers must respond to WriteData().
  virtual void WriteData() = 0;

  void Execute() {this->WriteData();}
  
private:
  std::string m_FileName;
  
};

  
} // namespace itk
  
#endif
  
