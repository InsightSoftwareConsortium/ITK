/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWriter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
 *
 * \ingroup IOFilters
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
  void PrintSelf(std::ostream& os, Indent indent) const;

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
  
