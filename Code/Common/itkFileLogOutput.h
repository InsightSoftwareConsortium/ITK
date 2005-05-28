/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFileLogOutput.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itk_FileLogOutput_h_
#define __itk_FileLogOutput_h_

#include <fstream>
#include <string>

#include "itkMacro.h"
#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkSimpleFastMutexLock.h"
#include "itkLogOutput.h"


namespace itk
{

/** \class FileLogOutput
 *  \brief Class FileLogOutput represents a file output stream.
 *  This class provides thread safety for the file output stream.
 *
 * \author Hee-Su Kim, Compute Science Dept. Kyungpook National University,
 *                     ISIS Center, Georgetown University.
 *
 *  \ingroup OSSystemObjects LoggingObjects                    
 */

class ITKCommon_EXPORT FileLogOutput : public LogOutput
{

public:

  typedef FileLogOutput Self;
  typedef LogOutput  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  typedef std::ofstream   FileType;
  typedef FileType      * FilePointerType;

  itkTypeMacro(FileLogOutput, LogOutput);
  
  itkNewMacro(FileLogOutput);

  itkGetMacro(File, FilePointerType);

  /** Set file stream */
  void SetFile(FileType & FileStream);
  
  /** flush a buffer */
  virtual void Flush();

  /** Write to multiple outputs */
  virtual void Write(double timestamp);

  /** Write to a buffer */
  virtual void Write( const std::string & content );

  /** Write to a buffer */
  virtual void Write( const std::string & content, double timestamp );

protected:
  /** Constructor */
  FileLogOutput();

  /** Destructor */
  virtual ~FileLogOutput();

  void PrintSelf(std::ostream &os, Indent indent) const;

private:

  FilePointerType m_File;

  SimpleFastMutexLock m_Mutex;
};

}

#endif //__itk_FileLogOutput_h_
