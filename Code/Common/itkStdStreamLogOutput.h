/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStdStreamLogOutput.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itk_StdStreamLogOutput_h_
#define __itk_StdStreamLogOutput_h_

#include <iostream>
#include <string>

#include "itkMacro.h"
#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkSimpleFastMutexLock.h"
#include "itkLogOutput.h"


namespace itk
{

/** \class StdStreamLogOutput
 *  \brief Class StdStreamLogOutput represents a standard stream output stream.
 *  This class provides thread safety for the standard stream output stream.
 *
 * \author Hee-Su Kim, Compute Science Dept. Kyungpook National University,
 *                     ISIS Center, Georgetown University.
 *
 *
 *  \ingroup OSSystemObjects LoggingObjects                    
 */

class ITKCommon_EXPORT StdStreamLogOutput : public LogOutput
{

public:

  typedef StdStreamLogOutput Self;
  typedef LogOutput  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  typedef std::ostream  StreamType;
  typedef std::ostream* StreamPointerType;

  itkTypeMacro(StdStreamLogOutput, LogOutput);
  
  itkNewMacro(StdStreamLogOutput);

  itkGetMacro(Stream, StreamPointerType);

  /** Set a standard stream pointer */
  void SetStream(StreamType &Stream);
  
  /** flush a buffer */
  virtual void Flush();

  /** Write to multiple outputs */
  virtual void Write(double timestamp);

  /** Write to a buffer */
  virtual void Write(std::string const &content);

  /** Write to a buffer */
  virtual void Write(std::string const &content, double timestamp);

protected:
  /** Constructor */
  StdStreamLogOutput();

  /** Destructor */
  virtual ~StdStreamLogOutput();

private:

  StreamPointerType m_Stream;

  SimpleFastMutexLock m_Mutex;
};

}

#endif //__itk_StdStreamLogOutput_h_
