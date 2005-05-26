/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConsoleLogOutput.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itk_ConsoleLogOutput_h_
#define __itk_ConsoleLogOutput_h_

#include <string>

#include "itkMacro.h"
#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkSimpleFastMutexLock.h"
#include "itkLogOutput.h"


namespace itk
{

/** \class ConsoleLogOutput
 *  \brief Class ConsoleLogOutput represents a standard output stream.
 *  This class provides thread safety for the standard output stream.
 *
 * \author Hee-Su Kim, Compute Science Dept. Kyungpook National University,
 *                     ISIS Center, Georgetown University.
 *
 *
 *  \ingroup OSSystemObjects LoggingObjects                    
 */

class ConsoleLogOutput : public LogOutput
{

public:

  typedef ConsoleLogOutput Self;
  typedef LogOutput  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  itkTypeMacro(ConsoleLogOutput, LogOutput);
  
  itkNewMacro(ConsoleLogOutput);
  
  /** flush a buffer */
  virtual void Flush();

  /** Write to a buffer */
  virtual void Write(double timestamp);

  /** Write to a buffer */
  virtual void Write(std::string const &content);

  /** Write to a buffer */
  virtual void Write(std::string const &content, double timestamp);

protected:
  /** Constructor */
  ConsoleLogOutput();

  /** Destructor */
  virtual ~ConsoleLogOutput();

private:

  static SimpleFastMutexLock m_Mutex;
};

}

#endif //__itk_ConsoleLogOutput_h_
