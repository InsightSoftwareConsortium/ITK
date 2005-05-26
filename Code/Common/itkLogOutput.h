/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLogOutput.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itk_LogOutput_h_
#define __itk_LogOutput_h_

#include<string>
#include"itkMacro.h"
#include"itkObject.h"
#include"itkObjectFactory.h"


namespace itk
{

/** \class LogOutput
 *  \brief Class LogOutput represents an output stream.
 *
 *  \author Hee-Su Kim, Compute Science Dept. Kyungpook National University,
 *                      ISIS Center, Georgetown University.
 *
 *  \ingroup OSSystemObjects LoggingObjects                    
 */

class LogOutput : public Object
{

public:

  typedef LogOutput Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** flush a buffer */
  virtual void Flush() = 0;

  /** Write to a buffer */
  virtual void Write( double timestamp ) = 0;

  /** Write to a buffer */
  virtual void Write(const std::string & content ) = 0;

  /** Write to a buffer */
  virtual void Write(const std::string & content, double timestamp) = 0;

protected:
  
  /** Destructor */
  LogOutput() {};

  /** Destructor */
  virtual ~LogOutput() {};

};

}

#endif //__itk_LogOutput_h_
