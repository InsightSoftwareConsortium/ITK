/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCostFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCostFunction_h
#define __itkCostFunction_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkArray.h"
#include "itkExceptionObject.h"

namespace itk
{
/** \class CostFunction
 * \brief Base class for cost functions intended to be used with Optimizers.
 *
 * \ingroup Numerics Optimizers
 *
 */

class ITK_EXPORT CostFunction:public Object
{
public:
  /** Standard class typedefs. */
  typedef CostFunction               Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(CostFunction, Object);

  /**  ParametersType typedef.
   *  It defines a position in the optimization search space. */
  typedef double                       ParametersValueType;
  typedef Array< ParametersValueType > ParametersType;

  /** Return the number of parameters required to compute
   *  this cost function.
   *  This method MUST be overloaded by derived classes. */
  virtual unsigned int GetNumberOfParameters(void) const  = 0;

protected:
  CostFunction() {}
  virtual ~CostFunction() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  CostFunction(const Self &);   //purposely not implemented
  void operator=(const Self &); //purposely not implemented
};
} // end namespace itk

#endif
