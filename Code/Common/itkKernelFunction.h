/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKernelFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkKernelFunction_h
#define _itkKernelFunction_h

#include "vnl/vnl_math.h"
#include "itkFunctionBase.h"

namespace itk
{

/** \class KernelFunction
 * \brief Kernel used for density estimation and nonparameteric regression.
 *
 * This class encapsulates the smoothing kernel used for statistical density
 * estimation and nonparameteric regression. The basic idea of the kernel
 * approach is to weight observations by a smooth function (the kernel) 
 * to created a smoothed approximation.
 *
 * Reference:
 * Silverman, B. W. (1986) Density Estimation. London: Chapman and Hall. 
 * 
 * \ingroup Functions
 */
class ITKCommon_EXPORT KernelFunction : public FunctionBase<double,double>
{
public:  
  /** Standard class typedefs. */
  typedef KernelFunction Self;
  typedef FunctionBase<double,double> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(KernelFunction, FunctionBase);

  /** Evaluate the function. Subclasses must implement this. */
  virtual double Evaluate (const double& u) const = 0;

protected:  
  KernelFunction(){};  
  ~KernelFunction(){};
  void PrintSelf(std::ostream& os, Indent indent) const
    { Superclass::PrintSelf( os, indent ); }  

};


} // namespace itk

#endif

