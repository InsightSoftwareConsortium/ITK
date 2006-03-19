/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianKernelFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGaussianKernelFunction_h
#define __itkGaussianKernelFunction_h

#include "itkKernelFunction.h"
#include "vnl/vnl_math.h"
#include <math.h>

namespace itk
{

/** \class GaussianKernelFunction
 * \brief Gaussian kernel used for density estimation and nonparameteric
 *  regression.
 *
 * This class encapsulates a Gaussian smoothing kernel for
 * density estimation or nonparameteric regression.
 * See documentation for KernelFunction for more details.
 *
 * \sa KernelFunction
 *
 * \ingroup Functions
 */
class ITKCommon_EXPORT GaussianKernelFunction : public KernelFunction
{
public:
  /** Standard class typedefs. */
  typedef GaussianKernelFunction Self;
  typedef KernelFunction Superclass;
  typedef SmartPointer<Self>  Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self); 

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianKernelFunction, KernelFunction); 

  /** Evaluate the function. */
  inline double Evaluate (const double& u) const
    { return ( vcl_exp(-0.5 * vnl_math_sqr( u ) ) * m_Factor ); }

protected:
  GaussianKernelFunction();
  ~GaussianKernelFunction();
  void PrintSelf(std::ostream& os, Indent indent) const
    { Superclass::PrintSelf( os, indent ); }  

private:
  GaussianKernelFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  static const double m_Factor;

};

} // end namespace itk

#endif
