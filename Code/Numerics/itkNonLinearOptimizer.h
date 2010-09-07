/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNonLinearOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNonLinearOptimizer_h
#define __itkNonLinearOptimizer_h

#include "itkOptimizer.h"

namespace itk
{
/** \class NonLinearOptimizer
 * \brief Wrap of the vnl_nonlinear_minimizer to be adapted
 *
 * This class is provided to support the structure of an
 * Optimizers Hierarchy. It is not intended to be instantiated.
 *
 * \ingroup Numerics Optimizers
 */
class ITK_EXPORT NonLinearOptimizer:public Optimizer

{
public:
  /** Standard class typedefs. */
  typedef NonLinearOptimizer         Self;
  typedef Optimizer                  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NonLinearOptimizer, Optimizer);

  /**  Types inherited from the superclass */
  typedef Superclass::ParametersType ParametersType;
  typedef Superclass::ScalesType     ScalesType;
protected:
  NonLinearOptimizer() {}
  virtual ~NonLinearOptimizer() {}
private:
  NonLinearOptimizer(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented
};
} // end namespace itk

#endif
