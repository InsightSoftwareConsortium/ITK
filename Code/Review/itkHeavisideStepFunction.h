/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHeavisideStepFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkHeavisideStepFunction_h
#define __itkHeavisideStepFunction_h

#include "itkHeavisideStepFunctionBase.h"

namespace itk
{
/** \class HeavisideStepFunction
 *
 * \brief Implementation of the classical Heaviside step function.
 *
 * The Heaviside Step function is a piece-wise function:
 *
 *     http://en.wikipedia.org/wiki/Heaviside_step_function
 *
 *
 * \author Mosaliganti K., Smith B., Gelas A., Gouaillard A., Megason S.
 *
 *  This code was taken from the Insight Journal paper:
 *
 *      "Cell Tracking using Coupled Active Surfaces for Nuclei and Membranes"
 *      http://www.insight-journal.org/browse/publication/642
 *      http://hdl.handle.net/10380/3055
 *
 *  That is based on the papers:
 *
 *      "Level Set Segmentation: Active Contours without edge"
 *      http://www.insight-journal.org/browse/publication/322
 *      http://hdl.handle.net/1926/1532
 *
 *      and
 *
 *      "Level set segmentation using coupled active surfaces"
 *      http://www.insight-journal.org/browse/publication/323
 *      http://hdl.handle.net/1926/1533
 *
 *
 */
template< class TInput = float, class TOutput = double >
class HeavisideStepFunction:
  public HeavisideStepFunctionBase< TInput, TOutput >
{
public:
  typedef HeavisideStepFunction                        Self;
  typedef HeavisideStepFunctionBase< TInput, TOutput > Superclass;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;

  itkNewMacro(Self);

  itkTypeMacro(HeavisideStepFunction, HeavisideStepFunctionBase);

  typedef typename Superclass::InputType  InputType;
  typedef typename Superclass::OutputType OutputType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate(const InputType & input) const
  {
    return ( input >= 0.0 ) ? 1.0 : 0.0;
  }

  /** Evaluate the derivative at the specified input position */
  virtual OutputType EvaluateDerivative(const InputType & input) const
  {
    return ( input == 0.0 ) ? 1.0 : 0.0;
  }

protected:
  HeavisideStepFunction() {}
  virtual ~HeavisideStepFunction() {}
private:
  HeavisideStepFunction(const Self &); //purposely not implemented
  void operator=(const Self &);        //purposely not implemented
};
}

#endif
