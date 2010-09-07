/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHeavisideStepFunctionBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkHeavisideStepFunctionBase_h
#define __itkHeavisideStepFunctionBase_h

#include "itkFunctionBase.h"

namespace itk
{
/** \class HeavisideStepFunctionBase
 *
 * \brief Base class of the Heaviside function.
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
template< typename TInput = float, typename TOutput = double >
class HeavisideStepFunctionBase:public FunctionBase< TInput, TOutput >
{
public:
  typedef HeavisideStepFunctionBase       Self;
  typedef FunctionBase< TInput, TOutput > Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  typedef typename Superclass::InputType  InputType;
  typedef typename Superclass::OutputType OutputType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate(const InputType & input) const = 0;

  /** Evaluate the derivative at the specified input position */
  virtual OutputType EvaluateDerivative(const InputType & input) const = 0;

protected:
  HeavisideStepFunctionBase() {}
  virtual ~HeavisideStepFunctionBase() {}
private:
  HeavisideStepFunctionBase(const Self &); //purposely not implemented
  void operator=(const Self &);            //purposely not implemented
};
}

#endif
