/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAtanRegularizedHeavisideStepFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkAtanRegularizedHeavisideStepFunction_h
#define __itkAtanRegularizedHeavisideStepFunction_h

#include "itkRegularizedHeavisideStepFunction.h"

namespace itk
{
/** \class AtanRegularizedHeavisideStepFunction
 *
 * \brief Atan-based implementation of the Regularized (smoothed) Heaviside functions.
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
class AtanRegularizedHeavisideStepFunction:
  public RegularizedHeavisideStepFunction< TInput, TOutput >
{
public:
  typedef AtanRegularizedHeavisideStepFunction                Self;
  typedef RegularizedHeavisideStepFunction< TInput, TOutput > Superclass;
  typedef SmartPointer< Self >                                Pointer;
  typedef SmartPointer< const Self >                          ConstPointer;

  itkNewMacro(Self);

  itkTypeMacro(AtanRegularizedHeavisideStepFunction, RegularizedHeavisideStepFunction);

  typedef typename Superclass::InputType  InputType;
  typedef typename Superclass::OutputType OutputType;
  typedef typename Superclass::RealType   RealType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate(const InputType & input) const
  {
    return 0.5 + ( vnl_math::one_over_pi * vcl_atan( input * this->GetOneOverEpsilon() ) );
  }

  /** Evaluate the derivative at the specified input position */
  virtual OutputType EvaluateDerivative(const InputType & input) const
  {
    const RealType t = ( input * this->GetOneOverEpsilon() );

    return static_cast< OutputType >( vnl_math::one_over_pi / ( 1.0 + t * t ) );
  }

protected:
  AtanRegularizedHeavisideStepFunction() {}
  virtual ~AtanRegularizedHeavisideStepFunction() {}
private:
  AtanRegularizedHeavisideStepFunction(const Self &); //purposely not
                                                      // implemented
  void operator=(const Self &);                       //purposely not
                                                      // implemented
};
}

#endif
