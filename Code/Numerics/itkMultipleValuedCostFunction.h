/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultipleValuedCostFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMultipleValuedCostFunction_h
#define __itkMultipleValuedCostFunction_h

#include "itkCostFunction.h"
#include "itkArray.h"
#include "itkArray2D.h"
#include "itkObjectFactory.h"
#include "itkNumericTraits.h"


namespace itk
{
  
/** \class MultipleValuedCostFunction
 * \brief This class is a base for the CostFunctions returning a 
 * multiple values
 *
 * \ingroup Numerics Optimizers
 */
class ITK_EXPORT MultipleValuedCostFunction : 
    public CostFunction 
{
public:
  /** Standard class typedefs. */
  typedef MultipleValuedCostFunction     Self;
  typedef CostFunction                 Superclass;
  typedef SmartPointer<Self>           Pointer;
  typedef SmartPointer<const Self>     ConstPointer;
 
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( MultipleValuedCostFunction, CostFunction );

 
  /**  ParametersType typedef.
   *  It defines a position in the optimization search space. */
  typedef Superclass::ParametersType         ParametersType;


  /**  MeasureType typedef.
   *  It defines a type used to return the cost function value. */
  typedef Array<double>                 MeasureType;

  /**  GradientType typedef.
   *  It defines a type used to return the cost function derivative.  */
  typedef Array2D<double>               DerivativeType;

  

  /** This method returns the value of the cost function corresponding
    * to the specified parameters     
    * This method MUST be overloaded by derived classes   */
  virtual MeasureType GetValue( const ParametersType & parameters ) const = 0;

  /** Return the number of values that are computed by the
   *  multivalued cost function.     
   *  This method MUST be overloaded by derived classes */
  virtual unsigned int GetNumberOfValues(void) const  = 0;


  /** This method returns the derivative of the cost function corresponding
    * to the specified parameters     
    * This method MUST be overloaded by derived classes   */
  virtual void GetDerivative( const ParametersType & parameters,
                              DerivativeType & derivative ) const = 0;
      

  
protected:
  MultipleValuedCostFunction() {};
  virtual ~MultipleValuedCostFunction() {};


private:
  MultipleValuedCostFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented


};






} // end namespace itk


#endif



