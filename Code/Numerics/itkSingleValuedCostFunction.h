/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSingleValuedCostFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSingleValuedCostFunction_h
#define __itkSingleValuedCostFunction_h

#include "itkCostFunction.h"
#include "itkArray.h"
#include "itkObjectFactory.h"
#include "itkNumericTraits.h"


namespace itk
{
  
/** \class SingleValuedCostFunction
 * \brief This class is a base for the CostFunctions returning a 
 * single value
 *
 * \ingroup Numerics Optimizers
 */
class ITK_EXPORT SingleValuedCostFunction : 
    public CostFunction 
{
public:
  /** Standard class typedefs. */
  typedef SingleValuedCostFunction     Self;
  typedef CostFunction                 Superclass;
  typedef SmartPointer<Self>           Pointer;
  typedef SmartPointer<const Self>     ConstPointer;
   
  /** Run-time type information (and related methods). */
  itkTypeMacro( SingleValuedCostFunction, CostFunction );

  /**  MeasureType typedef.
   *  It defines a type used to return the cost function value. */
  typedef double                        MeasureType;

  /** DerivativeType typedef.
   *  It defines a type used to return the cost function derivative.  */
  typedef Array<double>                 DerivativeType;
 
  /**  ParametersType typedef.
   *  It defines a position in the optimization search space. */
  typedef Superclass::ParametersType         ParametersType;

  /** This method returns the value of the cost function corresponding
    * to the specified parameters.    */ 
  virtual MeasureType GetValue( const ParametersType & parameters ) const = 0;

  /** This method returns the derivative of the cost function corresponding
    * to the specified parameters.   */ 
  virtual void GetDerivative( const ParametersType & parameters,
                              DerivativeType & derivative ) const = 0;

  /** This method returns the value and derivative of the cost function corresponding
    * to the specified parameters    */ 
  virtual void GetValueAndDerivative( const ParametersType & parameters,
                                      MeasureType & value,
                                      DerivativeType & derivative ) const 
  {
    value = this->GetValue( parameters );
    this->GetDerivative( parameters, derivative );
  };
  
protected:
  SingleValuedCostFunction() {};
  virtual ~SingleValuedCostFunction() {};

private:
  SingleValuedCostFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};






} // end namespace itk


#endif



