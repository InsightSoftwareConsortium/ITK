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
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( SingleValuedCostFunction, CostFunction );

  /**  MeasureType typedef.
   *  It defines a type used to return the cost function value. */
  typedef double                        MeasureType;

  /**  GradientType typedef.
   *  It defines a type used to return the cost function derivative.  */
  typedef Array<double>                 DerivativeType;

 
  /**  ParametersType typedef.
   *  It defines a position in the optimization search space. */
  typedef Superclass::ParametersType         ParametersType;

  /** This method returns the value of the cost function corresponding
    * to the specified parameters    */ 
  virtual MeasureType GetValue( const ParametersType & parameters ) const
      { return NumericTraits< MeasureType >::Zero; }

  /** This method returns the derivative of the cost function corresponding
    * to the specified parameters    */ 
  virtual DerivativeType GetDerivative( const ParametersType & parameters ) const
      { DerivativeType derivative( parameters.Size() );
        derivative.Fill( NumericTraits< MeasureType >::Zero );
        return derivative;  
      }

  /** This method returns the value and derivative of the cost function corresponding
    * to the specified parameters    */ 
  virtual void GetValueAndDerivative( const ParametersType & parameters,
                                            MeasureType & value,
                                            DerivativeType & derivative ) const
      { value = NumericTraits< MeasureType >::Zero; 
        derivative.Fill( NumericTraits< MeasureType >::Zero );
      }


  
protected:
  SingleValuedCostFunction() {};
  virtual ~SingleValuedCostFunction() {};

private:
  SingleValuedCostFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};






} // end namespace itk


#endif



