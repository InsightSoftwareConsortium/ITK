/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultipleValuedVnlCostFunctionAdaptor.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkMultipleValuedVnlCostFunctionAdaptor.h"
#include "itkExceptionObject.h"


namespace itk
{
  
/**  Constructor.  */
MultipleValuedVnlCostFunctionAdaptor 
::MultipleValuedVnlCostFunctionAdaptor(
        unsigned int spaceDimension, unsigned int numberOfValues ):
               vnl_least_squares_function(spaceDimension,numberOfValues) 
{ 
}    

    


/**  Delegate computation of the value to the CostFunction. */
void
MultipleValuedVnlCostFunctionAdaptor
::f( const InternalParametersType & inparameters, 
           InternalMeasureType    & measures        )
{
  if( !m_CostFunction )
    {
    ExceptionObject ex;
    ex.SetLocation(__FILE__);
    ex.SetDescription("Attempt to use a MultipleValuedVnlCostFunctionAdaptor without any CostFunction plugged in");
    throw ex;
    }

  ParametersType parameters( inparameters.size() );
  ConvertInternalToExternalParameters( inparameters, parameters );

  InternalMeasureType values = 
                        m_CostFunction->GetValue( parameters );

  measures = values;

}
  


/**  Delegate computation of the gradient to the costfunction.  */
void 
MultipleValuedVnlCostFunctionAdaptor
::gradf(  const InternalParametersType   & inparameters,
                InternalDerivativeType   & gradient       ) 
{
  if( !m_CostFunction )
    {
    ExceptionObject ex;
    ex.SetLocation(__FILE__);
    ex.SetDescription("Attempt to use a MultipleValuedVnlCostFunctionAdaptor without any CostFunction plugged in");
    throw ex;
    }

  ParametersType parameters( inparameters.size() );
  ConvertInternalToExternalParameters( inparameters, parameters );
  DerivativeType externalGradient;
  m_CostFunction->GetDerivative( parameters, externalGradient );
  ConvertExternalToInternalGradient( externalGradient, gradient);

}
  


/**  Delegate computation of value and gradient to the costfunction.     */
void 
MultipleValuedVnlCostFunctionAdaptor
::compute( const InternalParametersType   & x,
                 InternalMeasureType      * f, 
                 InternalDerivativeType   * g   )
{
  // delegate the computation to the CostFunction
  ParametersType parameters( x.size() );
  ConvertInternalToExternalParameters( x, parameters );

  *f = static_cast<InternalMeasureType>(
                      m_CostFunction->GetValue( parameters ) );

  DerivativeType externalGradient;
  m_CostFunction->GetDerivative( parameters, externalGradient );

  ConvertExternalToInternalGradient( externalGradient, *g );    
}



/**  Convert internal Parameters into external type.  */
void 
MultipleValuedVnlCostFunctionAdaptor
::ConvertInternalToExternalParameters( const InternalParametersType & input,
                                             ParametersType         & output )
{
  const unsigned int size = input.size();
  for( unsigned int i=0; i<size; i++ )
    {
    output[i] = input[i]; 
    }
}




/**  Convert external Parameters into internal type  */
void 
MultipleValuedVnlCostFunctionAdaptor
::ConvertExternalToInternalParameters( const  ParametersType         & input,
                                              InternalParametersType & output )
{
  const unsigned int size = input.size();
  for( unsigned int i=0; i<size; i++ ) 
    {
    output[i] = input[i];
    }
}
  

/**  Convert external derviative measures into internal type  */
void 
MultipleValuedVnlCostFunctionAdaptor
::ConvertExternalToInternalGradient( const DerivativeType         & input,
                                           InternalDerivativeType & output )
{
  const unsigned int rows = input.rows();
  const unsigned int cols = input.cols();
  for( unsigned int i=0; i<rows; i++ ) 
    {
    for( unsigned int j=0; j<cols; j++ ) 
      {
      output(j,i) = input(i,j);
      }
    }
}



/**  Convert external Measures into internal type  */
void 
MultipleValuedVnlCostFunctionAdaptor
::ConvertExternalToInternalMeasures( const  MeasureType         & input,
                                            InternalMeasureType & output )
{
  const unsigned int size = input.size();
  for( unsigned int i=0; i<size; i++ ) 
    {
    output[i] = input[i];
    }
}
  

/**  Define if the cost function will provide a Gradient computation */
void 
MultipleValuedVnlCostFunctionAdaptor
::SetUseGradient( bool useGradient )
{
  // delegate the task to the base class
  this->vnl_least_squares_function::use_gradient_ = useGradient;

}
 


/**  Return true if the cost function will provide a Gradient computation */
bool 
MultipleValuedVnlCostFunctionAdaptor
::GetUseGradient() const
{
  // delegate the task to the base class
  return this->vnl_least_squares_function::has_gradient();

}
 


} // end namespace itk





