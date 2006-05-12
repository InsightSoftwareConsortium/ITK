/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultipleValuedVnlCostFunctionAdaptor.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
  m_ScalesInitialized =false;
  m_Reporter = Object::New();
}    

    
/** Set current parameters scaling. */
void
MultipleValuedVnlCostFunctionAdaptor
::SetScales(const ScalesType & scales)
{
  m_Scales = scales;
  m_ScalesInitialized = true;
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

  ParametersType parameters(inparameters.size());
  // Use scales if they are provided
  if(m_ScalesInitialized)
    { 
    for(unsigned int i=0;i<parameters.size();i++)
      {
      parameters[i] = inparameters[i]/m_Scales[i];
      }
    }
  else
    {
    parameters.SetData(const_cast<double*>(inparameters.data_block()));
    }

   measures = m_CostFunction->GetValue( parameters );

  // Notify observers. This is used for overcoming the limitaion of VNL
  // optimizers of not providing callbacks per iteration.
  m_CachedValue = measures;
  m_CachedCurrentParameters = parameters;
  this->ReportIteration( FunctionEvaluationIterationEvent() ); 

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

  DerivativeType externalGradient;
  ParametersType parameters(inparameters.size());
  if(m_ScalesInitialized)
    {  
    for(unsigned int i=0;i<parameters.size();i++)
      {
      parameters[i] = inparameters[i]/m_Scales[i];
      }
    }
  else
    {
    parameters.SetData(const_cast<double*>(inparameters.data_block()));
    }

  m_CostFunction->GetDerivative( parameters, externalGradient ); 
  ConvertExternalToInternalGradient( externalGradient, gradient);
}
  


/**  Delegate computation of value and gradient to the costfunction.     */
void 
MultipleValuedVnlCostFunctionAdaptor
::compute( const InternalParametersType   & x,
           InternalMeasureType      * ff, 
           InternalDerivativeType   * g   )
{
  // delegate the computation to the CostFunction
  DerivativeType externalGradient;
  ParametersType parameters(x.size());
  if(m_ScalesInitialized)
    {
    for(unsigned int i=0;i<parameters.size();i++)
      {
      parameters[i] = x[i]/m_Scales[i];
      }
    }
  else
    {
    parameters.SetData(const_cast<double*>(x.data_block()));
    }

   *ff = static_cast<InternalMeasureType>(
        m_CostFunction->GetValue( parameters ) );
  m_CostFunction->GetDerivative( parameters, externalGradient );

  ConvertExternalToInternalGradient( externalGradient, *g );  

  // Notify observers. This is used for overcoming the limitaion of VNL
  // optimizers of not providing callbacks per iteration.
  // Note that m_CachedDerivative is already loaded in the GetDerivative() above.
  m_CachedValue = *ff;
  m_CachedCurrentParameters = parameters;
  this->ReportIteration( FunctionAndGradientEvaluationIterationEvent() ); 
 
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
 


/**  This method reports iterations events. It is intended to 
 *   help monitoring the progress of the optimization process. */
void 
MultipleValuedVnlCostFunctionAdaptor
::ReportIteration( const EventObject & event ) const
{
  this->m_Reporter->InvokeEvent( event );
}
 


/**  Connects a Command/Observer to the internal reporter class.
 *   This is useful for reporting iteration event to potential observers. */
unsigned long 
MultipleValuedVnlCostFunctionAdaptor
::AddObserver(const EventObject & event, Command * command) const
{
  return m_Reporter->AddObserver( event, command );
}


/**  Return the cached value of the cost function */
const MultipleValuedVnlCostFunctionAdaptor::MeasureType &
MultipleValuedVnlCostFunctionAdaptor
::GetCachedValue() const
{
  return m_CachedValue;
}


/**  Return the cached value of the cost function derivative */
const MultipleValuedVnlCostFunctionAdaptor::DerivativeType &
MultipleValuedVnlCostFunctionAdaptor
::GetCachedDerivative() const
{
  return m_CachedDerivative;
}

/**  Return the cached value of the parameters used for computing the function */
const MultipleValuedVnlCostFunctionAdaptor::ParametersType &
MultipleValuedVnlCostFunctionAdaptor
::GetCachedCurrentParameters() const
{
  return m_CachedCurrentParameters;
}


} // end namespace itk





