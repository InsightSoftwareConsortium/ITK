/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultipleValuedNonLinearVnlOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkMultipleValuedNonLinearVnlOptimizer_h
#define __itkMultipleValuedNonLinearVnlOptimizer_h


#include "vnl/vnl_least_squares_function.h"
#include "itkMultipleValuedNonLinearOptimizer.h"
#include "itkExceptionObject.h"

namespace itk
{
  
/** \class MultipleValuedNonLinearVnlOptimizer
 * \brief This class is a base for the Optimization methods that 
 * optimize a single valued function.
 *
 * \ingroup Numerics
 */

  
template <class TCostFunction>
class ITK_EXPORT MultipleValuedNonLinearVnlOptimizer : 
          public MultipleValuedNonLinearOptimizer< TCostFunction > 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef MultipleValuedNonLinearVnlOptimizer  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef   MultipleValuedNonLinearOptimizer< TCostFunction > Superclass;

  
  /**
   * Dimension of the search space
   */
  enum { 
    SpaceDimension = TCostFunction::SpaceDimension,
    RangeDimension = TCostFunction::RangeDimension
    };


  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * InternalParameters typedef.
   */
  typedef   vnl_vector<double>     InternalParametersType;


  /**
   * InternalMeasure typedef.
   */
  typedef   vnl_vector<double>     InternalMeasureType;


  /**
   * InternalGradient typedef.
   */
  typedef   vnl_matrix<double>     InternalDerivativeType;


  /**
   *  ParametersType typedef.
   *  it defines a position in the optimization search space
   */
  typedef typename TCostFunction::ParametersType    ParametersType;


  /**
   *  MeasureType typedef.
   *  it defines a type used to return the cost function value 
   */
  typedef typename TCostFunction::MeasureType         MeasureType;


  /**
   *  GradientType typedef.
   *  it defines a type used to return the cost function derivative 
   */
  typedef typename TCostFunction::DerivativeType      DerivativeType;


 /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( MultipleValuedNonLinearOptimizer, 
      NonLinearOptimizer );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

 
  /** \class VnlCostFunction
   * \brief Adaptor between the CostFunction and the
   * vnl_least_squares_function classes
   *
   */
  class VnlCostFunctionAdaptor : public vnl_least_squares_function 
  {
  public:
    VnlCostFunctionAdaptor():
        vnl_least_squares_function(SpaceDimension,
                                   RangeDimension,
                                   no_gradient) 
      { m_CostFunction = 0; }    

      void SetCostFunction( TCostFunction * costFunction ) 
        { m_CostFunction = costFunction; }
      

      /** 
       *  Delegate computation of the value to the CostFunction
       */
      virtual void f( const InternalParametersType & parameters, 
                            InternalMeasureType    & output      )
      {
        if( !m_CostFunction )
        {
          throw ExceptionObject();
        }

        ParametersType externalParameters;
        ConvertInternalToExternalParameters( parameters, externalParameters ); 
        ConvertExternalToInternalMeasure( 
                         m_CostFunction->GetValue( externalParameters ),output);
      }
      
      /** 
       *  Delegate computation of the gradient to the CostFunction
       */
      virtual void gradf(const InternalParametersType & parameters,
                               InternalDerivativeType & derivative ) 
      {
        if( !m_CostFunction )
        {
          throw ExceptionObject();
        }

       ParametersType externalParameters; 
       ConvertInternalToExternalParameters( parameters, externalParameters );

        ConvertGradient( m_CostFunction->GetDerivative( externalParameters ),
                         derivative );

      }
      /** 
       *  Delegate computation of value and gradient to the CostFunction
       */
      virtual void compute(const InternalParametersType & x,
                                 InternalMeasureType * f, 
                                 InternalDerivativeType * g ) 
      {
        // delegate the computation to the CostFunction

        ParametersType externalParameters; 
        ConvertInternalToExternalParameters( x, externalParameters );

        ConvertExternalToInternalMeasure(
            m_CostFunction->GetValue( externalParameters ),
            *f);
        
        ConvertGradient( 
            m_CostFunction->GetDerivative( externalParameters ), 
            *g ); 

      }

      /**
       *  Convert internal Parameters (vnl_Vector) 
       *  into VectorContainer type
       */
      static void ConvertInternalToExternalParameters( 
                        const InternalParametersType & input,
                              ParametersType         & output )
      {
        for( unsigned int i=0; i<SpaceDimension; i++)
        {
          output[i] = input[i]; 
        }
      }

      /**
       *  Convert external Parameters VectorContainer 
       *  into internal type (vnl_Vector)
       */
      static void ConvertExternalToInternalParameters(
                        const  ParametersType         & input,
                               InternalParametersType & output )
      {
        for( unsigned int i=0; i<SpaceDimension; i++ ) 
        {
          output[i] = input[i];
        }
      }
 
     
      /**
       *  Convert external Gradient (itkMatrix) 
       *  into internal gradient type (vnl_Matrix)
       */
      void ConvertGradient(const DerivativeType & input,
                           InternalDerivativeType & output )
      {       
        output = input;         
      }


     /**
      *  Convert external Measure (itkPoint) 
      *  into internal Measure type (vnl_Vector)
      */
     void ConvertExternalToInternalMeasure(
                  const MeasureType         & input,
                        InternalMeasureType & output )
      {
        for( unsigned int i=0; i<RangeDimension; i++ )
        {
          output[i] = input[i];
        }
      }

     /**
      *  Convert internal Measure (vnl_vector) 
      *  into external Measure type (itkPoint)
      */
       void ConvertInternalToExternalMeasure(
                    const InternalMeasureType & input,
                          MeasureType         & output )
      {
        for( unsigned int i=0; i<RangeDimension; i++ )
        {
          output[i] = input[i];
        }
      }


  private:
    typename TCostFunction::Pointer   m_CostFunction;
  };  // end of Class CostFunction


  /**
   * Set the cost Function of type TCostFunction
   */
  void SetCostFunction( TCostFunction * costFunction ) 
    { m_CostFunctionAdaptor.SetCostFunction( costFunction ); }
    
  

protected:

  MultipleValuedNonLinearVnlOptimizer();
  virtual ~MultipleValuedNonLinearVnlOptimizer() {};
  MultipleValuedNonLinearVnlOptimizer(const Self&) {}
  void operator=(const Self&) {}

protected:

  VnlCostFunctionAdaptor            m_CostFunctionAdaptor;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMultipleValuedNonLinearVnlOptimizer.txx"
#endif

#endif
