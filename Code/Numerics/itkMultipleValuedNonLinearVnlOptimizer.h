/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultipleValuedNonLinearVnlOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkMultipleValuedNonLinearOptimizer_h
#define __itkMultipleValuedNonLinearOptimizer_h

#include "itkNonLinearOptimizer.h"
#include "vnl/vnl_least_squares_function.h"
#include "itkExceptionObject.h"

namespace itk
{
  
/** \class MultipleValuedNonLinearOptimizer
 * \brief This class is a base for the Optimization methods that 
 * optimize a single valued function.
 *
 */

  
template <class TCostFunction>
class ITK_EXPORT MultipleValuedNonLinearVnlOptimizer : 
      public NonLinearOptimizer< typename TCostFunction::ParametersType > 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef MultipleValuedNonLinearVnlOptimizer  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef   NonLinearOptimizer<typename TCostFunction::ParametersType> Superclass;

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
   * Parameters typedef.
   */
  typedef typename TCostFunction::ParametersType    ParametersType;

  /**
   * Parameters Pointer
   */
  typedef ParametersType::Pointer ParametersPointer;

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
    /**
   * MeasureType typedef.
   */
  typedef typename TCostFunction::VectorMeasureType       MeasureType;


  /**
   * GradientType typedef.
   */
  typedef typename TCostFunction::DerivativeType      DerivativeType;


  class VnlCostFunctionAdaptor : public vnl_least_squares_function 
  {
  public:
    VnlCostFunctionAdaptor():
        vnl_least_squares_function(TCostFunction::SpaceDimension,
                                   TCostFunction::RangeDimension) 
      { m_CostFunction = 0; }    

      void SetCostFunction( TCostFunction * costFunction ) 
        { m_CostFunction = costFunction; }
      

      /** 
       *  Delegate computation of the value to the CostFunction
       */
      virtual void f( const InternalParametersType & parameters, InternalMeasureType & output ) {
        if( !m_CostFunction )
        {
          throw ExceptionObject();
        }
        ConvertParameters( parameters); // should be transfertparameters
        MeasureType::Pointer externalOutput = MeasureType::New();
        externalOutput->Reserve(output.size());
        ConvertMeasure(output,externalOutput);
        m_CostFunction->GetValue( externalOutput);
        ConvertMeasure(externalOutput,output);
      }
      
      /** 
       *  Delegate computation of the gradient to the CostFunction
       */
      virtual void gradf(const InternalParametersType & parameters,
                               InternalDerivativeType & derivative ) {
        if( !m_CostFunction )
        {
          throw ExceptionObject();
        }
        
       ConvertParameters( parameters );
       DerivativeType::Pointer externalDerivative = DerivativeType::New();
             externalDerivative = m_CostFunction->GetDerivative();

        ConvertGradient( externalDerivative, derivative );  
      }
      
      /** 
       *  Delegate computation of value and gradient to the CostFunction
       */
      virtual void compute(const InternalParametersType & x,
                                 InternalMeasureType * f, 
                                 InternalDerivativeType * g ) {
        // delegate the computation to the CostFunction

        ConvertParameters( x );
        MeasureType::Pointer externalMeasure = MeasureType::New();
        externalMeasure->Reserve(f->size());
        ConvertMeasure(*f,externalMeasure);
        m_CostFunction->GetValue( externalMeasure);
        ConvertMeasure(externalMeasure,*f);

        DerivativeType::Pointer externalGradient = 
                                      m_CostFunction->GetDerivative();

        ConvertGradient( externalGradient, *g ); 
      }
     
      /**
       *  Convert internal Parameters (vnl_Vector) 
       *  into external type (VectorContainer)
       */
      void ConvertParameters( const InternalParametersType & input )
      {
        ParametersType::Pointer output = m_CostFunction->GetParameters();
        ParametersType::Iterator it = output->Begin(); 
        unsigned int i=0;
        while( it != output->End() )
        {
          it.Value() = input[i]; 
          it++;
          i++;
        }
      }


      /**
       *  Convert external Parameters (VectorContainer) 
       *  into internal parameters type (vnl_Vector)
       */
      static void ConvertParameters(ParametersType::Pointer & input,
                                    InternalParametersType & output )
      {
        const unsigned size = input->Size();
        if( output.size() != size ) 
        {
          output.resize( size );
        }

        ParametersType::ConstIterator it;
        it = input->Begin(); 
        for(unsigned int i=0; i< size; i++)
        {
          output[i] = it.Value();
          it++;
        }

      }
      
      /**
       *  Convert external Gradient (VectorContainer) 
       *  into internal gradient type (vnl_Matrix)
       */
      void ConvertGradient(const DerivativeType::Pointer & input,
                           InternalDerivativeType & output )
      {
        const unsigned size = input->Size();
        if( output.size() != size ) 
        {
          output.resize( size , size);
        }
        unsigned int i=0;

        DerivativeType::Iterator it = input->Begin();
        for(unsigned int i=0; i<(size/2); i++)
          for(unsigned int j=0; j<(size/2); j++)
            {
              output[i][j] = it.Value();
              it++;
            }
          
      }

     /**
      *  Convert external Measure (VectorContainer) 
      *  into internal Measure type (vnl_Vector)
      */
     void ConvertMeasure(const MeasureType::Pointer & input,
                           InternalMeasureType & output )
      {
        const unsigned size = input->Size();
        if( output.size() != size ) 
        {
          output.resize( size );
        }
        unsigned int i=0;
        MeasureType::Iterator it = input->Begin();
        while( it != input->End() )
        {
          output[i] = it.Value();
          i++;
          it++;
        }
      }

     /**
      *  Convert internal Measure (vnl_vector) 
      *  into external Measure type (VectorContainer)
      */
       void ConvertMeasure(const InternalMeasureType & input,
                           MeasureType::Pointer & output )
      {
        const unsigned size = input.size();
        if( output->Size() != size ) 
        { 
          output->Reserve( size );
          output->Squeeze();
        }
        unsigned int i=0;
        MeasureType::Iterator it = output->Begin();

        while( it != output->End() )
        {
          it.Value() = input[i];
          i++;
          it++;
        }

      }


  private:
    TCostFunction::Pointer   m_CostFunction;
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



