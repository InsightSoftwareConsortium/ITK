#ifndef __itkSPSAOptimizer_cxx
#define __itkSPSAOptimizer_cxx

#include "itkSPSAOptimizer.h"
#include "itkCommand.h"
#include "itkEventObject.h"
#include "itkExceptionObject.h"

#include "vnl/vnl_sample.h"
#include "vnl/vnl_math.h"


namespace itk
{

  /**
   * ************************* Constructor ************************
   */
  SPSAOptimizer
  ::SPSAOptimizer()
  {
    itkDebugMacro( "Constructor" );
    
    m_CurrentIteration = 0;
    m_Maximize = false;
    m_StopCondition = Unknown;
    m_StateOfConvergenceDecayRate = 0.9;
    m_Tolerance=1e-06;
    m_StateOfConvergence = 0;
    m_MaximumNumberOfIterations = 100;
    m_MinimumNumberOfIterations = 10;
    m_GradientMagnitude = 0.0;
    m_NumberOfPerturbations = 1;
    m_LearningRate = 0.0;
    m_a = 1.0;
    m_c = 1.0;
    m_A = m_MaximumNumberOfIterations / 10;
    m_Alpha = 0.602;
    m_Gamma = 0.101;    
    
  } // end Constructor
  
  /**
   * ************************* PrintSelf **************************
   */
  void
  SPSAOptimizer
  ::PrintSelf( std::ostream& os, Indent indent ) const
  {
    Superclass::PrintSelf( os, indent );
    
    os << indent << "a: " << m_a << std::endl;
    os << indent << "A: " << m_A << std::endl;
    os << indent << "Alpha: " << m_Alpha << std::endl;
    os << indent << "c: " << m_c << std::endl;
    os << indent << "Gamma: " << m_Gamma << std::endl;

    os << indent << "NumberOfPerturbations: " << m_NumberOfPerturbations << std::endl;

    os << indent << "LearningRate: "
       << m_LearningRate << std::endl;
    
    os << indent << "MaximumNumberOfIterations: "
       << m_MaximumNumberOfIterations << std::endl;
    os << indent << "MinimumNumberOfIterations: "
       << m_MinimumNumberOfIterations << std::endl;

    os << indent << "Maximize: "
       << m_Maximize << std::endl;
    
    os << indent << "CurrentIteration: "
       << m_CurrentIteration;
    if ( m_CostFunction )
      {
      os << indent << "CostFunction: "
         << m_CostFunction;
      }
    os << indent << "StopCondition: "
       << m_StopCondition;
    os << std::endl;
    
  } // end PrintSelf


  
  /**
   * ***************** GetValue(parameters) *********************
   * Get the cost function value at a position.
   */
  SPSAOptimizer::MeasureType
  SPSAOptimizer
  ::GetValue( const ParametersType & parameters ) const
  {
    /**
     * This method just calls the Superclass' implementation,
     * but is necessary because GetValue(void) is also declared
     * in this class.
     */
    return this->Superclass::GetValue( parameters );
  }
  

  /**
   * ***************** GetValue() ********************************
   * Get the cost function value at the current position.
   */
  SPSAOptimizer::MeasureType
  SPSAOptimizer
  ::GetValue( void ) const
  {
    /**
     * The SPSA does not compute the cost function value at
     * the current position during the optimization, so calculate
     * it on request:
     */
    return this->GetValue( this->GetCurrentPosition() );
  }
  
  /**
   * *********************** StartOptimization ********************
   */
  void
  SPSAOptimizer
  ::StartOptimization(void)
  {    
    itkDebugMacro( "StartOptimization" );
    
    if (!m_CostFunction)
      {
      itkExceptionMacro(<<"No objective function defined! ");
      }

    /** The number of parameters: */
    const unsigned int spaceDimension = 
      m_CostFunction->GetNumberOfParameters();
    if ( spaceDimension != this->GetInitialPosition().GetSize() )
      {
      itkExceptionMacro(<<"Number of parameters not correct!");
      }

    m_CurrentIteration = 0;
    m_StopCondition = Unknown;
    m_StateOfConvergence = 0.0;
        
    this->SetCurrentPosition( this->GetInitialPosition() );
    this->ResumeOptimization();
    
  } // end StartOptimization  
  
  
  /**
   * ********************** ResumeOptimization ********************
   */

  void
  SPSAOptimizer
  ::ResumeOptimization( void )
  {    
    itkDebugMacro( "ResumeOptimization" );
    
    m_Stop = false;
  
    InvokeEvent( StartEvent() );
    while( !m_Stop ) 
      {
      
      AdvanceOneStep();
      this->InvokeEvent( IterationEvent() );

      if (m_Stop)
        {
        break;
        }
            
      m_CurrentIteration++;
      
      if( m_CurrentIteration >= m_MaximumNumberOfIterations )
        {
        m_StopCondition = MaximumNumberOfIterations;
        StopOptimization();
        break;
        }
      
      /** Check convergence */
      if ( (m_StateOfConvergence < m_Tolerance)
           && (m_CurrentIteration >= m_MinimumNumberOfIterations) )
        {
        m_StopCondition = BelowTolerance;
        StopOptimization();
        break;
        }
      m_StateOfConvergence *= m_StateOfConvergenceDecayRate;
      } // while !m_stop
  } // end ResumeOptimization
  
  
  /**
   * ********************** StopOptimization **********************
   */
  void
  SPSAOptimizer
  ::StopOptimization( void )
  {    
    itkDebugMacro( "StopOptimization" );
    m_Stop = true;
    InvokeEvent( EndEvent() );
  } // end StopOptimization
  
  
  /**
   * ********************** AdvanceOneStep ************************
   */
  void
  SPSAOptimizer
  ::AdvanceOneStep( void )
  {    
    itkDebugMacro( "AdvanceOneStep" );
    
    /** Maximize of Minimize the function? */
    double direction;
    if( this->m_Maximize )
      {
      direction = 1.0;
      }
    else
      {
      direction = -1.0;
      }
    
    /** The number of parameters: */
    const unsigned int spaceDimension = 
      m_CostFunction->GetNumberOfParameters();
    
    /** Instantiate the newPosition vector and get the current 
     * parameters */
    ParametersType newPosition( spaceDimension );
    const ParametersType & currentPosition = this->GetCurrentPosition();
    
    /** Compute the gradient as an average of q estimates, where
     * q = m_NumberOfPerturbations
     */
    try
      {
      this->ComputeGradient(currentPosition, m_Gradient);
      }
    catch( ExceptionObject& err )
      {
      // An exception has occurred. 
      // Terminate immediately.
      m_StopCondition = MetricError;
      StopOptimization();
      // Pass exception to caller
      throw err;
      }

    /** Compute the gain a_k */
    const double ak = this->Compute_a( m_CurrentIteration );
    /** And save it for users that are interested */
    m_LearningRate = ak;

    /**
     * Compute the new parameters.
     */
    newPosition = currentPosition + (direction * ak) * m_Gradient;
    this->SetCurrentPosition( newPosition );

    /** Compute the GradientMagnitude (for checking convergence) */
    m_GradientMagnitude = m_Gradient.magnitude();

    /** Update the state of convergence: */  
    m_StateOfConvergence += ak * m_GradientMagnitude;
    
  } // end AdvanceOneStep
  
  
    
  /**
   * ************************** Compute_a *************************
   *
   * This function computes the parameter a at iteration k, as
   * described by Spall.
   */

  double SPSAOptimizer
  ::Compute_a( unsigned long k ) const
  { 
    return static_cast<double>(
                               m_a / vcl_pow( m_A + k + 1, m_Alpha ) );
    
  } // end Compute_a
  
  
  
  /**
   * ************************** Compute_c *************************
   *
   * This function computes the parameter a at iteration k, as
   * described by Spall.
   */
  
  double SPSAOptimizer
  ::Compute_c( unsigned long k ) const
  { 
    return static_cast<double>(
                               m_c / vcl_pow( k + 1, m_Gamma ) );
    
  } // end Compute_c
  
  
  
  /**
   * ********************** GenerateDelta *************************
   *
   * This function generates a perturbation vector delta.
   * Currently the elements are drawn from a bernouilli
   * distribution. (+- 1)
   */

  void SPSAOptimizer
  ::GenerateDelta( const unsigned int spaceDimension )
  { 
    m_Delta = DerivativeType( spaceDimension );
    
    const ScalesType & scales = this->GetScales();

    // Make sure the scales have been set properly
    if (scales.size() != spaceDimension)
      {
      itkExceptionMacro(<< "The size of Scales is "
                        << scales.size()
                        << ", but the NumberOfParameters for the CostFunction is "
                        << spaceDimension
                        << ".");
      }

    for ( unsigned int j = 0; j < spaceDimension; j++ )
      {
      /** Generate randomly -1 or 1. */
      m_Delta[ j ] = 2 * vnl_math_rnd( vnl_sample_uniform(0.0f,1.0f) ) - 1;

      /**
       * Take scales into account. The perturbation of a parameter that has a 
       * large range (and thus is assigned a small scaler) should be higher than
       * the perturbation of a parameter that has a small range.
       */
      m_Delta[j] /= scales[j];
      }

  } // end GenerateDelta
  


  /**
   * *************** ComputeGradient() *****************************
   */
  void 
  SPSAOptimizer::
  ComputeGradient(
                  const ParametersType & parameters,
                  DerivativeType & gradient)
  {

    const unsigned int spaceDimension = parameters.GetSize();

    /** Compute c_k */
    const  double ck = this->Compute_c( m_CurrentIteration );

    /** Instantiate the vectors thetaplus, thetamin,
     * set the gradient to the correct size, and get the scales.
     */
    ParametersType thetaplus( spaceDimension );
    ParametersType thetamin( spaceDimension );
    gradient = DerivativeType( spaceDimension );
    gradient.Fill(0.0);
    const ScalesType & scales = this->GetScales();

    /** Compute the gradient as an average of q estimates, where
     * q = m_NumberOfPerturbations
     */
    for (unsigned long perturbation = 1;
         perturbation <= this->GetNumberOfPerturbations();
         ++perturbation)
      {
      /** Generate a (scaled) perturbation vector m_Delta   */
      this->GenerateDelta( spaceDimension );

      /** Create thetaplus and thetamin */
      for ( unsigned int j = 0; j < spaceDimension; j++ )
        {
        thetaplus[j] = parameters[ j ] + ck * m_Delta[ j ];
        thetamin[j]  = parameters[ j ] - ck * m_Delta[ j ];
        }

      /** Compute the cost function value at thetaplus */
      const double valueplus = this->GetValue( thetaplus );

      /** Compute the cost function value at thetamin */
      const double valuemin = this->GetValue( thetamin );
      
      /** Compute the contribution to the gradient g_k  */
      const double valuediff = ( valueplus - valuemin ) / ( 2 * ck );
      for ( unsigned int j = 0; j < spaceDimension; j++ )
        {
        // remember to divide the gradient by the NumberOfPerturbations!
        gradient[ j ] += valuediff / m_Delta[j];
        }
      } //end for ++perturbation
    
    /** Apply scaling (see below) and divide by the NumberOfPerturbations */
    for ( unsigned int j = 0; j < spaceDimension; j++ )
      {
      gradient[j] /= (scales[j] * static_cast<double>(m_NumberOfPerturbations) );
      }
    /**
     * Scaling was still needed, because the gradient
     * should point along the direction of the applied 
     * perturbation.
     *
     * Recall that we scaled the perturbation vector by dividing each
     * element j by sqrt(scales[j]):
     *   delta'[j] = delta[j] / sqrt(scales[j])
     *             = (+ or -) 1 / sqrt(scales[j])
     * 
     * Consider the case of NumberOfPerturbations=1.
     * If we would not do any scaling the gradient would
     * be computed as:
     *   grad[j] = valuediff / delta'[j]
     *           = valuediff / ( delta[j] / sqrt(scales[j]) )
     *           = sqrt(scales[j] * valuediff / delta[j]
     *           =  (+ or -) valuediff * sqrt(scales[j] 
     *
     * This is wrong, because it gives a vector that points
     * in a different direction than the perturbation. Besides,
     * it would give an opposite effect as expected from the scaling.
     * For rigid registration for example, we choose the scaler for 
     * the rotation 1 and for the translation 1/1000 (see
     * Examples/Registration/ImageRegistration5.cxx), because
     * we want the optimizer to adjust the translation in bigger steps.
     * In the formula above, the grad[translation] would then be SMALLER
     * than grad[rotation], so the optimizer would adjust the translation
     * in smaller steps.
     *
     * To make the gradient point along the perturbation direction we
     * have to divide it by the scales:
     *  grad[j] = (+ or -) valuediff * sqrt(scales[j] / scales[j]
     *          = (+ or -) valuediff / sqrt(scales[j]
     * which is correct. Now the optimizer will take a step
     * in the direction of the perturbation (or the opposite
     * of course, if valuediff is negative). 
     * 
     */

  } //end ComputeGradient


  /**
   * ************* GuessParameters *************************
   */
  void
  SPSAOptimizer::
  GuessParameters(
                  unsigned long numberOfGradientEstimates,
                  double initialStepSize)
  {
    /** Guess A */
    this->SetA( static_cast<double>(this->GetMaximumNumberOfIterations()) / 10.0 );

    if (!m_CostFunction)
      {
      itkExceptionMacro(<<"No objective function defined! ");
      }
  
    /** The number of parameters: */
    const unsigned int spaceDimension = 
      m_CostFunction->GetNumberOfParameters();

    /** Check if the initial position has the correct number of parameters */
    const ParametersType & initialPosition  = this->GetInitialPosition();
    if ( spaceDimension != initialPosition.GetSize() )
      {
      itkExceptionMacro(<<"Number of parameters not correct!");
      }
  
    /** Estimate the maximum absolute element of the initial gradient */
    DerivativeType averageAbsoluteGradient(spaceDimension);
    m_CurrentIteration = 0;
    for (unsigned long n=1; n<= numberOfGradientEstimates; ++n)
      {
      this->ComputeGradient(initialPosition, m_Gradient);
      for ( unsigned int j = 0; j < spaceDimension; j++ )
        {
        averageAbsoluteGradient[j] += fabs(m_Gradient[j]);
        }
      } // end for ++n
    averageAbsoluteGradient /= static_cast<double>(numberOfGradientEstimates);
  
    /** Set a in order to make the first steps approximately have an initialStepSize */
    this->Seta( initialStepSize * vcl_pow(m_A + 1.0, m_Alpha) /
                averageAbsoluteGradient.max_value() );
  
  } //end GuessParameters

  
} // end namespace itk


#endif // end #ifndef __itkSPSAOptimizer_cxx

