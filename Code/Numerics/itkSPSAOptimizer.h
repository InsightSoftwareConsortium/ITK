#ifndef __itkSPSAOptimizer_h
#define __itkSPSAOptimizer_h

#include "itkSingleValuedNonLinearOptimizer.h"


namespace itk
{
  
  /**
   * \class SPSAOptimizer
   * \brief An optimizer based on simultaneous perturbation...
   *
   * This optimizer is an implementation of the Simultaneous
   * Perturbation Stochastic Approximation method, described in:
   * 
   * - http://www.jhuapl.edu/SPSA/
   *
   * - Spall, J.C. (1998), "An Overview of the Simultaneous
   * Perturbation Method for Efficient Optimization," Johns
   * Hopkins APL Technical Digest, vol. 19, pp. 482-492
   *
   * \ingroup Optimizers
   */
  
  class SPSAOptimizer
    : public SingleValuedNonLinearOptimizer
  {
   public:
    
    /** Standard class typedefs. */
    typedef SPSAOptimizer                       Self;
    typedef SingleValuedNonLinearOptimizer      Superclass;
    typedef SmartPointer<Self>                  Pointer;
    typedef SmartPointer<const Self>             ConstPointer;
    
    /** Method for creation through the object factory. */
    itkNewMacro( Self );
    
    /** Run-time type information (and related methods). */
    itkTypeMacro( SPSAOptimizer, SingleValuedNonLinearOptimizer );
    
    /** Codes of stopping conditions */
    typedef enum {
      Unknown,
      MaximumNumberOfIterations,
      BelowTolerance,
      MetricError  } StopConditionType;
    
    /** Advance one step following the gradient direction. */
    virtual void AdvanceOneStep( void );
    
    /** Start optimization. */
    void StartOptimization( void );
    
    /** Resume previously stopped optimization with current parameters
     * \sa StopOptimization. */
    void ResumeOptimization( void );
    
    /** Stop optimization.
     * \sa ResumeOptimization */
    void StopOptimization( void );
    
    /** Get the cost function value at the current position. */
    virtual MeasureType GetValue( void ) const;

    /** Get the cost function value at any position */
    virtual MeasureType GetValue( const ParametersType & parameters ) const;

    /** Guess the parameters a and A. This function needs the 
     * number of GradientEstimates used for estimating a and A and 
     * and the expected initial step size (where step size is
     * defined as the maximum of the absolute values of the 
     * parameter update). Make sure you set c, Alpha, Gamma, 
     * the MaximumNumberOfIterations, the Scales, and the 
     * the InitialPosition before calling this method.
     *
     * Described in:
     * Spall, J.C. (1998), "Implementation of the Simultaneous Perturbation 
     * Algorithm for Stochastic Optimization", IEEE Trans. Aerosp. Electron.
     * Syst. 34(3), 817-823.
     */
    virtual void GuessParameters(
                                 unsigned long numberOfGradientEstimates,
                                 double initialStepSize);

    /** Get the current iteration number. */
    itkGetConstMacro( CurrentIteration, unsigned long );
    
    /** Get Stop condition. */
    itkGetConstMacro( StopCondition, StopConditionType );

    /** Get the current LearningRate (a_k) */
    itkGetConstMacro( LearningRate, double);

    /** Get the GradientMagnitude of the latest computed gradient */
    itkGetConstMacro( GradientMagnitude, double);
    
    /** Get the latest computed gradient */
    itkGetConstReferenceMacro( Gradient, DerivativeType);

    /** Set/Get a. */
    itkSetMacro( a, double );
    itkGetConstMacro( a, double );
    
    /** Set/Get c. */
    itkSetMacro( c, double );
    itkGetConstMacro( c, double );
    
    /** Set/Get A. */
    itkSetMacro( A, double );
    itkGetConstMacro( A, double );
    
    /** Set/Get alpha. */
    itkSetMacro( Alpha, double );
    itkGetConstMacro( Alpha, double );
    
    /** Set/Get gamma. */
    itkSetMacro( Gamma, double );
    itkGetConstMacro( Gamma, double );

    /** Methods to configure the cost function. */
    itkGetConstMacro( Maximize, bool );
    itkSetMacro( Maximize, bool );
    itkBooleanMacro( Maximize );
    bool GetMinimize( ) const
    { return !m_Maximize; }
    void SetMinimize(bool v)
    { this->SetMaximize(!v); }
    void MinimizeOn()
    { this->MaximizeOff(); }
    void MinimizeOff()
    { this->MaximizeOn(); }

    /** Set/Get the number of perturbation used to construct
     * a gradient estimate g_k.
     * q = NumberOfPerturbations
     * g_k = 1/q sum_{j=1..q} g^(j)_k
     */
    itkSetMacro( NumberOfPerturbations, unsigned long );
    itkGetConstMacro( NumberOfPerturbations, unsigned long );


    /**
     * Get the state of convergence in the last iteration. When the
     * StateOfConvergence is lower than the Tolerance, and the minimum
     * number of iterations has been performed, the optimization
     * stops.
     * 
     * The state of convergence (SOC) is initialized with 0.0 and 
     * updated after each iteration as follows:
     *   SOC *= SOCDecayRate
     *   SOC += a_k * GradientMagnitude
     */
    itkGetConstMacro( StateOfConvergence, double );

    /** Set/Get StateOfConvergenceDecayRate (number between 0 and 1). */
    itkSetMacro( StateOfConvergenceDecayRate, double );
    itkGetConstMacro( StateOfConvergenceDecayRate, double );

    /** Set/Get the minimum number of iterations */
    itkSetMacro( MinimumNumberOfIterations, unsigned long);
    itkGetConstMacro( MinimumNumberOfIterations, unsigned long);
    
    /** Set/Get the maximum number of iterations. */
    itkSetMacro( MaximumNumberOfIterations, unsigned long );
    itkGetConstMacro( MaximumNumberOfIterations, unsigned long );
    
    /** Set/Get Tolerance */
    itkSetMacro(Tolerance, double);
    itkGetConstMacro(Tolerance, double);
    
   protected:

    SPSAOptimizer();
    virtual ~SPSAOptimizer() {};

    /** PrintSelf method.*/
    void PrintSelf( std::ostream& os, Indent indent ) const;
    
    /** Variables updated during optimization */
    DerivativeType               m_Gradient; 
    double                       m_LearningRate;
    DerivativeType               m_Delta;
    bool                         m_Stop;
    StopConditionType            m_StopCondition;
    double                       m_StateOfConvergence;
    unsigned long                m_CurrentIteration;
    
    /** Method to compute the learning rate at iteration k (a_k).*/
    virtual double Compute_a( unsigned long k ) const;

    /**
     * Method to compute the gain factor for the perturbation
     * at iteration k (c_k).
     */
    virtual double Compute_c( unsigned long k ) const;
    
    /** Method to generate a perturbation vector. Takes scales into account. */
    virtual void GenerateDelta( const unsigned int spaceDimension );

    /** 
     * Compute the gradient at a position. m_NumberOfPerturbations are used, 
     * and scales are taken into account.
     */
    virtual void ComputeGradient(
                                 const ParametersType & parameters,
                                 DerivativeType & gradient);
    
   private:

    SPSAOptimizer( const Self& );    // purposely not implemented
    void operator=( const Self& );  // purposely not implemented
    
    /** Settings.*/
    unsigned long                 m_MinimumNumberOfIterations;
    unsigned long                 m_MaximumNumberOfIterations;
    double                        m_StateOfConvergenceDecayRate;
    double                        m_Tolerance;
    bool                          m_Maximize;
    double                        m_GradientMagnitude;
    unsigned long                 m_NumberOfPerturbations;
    
    /** Parameters, as described by Spall.*/
    double                        m_a;
    double                        m_c;
    double                        m_A;
    double                        m_Alpha;
    double                        m_Gamma;
    
  }; // end class SPSAOptimizer

} // end namespace itk

#endif // end #ifndef __itkSPSAOptimizer_h

