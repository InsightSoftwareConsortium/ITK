/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevenbergMarquardtOptimizerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include <itkLevenbergMarquardtOptimizer.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <itkPoint.h>


typedef vnl_matrix<double> MatrixType;
typedef vnl_vector<double> VectorType;


const double ra = 11.0;
const double rb = 17.0;
const double rc = 29.0;

/** 
 *
 *   This example minimize the equation:
 *
 *   ( a * x + b * y + c ) 
 *  -( 2 * x + 3 * y + 4 )
 *  
 *   for the (a,b,c) parameters
 *
 *   the solution is the vector | 2 3 4 |
 *
 *   (x,y) values are sampled over a rectangular domain
 *   whose size is defined by XRange and YRange
 *
 */ 
class CostFunction : public itk::LightObject
{
public:
  typedef CostFunction Self;
  typedef itk::LightObject  Superclass;
  typedef itk::SmartPointer<Self> Pointer;
  typedef itk::SmartPointer<const Self> ConstPointer;
  itkNewMacro( Self );

  enum { XRange = 1,
         YRange = 1 };   // size of the domain to sample the cost function
         
  enum { SpaceDimension =  3 };
  enum { RangeDimension =  ( 2*XRange+1 ) * ( 2*YRange+1 ) };

  typedef itk::Point<double,SpaceDimension>    ParametersType;
  typedef VectorType                           MeasureType;
  typedef MatrixType                           DerivativeType;

  CostFunction() 
  {

    m_Measure.resize(RangeDimension);
    m_Derivative.resize(SpaceDimension,RangeDimension);
    m_TheoricData.resize(RangeDimension);
    
    // Compute points of the function over a square domain
    unsigned valueindex = 0;
    for( int y = -YRange; y<=YRange; y++ ) 
    {
      const double yd = (double)y;
      for( int x = -XRange; x<=XRange; x++ ) 
      {
        const double xd = (double)x;
        m_TheoricData[valueindex] = ra*xd + rb*yd + rc;
        std::cout << m_TheoricData[valueindex] << "  ";
        valueindex++;
      }
    }

    std::cout << std::endl;

  }

  const ParametersType  & GetParameters(void) const 
  { 
    return m_Parameters;
  }

  const MeasureType & GetValue( const ParametersType & parameters ) 
  {

    m_Parameters = parameters;
    
    std::cout << "GetValue( ";
    double a = m_Parameters[0];
    double b = m_Parameters[1];
    double c = m_Parameters[2];

    std::cout << a << " , ";
    std::cout << b << " , ";
    std::cout << c << ") = ";

    // Compute points of the function over a square domain
    unsigned valueindex = 0;
    for( int y = -YRange; y<=YRange; y++ ) 
    {
      const double yd = (double)y;
      for( int x = -XRange; x<=XRange; x++ ) 
      {
        const double xd = (double)x;
        m_Measure[valueindex]  = a * xd + b * yd + c;
        m_Measure[valueindex] -= m_TheoricData[valueindex];
        std::cout << m_Measure[valueindex] << "  ";
        valueindex++;
      }
    }

    std::cout << std::endl;

    return m_Measure; 
 }

  const DerivativeType &  GetDerivative( 
                 const ParametersType & parameters ) const
  {
 
    m_Parameters = parameters;
    
    std::cout << "GetDerivative( ";
    double a = m_Parameters[0];
    double b = m_Parameters[1];
    double c = m_Parameters[2];

    std::cout << a << " , ";
    std::cout << b << " , ";
    std::cout << c << ") = " << std::endl;

    // Compute points of the function over a square domain
    unsigned valueindex = 0;
    for( int y = -YRange; y<=YRange; y++ ) 
    {
      const double yd = (double)y;
      for( int x = -XRange; x<=XRange; x++ ) 
      {
        const double xd = (double)x;
        m_Derivative[0][valueindex] =  xd;
        m_Derivative[1][valueindex] =  yd;
        m_Derivative[2][valueindex] =  1.0;
        valueindex++;
      }
    }

    for(unsigned int dim1=0; dim1 < SpaceDimension; dim1++)
    {
      std::cout << std::endl;
      for(unsigned int dim2=0; dim2 < RangeDimension; dim2++)
      {
        std::cout << m_Derivative[dim1][dim2] << " ";
      }
    }
    std::cout << std::endl;

    return m_Derivative;
  }

private:

  mutable ParametersType    m_Parameters;
  mutable MeasureType       m_Measure;
  mutable DerivativeType    m_Derivative;
          MeasureType       m_TheoricData;

};



int main() 
{
  std::cout << "Levenberg Marquardt optimizer test \n \n"; 
  
  typedef  itk::LevenbergMarquardtOptimizer< \
                                CostFunction >  OptimizerType;

  typedef  OptimizerType::InternalOptimizerType  vnlOptimizerType;

  
  
  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();


  // Declaration of the CostFunction adaptor
  CostFunction costFunction;


  itkOptimizer->SetCostFunction( &costFunction );

  
  const double F_Tolerance      = 1e-15;  // Function value tolerance
  const double G_Tolerance      = 1e-17;  // Gradient magnitude tolerance 
  const double X_Tolerance      = 1e-16;  // Search space tolerance
  const double Epsilon_Function = 1e-10;  // Step
  const int    Max_Iterations   =    20;  // Maximum number of iterations


  vnlOptimizerType & vnlOptimizer = itkOptimizer->GetOptimizer();

  vnlOptimizer.set_f_tolerance( F_Tolerance );
  vnlOptimizer.set_g_tolerance( G_Tolerance );
  vnlOptimizer.set_x_tolerance( X_Tolerance ); 
  vnlOptimizer.set_epsilon_function( Epsilon_Function );
  vnlOptimizer.set_max_function_evals( Max_Iterations );

  vnlOptimizer.set_trace( true );
  vnlOptimizer.set_verbose( true );
    
  // We start not so far from the solution 
  typedef CostFunction::ParametersType ParametersType;
  ParametersType initialValue;
  initialValue = 100,200,150;

  itkOptimizer->SetInitialPosition( initialValue );

  itkOptimizer->StartOptimization();

  // Error codes taken from vxl/vnl/vnl_nonlinear_minimizer.h
  std::cout << "End condition   = ";
  switch( vnlOptimizer.get_failure_code() )
  {
    case vnl_nonlinear_minimizer::ERROR_FAILURE: 
                      std::cout << " Error Failure"; break;
    case vnl_nonlinear_minimizer::ERROR_DODGY_INPUT: 
                      std::cout << " Error Dogy Input"; break;
    case  vnl_nonlinear_minimizer::CONVERGED_FTOL: 
                      std::cout << " Converged F  Tolerance"; break;
    case  vnl_nonlinear_minimizer::CONVERGED_XTOL: 
                      std::cout << " Converged X  Tolerance"; break;
    case  vnl_nonlinear_minimizer::CONVERGED_XFTOL:
                      std::cout << " Converged XF Tolerance"; break;
    case  vnl_nonlinear_minimizer::CONVERGED_GTOL: 
                      std::cout << " Converged G  Tolerance"; break;
    case  vnl_nonlinear_minimizer::FAILED_TOO_MANY_ITERATIONS:
                      std::cout << " Too many iterations   "; break;
    case  vnl_nonlinear_minimizer::FAILED_FTOL_TOO_SMALL:
                      std::cout << " Failed F Tolerance too small "; break;
    case  vnl_nonlinear_minimizer::FAILED_XTOL_TOO_SMALL:
                      std::cout << " Failed X Tolerance too small "; break;
    case  vnl_nonlinear_minimizer::FAILED_GTOL_TOO_SMALL:
                      std::cout << " Failed G Tolerance too small "; break;
  }
  std::cout << std::endl;
  std::cout << "Number of iters = " << vnlOptimizer.get_num_iterations() << std::endl;
  std::cout << "Number of evals = " << vnlOptimizer.get_num_evaluations() << std::endl;    
  std::cout << std::endl;

  ParametersType finalPosition;
  finalPosition = costFunction.GetParameters();
  std::cout << "Solution        = (";
  std::cout << finalPosition[0] << "," ;
  std::cout << finalPosition[1] << "," ;
  std::cout << finalPosition[2] << ")" << std::endl;  


  //
  // check results to see if it is within range
  //
  bool pass = true;
  double trueParameters[3] = { ra,rb,rc };
  for( unsigned int j = 0; j < CostFunction::SpaceDimension; j++ )
    {
    if( vnl_math_abs( finalPosition[j] - trueParameters[j] ) > 0.01 )
      pass = false;
    }

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}



