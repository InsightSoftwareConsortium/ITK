/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevenbergMarquardtOptimizerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <itkLevenbergMarquardtOptimizer.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_math.h>
#include <itkArray.h>
#include <itkArray2D.h>



typedef vnl_matrix<double> MatrixType;
typedef vnl_vector<double> VectorType;


const double ra = 11.0;
const double rb = 17.0;
const double rc = 29.0;

/** 
 *
 *   This example minimize the equation:
 *
 *   sum { [   (  a * x +  b * y +  c ) 
 *            -( 11 * x + 17 * y + 29 ) ] ^ 2  }
 *  
 *   for the (a,b,c) parameters
 *
 *   the solution is the vector |  11  17  29  |
 *
 *   (x,y) values are sampled over a rectangular region
 *   whose size is defined by XRange and YRange
 *
 */ 
class LMCostFunction : public itk::MultipleValuedCostFunction
{
public:
  typedef LMCostFunction                    Self;
  typedef itk::MultipleValuedCostFunction   Superclass;
  typedef itk::SmartPointer<Self>           Pointer;
  typedef itk::SmartPointer<const Self>     ConstPointer;
  itkNewMacro( Self );

  enum { XRange = 2,
         YRange = 2 };   // size of the region to sample the cost function
         
  enum { SpaceDimension =  3 };
  enum { RangeDimension =  ( 2*XRange+1 ) * ( 2*YRange+1 ) };

  typedef Superclass::ParametersType              ParametersType;
  typedef Superclass::DerivativeType              DerivativeType;
  typedef Superclass::MeasureType                 MeasureType;

  LMCostFunction():
            m_Measure(RangeDimension),
            m_Derivative(SpaceDimension,RangeDimension),
            m_TheoreticalData(SpaceDimension)  
  {

    m_Measure.resize(RangeDimension);
    m_Derivative.resize(SpaceDimension,RangeDimension);
    m_TheoreticalData.resize(RangeDimension);
    
    // Compute points of the function over a square region
    unsigned valueindex = 0;
    for( int y = -YRange; y<=YRange; y++ ) 
      {
      const double yd = (double)y;
      for( int x = -XRange; x<=XRange; x++ ) 
        {
        const double xd = (double)x;
        m_TheoreticalData[valueindex] = ra*xd + rb*yd + rc;
        valueindex++;
        }
      }
  }


  MeasureType GetValue( const ParametersType & parameters ) const
  {
  
    std::cout << "GetValue( ";
    double a = parameters[0];
    double b = parameters[1];
    double c = parameters[2];

    std::cout << a << " , ";
    std::cout << b << " , ";
    std::cout << c << ")  " << std::endl;

    // Compute points of the function over a square region
    unsigned valueindex = 0;
    for( int y = -YRange; y<=YRange; y++ ) 
      {
      const double yd = (double)y;
      for( int x = -XRange; x<=XRange; x++ ) 
        {
        const double xd = (double)x;
        double value = a * xd + b * yd + c;
        value       -= m_TheoreticalData[valueindex];
        m_Measure[valueindex] = value;
        valueindex++;
        }
      }

    return m_Measure; 
 }

  void GetDerivative( const ParametersType & parameters,
                            DerivativeType  & derivative ) const
  {
    
    std::cout << "GetDerivative( ";
    double a = parameters[0];
    double b = parameters[1];
    double c = parameters[2];

    std::cout << a << " , ";
    std::cout << b << " , ";
    std::cout << c << ") " << std::endl;

    // Compute points of the function over a square region
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

    derivative = m_Derivative;

  }

  unsigned int GetNumberOfParameters(void) const
  {
    return SpaceDimension;
  }

  unsigned int GetNumberOfValues(void) const
  {
    return RangeDimension;
  }

private:

  mutable MeasureType       m_Measure;
  mutable DerivativeType    m_Derivative;
          MeasureType       m_TheoreticalData;

};



int itkRunLevenbergMarquardOptimization( bool useGradient, 
                double fTolerance, double gTolerance, double xTolerance, 
                double epsilonFunction, int maxIterations )
{
  std::cout << "Levenberg Marquardt optimizer test \n \n"; 
  
  typedef  itk::LevenbergMarquardtOptimizer  OptimizerType;

  typedef  OptimizerType::InternalOptimizerType  vnlOptimizerType;
  
  // Declaration of a itkOptimizer
  OptimizerType::Pointer  Optimizer = OptimizerType::New();

  // Declaration of the CostFunction adaptor
  LMCostFunction::Pointer costFunction = LMCostFunction::New();

  typedef LMCostFunction::ParametersType ParametersType;
  ParametersType  parameters(LMCostFunction::SpaceDimension);
  costFunction->GetValue(parameters);
  
  std::cout << "Number of Values = " << costFunction->GetNumberOfValues() << "\n";
 
  try 
    {
    Optimizer->SetCostFunction( costFunction.GetPointer() );
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error ocurred during Optimization" << std::endl;
    std::cout << e << std::endl;
    return EXIT_FAILURE;
    }
  
  // this following call is equivalent to invoke: costFunction->SetUseGradient( useGradient );
  Optimizer->SetUseCostFunctionGradient( useGradient );

  vnlOptimizerType * vnlOptimizer = Optimizer->GetOptimizer();

  vnlOptimizer->set_f_tolerance( fTolerance );
  vnlOptimizer->set_g_tolerance( gTolerance );
  vnlOptimizer->set_x_tolerance( xTolerance ); 
  vnlOptimizer->set_epsilon_function( epsilonFunction );
  vnlOptimizer->set_max_function_evals( maxIterations );

  // We start not so far from the solution 
  typedef LMCostFunction::ParametersType ParametersType;
  ParametersType  initialValue(LMCostFunction::SpaceDimension);

  initialValue[0] = 200;
  initialValue[1] = 300;
  initialValue[2] = 400;

  OptimizerType::ParametersType currentValue(LMCostFunction::SpaceDimension);

  currentValue = initialValue;

  Optimizer->SetInitialPosition( currentValue );

  try 
    {
    Optimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << "Exception thrown ! " << std::endl;
    std::cerr << "An error ocurred during Optimization" << std::endl;
    std::cerr << "Location    = " << e.GetLocation()    << std::endl;
    std::cerr << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }


  // Error codes taken from vxl/vnl/vnl_nonlinear_minimizer.h
  std::cout << "End condition   = ";
  switch( vnlOptimizer->get_failure_code() )
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
  std::cout << "Number of iters = " << vnlOptimizer->get_num_iterations() << std::endl;
  std::cout << "Number of evals = " << vnlOptimizer->get_num_evaluations() << std::endl;    
  std::cout << std::endl;


  OptimizerType::ParametersType finalPosition;
  finalPosition = Optimizer->GetCurrentPosition();

  std::cout << "Solution        = (";
  std::cout << finalPosition[0] << "," ;
  std::cout << finalPosition[1] << "," ;
  std::cout << finalPosition[2] << ")" << std::endl;  


  //
  // check results to see if it is within range
  //
  bool pass = true;
  double trueParameters[3] = { ra,rb,rc };
  for( unsigned int j = 0; j < LMCostFunction::SpaceDimension; j++ )
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


int itkLevenbergMarquardtOptimizerTest(int argc, char** argv ) 
{
  std::cout << "Levenberg Marquardt optimizer test \n \n"; 


  bool useGradient;
  int  result;

  double F_Tolerance      =  1e-2;  // Function value tolerance
  double G_Tolerance      =  1e-2;  // Gradient magnitude tolerance 
  double X_Tolerance      =  1e-5;  // Search space tolerance
  double Epsilon_Function =  1e-9;  // Step
  int    Max_Iterations   =   200;  // Maximum number of iterations

  if( argc > 1 ) 
    {
    F_Tolerance = atof( argv[1] );
    }

  if( argc > 2 ) 
    {
    G_Tolerance = atof( argv[2] );
    }

  if( argc > 3 ) 
    {
    X_Tolerance = atof( argv[3] );
    }

  if( argc > 4 ) 
    {
    Epsilon_Function = atof( argv[4] );
    }

  if( argc > 5 ) 
    {
    Max_Iterations = atoi( argv[5] );
    }

  std::cout << "F_Tolerance      = " << F_Tolerance << std::endl;
  std::cout << "G_Tolerance      = " << G_Tolerance << std::endl;
  std::cout << "X_Tolerance      = " << X_Tolerance << std::endl;
  std::cout << "Epsilon_Function = " << Epsilon_Function << std::endl;
  std::cout << "Max_Iterations   = " << Max_Iterations << std::endl;


  
  std::cout << std::endl;
  std::cout << std::endl;
  std::cout << "Running using the Gradient computed by vnl " << std::endl;
  useGradient = false;
  result = itkRunLevenbergMarquardOptimization( useGradient,F_Tolerance,
                                                G_Tolerance,X_Tolerance, 
                                                Epsilon_Function,Max_Iterations );
  if( result == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }
 

  
  std::cout << std::endl;
  std::cout << std::endl;
  std::cout << "Running using the Gradient provided by the Cost function" << std::endl;
  useGradient = true;
  result = itkRunLevenbergMarquardOptimization( useGradient,F_Tolerance,
                                                G_Tolerance,X_Tolerance, 
                                                Epsilon_Function,Max_Iterations );
  if( result == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }
  

  return EXIT_SUCCESS; 
}



