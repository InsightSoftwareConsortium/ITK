/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkAmoebaOptimizerv4.h"
#include "vnl/vnl_vector_fixed.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"
#include "itkMath.h"
#include <iostream>

/**
 *  The objective function is the quadratic form:
 *
 *  1/2 x^T A x - b^T x
 *
 *  Where A is represented as an itkMatrix and
 *  b is represented as a itkVector
 *
 *  The system in this example is:
 *
 *     | 3  2 ||x|   | 2|   |0|
 *     | 2  6 ||y| + |-8| = |0|
 *
 *
 *   the solution is the vector | 2 -2 |
 *
 *   and the expected final value of the function is 10.0
 *
 * \class AmoebaOptimizerv4TestMetric1
 */
class itkAmoebaOptimizerv4TestMetric1 : public itk::ObjectToObjectMetricBase
{
public:

  typedef itkAmoebaOptimizerv4TestMetric1   Self;
  typedef itk::ObjectToObjectMetricBase     Superclass;
  typedef itk::SmartPointer<Self>           Pointer;
  typedef itk::SmartPointer<const Self>     ConstPointer;
  itkNewMacro( Self );

  itkTypeMacro( itkAmoebaOptimizerv4TestMetric1, ObjectToObjectMetricBase );

  enum { SpaceDimension=2 };

  typedef Superclass::ParametersType              ParametersType;
  typedef Superclass::DerivativeType              DerivativeType;
  typedef Superclass::MeasureType                 MeasureType;

  typedef vnl_vector<double>                      VectorType;
  typedef vnl_matrix<double>                      MatrixType;

  itkAmoebaOptimizerv4TestMetric1()
  {
    m_HasLocalSupport = false;
  }

  virtual MeasureType GetValue() const ITK_OVERRIDE
  {
    double x = this->m_Parameters[0];
    double y = this->m_Parameters[1];
    double val = 0.5*(3*x*x+4*x*y+6*y*y) - 2*x + 8*y;
    return val;
  }

  virtual void GetDerivative( DerivativeType  & derivative ) const ITK_OVERRIDE
  {
    double x = this->m_Parameters[0];
    double y = this->m_Parameters[1];

    derivative = DerivativeType(SpaceDimension);
    derivative[0] = -(3*x + 2*y -2);
    derivative[1] = -(2*x + 6*y +8);
  }

  virtual void GetValueAndDerivative( MeasureType & value, DerivativeType & derivative ) const ITK_OVERRIDE
  {
    value = GetValue();
    GetDerivative( derivative );
  }

  virtual void Initialize(void) throw ( itk::ExceptionObject ) ITK_OVERRIDE
  {
    m_Parameters.SetSize( SpaceDimension );
  }

  virtual Superclass::NumberOfParametersType GetNumberOfLocalParameters() const ITK_OVERRIDE
  {
    return SpaceDimension;
  }

  virtual Superclass::NumberOfParametersType GetNumberOfParameters(void) const ITK_OVERRIDE
  {
    return SpaceDimension;
  }

  virtual void SetParameters( ParametersType & params ) ITK_OVERRIDE
  {
    this->m_Parameters =  params;
  }

  virtual const ParametersType & GetParameters() const ITK_OVERRIDE
  {
    return this->m_Parameters;
  }

  virtual bool HasLocalSupport() const ITK_OVERRIDE
  {
    return m_HasLocalSupport;
  }

  void SetHasLocalSupport(bool hls)
  {
    m_HasLocalSupport = hls;
  }

  virtual void UpdateTransformParameters( const DerivativeType &, ParametersValueType ) ITK_OVERRIDE
  {
  }

private:

  ParametersType  m_Parameters;
  bool            m_HasLocalSupport;

};


/**
 * Function we want to optimize, comprised of two parabolas with C0 continuity
 * at 0:
 * f(x) = if(x<0) x^2+4x; else 2x^2-8x
 *
 * Minima are at -2 and 2 with function values of -4 and -8 respectively.
 *
 * \class AmoebaOptimizerv4TestMetric2
 */
class itkAmoebaOptimizerv4TestMetric2 : public itk::ObjectToObjectMetricBase
{
public:

  typedef itkAmoebaOptimizerv4TestMetric2   Self;
  typedef itk::ObjectToObjectMetricBase     Superclass;
  typedef itk::SmartPointer<Self>           Pointer;
  typedef itk::SmartPointer<const Self>     ConstPointer;
  itkNewMacro( Self );
  itkTypeMacro( itkAmoebaOptimizerv4TestMetric2, ObjectToObjectMetricBase );

  enum { SpaceDimension=1 };

  typedef Superclass::ParametersType              ParametersType;
  typedef Superclass::DerivativeType              DerivativeType;
  typedef Superclass::MeasureType                 MeasureType;

  typedef vnl_vector<double>                      VectorType;
  typedef vnl_matrix<double>                      MatrixType;

  itkAmoebaOptimizerv4TestMetric2()
    {
    m_HasLocalSupport = false;
    }

  virtual double GetValue() const ITK_OVERRIDE
    {
    double x = this->m_Parameters[0];
    double val;
    if( x<0 )
      {
      val = x*x+4*x;
      }
    else
      {
      val = 2*x*x-8*x;
      }
    return val;
    }

  void GetDerivative( DerivativeType & itkNotUsed(derivative) ) const ITK_OVERRIDE
    {
      throw itk::ExceptionObject( __FILE__, __LINE__,
                                  "no derivative available" );
    }

  virtual void GetValueAndDerivative( MeasureType & value, DerivativeType & derivative ) const ITK_OVERRIDE
    {
    value = GetValue();
    GetDerivative( derivative );
    }

  virtual void Initialize(void) throw ( itk::ExceptionObject ) ITK_OVERRIDE
    {
    m_Parameters.SetSize( SpaceDimension );
    }

  virtual Superclass::NumberOfParametersType GetNumberOfLocalParameters() const ITK_OVERRIDE
    {
    return SpaceDimension;
    }

  virtual Superclass::NumberOfParametersType GetNumberOfParameters(void) const ITK_OVERRIDE
    {
    return SpaceDimension;
    }

  virtual void SetParameters( ParametersType & params ) ITK_OVERRIDE
    {
    this->m_Parameters =  params;
    }

  virtual const ParametersType & GetParameters() const ITK_OVERRIDE
    {
    return this->m_Parameters;
    }

  virtual bool HasLocalSupport() const ITK_OVERRIDE
    {
    return m_HasLocalSupport;
    }

  void SetHasLocalSupport(bool hls)
    {
    m_HasLocalSupport = hls;
    }

  virtual void UpdateTransformParameters( const DerivativeType &, ParametersValueType ) ITK_OVERRIDE
    {
    }

private:

  ParametersType  m_Parameters;
  bool            m_HasLocalSupport;

};

class CommandIterationUpdateAmoeba : public itk::Command
{
public:
  typedef  CommandIterationUpdateAmoeba   Self;
  typedef  itk::Command                   Superclass;
  typedef itk::SmartPointer<Self>         Pointer;
  itkNewMacro( Self );

  void Reset() { m_IterationNumber = 0; }

  virtual void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
      Execute( (const itk::Object *)caller, event);
    }

  virtual void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {
    const itk::AmoebaOptimizerv4 *optimizer = static_cast< const itk::AmoebaOptimizerv4 * >( object );
    if( dynamic_cast< const itk::FunctionEvaluationIterationEvent * >( &event ) != ITK_NULLPTR)
      {
      std::cout << m_IterationNumber++ << ":  ";
      std::cout << "x: "<< optimizer->GetCurrentPosition() <<"  ";
      std::cout << "f(x): " << optimizer->GetValue() <<std::endl;

      }
    }

protected:
  CommandIterationUpdateAmoeba()
  {
    m_IterationNumber=0;
  }

private:
  unsigned long m_IterationNumber;
};

/**
 * Test Amoeba with a 2D quadratic function - happy day scenario.
 */
int AmoebaTest1();

/**
 * Test Amoeba and Amoeba with restarts on a function with two minima.
 */
int AmoebaTest2();

int itkAmoebaOptimizerv4Test(int, char* [] )
{
  int result1 = AmoebaTest1();
  int result2 = AmoebaTest2();

  std::cout<< "All Tests Completed."<< std::endl;

  if( result1 == EXIT_FAILURE ||
      result2 == EXIT_FAILURE )
    {
    std::cerr<<"[FAILURE]\n";
    return EXIT_FAILURE;
    }
  std::cout<<"[SUCCESS]\n";
  return EXIT_SUCCESS;
}

int AmoebaTest1()
{

  std::cout << "Amoeba Optimizer Test 1\n \n";

  typedef  itk::AmoebaOptimizerv4  OptimizerType;

  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();

  // set optimizer parameters
  itkOptimizer->SetNumberOfIterations( 10 );

  double xTolerance = 0.01;
  itkOptimizer->SetParametersConvergenceTolerance( xTolerance );

  double fTolerance = 0.001;
  itkOptimizer->SetFunctionConvergenceTolerance( fTolerance );

  itkAmoebaOptimizerv4TestMetric1::Pointer metric = itkAmoebaOptimizerv4TestMetric1::New();
  itkOptimizer->SetMetric( metric.GetPointer() );
  std::cout << "itkOptimizer->GetMetric(): " << itkOptimizer->GetMetric() << std::endl;

  OptimizerType::ParametersType initialValue(2);       // constructor requires vector size

  initialValue[0] =  100;             // We start not far from  | 2 -2 |
  initialValue[1] = -100;

  // Set the initial position by setting the metric
  // parameters.
  std::cout << "Set metric parameters." << std::endl;
  metric->SetParameters( initialValue );

  try
    {
    std::cout << "Run for " << itkOptimizer->GetNumberOfIterations();
    std::cout << " iterations or less." << std::endl;

    itkOptimizer->StartOptimization();

    itkOptimizer->SetNumberOfIterations( 100 );
    std::cout << "Continue for " << itkOptimizer->GetNumberOfIterations();
    std::cout << " iterations or less." << std::endl;

    OptimizerType::ParametersType currentPosition = itkOptimizer->GetCurrentPosition();
    metric->SetParameters( currentPosition );

    itkOptimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << "Exception thrown ! " << std::endl;
    std::cerr << "An error occurred during Optimization" << std::endl;
    std::cerr << "Location    = " << e.GetLocation()    << std::endl;
    std::cerr << "Description = " << e.GetDescription() << std::endl;
    std::cerr <<"[TEST 1 FAILURE]\n";
    return EXIT_FAILURE;
    }

  std::cout << "Optimizer: " << itkOptimizer;

  //
  // check results to see if it is within range
  //
  OptimizerType::ParametersType finalPosition;
  finalPosition = itkOptimizer->GetCurrentPosition();

  double trueParameters[2] = { 2, -2 };
  bool pass = true;

  std::cout << "Right answer   = " << trueParameters[0] << " , " << trueParameters[1] << std::endl;
  std::cout << "Final position = " << finalPosition     << std::endl;

  for( unsigned int j = 0; j < 2; j++ )
    {
    if( itk::Math::abs( finalPosition[j] - trueParameters[j] ) > xTolerance )
      pass = false;
    }

  if( !pass )
    {
    std::cerr<<"[TEST 1 FAILURE]\n";
    return EXIT_FAILURE;
    }

  // Get the final value of the optimizer
  std::cout << "Testing optimizers GetValue() : ";
  OptimizerType::MeasureType finalValue = itkOptimizer->GetValue();
  if(std::fabs(finalValue+9.99998)>0.01)
    {
    std::cerr << "failed\n";
    std::cerr<<"[TEST 1 FAILURE]\n";
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "succeeded\n";
    }

  std::cout<<"[TEST 1 SUCCESS]\n";
  return EXIT_SUCCESS;
}

int AmoebaTest2()
{
  std::cout << "Amoeba Optimizer Test 2\n \n";

  typedef  itk::AmoebaOptimizerv4  OptimizerType;
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();

         // set optimizer parameters
  unsigned int maxIterations = 100;
  itkOptimizer->SetNumberOfIterations( maxIterations );

  double xTolerance = 0.01;
  itkOptimizer->SetParametersConvergenceTolerance( xTolerance );

  double fTolerance = 0.001;
  itkOptimizer->SetFunctionConvergenceTolerance( fTolerance );

  //the initial simplex is constructed as:
  //x,
  //x_i = [x[0], ... , x[i]+initialSimplexDelta[i], ... , x[n]]
  //
  OptimizerType::ParametersType initialSimplexDelta( 1 );
  initialSimplexDelta[0] = 10;
  itkOptimizer->SetInitialSimplexDelta( initialSimplexDelta );

  OptimizerType::ParametersType initialParameters( 1 ), finalParameters;
  //starting position
  initialParameters[0] =  -100;

  //the function we want to optimize
  itkAmoebaOptimizerv4TestMetric2::Pointer metric = itkAmoebaOptimizerv4TestMetric2::New();
  itkOptimizer->SetMetric( metric.GetPointer() );

  metric->SetParameters( initialParameters );

  //observe the iterations
  CommandIterationUpdateAmoeba::Pointer observer =
    CommandIterationUpdateAmoeba::New();
  itkOptimizer->AddObserver( itk::IterationEvent(), observer );

  try
    {
    itkOptimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << "Exception thrown ! " << std::endl;
    std::cerr << "An error occurred during Optimization" << std::endl;
    std::cerr << "Location    = " << e.GetLocation()    << std::endl;
    std::cerr << "Description = " << e.GetDescription() << std::endl;
    std::cerr <<"[TEST 2 FAILURE]\n";
    return EXIT_FAILURE;
    }

  //we should have converged to the local minimum, -2
  finalParameters = itkOptimizer->GetCurrentPosition();
  double knownParameters = -2.0;
  std::cout<<"Standard Amoeba:\n";
  std::cout << "Known parameters   = " << knownParameters<<"   ";
  std::cout << "Estimated parameters = " << finalParameters << std::endl;
  std::cout<< "Converged to local minimum." << std::endl;
  if( fabs( finalParameters[0] - knownParameters ) > xTolerance )
    {
    std::cerr<<"[TEST 2 FAILURE]\n";
    return EXIT_FAILURE;
    }

  //run again using multiple restarts
  observer->Reset();
  metric->SetParameters( initialParameters );
  itkOptimizer->OptimizeWithRestartsOn();

  try
    {
    itkOptimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << "Exception thrown ! " << std::endl;
    std::cerr << "An error occurred during Optimization" << std::endl;
    std::cerr << "Location    = " << e.GetLocation()    << std::endl;
    std::cerr << "Description = " << e.GetDescription() << std::endl;
    std::cerr <<"[TEST 2 FAILURE]\n";
    return EXIT_FAILURE;
    }

  //we should have converged to the global minimum, 2
  finalParameters = itkOptimizer->GetCurrentPosition();
  knownParameters = 2.0;
  std::cout<<"Amoeba with restarts:\n";
  std::cout << "Known parameters   = " << knownParameters<<"   ";
  std::cout << "Estimated parameters = " << finalParameters << std::endl;
  std::cout<< "Converged to global minimum." << std::endl;

  if( fabs( finalParameters[0] - knownParameters ) > xTolerance )
    {
    std::cerr <<"[TEST 2 FAILURE]\n";
    return EXIT_FAILURE;
    }
  std::cout<<"[TEST 1 SUCCESS]\n";
  return EXIT_SUCCESS;
}
