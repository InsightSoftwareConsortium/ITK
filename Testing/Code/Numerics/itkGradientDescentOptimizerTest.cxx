/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientDescentOptimizerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <itkGradientDescentOptimizer.h>
#include <vnl/vnl_math.h>


/** 
 *  The objectif function is the quadratic form:
 *
 *  1/2 x^T A x - b^T x
 *
 *  Where A is a matrix and b is a vector
 *  The system in this example is:
 *
 *     | 3  2 ||x|   | 2|   |0|
 *     | 2  6 ||y| + |-8| = |0|
 *
 *
 *   the solution is the vector | 2 -2 |
 *
 */ 
class gradientCostFunction : public itk::SingleValuedCostFunction 
{
public:

  typedef gradientCostFunction                  Self;
  typedef itk::SingleValuedCostFunction   Superclass;
  typedef itk::SmartPointer<Self>         Pointer;
  typedef itk::SmartPointer<const Self>   ConstPointer;
  itkNewMacro( Self );
  itkTypeMacro( gradientCostFunction, SingleValuedCostFunction );

  enum { SpaceDimension=2 };
  
  typedef Superclass::ParametersType      ParametersType;
  typedef Superclass::DerivativeType      DerivativeType;
  typedef Superclass::MeasureType         MeasureType ;

  gradientCostFunction()
  {
  }


  MeasureType  GetValue( const ParametersType & parameters ) const
  { 
    
    double x = parameters[0];
    double y = parameters[1];

    std::cout << "GetValue( " ;
    std::cout << x << " ";
    std::cout << y << ") = ";

    MeasureType measure = 0.5*(3*x*x+4*x*y+6*y*y) - 2*x + 8*y;

    std::cout << measure << std::endl; 

    return measure;

  }

  void GetDerivative( const ParametersType & parameters, 
                            DerivativeType & derivative ) const
  {

    double x = parameters[0];
    double y = parameters[1];

    std::cout << "GetDerivative( " ;
    std::cout << x << " ";
    std::cout << y << ") = ";

    DerivativeType temp(SpaceDimension);
    temp.Fill( 0 );
    derivative = temp;
    derivative[0] = 3 * x + 2 * y -2;
    derivative[1] = 2 * x + 6 * y +8;

    std::cout << derivative << std::endl;

  }
 

  unsigned int GetNumberOfParameters(void) const
    {
    return SpaceDimension;
    }


private:


};



int itkGradientDescentOptimizerTest(int, char* [] ) 
{
  std::cout << "Gradient Descent Optimizer Test ";
  std::cout << std::endl << std::endl;

  typedef  itk::GradientDescentOptimizer  OptimizerType;

  typedef OptimizerType::ScalesType        ScalesType;
    
  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();


  // Declaration of the CostFunction 
  gradientCostFunction::Pointer costFunction = gradientCostFunction::New();


  itkOptimizer->SetCostFunction( costFunction.GetPointer() );

  
  typedef gradientCostFunction::ParametersType    ParametersType;

  const unsigned int spaceDimension = 
                      costFunction->GetNumberOfParameters();

  // We start not so far from  | 2 -2 |
  ParametersType  initialPosition( spaceDimension );

  initialPosition[0] =  100;
  initialPosition[1] = -100;

  itkOptimizer->MinimizeOn();
  itkOptimizer->SetLearningRate( 0.1 );
  itkOptimizer->SetNumberOfIterations( 50 );

  itkOptimizer->SetInitialPosition( initialPosition );

  try 
    {
    itkOptimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error ocurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation()    << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }

  ParametersType finalPosition = itkOptimizer->GetCurrentPosition();
  std::cout << "Solution        = (";
  std::cout << finalPosition[0] << "," ;
  std::cout << finalPosition[1] << ")" << std::endl;  

  //
  // check results to see if it is within range
  //
  bool pass = true;
  double trueParameters[2] = { 2, -2 };
  for( unsigned int j = 0; j < 2; j++ )
    {
    if( vnl_math_abs( finalPosition[j] - trueParameters[j] ) > 0.01 )
      pass = false;
    }

  // Exercise various member functions.
  std::cout << "Maximize: " << itkOptimizer->GetMaximize() << std::endl;
  std::cout << "LearningRate: " << itkOptimizer->GetLearningRate();
  std::cout << std::endl;
  std::cout << "NumberOfIterations: " << itkOptimizer->GetNumberOfIterations();
  std::cout << std::endl;

  itkOptimizer->Print( std::cout );

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}



