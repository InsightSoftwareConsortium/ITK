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
#include "itkObjectToObjectOptimizerBase.h"
#include "itkImage.h"
#include "itkTestingMacros.h"

/* Create a simple metric to use for testing here. */
template< typename TFixedObject,  typename TMovingObject >
class ObjectToObjectOptimizerBaseTestMetric:
  public itk::ObjectToObjectMetricBase
{
public:
  /** Standard class typedefs. */
  typedef ObjectToObjectOptimizerBaseTestMetric     Self;
  typedef itk::ObjectToObjectMetricBase             Superclass;
  typedef itk::SmartPointer< Self >                 Pointer;
  typedef itk::SmartPointer< const Self >           ConstPointer;

  typedef typename Superclass::MeasureType          MeasureType;
  typedef typename Superclass::DerivativeType       DerivativeType;
  typedef typename Superclass::ParametersType       ParametersType;
  typedef typename Superclass::ParametersValueType  ParametersValueType;

  itkTypeMacro(ObjectToObjectOptimizerBaseTestMetric, ObjectToObjectMetricBase);

  itkNewMacro(Self);

  // Pure virtual functions that all Metrics must provide
  virtual unsigned int GetNumberOfParameters() const ITK_OVERRIDE { return 5; }

  virtual MeasureType GetValue() const ITK_OVERRIDE
    {
    return 1.0;
    }

  virtual void GetDerivative( DerivativeType & derivative ) const ITK_OVERRIDE
    {
    derivative.Fill(0.0);
    }

  virtual bool HasLocalSupport() const ITK_OVERRIDE
    {
    return false;
    }

  virtual void GetValueAndDerivative( MeasureType & value, DerivativeType & derivative ) const ITK_OVERRIDE
    {
    value = 1.0; derivative.Fill(0.0);
    }

  virtual unsigned int GetNumberOfLocalParameters() const ITK_OVERRIDE
  { return 3; }

  virtual void UpdateTransformParameters( const DerivativeType &, ParametersValueType ) ITK_OVERRIDE {}

  virtual const ParametersType & GetParameters() const ITK_OVERRIDE
  { return m_Parameters; }

  virtual void SetParameters( ParametersType & ) ITK_OVERRIDE {}

  virtual void Initialize(void) throw ( itk::ExceptionObject ) ITK_OVERRIDE {}

  virtual void PrintSelf(std::ostream& os, itk::Indent indent) const ITK_OVERRIDE
  { Superclass::PrintSelf( os, indent ); }

  ParametersType  m_Parameters;

private:
  ObjectToObjectOptimizerBaseTestMetric() {}
  ~ObjectToObjectOptimizerBaseTestMetric() ITK_OVERRIDE {}
};

/* Define a simple derived optimizer class.
 * \class ObjectToObjectOptimizerBaseTestOptimizer */
class ObjectToObjectOptimizerBaseTestOptimizer
  : public itk::ObjectToObjectOptimizerBase
{
public:
  /** Standard "Self" typedef. */
  typedef ObjectToObjectOptimizerBaseTestOptimizer Self;
  typedef itk::ObjectToObjectOptimizerBase         Superclass;
  typedef itk::SmartPointer< Self >                Pointer;
  typedef itk::SmartPointer< const Self >          ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ObjectToObjectOptimizerBaseTestOptimizer, ObjectToObjectOptimizerBase);

  /* Provide initialization for this class */
  virtual void StartOptimization( bool doOnlyInitialization = false ) ITK_OVERRIDE
    {
    Superclass::StartOptimization( doOnlyInitialization );
    std::cout << "StartOptimization called from derived class. doOnlyInitialization: " << doOnlyInitialization << std::endl;
    }

  /** Stop condition return string type */
  virtual const StopConditionReturnStringType GetStopConditionDescription() const ITK_OVERRIDE
    {
    return std::string("Placeholder test return string" );
    }

};

/**
 */
int itkObjectToObjectOptimizerBaseTest(int , char* [])
{
  const int ImageDimension = 2;
  typedef itk::Image<double, ImageDimension>                    ImageType;

  typedef ObjectToObjectOptimizerBaseTestMetric<ImageType,ImageType> MetricType;

  MetricType::Pointer metric = MetricType::New();
  ObjectToObjectOptimizerBaseTestOptimizer::Pointer optimizer = ObjectToObjectOptimizerBaseTestOptimizer::New();

  if( optimizer->GetStopConditionDescription() != std::string("Placeholder test return string") )
    {
    std::cerr << "GetStopConditionDescription did not return properly" << std::endl;
    return EXIT_FAILURE;
    }
  /* exercise some methods */
  optimizer->SetMetric( metric );
  if( optimizer->GetMetric() != metric )
    {
    std::cerr << "Set/GetMetric failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "value: " << optimizer->GetCurrentMetricValue() << std::endl;

  /* Test set/get of scales */
  ObjectToObjectOptimizerBaseTestOptimizer::NumberOfParametersType scalesSize = metric->GetNumberOfLocalParameters();
  typedef ObjectToObjectOptimizerBaseTestOptimizer::ScalesType ScalesType;
  ScalesType scales(scalesSize);
  scales.Fill(3.19);
  optimizer->SetScales( scales );
  const ScalesType& scalesReturn = optimizer->GetScales();
  if( scalesReturn != scales )
    {
    std::cerr << "Set/GetScales failed." << std::endl;
    return EXIT_FAILURE;
    }

  optimizer->SetNumberOfThreads( 1 );

  /* Test StartOptimization */
  TRY_EXPECT_NO_EXCEPTION( optimizer->StartOptimization() );

  /* Test with incorrectly-sized scales. Expect exception */
  scales.SetSize(scalesSize+1);
  optimizer->SetScales( scales );
  TRY_EXPECT_EXCEPTION( optimizer->StartOptimization() );

  /* Test with scales close to identity, within tolerance.
   * The flag indicating identity scales should be set. */
  scales.SetSize(scalesSize);
  scales.Fill( 0.999 );
  optimizer->SetScales( scales );
  TRY_EXPECT_NO_EXCEPTION( optimizer->StartOptimization() );
  if( ! optimizer->GetScalesAreIdentity() )
    {
    std::cerr << "Expected GetScalesAreIdentity to return true." << std::endl;
    return EXIT_FAILURE;
    }

  /* Test that weights are init'ed by default to identity */
  ObjectToObjectOptimizerBaseTestOptimizer::NumberOfParametersType weightsSize = metric->GetNumberOfLocalParameters();
  TRY_EXPECT_NO_EXCEPTION( optimizer->StartOptimization() );
  ScalesType weightsReturn = optimizer->GetWeights();
  if( weightsReturn.Size() != 0 || ! optimizer->GetWeightsAreIdentity() )
    {
    std::cerr << "Expected returned weights to be empty, and flag set to idenity. But got: " << weightsReturn
              << ", GetWeightsAreIdentity: " <<  optimizer->GetWeightsAreIdentity() << std::endl;
    return EXIT_FAILURE;
    }

  /* Test set/get of weights */
  ScalesType weights(weightsSize);
  weights.Fill(3.19);
  optimizer->SetWeights( weights );
  weightsReturn = optimizer->GetWeights();
  if( weightsReturn != weights )
    {
    std::cerr << "Set/GetWeights failed." << std::endl;
    return EXIT_FAILURE;
    }

  /* Test with incorrectly-sized weights. Expect exception */
  weights.SetSize(weightsSize+1);
  optimizer->SetWeights( weights );
  TRY_EXPECT_EXCEPTION( optimizer->StartOptimization() );

  /* Test with weights close to identity, within tolerance.
   * The flag indicating identity weights should be set. */
  weights.SetSize(weightsSize);
  weights.Fill( 0.99999 );
  optimizer->SetWeights( weights );
  TRY_EXPECT_NO_EXCEPTION( optimizer->StartOptimization() );
  if( ! optimizer->GetWeightsAreIdentity() )
    {
    std::cerr << "Expected GetWeightsAreIdentity to return true." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Printing self.." << std::endl;
  std::cout << optimizer << std::endl;

  return EXIT_SUCCESS;
}
