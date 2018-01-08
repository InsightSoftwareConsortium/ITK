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
#include "itkGradientDescentOptimizerBasev4.h"
#include "itkImage.h"
#include "itkTestingMacros.h"

/* Create a simple metric to use for testing here. */
template< typename TFixedObject,  typename TMovingObject >
class GradientDescentOptimizerBasev4TestMetric:
  public itk::ObjectToObjectMetricBase
{
public:
  /** Standard class typedefs. */
  typedef GradientDescentOptimizerBasev4TestMetric      Self;
  typedef itk::ObjectToObjectMetricBase                 Superclass;
  typedef itk::SmartPointer< Self >                     Pointer;
  typedef itk::SmartPointer< const Self >               ConstPointer;

  typedef typename Superclass::MeasureType          MeasureType;
  typedef typename Superclass::DerivativeType       DerivativeType;
  typedef typename Superclass::ParametersType       ParametersType;
  typedef typename Superclass::ParametersValueType  ParametersValueType;

  itkTypeMacro(GradientDescentOptimizerBasev4TestMetric, ObjectToObjectMetricBase);

  itkNewMacro(Self);

  // Pure virtual functions that all Metrics must provide
  unsigned int GetNumberOfParameters() const override { return 5; }

  MeasureType GetValue() const override
    {
    return itk::NumericTraits< MeasureType >::OneValue();
    }

  void GetDerivative( DerivativeType & derivative ) const override
    {
    derivative.Fill( itk::NumericTraits< ParametersValueType >::ZeroValue() );
    }

  void GetValueAndDerivative( MeasureType & value, DerivativeType & derivative ) const override
    {
    value = itk::NumericTraits< MeasureType >::OneValue();
    derivative.Fill( itk::NumericTraits< ParametersValueType >::ZeroValue() );
    }

  unsigned int GetNumberOfLocalParameters() const override
  { return 3; }

  void UpdateTransformParameters( const DerivativeType &, ParametersValueType ) override {}

  const ParametersType & GetParameters() const override
  { return m_Parameters; }

  void SetParameters( ParametersType & ) override {}

  bool HasLocalSupport() const override
    {
    return false;
    }

  void Initialize(void) throw ( itk::ExceptionObject ) override {}

  void PrintSelf(std::ostream& os, itk::Indent indent) const override
  { Superclass::PrintSelf( os, indent ); }

protected:
  ~GradientDescentOptimizerBasev4TestMetric() override {}

private:
  GradientDescentOptimizerBasev4TestMetric() {}
  ITK_DISALLOW_COPY_AND_ASSIGN(GradientDescentOptimizerBasev4TestMetric);

  ParametersType m_Parameters;
};

/* Define a simple derived optimizer class.
 * \class GradientDescentOptimizerBasev4TestOptimizer */
class GradientDescentOptimizerBasev4TestOptimizer
  : public itk::GradientDescentOptimizerBasev4
{
public:
  /** Standard "Self" typedef. */
  typedef GradientDescentOptimizerBasev4TestOptimizer     Self;
  typedef itk::GradientDescentOptimizerBasev4             Superclass;
  typedef itk::SmartPointer< Self >                       Pointer;
  typedef itk::SmartPointer< const Self >                 ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( GradientDescentOptimizerBasev4TestOptimizer,
                GradientDescentOptimizerBasev4);

  /* Provide an override for the pure virtual StartOptimization */
  void StartOptimization( bool doOnlyInitialization = false ) override
    {
    Superclass::StartOptimization( doOnlyInitialization );
    std::cout << "StartOptimization called. doOnlyInitialization: " << doOnlyInitialization << std::endl;
    }

  void ResumeOptimization() override
    {
    std::cout << "ResumeOptimization called." << std::endl;
    }

  void ModifyGradientByScalesOverSubRange (const IndexRangeType& index ) override
    {
    std::cout << "ModifyGradientByScalesOverSubRange called with index:"
              << index << std::endl;
    }

  void ModifyGradientByLearningRateOverSubRange (const IndexRangeType& index ) override
    {
    std::cout << "ModifyGradientByLearningRateOverSubRange called with index:"
              << index << std::endl;
    }

protected:

  GradientDescentOptimizerBasev4TestOptimizer(){}
  ~GradientDescentOptimizerBasev4TestOptimizer() override {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GradientDescentOptimizerBasev4TestOptimizer);

};


int itkGradientDescentOptimizerBasev4Test(int , char* [])
{
  const int ImageDimension = 2;
  typedef itk::Image<double, ImageDimension>                    ImageType;

  typedef GradientDescentOptimizerBasev4TestMetric<ImageType,ImageType> MetricType;

  MetricType::Pointer metric = MetricType::New();
  GradientDescentOptimizerBasev4TestOptimizer::Pointer optimizer = GradientDescentOptimizerBasev4TestOptimizer::New();

  /* exercise some methods */
  optimizer->SetMetric( metric );
  if( optimizer->GetMetric() != metric )
    {
    std::cerr << "Set/GetMetric failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "value: " << optimizer->GetCurrentMetricValue() << std::endl;

  optimizer->SetNumberOfThreads( 2 );

  TRY_EXPECT_NO_EXCEPTION( optimizer->StartOptimization() );

  std::cout << "Printing self.." << std::endl;
  std::cout << optimizer << std::endl;

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
