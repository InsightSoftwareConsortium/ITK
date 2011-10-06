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
#include "itkGradientDescentObjectOptimizerBase.h"
#include "itkImage.h"
#include "itkTestingMacros.h"

/* Create a simple metric to use for testing here. */
template< class TFixedObject,  class TMovingObject >
class ITK_EXPORT GradientDescentObjectOptimizerBaseTestMetric:
  public itk::ObjectToObjectMetric
{
public:
  /** Standard class typedefs. */
  typedef GradientDescentObjectOptimizerBaseTestMetric  Self;
  typedef itk::ObjectToObjectMetric                     Superclass;
  typedef itk::SmartPointer< Self >                     Pointer;
  typedef itk::SmartPointer< const Self >               ConstPointer;

  typedef typename Superclass::MeasureType          MeasureType;
  typedef typename Superclass::DerivativeType       DerivativeType;
  typedef typename Superclass::ParametersType       ParametersType;
  typedef typename Superclass::ParametersValueType  ParametersValueType;

  itkTypeMacro(GradientDescentObjectOptimizerBaseTestMetric, ObjectToObjectMetric);

  itkNewMacro(Self);

  // Pure virtual functions that all Metrics must provide
  unsigned int GetNumberOfParameters() const { return 5; }

  MeasureType GetValue() const
    {
    return itk::NumericTraits< MeasureType >::One;
    }

  virtual void GetValueAndDerivative( MeasureType & value, DerivativeType & derivative ) const
    {
    value = itk::NumericTraits< MeasureType >::One;
    derivative.Fill( itk::NumericTraits< ParametersValueType >::Zero );
    }

  unsigned int GetNumberOfLocalParameters() const
  { return 3; }

  bool HasLocalSupport() const
  { return false; }

  void UpdateTransformParameters( DerivativeType &, ParametersValueType ) {}

  const ParametersType & GetParameters() const
  { return m_Parameters; }

  void Initialize(void) throw ( itk::ExceptionObject ) {}

  void PrintSelf(std::ostream& os, itk::Indent indent) const
  { Superclass::PrintSelf( os, indent ); }

protected:
  GradientDescentObjectOptimizerBaseTestMetric() {}
  ~GradientDescentObjectOptimizerBaseTestMetric() {}//purposely not implemented

private:
  GradientDescentObjectOptimizerBaseTestMetric( const Self& ); //purposely not implemented
  void operator = ( const Self& ); //purposely not implemented

  ParametersType m_Parameters;
};

/* Define a simple derived optimizer class.
 * \class GradientDescentObjectOptimizerBaseTestOptimizer */
class GradientDescentObjectOptimizerBaseTestOptimizer
  : public itk::GradientDescentObjectOptimizerBase
{
public:
  /** Standard "Self" typedef. */
  typedef GradientDescentObjectOptimizerBaseTestOptimizer Self;
  typedef GradientDescentObjectOptimizerBase              Superclass;
  typedef itk::SmartPointer< Self >                       Pointer;
  typedef itk::SmartPointer< const Self >                 ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( GradientDescentObjectOptimizerBaseTestOptimizer,
                GradientDescentObjectOptimizerBase);

  /* Provide an override for the pure virtual StartOptimization */
  void StartOptimization()
    {
    Superclass::StartOptimization();
    std::cout << "StartOptimization called." << std::endl;
    }

  void ResumeOptimization()
    {
    std::cout << "ResumeOptimization called." << std::endl;
    }

  void ModifyGradientByScalesOverSubRange (const IndexRangeType& index )
    {
    std::cout << "ModifyGradientByScalesOverSubRange called with index:"
              << index << std::endl;
    }

  void ModifyGradientByLearningRateOverSubRange (const IndexRangeType& index )
    {
    std::cout << "ModifyGradientByLearningRateOverSubRange called with index:"
              << index << std::endl;
    }

protected:
  void EstimateLearningRate()
    {
    std::cout << "EstimateLearningRate called" << std::endl;
    }
  GradientDescentObjectOptimizerBaseTestOptimizer(){}
  ~GradientDescentObjectOptimizerBaseTestOptimizer(){}

private:
  GradientDescentObjectOptimizerBaseTestOptimizer(const Self& ); //purposely not implemented
  void operator = (const Self&); //purposely not implemented

};


int itkGradientDescentObjectOptimizerBaseTest(int , char* [])
{
  const int ImageDimension = 2;
  typedef itk::Image<double, ImageDimension>                    ImageType;

  typedef GradientDescentObjectOptimizerBaseTestMetric<ImageType,ImageType> MetricType;

  MetricType::Pointer metric = MetricType::New();
  GradientDescentObjectOptimizerBaseTestOptimizer::Pointer optimizer = GradientDescentObjectOptimizerBaseTestOptimizer::New();

  /* exercise some methods */
  optimizer->SetMetric( metric );
  if( optimizer->GetMetric() != metric )
    {
    std::cerr << "Set/GetMetric failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "value: " << optimizer->GetValue() << std::endl;

  optimizer->SetNumberOfThreads( 2 );

  TRY_EXPECT_NO_EXCEPTION( optimizer->StartOptimization() );

  std::cout << "Printing self.." << std::endl;
  std::cout << optimizer << std::endl;

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
