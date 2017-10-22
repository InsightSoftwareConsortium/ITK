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
#include "itkImage.h"
#include "itkObjectToObjectMetricBase.h"
#include "itkTestingMacros.h"

/* Test basic operation of ObjectToObjectMetricBase.
 *
 * TODO Finish exercising all methods.
 */

template< typename TFixedObject,  typename TMovingObject >
class ObjectToObjectMetricTestMetric:
  public itk::ObjectToObjectMetricBase
{
public:
  /** Standard class typedefs. */
  typedef ObjectToObjectMetricTestMetric                          Self;
  typedef itk::ObjectToObjectMetricBase                           Superclass;
  typedef itk::SmartPointer< Self >                               Pointer;
  typedef itk::SmartPointer< const Self >                         ConstPointer;

  typedef typename Superclass::MeasureType          MeasureType;
  typedef typename Superclass::DerivativeType       DerivativeType;
  typedef typename Superclass::ParametersType       ParametersType;
  typedef typename Superclass::ParametersValueType  ParametersValueType;

  itkTypeMacro(ObjectToObjectMetricTestMetric, ObjectToObjectMetricBase);

  itkNewMacro(Self);

  // Pure virtual functions that all Metrics must provide
  virtual unsigned int GetNumberOfParameters() const ITK_OVERRIDE { return 5; }

  virtual MeasureType GetValue() const ITK_OVERRIDE
  {
    this->m_Value = 1.0;
    return this->m_Value;
  }

  virtual void GetDerivative( DerivativeType & derivative ) const ITK_OVERRIDE
  {
    derivative.Fill(0.0);
  }

  virtual void GetValueAndDerivative( MeasureType & value, DerivativeType & derivative ) const ITK_OVERRIDE
  {
    value = 1.0; derivative.Fill(0.0);
  }

  virtual unsigned int GetNumberOfLocalParameters() const ITK_OVERRIDE
  { return 0; }

  virtual void UpdateTransformParameters( const DerivativeType &, ParametersValueType ) ITK_OVERRIDE {}

  virtual const ParametersType & GetParameters() const ITK_OVERRIDE
  { return m_Parameters; }

  virtual bool HasLocalSupport() const ITK_OVERRIDE
    { return false; }

  virtual void SetParameters( ParametersType & ) ITK_OVERRIDE
  {
  }

  virtual void Initialize(void) throw ( itk::ExceptionObject ) ITK_OVERRIDE {}

  virtual void PrintSelf(std::ostream& os, itk::Indent indent) const ITK_OVERRIDE
  { Superclass::PrintSelf( os, indent ); }

  ParametersType  m_Parameters;

private:
  ObjectToObjectMetricTestMetric() {}
  ~ObjectToObjectMetricTestMetric() ITK_OVERRIDE {}
};

int itkObjectToObjectMetricBaseTest(int ,char * [])
{
  typedef itk::Image< unsigned char, 3 >                       ImageType;
  typedef ObjectToObjectMetricTestMetric<ImageType, ImageType> ObjectMetricType;

  ObjectMetricType::Pointer objectMetric = ObjectMetricType::New();

  objectMetric->Print( std::cout );

  std::cout << objectMetric << std::endl;

  std::cout << objectMetric->GetNameOfClass() << std::endl;

  typedef ObjectMetricType::ParametersType ParametersType;

  ParametersType parameters(13);
  parameters.Fill( 19.5);

  TEST_EXPECT_EQUAL( objectMetric->GetValue( ), 1.0 );

  TEST_EXPECT_EQUAL( objectMetric->GetCurrentValue( ), 1.0 );

  return EXIT_SUCCESS;
}
