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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"

#include "itkObjectToObjectMetric.h"

namespace itkObjectToObjectMetricTestHelpers
{

template< class TFixedObject,  class TMovingObject >
class ITK_EXPORT ObjectToObjectMetricSurrogate:
  public itk::ObjectToObjectMetric<TFixedObject, TMovingObject>
{
public:
  /** Standard class typedefs. */
  typedef ObjectToObjectMetricSurrogate                           Self;
  typedef itk::ObjectToObjectMetric<TFixedObject,TMovingObject>   Superclass;
  typedef itk::SmartPointer< Self >                               Pointer;
  typedef itk::SmartPointer< const Self >                         ConstPointer;

  typedef typename Superclass::MeasureType    MeasureType;
  typedef typename Superclass::DerivativeType DerivativeType;
  typedef typename Superclass::ParametersType ParametersType;

  itkTypeMacro(ObjectToObjectMetricSurrogate, ObjectToObjectMetric);

  itkNewMacro(Self);

  // Pure virtual functions that all Metrics must provide
  unsigned int GetNumberOfParameters() const { return 5; }
  MeasureType GetValue( const ParametersType & parameters ) const
    {
    this->m_Parameters = parameters;
    return 1.0;
    }
  void GetDerivative(const ParametersType &,
                             DerivativeType & derivative) const { derivative.Fill(0.0); }
  void Initialize(void) throw ( itk::ExceptionObject ) {}
  void PrintSelf(std::ostream& os, itk::Indent indent) const
  {
    Superclass::PrintSelf( os, indent );
  }

private:
  ObjectToObjectMetricSurrogate() {}
  ~ObjectToObjectMetricSurrogate() {}
};

}

int itkObjectToObjectMetricTest(int ,char * [])
{
  typedef itk::Image< unsigned char, 3 > ImageType;
  typedef itkObjectToObjectMetricTestHelpers::ObjectToObjectMetricSurrogate<
    ImageType, ImageType> ObjectMetricType;

  ObjectMetricType::Pointer objectMetric = ObjectMetricType::New();

  objectMetric->Print( std::cout );

  std::cout << objectMetric << std::endl;

  std::cout << objectMetric->GetNameOfClass() << std::endl;

  typedef ObjectMetricType::ParametersType ParametersType;

  ParametersType parameters(13);
  parameters.Fill( 19.5);

  objectMetric->GetValue( parameters );

  return EXIT_SUCCESS;
}
