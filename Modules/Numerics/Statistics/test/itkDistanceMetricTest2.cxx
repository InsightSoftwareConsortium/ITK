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

#include "itkDistanceMetric.h"

namespace itk {
namespace Statistics {
namespace DistanceMetricTest {

template <typename TMeasurementVector>
class MyDistanceMetric : public DistanceMetric< TMeasurementVector >
{
public:
  /** Standard class typedef. */
  typedef MyDistanceMetric                     Self;
  typedef DistanceMetric< TMeasurementVector > Superclass;
  typedef SmartPointer< Self >                 Pointer;
  typedef SmartPointer<const Self>             ConstPointer;

  /** Standard macros */
  itkTypeMacro(MyDistanceMetric, DistanceMetric);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Evaluate membership score */
  double Evaluate(const TMeasurementVector & ) const override
    {
    double score;
    score = 1;
    return score;
    }

  double Evaluate(const TMeasurementVector &, const TMeasurementVector & ) const override
    {
    double score;
    score = 1;
    return score;
    }
};

}
}
}

//test DistanceMetric using resizable measurement vector type
int itkDistanceMetricTest2(int, char* [] )
{

  typedef itk::Array< float>  MeasurementVectorType;


  typedef itk::Statistics::DistanceMetricTest::MyDistanceMetric<
    MeasurementVectorType >   DistanceMetricType;

  typedef DistanceMetricType::MeasurementVectorSizeType MeasurementVectorSizeType;

  DistanceMetricType::Pointer distance = DistanceMetricType::New();

  std::cout << distance->GetNameOfClass() << std::endl;
  std::cout << distance->DistanceMetricType::Superclass::GetNameOfClass() << std::endl;

  distance->Print(std::cout);

  MeasurementVectorSizeType measurementVectorSize = 3;
  distance->SetMeasurementVectorSize( measurementVectorSize );

  if( distance->GetMeasurementVectorSize() != measurementVectorSize )
    {
    std::cerr << "Error in Set/GetMeasurementVectorSize()" << std::endl;
    return EXIT_FAILURE;
    }

  //try re-setting the measurment vector size to the same value, no exceptins should be
  //thrown
  try
    {
    MeasurementVectorSizeType sameSize = 3;
    distance->SetMeasurementVectorSize( sameSize );
    }
  catch( itk::ExceptionObject & excpt )
    {
    std::cerr << "Exception thrown: " << excpt << std::endl;
    return EXIT_FAILURE;
    }


  //try setting an origin vector with a different size it should throw an exception
  try
    {
    DistanceMetricType::OriginType origin;
    MeasurementVectorSizeType newSize = 4;
    origin.SetSize( newSize );
    distance->SetOrigin( origin );

    std::cerr << "Attempting to set an origin vector with a different size,"
              << "should result in an exception" << std::endl;
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & excpt )
    {
    std::cerr << "Exception thrown: " << excpt << std::endl;
    }

  return EXIT_SUCCESS;
}
