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

#include <iostream>
#include "itkListSample.h"
#include "itkSampleToSubsampleFilter.h"

namespace itk {
namespace Statistics {
namespace itkSampleToSubsampleFilter1Namespace {

template<typename TSample >
class SubsamplerTester : public SampleToSubsampleFilter< TSample >
{
public:
  /** Standard class typedefs. */
  typedef SubsamplerTester                    Self;
  typedef SampleToSubsampleFilter<TSample>    Superclass;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self >          ConstPointer;

  /** Standard macros */
  itkTypeMacro(SubsamplerTester,SampleToSubsampleFilter);
  itkNewMacro(Self);

protected:
  SubsamplerTester() {}
  virtual ~SubsamplerTester() ITK_OVERRIDE {}
  void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE
    {
    this->Superclass::PrintSelf(os,indent);
    os << "Superclass = " <<  this->Superclass::GetNameOfClass() << std::endl;
    }

  void GenerateData() ITK_OVERRIDE
    {
    }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SubsamplerTester);


};

} // end of itkSampleToSubsampleFilter1Namespace
} // end of Standard namespace
} // end of itk namespace


int itkSampleToSubsampleFilterTest1(int, char* [] )
{

  const unsigned int MeasurementVectorSize = 17;

  typedef itk::FixedArray<
    float, MeasurementVectorSize >  MeasurementVectorType;

  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;

  typedef itk::Statistics::itkSampleToSubsampleFilter1Namespace::SubsamplerTester< SampleType > FilterType;


  SampleType::Pointer sample = SampleType::New();

  FilterType::Pointer filter = FilterType::New();

  // Test GetInput() before setting the input
  if( filter->GetInput() != ITK_NULLPTR )
    {
    std::cerr << "GetInput() should have returned ITK_NULLPTR" << std::endl;
    return EXIT_FAILURE;
    }

  // Test GetOutput() before creating the output
  if( filter->GetOutput() == ITK_NULLPTR )
    {
    std::cerr << "GetOutput() should have returned NON-ITK_NULLPTR" << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetInput( sample );

  if( filter->GetInput() != sample.GetPointer() )
    {
    std::cerr << "GetInput() didn't matched SetInput()" << std::endl;
    return EXIT_FAILURE;
    }


  //
  // Exercise the Print() method
  //
  filter->Print( std::cout );


  filter->Update();


  // Exercise the GetNameOfClass() method in the
  // SampleToSubsampleFilter:
  std::cout << "Classname  " << filter->GetNameOfClass() << std::endl;
  std::cout << "Superclass " << filter->FilterType::Superclass::GetNameOfClass() << std::endl;

  std::cout << "Test Passed !" << std::endl;
  return EXIT_SUCCESS;
}
