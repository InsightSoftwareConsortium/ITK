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

#include "itkRandomImageSource.h"
#include "itkThresholdImageFilter.h"
#include "itkTextOutput.h"

#include <sstream>


int itkThresholdImageFilterTest(int, char* [] )
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  // Uncomment the following if you want to see each message independently
  // itk::OutputWindow::GetInstance()->PromptUserOn();

  typedef itk::Image<float,2> FloatImage2DType;

  itk::RandomImageSource<FloatImage2DType>::Pointer random;
  random = itk::RandomImageSource<FloatImage2DType>::New();
  random->SetMin(0.0);
  random->SetMax(1000.0);
  random->ReleaseDataFlagOn();

  FloatImage2DType::SpacingValueType spacing[2] = {0.7, 2.1};
  random->SetSpacing( spacing );

  FloatImage2DType::PointValueType origin[2] = {15, 400};
  random->SetOrigin( origin );

  std::ostringstream *os;

  // Test #1, filter goes out of scope
  itk::OutputWindow::GetInstance()->DisplayText( "Test #1: Filter goes out of scope -----------------" );
    {
    itk::ThresholdImageFilter<FloatImage2DType>::Pointer threshold;
    threshold = itk::ThresholdImageFilter<FloatImage2DType>::New();
    threshold->SetInput(random->GetOutput());

    // Exercise threshold setting functions
    threshold->ThresholdAbove( 10.0 );
    threshold->ThresholdBelow( 900.0 );
    threshold->ThresholdOutside( 5.0, 40.0 );

    // Call update multiple times to make sure that the RandomImageSource
    // is releasing and regenerating its data
    threshold->Update();
    threshold->Modified();
    threshold->Update();

    std::cout << "Input spacing: " << random->GetOutput()->GetSpacing()[0]
      << ", "
      << random->GetOutput()->GetSpacing()[1] << std::endl;
    std::cout << "Output spacing: " << threshold->GetOutput()->GetSpacing()[0]
      << ", "
      << threshold->GetOutput()->GetSpacing()[1] << std::endl;

    os = new std::ostringstream();
    *os << "Filter: " << threshold.GetPointer();
    itk::OutputWindow::GetInstance()->DisplayText( os->str().c_str() );
    delete os;
    os = new std::ostringstream();
    *os << "Output #0: " << threshold->GetOutput(0);
    itk::OutputWindow::GetInstance()->DisplayText( os->str().c_str() );
    delete os;


    itk::OutputWindow::GetInstance()->DisplayText( "Ending Test #1: filter goes out of scope" );
    itk::OutputWindow::GetInstance()->DisplayText( "End of Test #1 -----------------------------------" );
    }

  // Test #2, user keeps an extra handle to an output
  itk::OutputWindow::GetInstance()->DisplayText( "Test #2: User keeps an extra hold on an output  -----------------" );
    {
    FloatImage2DType::Pointer keep;

    itk::ThresholdImageFilter<FloatImage2DType>::Pointer threshold;
    threshold = itk::ThresholdImageFilter<FloatImage2DType>::New();
    threshold->SetInput(random->GetOutput());
    threshold->Update();

    os = new std::ostringstream();
    *os << "Filter: " << threshold.GetPointer();
    itk::OutputWindow::GetInstance()->DisplayText( os->str().c_str() );
    delete os;
    os = new std::ostringstream();
    *os << "Output #0: " << threshold->GetOutput(0);
    itk::OutputWindow::GetInstance()->DisplayText( os->str().c_str() );
    delete os;

    keep = threshold->GetOutput(0);

    itk::OutputWindow::GetInstance()->DisplayText( "End of Test #2: last handle on output 0 should go out of scope");
    }
  itk::OutputWindow::GetInstance()->DisplayText( "End of Test #2 -----------------------------------");

  // Test #3, user disconnects a data object from the pipeline
  itk::OutputWindow::GetInstance()->DisplayText( "Test #3: user disconnects a data object from the pipeline  -----------------" );
    {
    FloatImage2DType::Pointer keep;

    itk::ThresholdImageFilter<FloatImage2DType>::Pointer threshold;
    threshold = itk::ThresholdImageFilter<FloatImage2DType>::New();
    threshold->SetInput(random->GetOutput());
    threshold->Update();

    os = new std::ostringstream;
    *os << "Filter: " << threshold.GetPointer();
    itk::OutputWindow::GetInstance()->DisplayText( os->str().c_str() );
    delete os;
    os = new std::ostringstream();
    *os << "Output #0: " << threshold->GetOutput(0);
    itk::OutputWindow::GetInstance()->DisplayText( os->str().c_str() );
    delete os;

    keep = threshold->GetOutput(0);
    keep->DisconnectPipeline();

    itk::OutputWindow::GetInstance()->DisplayText( "End of Test #3: last handle on output 0 should go out of scope");
    }
  itk::OutputWindow::GetInstance()->DisplayText( "End of Test #3 -----------------------------------");

  // Test #4, threshold values
  itk::OutputWindow::GetInstance()->DisplayText( "Test #4: threshold values  -----------------" );
    {
    typedef itk::Image<int,1> IntImage1DType;
    IntImage1DType::Pointer input = IntImage1DType::New();
    IntImage1DType::SpacingValueType inputSpacing[1] = {0.7};
    input->SetSpacing( inputSpacing );
    IntImage1DType::PointValueType inputOrigin[1] = {15};
    input->SetOrigin(inputOrigin);
    IntImage1DType::SizeValueType inputSize = 1;
    IntImage1DType::RegionType inputRegion;
    inputRegion.SetSize(0, inputSize);
    input->SetRegions(inputRegion);
    input->Allocate();
    // The inputValue can be any random value.
    int inputValue = 9;
    input->FillBuffer(inputValue);

    itk::ThresholdImageFilter<IntImage1DType>::Pointer threshold;
    threshold = itk::ThresholdImageFilter<IntImage1DType>::New();
    threshold->SetInput(input);
    int outsideValue = 99;
    threshold->SetOutsideValue(outsideValue);
    IntImage1DType::IndexType index;
    index.Fill(0);

    int outputValue;
    // Above inputValue-1
    threshold->ThresholdAbove(inputValue-1);
    threshold->Update();
    outputValue = threshold->GetOutput()->GetPixel(index);
    if ( outputValue != outsideValue)
      {
      os = new std::ostringstream;
      *os << "Filter above failed:"
          << " lower: " << threshold->GetLower()
          << " upper: " << threshold->GetUpper()
          << " output: " << outputValue;
      itk::OutputWindow::GetInstance()->DisplayText( os->str().c_str() );
      return EXIT_FAILURE;
      }

    // Above inputValue+1
    threshold->ThresholdAbove(inputValue+1);
    threshold->Update();
    outputValue = threshold->GetOutput()->GetPixel(index);
    if ( outputValue != inputValue)
      {
      os = new std::ostringstream;
      *os << "Filter above failed:"
          << " lower: " << threshold->GetLower()
          << " upper: " << threshold->GetUpper()
          << " output: " << outputValue;
      itk::OutputWindow::GetInstance()->DisplayText( os->str().c_str() );
      return EXIT_FAILURE;
      }

    // Above inputValue
    // By definition, the values greater than the value are set to OutsideValue.
    // So, with the threshold set to inputValue, the output should still be equal to the input.
    threshold->ThresholdAbove(inputValue);
    threshold->Update();
    outputValue = threshold->GetOutput()->GetPixel(index);
    if ( outputValue != inputValue)
      {
      os = new std::ostringstream;
      *os << "Filter above failed:"
          << " lower: " << threshold->GetLower()
          << " upper: " << threshold->GetUpper()
          << " output: " << outputValue;
      itk::OutputWindow::GetInstance()->DisplayText( os->str().c_str() );
      return EXIT_FAILURE;
      }

    // Below inputValue-1
    threshold->ThresholdBelow(inputValue-1);
    threshold->Update();
    outputValue = threshold->GetOutput()->GetPixel(index);
    if ( outputValue != inputValue)
      {
      os = new std::ostringstream;
      *os << "Filter below failed:"
          << " lower: " << threshold->GetLower()
          << " upper: " << threshold->GetUpper()
          << " output: " << outputValue;
      itk::OutputWindow::GetInstance()->DisplayText( os->str().c_str() );
      return EXIT_FAILURE;
      }

    // Below inputValue+1
    threshold->ThresholdBelow(inputValue+1);
    threshold->Update();
    outputValue = threshold->GetOutput()->GetPixel(index);
    if ( outputValue != outsideValue)
      {
      os = new std::ostringstream;
      *os << "Filter below failed:"
          << " lower: " << threshold->GetLower()
          << " upper: " << threshold->GetUpper()
          << " output: " << outputValue;
      itk::OutputWindow::GetInstance()->DisplayText( os->str().c_str() );
      return EXIT_FAILURE;
      }

    // Below inputValue
    // By definition, the values less than the value are set to OutsideValue
    // So, with the threshold set to inputValue, the output should still be equal to the input.
    threshold->ThresholdBelow(inputValue);
    threshold->Update();
    outputValue = threshold->GetOutput()->GetPixel(index);
    if ( outputValue != inputValue)
    {
      os = new std::ostringstream;
      *os << "Filter below failed:"
          << " lower: " << threshold->GetLower()
          << " upper: " << threshold->GetUpper()
          << " output: " << outputValue;
      itk::OutputWindow::GetInstance()->DisplayText( os->str().c_str() );
      return EXIT_FAILURE;
    }

    // Outside [inputValue-1 inputValue+1]
    threshold->ThresholdOutside(inputValue-1, inputValue+1);
    threshold->Update();
    outputValue = threshold->GetOutput()->GetPixel(index);
    if ( outputValue != inputValue)
      {
      os = new std::ostringstream;
      *os << "Filter outside failed:"
          << " lower: " << threshold->GetLower()
          << " upper: " << threshold->GetUpper()
          << " output: " << outputValue;
      itk::OutputWindow::GetInstance()->DisplayText( os->str().c_str() );
      return EXIT_FAILURE;
      }

    // Outside [inputValue, inputValue+2]
    // By definition, the values outside the range (less than lower or greater than upper) are set to OutsideValue
    // So, with the threshold including inputValue=0, the output should still be equal to the input.
    threshold->ThresholdOutside(inputValue, inputValue+2);
    threshold->Update();
    outputValue = threshold->GetOutput()->GetPixel(index);
    if ( outputValue != inputValue)
      {
      os = new std::ostringstream;
      *os << "Filter outside failed:"
          << " lower: " << threshold->GetLower()
          << " upper: " << threshold->GetUpper()
          << " output: " << outputValue;
      itk::OutputWindow::GetInstance()->DisplayText( os->str().c_str() );
      return EXIT_FAILURE;
      }

    // Outside [inputValue+1, inputValue+3]
    threshold->ThresholdOutside(inputValue+1, inputValue+3);
    threshold->Update();
    outputValue = threshold->GetOutput()->GetPixel(index);
    if ( outputValue != outsideValue)
      {
      os = new std::ostringstream;
      *os << "Filter above failed:"
          << " lower: " << threshold->GetLower()
          << " upper: " << threshold->GetUpper()
          << " output: " << outputValue;
      itk::OutputWindow::GetInstance()->DisplayText( os->str().c_str() );
      return EXIT_FAILURE;
      }

    }
  itk::OutputWindow::GetInstance()->DisplayText( "End of Test #4 -----------------------------------");

  return EXIT_SUCCESS;
}
