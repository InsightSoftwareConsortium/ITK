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

/** Example illustrating use of functions to convert between complex valued
 * voxels, magnitude and phase, and real and imaginary representations.
 *
 * \author Simon K. Warfield simon.warfield\@childrens.harvard.edu
 *
 * \note Attribution Notice. This research work was made possible by Grant
 * Number R01 RR021885 (PI Simon K. Warfield, Ph.D.) from
 * the National Center for Research Resources (NCRR), a component of the
 * National Institutes of Health (NIH).  Its contents are solely the
 * responsibility of the authors and do not necessarily represent the
 * official view of NCRR or NIH.
 *
 */

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMagnitudeAndPhaseToComplexImageFilter.h"
#include "itkTestingMacros.h"

int itkMagnitudeAndPhaseToComplexImageFilterTest( int argc, char * argv[] )
{
  if( argc != 4 )
    {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputMagnitude inputPhase outputComplex" << std::endl;
    return EXIT_FAILURE;
    }
  const char * magnitudeImageFileName = argv[1];
  const char * phaseImageFileName = argv[2];
  const char * complexImageFileName = argv[3];

  constexpr unsigned int Dimension = 2;

  using InputPixelType = float;
  using OutputPixelType = std::complex< InputPixelType >;

  using InputImageType = itk::Image< InputPixelType, Dimension >;
  using OutputImageType = itk::Image< OutputPixelType, Dimension >;

  using ReaderType = itk::ImageFileReader< InputImageType >;
  ReaderType::Pointer magnitudeReader = ReaderType::New();
  ReaderType::Pointer phaseReader = ReaderType::New();
  magnitudeReader->SetFileName( magnitudeImageFileName );
  phaseReader->SetFileName( phaseImageFileName );

  using MagnitudeAndPhaseToComplexFilterType = itk::MagnitudeAndPhaseToComplexImageFilter <
    InputImageType, InputImageType, OutputImageType >;

  MagnitudeAndPhaseToComplexFilterType::Pointer magnitudeAndPhaseToComplexFilter =
    MagnitudeAndPhaseToComplexFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( magnitudeAndPhaseToComplexFilter,
    MagnitudeAndPhaseToComplexImageFilter, BinaryGeneratorImageFilter );

  magnitudeAndPhaseToComplexFilter->SetInput1( magnitudeReader->GetOutput() );
  magnitudeAndPhaseToComplexFilter->SetInput2( phaseReader->GetOutput() );

  using WriterType = itk::ImageFileWriter< OutputImageType >;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( complexImageFileName );
  writer->SetInput( magnitudeAndPhaseToComplexFilter->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  // Check that the default template parameters work
  using DefaultParametersFilterType = itk::MagnitudeAndPhaseToComplexImageFilter< InputImageType >;
  DefaultParametersFilterType::Pointer temp = DefaultParametersFilterType::New();
  if( temp.IsNull() )
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
