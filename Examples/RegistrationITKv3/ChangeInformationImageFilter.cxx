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

//  Software Guide : BeginLatex
//
//  The \doxygen{ChangeInformationImageFilter} is commonly used to modify
//  image metadata such as origin, spacing, and orientation. This filter
//  leaves intact the pixel data of the image. This filter should be used
//  with extreme caution, since it can easily change information that is
//  critical for the safety of many medical image analysis tasks, such as
//  measurement the volume of a tumor, or providing guidance for surgery.
//
//  The following example illustrates the use of the ChangeInformation image
//  filter in the context of generating synthetic inputs for image registration
//  tests.
//
//  \index{itk::ChangeInformationImageFilter}
//
//  Software Guide : EndLatex

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkVersor.h"

//  Software Guide : BeginLatex
//
//  The header file corresponding to this filter should be included first.
//
//  \index{itk::ChangeInformationImageFilter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkChangeInformationImageFilter.h"
// Software Guide : EndCodeSnippet

int main( int argc, char * argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile   outputImageFile" << std::endl;
    std::cerr << " [scalingFactor] [translationX translationY translationZ]" << std::endl;
    std::cerr << " [rotationZinDegrees]" << std::endl;
    return EXIT_FAILURE;
    }

  //  Software Guide : BeginLatex
  //
  //  Then the pixel and image types of the input and output must be defined.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef   unsigned char  PixelType;

  const unsigned int Dimension = 3;

  typedef itk::Image< PixelType,  Dimension >   ImageType;
  // Software Guide : EndCodeSnippet

  typedef itk::ImageFileReader< ImageType >  ReaderType;
  typedef itk::ImageFileWriter< ImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  //  Software Guide : BeginLatex
  //
  //  Using the image types, it is now possible to define the filter type
  //  and create the filter object.
  //
  //  \index{itk::ChangeInformationImageFilter!instantiation}
  //  \index{itk::ChangeInformationImageFilter!New()}
  //  \index{itk::ChangeInformationImageFilter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ChangeInformationImageFilter< ImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet

  //
  // The reader must be triggered in order to make possible to gather
  // information from the input image.
  //
  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  ImageType::ConstPointer inputImage = reader->GetOutput();

  ImageType::PointType     origin    = inputImage->GetOrigin();
  ImageType::SpacingType   spacing   = inputImage->GetSpacing();
  ImageType::DirectionType direction = inputImage->GetDirection();

  if( argc > 3 )
    {
    double scale = atof( argv[3] );
    for(unsigned int i=0; i<Dimension; i++)
      {
      spacing[i] *= scale;
      }

    filter->SetOutputSpacing( spacing );
    filter->ChangeSpacingOn();
    }

  if( argc > 6 )
    {
    ImageType::PointType::VectorType translation;

    translation[0] = atof ( argv[4] );
    translation[1] = atof ( argv[5] );
    translation[2] = atof ( argv[6] );

    origin += translation;

    filter->SetOutputOrigin( origin );
    filter->ChangeOriginOn();
    }

  if( argc > 7 )
    {
    double additionalAngle = atof( argv[7] );

    itk::Versor< itk::SpacePrecisionType >  rotation;
    double angleInRadians = additionalAngle * itk::Math::pi / 180.0;
    rotation.SetRotationAroundZ( angleInRadians );

    ImageType::DirectionType newDirection = direction * rotation.GetMatrix();

    filter->SetOutputDirection( newDirection );
    filter->ChangeDirectionOn();
    }

  //  Software Guide : BeginLatex
  //
  //  The input to the filter can be taken from any other filter, for example
  //  a reader. The output can be passed down the pipeline to other filters,
  //  for example, a writer. An update call on any downstream filter will
  //  trigger the execution of the median filter.
  //
  //  \index{itk::ChangeInformationImageFilter!SetInput()}
  //  \index{itk::ChangeInformationImageFilter!GetOutput()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );
  // Software Guide : EndCodeSnippet

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
