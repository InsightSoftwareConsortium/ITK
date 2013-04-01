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

//

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"

#include "itkHConvexImageFilter.h"
#include "itkHConcaveImageFilter.h"
#include "itkAddImageFilter.h"
#include "itkFilterWatcher.h"

int itkHConvexConcaveImageFilterTest( int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  ";
    std::cerr << " outputImageFile  height" << std::endl;
    return EXIT_FAILURE;
    }


  //
  //  The following code defines the input and output pixel types and their
  //  associated image types.
  //
  const unsigned int Dimension = 2;

  typedef float         InputPixelType;
  typedef float         OutputPixelType;
  typedef unsigned char WritePixelType;

  typedef itk::Image< InputPixelType,  Dimension > InputImageType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
  typedef itk::Image< WritePixelType, Dimension >  WriteImageType;

  // readers/writers
  typedef itk::ImageFileReader< InputImageType  > ReaderType;
  typedef itk::ImageFileWriter< WriteImageType >  WriterType;
  typedef itk::RescaleIntensityImageFilter<OutputImageType, WriteImageType>
                                                  RescaleType;

  // define the hconvex/hconcave filter
  typedef itk::HConvexImageFilter<
                            InputImageType,
                            InputImageType >  HconvexFilterType;
  typedef itk::HConcaveImageFilter<
                            InputImageType,
                            InputImageType >  HconcaveFilterType;


  // Creation of Reader and Writer filters
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer  = WriterType::New();
  RescaleType::Pointer rescaler = RescaleType::New();

  // Create the filters
  HconvexFilterType::Pointer  hconvex = HconvexFilterType::New();
  HconcaveFilterType::Pointer  hconcave = HconcaveFilterType::New();
  FilterWatcher watchConvex(hconvex, "Convex");
  FilterWatcher watchConcave(hconcave, "Concave");

  // Setup the input and output files
  reader->SetFileName( argv[1] );
  writer->SetFileName(  argv[2] );

  // Setup the hconvex method
  hconvex->SetInput(  reader->GetOutput() );
  hconvex->SetHeight( atof(argv[3]) );

  hconcave->SetInput( reader->GetOutput() );
  hconcave->SetHeight( atof(argv[3]) );

  // create a filter to add the two images
  typedef itk::AddImageFilter<
                            InputImageType, InputImageType,
                            OutputImageType> AddFilterType;

  AddFilterType::Pointer add = AddFilterType::New();
  add->SetInput1(hconvex->GetOutput());
  add->SetInput2(hconcave->GetOutput());

  // Run the filter
  rescaler->SetInput( add->GetOutput() );
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );

  writer->SetInput( rescaler->GetOutput() );
  writer->Update();

  return EXIT_SUCCESS;
}
