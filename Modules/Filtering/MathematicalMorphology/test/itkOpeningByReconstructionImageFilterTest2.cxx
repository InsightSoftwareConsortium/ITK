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

#include "itkOpeningByReconstructionImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkSimpleFilterWatcher.h"

int itkOpeningByReconstructionImageFilterTest2(int argc, char* argv [] )
{
 if ( argc < 8 )
  {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: " << argv[0] << " OutputImage Radius PreserveIntensities(0,1) OriginX OriginY SpacingX SpacingY [Diffmage]" << std::endl;
    return EXIT_FAILURE;
  }

  const int Dimension = 2;
  typedef unsigned char                      PixelType;
  typedef itk::Image< PixelType, Dimension > InputImageType;
  typedef itk::Image< PixelType, Dimension > OutputImageType;
  typedef InputImageType::RegionType         RegionType;
  typedef InputImageType::SizeType           SizeType;
  typedef InputImageType::IndexType          IndexType;
  typedef InputImageType::SpacingType        SpacingType;
  typedef InputImageType::PointType          OriginType;

  typedef itk::ImageFileWriter< OutputImageType > WriterType;

  // Declare the type of the Structuring element to be used
  typedef itk::BinaryBallStructuringElement<
                            PixelType,
                            Dimension>                  StructuringElementType;

  // Declare the type for the Morphology Filters to be Tested
  typedef itk::OpeningByReconstructionImageFilter<
                                InputImageType,
                                OutputImageType,
                                StructuringElementType >  MorphologicalFilterType;


  WriterType::Pointer           writer = WriterType::New();

  //create image
  InputImageType::Pointer inputImage = InputImageType::New();

  // Define regions of input image
  RegionType region;
  SizeType size;
  size.Fill(atoi(argv[2]));
  IndexType index;
  index.Fill(0);
  region.SetSize(size);
  region.SetIndex(index);

  //fill spacing and origin
  OriginType origin;
  origin[0]=atof(argv[4]);
  origin[1]=atof(argv[5]);
  inputImage->SetOrigin(origin);

  SpacingType spacing;
  spacing[0]=atof(argv[6]);
  spacing[1]=atof(argv[7]);
  inputImage->SetSpacing(spacing);


  inputImage->SetRegions(region);
  inputImage->Allocate();
  // Fill with zero values
  inputImage->FillBuffer(static_cast<PixelType>(0));

  // Create writer
  writer->SetFileName( argv[1] );

  // Create the filter
  MorphologicalFilterType::Pointer   filter = MorphologicalFilterType::New();
  itk::SimpleFilterWatcher watcher(filter, "Opening"); watcher.QuietOn();

  StructuringElementType   structuringElement;

  structuringElement.SetRadius(atoi(argv[2]));
  structuringElement.CreateStructuringElement();

  filter->SetKernel( structuringElement );
  if (atoi(argv[3]) == 0)
    {
    filter->PreserveIntensitiesOff();
    }
  else
    {
    filter->PreserveIntensitiesOn();
    }

  // Connect the pipelines
  filter->SetInput ( inputImage );
  writer->SetInput ( filter-> GetOutput() );


  // Execute print
  filter->Print( std::cout );

  // Execute the filter
  try
  {
    writer->Update();
  }
  catch( itk::ExceptionObject & excp )
  {
    std::cerr << "Exception caught:" << excp << std::endl;
    return  EXIT_FAILURE;
  }

  // Create a difference image if one is requested
  if (argc == 8)
    {
    itk::SubtractImageFilter<InputImageType, OutputImageType, OutputImageType>::Pointer subtract = itk::SubtractImageFilter<InputImageType, OutputImageType, OutputImageType>::New();
    subtract->SetInput( 0, inputImage);
    subtract->SetInput( 1, filter->GetOutput() );
    try
      {
      writer->SetFileName( argv[7] );
      writer->SetInput( subtract->GetOutput() );
      writer->Update();
      }
    catch( itk::ExceptionObject & excp )
      {
      std::cerr << "Exception caught writing diff image:" << excp << std::endl;
      return  EXIT_FAILURE;
      }
    }
  return EXIT_SUCCESS;

}
