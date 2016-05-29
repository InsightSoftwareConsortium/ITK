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

#include <fstream>
#include "itkMaskNeighborhoodOperatorImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSobelOperator.h"
#include "itkTestingMacros.h"

int itkMaskNeighborhoodOperatorImageFilterTest(int ac, char* av[] )
{
  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " InputImage OutputImage\n";
    return -1;
    }

  const unsigned int Dimension = 2;
  typedef float         PixelType;
  typedef unsigned char OutputPixelType;

  typedef itk::Image<PixelType, Dimension>       InputImageType;
  typedef itk::Image<OutputPixelType, Dimension> OutputImageType;

  itk::ImageFileReader<InputImageType>::Pointer input
    = itk::ImageFileReader<InputImageType>::New();
  input->SetFileName(av[1]);
  input->Update();

  // create a mask the size of the input file
  typedef itk::Image<unsigned char, Dimension> MaskImageType;
  MaskImageType::Pointer mask1 = MaskImageType::New();
  MaskImageType::Pointer mask2 = MaskImageType::New();
  MaskImageType::RegionType region;
  MaskImageType::SizeType size;
  MaskImageType::IndexType index;

  region = input->GetOutput()->GetBufferedRegion();
  mask1->SetRegions( region );
  mask1->Allocate(true); // initialize buffer to zero

  mask2->SetRegions( region );
  mask2->Allocate(true); // initialize buffer to zero


  size = region.GetSize();
  index = region.GetIndex();
  unsigned int width = size[0];
  size[0] = width / 2 - static_cast<unsigned int>(.25 * static_cast<float>(width));
  index[0] = size[0] + static_cast<unsigned int>(.25 * static_cast<float>(width));
  region.SetSize(size);
  region.SetIndex(index);
  {
  itk::ImageRegionIterator<MaskImageType> it(mask1, region);
  it.GoToBegin();
  while (!it.IsAtEnd())
    {
    it.Set(1);
    ++it;
    }
  }
  index[0] = 0;
  region.SetIndex(index);
  {
  itk::ImageRegionIterator<MaskImageType> it(mask2, region);
  it.GoToBegin();
  while (!it.IsAtEnd())
    {
    it.Set(1);
    ++it;
    }
  }

  // Create a filter
  typedef itk::MaskNeighborhoodOperatorImageFilter<InputImageType, MaskImageType, InputImageType> FilterType;

  itk::SobelOperator<float, 2> sobelHorizontal;
  sobelHorizontal.SetDirection(0);
  sobelHorizontal.CreateDirectional();

  itk::SobelOperator<float, 2> sobelVertical;
  sobelVertical.SetDirection(1);
  sobelVertical.CreateDirectional();

  FilterType::Pointer filter1 = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter1, MaskNeighborhoodOperatorImageFilter,
    NeighborhoodOperatorImageFilter );

  filter1->SetInput(input->GetOutput());
  filter1->SetMaskImage( mask1.GetPointer() );
  filter1->SetOperator( sobelHorizontal );
  filter1->UseDefaultValueOff();

  FilterType::Pointer filter2 = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter2, MaskNeighborhoodOperatorImageFilter,
    NeighborhoodOperatorImageFilter );

  filter2->SetInput(filter1->GetOutput());
  filter2->SetMaskImage( mask2.GetPointer() );
  filter2->SetOperator( sobelVertical );
  filter2->UseDefaultValueOff();

  typedef itk::RescaleIntensityImageFilter<
               InputImageType, OutputImageType > RescaleFilterType;
  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  rescaler->SetInput( filter2->GetOutput() );

  // Generate test image
  itk::ImageFileWriter<OutputImageType>::Pointer writer =
    itk::ImageFileWriter<OutputImageType>::New();
  writer->SetInput( rescaler->GetOutput() );
  writer->SetFileName( av[2] );

  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: " << e.GetDescription();
    return -1;
    }
  catch (...)
    {
    std::cerr << "Some other exception occurred" << std::endl;
    return -2;
    }

  return EXIT_SUCCESS;
}
