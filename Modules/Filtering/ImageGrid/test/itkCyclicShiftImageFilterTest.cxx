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

#include "itkChangeInformationImageFilter.h"
#include "itkCyclicShiftImageFilter.h"
#include "itkImageFileReader.h"

#include <iostream>

int itkCyclicShiftImageFilterTest(int argc, char * argv[])
{
  if ( argc != 4 )
    {
    std::cerr << "Usage: " << argv[0] << " inputImage shiftX shiftY" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef unsigned char                                  PixelType;
  typedef itk::Image< PixelType, Dimension >             ImageType;
  typedef itk::ImageFileReader< ImageType >              ReaderType;
  typedef itk::ChangeInformationImageFilter< ImageType > ChangeInfoFilterType;
  typedef itk::CyclicShiftImageFilter< ImageType >       CyclicShiftFilterType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  // Change the origin of the image to make sure we can handle that case.
  ChangeInfoFilterType::OutputImageOffsetType newOrigin;
  newOrigin[0] = -52;
  newOrigin[1] =  37;

  ChangeInfoFilterType::Pointer changeInfoFilter = ChangeInfoFilterType::New();
  changeInfoFilter->ChangeRegionOn();
  changeInfoFilter->SetOutputOffset( newOrigin );
  changeInfoFilter->SetInput( reader->GetOutput() );

  CyclicShiftFilterType::OffsetType shift;
  shift[0] = atoi( argv[2] );
  shift[1] = atoi( argv[3] );

  CyclicShiftFilterType::Pointer shiftFilter = CyclicShiftFilterType::New();
  shiftFilter->SetShift( shift );
  shiftFilter->SetInput( changeInfoFilter->GetOutput() );

  if ( shift != shiftFilter->GetShift() )
    {
    std::cerr << "Got unexpected shift " << shiftFilter->GetShift() << " from filter."
              << "Expected " << shift << std::endl;
    return EXIT_FAILURE;
    }

  shiftFilter->Print(std::cout);

  // Iterate over the input, map the indices to their indices in the
  // output of the filter, and make sure the values match.
  shiftFilter->UpdateLargestPossibleRegion();
  itk::ImageRegionConstIteratorWithIndex< ImageType > inputIter( reader->GetOutput(),
                                                                 reader->GetOutput()->GetLargestPossibleRegion() );
  ImageType::RegionType imageRegion = reader->GetOutput()->GetLargestPossibleRegion();
  ImageType::SizeType imageSize = imageRegion.GetSize();
  bool success = true;

  const ImageType *shiftFilterOutput = shiftFilter->GetOutput();

  for (; !inputIter.IsAtEnd(); ++inputIter )
    {
    ImageType::IndexType inputIndex = inputIter.GetIndex();
    CyclicShiftFilterType::IndexType outputIndex( inputIndex );

    for ( unsigned int i = 0; i < Dimension; ++i )
      {
      outputIndex[i] = ( outputIndex[i] + shift[i] ) % imageSize[i];
      if ( outputIndex[i] < 0 ) outputIndex[i] += imageSize[i];
      outputIndex[i] += newOrigin[i];
      }

    PixelType inputPixel  = inputIter.Get();
    PixelType outputPixel = shiftFilterOutput->GetPixel( outputIndex );

    if ( inputPixel != outputPixel )
      {
      std::cerr << "Mismatch pixel value: inputIndex " << inputIter.GetIndex() << ", inputPixel "
                << static_cast< itk::NumericTraits< PixelType >::PrintType >( inputPixel )
                << ", outputIndex " << outputIndex << ", outputPixel "
                << static_cast< itk::NumericTraits< PixelType >::PrintType >( outputPixel )
                << std::endl;
      success = false;
      }
    }

  if ( !success )
    {
    std::cerr << "Unexpected pixel mismatch(es)." << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
