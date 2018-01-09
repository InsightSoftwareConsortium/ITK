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
#include "itkImage.h"
#include "itkImageFileReader.h"

int main( int argc, char * argv [] )
{

  if( argc < 2 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImageFile " << std::endl;
    return EXIT_FAILURE;
    }

  typedef unsigned char                       PixelType;
  const unsigned int Dimension = 3;

  typedef itk::Image< PixelType, Dimension >  ImageType;

  typedef itk::ImageFileReader< ImageType >   ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  const ImageType * image = reader->GetOutput();

  const ImageType::PointType origin = image->GetOrigin();
  const ImageType::SpacingType spacing = image->GetSpacing();
  const ImageType::RegionType region = image->GetBufferedRegion();
  const ImageType::SizeType size = region.GetSize();
  const ImageType::DirectionType direction = image->GetDirection();

  std::cout << "File = " << argv[1] << std::endl;
  std::cout << "Origin = " << origin << std::endl;
  std::cout << "Spacing = " << spacing << std::endl;
  std::cout << "Size = " << size << std::endl;
  std::cout << "Direction = " << std::endl << direction << std::endl;

  return EXIT_SUCCESS;
}
