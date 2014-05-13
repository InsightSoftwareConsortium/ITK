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
#include "itkShapedNeighborhoodIterator.h"
#include "itkBinaryBallStructuringElement.h"

typedef itk::Image<int, 2>  LocalImageType;
void CreateImagex(LocalImageType::Pointer& image)
{
  LocalImageType::IndexType start;
  start.Fill(0);

  LocalImageType::SizeType size;
  size.Fill(10);

  LocalImageType::RegionType region(start,size);

  image->SetRegions(region);
  image->Allocate(true); // initialize buffer
                                                // to zero
}

int itkShapedIteratorFromStructuringElementTest(int, char*[])
{
  typedef itk::Image<int, 2>   ImageType;
  typedef ImageType::PixelType PixelType;

  ImageType::Pointer image = ImageType::New();
  CreateImagex(image);

  typedef itk::BinaryBallStructuringElement<PixelType, 2>
    StructuringElementType;
  StructuringElementType::RadiusType elementRadius;
  elementRadius.Fill(2);

  StructuringElementType structuringElement;
    structuringElement.SetRadius(elementRadius);
    structuringElement.CreateStructuringElement();

  typedef itk::ShapedNeighborhoodIterator<ImageType> IteratorType;
  IteratorType siterator(structuringElement.GetRadius(),
                         image,
                         image->GetLargestPossibleRegion());

  siterator.CreateActiveListFromNeighborhood(structuringElement);
  siterator.NeedToUseBoundaryConditionOff();

  IteratorType::IndexType location;
  location[0] = 4;
  location[1] = 5;
  siterator.SetLocation(location);
  IteratorType::Iterator i;
  for (i = siterator.Begin(); !i.IsAtEnd(); ++i)
    {
    i.Set(1);
    }

  // Now show the results
  typedef itk::ImageRegionConstIterator<ImageType> ImageIteratorType;
  ImageIteratorType imit(image, image->GetLargestPossibleRegion());
  imit.GoToBegin();
  unsigned int col = 0;
  while( !imit.IsAtEnd() )
    {
    PixelType value = imit.Get();
    ++imit;
    ++col;
    std::cout << value << " ";
    if ((col % 10) == 0)
      {
      std::cout << std::endl;
      }
    }
  // Check for radius mismatch between shaped iterator and
  // neighborhood
  IteratorType biterator(structuringElement.GetRadius(),
                         image,
                         image->GetLargestPossibleRegion());
  elementRadius.Fill(3);
  structuringElement.SetRadius(elementRadius);

  bool caught = false;
  try
    {
    biterator.CreateActiveListFromNeighborhood(structuringElement);
    }
  catch (itk::ExceptionObject& e)
    {
    caught = true;
    std::cout << "Caught expected exception." << e << std::endl;
    }
  if (!caught)
    {
    std::cout << "Faile to catch expected exception." << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
