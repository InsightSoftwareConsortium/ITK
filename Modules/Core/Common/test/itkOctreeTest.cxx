/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkOctree.h"
#include "itkNumericTraits.h"
#include "itkImageRegionIterator.h"
#include <cstdlib>
#include <ctime>
#include <cmath>

template <typename TPixel, unsigned int TableSize>
class IdentityMap
{
public:
  unsigned int
  Evaluate(const TPixel * pixel)
  {
    auto pixval = static_cast<unsigned int>(*pixel);
    return pixval < TableSize ? pixval : TableSize - 1;
  }
};

int
itkOctreeTest(int, char *[])
{
  using ImageType = itk::Image<unsigned int, 3>;
  const ImageType::SizeType  imageSize = { { 4, 4, 4 } };
  const ImageType::IndexType imageIndex = { { 0, 0, 0 } };
  ImageType::RegionType      region;
  region.SetSize(imageSize);
  region.SetIndex(imageIndex);
  ImageType::Pointer img = ImageType::New();
  img->SetLargestPossibleRegion(region);
  img->SetBufferedRegion(region);
  img->SetRequestedRegion(region);
  img->Allocate();
  srand((unsigned)time(nullptr));
  itk::ImageRegionIterator<ImageType> ri(img, region);
  try
  {
    unsigned int counter = 0;
    while (!ri.IsAtEnd())
    {
      unsigned int val = rand() % 16384;
      if (counter && counter % 8 == 0)
        std::cerr << val << std::endl;
      else
        std::cerr << val << " ";
      counter++;
      ri.Set(val);
      ++ri;
    }
  }
  catch (const itk::ExceptionObject & ex)
  {
    ex.Print(std::cerr);
    return EXIT_FAILURE;
  }

  using OctreeType = itk::Octree<unsigned int, 16384, IdentityMap<unsigned int, 16384>>;
  OctreeType::Pointer octree = OctreeType::New();
  octree->BuildFromImage(img);
  ImageType::Pointer                  output = octree->GetImage();
  itk::ImageRegionIterator<ImageType> ri2(output, region);
  ri.GoToBegin();
  IdentityMap<unsigned int, 16384> id;
  try
  {
    while (!ri.IsAtEnd() && !ri2.IsAtEnd())
    {
      unsigned int x = ri.Get();
      unsigned int y = ri2.Get();
      unsigned int mapped = id.Evaluate(&x);
      std::cerr << "x = " << x << " mapped(x) " << mapped << " y = " << y << std::endl;
      if (mapped != y)
      {
        std::cerr << "Error comparing Input and Output of Octree" << std::endl;
        return -1;
      }
      ++ri;
      ++ri2;
    }
    if (!ri.IsAtEnd() || !ri2.IsAtEnd())
    {
      std::cerr << "Error, inconsistent image sizes in Octree" << std::endl;
      return EXIT_FAILURE;
    }
  }
  catch (const itk::ExceptionObject & ex)
  {
    ex.Print(std::cerr);
    return EXIT_FAILURE;
  }

  // Test streaming enumeration for OctreeEnums::Octree elements
  const std::set<itk::OctreeEnums::Octree> allOctree{ itk::OctreeEnums::Octree::UNKNOWN_PLANE,
                                                      itk::OctreeEnums::Octree::SAGITAL_PLANE,
                                                      itk::OctreeEnums::Octree::CORONAL_PLANE,
                                                      itk::OctreeEnums::Octree::TRANSVERSE_PLANE };
  for (const auto & ee : allOctree)
  {
    std::cout << "STREAMED ENUM VALUE Octree: " << ee << std::endl;
  }

  // Test streaming enumeration for OctreeEnums::LeafIdentifier elements
  const std::set<itk::OctreeEnums::LeafIdentifier> allLeafIdentifier{
    itk::OctreeEnums::LeafIdentifier::ZERO, itk::OctreeEnums::LeafIdentifier::ONE,
    itk::OctreeEnums::LeafIdentifier::TWO,  itk::OctreeEnums::LeafIdentifier::THREE,
    itk::OctreeEnums::LeafIdentifier::FOUR, itk::OctreeEnums::LeafIdentifier::FIVE,
    itk::OctreeEnums::LeafIdentifier::SIX,  itk::OctreeEnums::LeafIdentifier::SEVEN
  };
  for (const auto & ee : allLeafIdentifier)
  {
    std::cout << "STREAMED ENUM VALUE Octree: " << ee << std::endl;
  }

  return EXIT_SUCCESS;
}
