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

#include <iostream>
#include "itkLabelImageToLabelMapFilter.h"
#include "itkTestingMacros.h"

int
itkLabelImageToLabelMapFilterTest(int argc, char * argv[])
{

  if (argc != 1)
  {
    std::cerr << "usage: " << itkNameOfTestExecutableMacro(argv) << "" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr int dim = 2;

  using LabelObjectType = itk::LabelObject<unsigned long, dim>;
  using IndexType = LabelObjectType::IndexType;
  using LabelMapType = itk::LabelMap<LabelObjectType>;
  using SizeType = LabelMapType::SizeType;
  using ImageType = itk::Image<unsigned char, dim>;

  using LabelImageToLabelMapFilterType = itk::LabelImageToLabelMapFilter<ImageType, LabelMapType>;

  ImageType::Pointer image = ImageType::New();

  SizeType sizeIn;
  sizeIn[0] = 11;
  sizeIn[1] = 11;
  image->SetRegions(sizeIn);
  image->Allocate();
  image->FillBuffer(255);

  IndexType idxHorizontal;
  idxHorizontal[1] = 5;

  IndexType idxVertical;
  idxVertical[0] = 5;
  for (int ctr = 0; ctr < 11; ctr++)
  {
    idxHorizontal[0] = ctr;
    idxVertical[1] = ctr;
    image->SetPixel(idxHorizontal, 1);
    image->SetPixel(idxVertical, 1);
  }

  idxHorizontal[1] = 7;
  idxVertical[0] = 7;

  for (int ctr = 0; ctr < 11; ctr++)
  {
    idxHorizontal[0] = ctr;
    idxVertical[1] = ctr;
    image->SetPixel(idxHorizontal, 5);
    image->SetPixel(idxVertical, 2);
  }

  idxHorizontal[0] = 7;
  image->SetPixel(idxHorizontal, 3);

  LabelImageToLabelMapFilterType::Pointer conversion = LabelImageToLabelMapFilterType::New();
  conversion->SetInput(image);
  conversion->SetBackgroundValue(255);
  conversion->Update();

  itkAssertOrThrowMacro((conversion->GetBackgroundValue() == 255), "Error conversion background value.");

  LabelMapType::Pointer map;
  map = conversion->GetOutput();

  itkAssertOrThrowMacro((map->GetBackgroundValue() == 255), "Error in Label Image (background).");

  map->Print(std::cout);

  std::cout << "Printing out map." << std::endl;
  for (int ctrI = 0; ctrI < 11; ctrI++)
  {
    for (int ctrJ = 0; ctrJ < 11; ctrJ++)
    {
      IndexType index;
      index[0] = ctrI;
      index[1] = ctrJ;
      unsigned long val;
      val = map->GetPixel(index);
      std::cout << "Pixel[" << ctrI << "," << ctrJ << "]: " << val << std::endl;
      if (((ctrI == 5) || (ctrJ == 5)) && (ctrI != 7) && (ctrJ != 7))
      {
        itkAssertOrThrowMacro((val == 1), "Error in Label Image (foreground).");
      }
      else
      {
        if ((ctrI == 7) && (ctrJ == 7))
        {
          itkAssertOrThrowMacro((val == 3), "Error in Label Image (foreground).");
          continue;
        }
        else if (ctrJ == 7)
        {
          itkAssertOrThrowMacro((val == 5), "Error in Label Image (foreground).");
        }
        else if (ctrI == 7)
        {
          itkAssertOrThrowMacro((val == 2), "Error in Label Image (foreground).");
        }
        else
        {
          itkAssertOrThrowMacro((val == 255), "Error in Label Image (background).");
        }
      }
    }
  }
  std::cout << "End - Printing out map." << std::endl << std::endl;

  conversion->Print(std::cout);

  return EXIT_SUCCESS;
}
