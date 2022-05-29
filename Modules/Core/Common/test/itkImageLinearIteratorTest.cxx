/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>

#include "itkImageLinearIteratorWithIndex.h"
#include "itkTestingMacros.h"

int
itkImageLinearIteratorTest(int, char *[])
{
  std::cout << "Creating an image of indices" << std::endl;

  constexpr unsigned int ImageDimension = 3;

  using PixelType = itk::Index<ImageDimension>;

  using ImageType = itk::Image<PixelType, ImageDimension>;

  auto                    myImage = ImageType::New();
  ImageType::ConstPointer myConstImage = myImage;

  ImageType::SizeType size0;

  size0[0] = 100;
  size0[1] = 100;
  size0[2] = 100;

  ImageType::IndexType start0;
  start0.Fill(0);

  ImageType::RegionType region0;
  region0.SetIndex(start0);
  region0.SetSize(size0);

  myImage->SetRegions(region0);
  myImage->Allocate();

  using IteratorType = itk::ImageLinearIteratorWithIndex<ImageType>;

  using ConstIteratorType = itk::ImageLinearConstIteratorWithIndex<ImageType>;

  IteratorType it(myImage, region0);

  it.GoToBegin();
  it.SetDirection(0); // 0=x, 1=y, 2=z

  ImageType::IndexType index0;

  while (!it.IsAtEnd())
  {
    while (!it.IsAtEndOfLine())
    {
      index0 = it.GetIndex();
      it.Set(index0);
      ++it;
    }
    it.NextLine();
  }

  // Verification
  IteratorType ot(myImage, region0);

  ot.GoToBegin();
  ot.SetDirection(0); // 0=x, 1=y, 2=z

  std::cout << "Verifying iterator... ";

  while (!ot.IsAtEnd())
  {
    while (!ot.IsAtEndOfLine())
    {
      index0 = ot.GetIndex();
      if (ot.Get() != index0)
      {
        std::cerr << "Values don't correspond to what was stored " << std::endl;
        std::cerr << "Test failed at index ";
        std::cerr << index0 << std::endl;
        return EXIT_FAILURE;
      }
      ++ot;
    }
    ot.NextLine();
  }
  std::cout << "   Done ! " << std::endl;

  // Verification
  ConstIteratorType cot(myConstImage, region0);

  // Test exceptions
  int direction = ImageType::GetImageDimension() + 1;
  ITK_TRY_EXPECT_EXCEPTION(cot.SetDirection(direction));

  cot.GoToBegin();
  cot.SetDirection(0); // 0=x, 1=y, 2=z

  std::cout << "Verifying const iterator... ";

  while (!cot.IsAtEnd())
  {
    while (!cot.IsAtEndOfLine())
    {
      index0 = cot.GetIndex();
      if (cot.Get() != index0)
      {
        std::cerr << "Values don't correspond to what was stored " << std::endl;
        std::cerr << "Test failed at index ";
        std::cerr << index0 << " value is " << cot.Get() << std::endl;
        return EXIT_FAILURE;
      }
      ++cot;
    }
    cot.NextLine();
  }
  std::cout << "   Done ! " << std::endl;

  // Test GoToReverseBeginOfLine()
  cot.GoToBegin();
  cot.GoToReverseBeginOfLine();
  index0 = cot.GetIndex();
  if (cot.Get() != index0)
  {
    std::cerr << "Values don't correspond to what was stored " << std::endl;
    std::cerr << "Test failed at index ";
    std::cerr << index0 << " value is " << cot.Get() << std::endl;
    return EXIT_FAILURE;
  }


  // Verification
  {
    std::cout << "Verifying iterator in reverse direction... ";

    ImageType::IndexType start;
    start[0] = 10;
    start[1] = 20;
    start[2] = 30;

    ImageType::SizeType size;
    size[0] = 2;
    size[1] = 3;
    size[2] = 4;

    ImageType::RegionType region;
    region.SetIndex(start);
    region.SetSize(size);

    std::cout << "  IteratorType ior( myImage, region );" << std::endl;
    IteratorType ior(myImage, region);

    std::cout << "    GetIndex(): " << ior.GetIndex() << std::endl;
    std::cout << "  ior.GoToReverseBegin();" << std::endl;
    ior.GoToReverseBegin();
    std::cout << "    GetIndex(): " << ior.GetIndex() << std::endl;
    std::cout << "  ior.SetDirection( 0 ); // 0=x, 1=y, 2=z" << std::endl;
    ior.SetDirection(0); // 0=x, 1=y, 2=z
    std::cout << " ior.GetDirection(): " << ior.GetDirection() << std::endl;
    std::cout << "  while( !ior.IsAtReverseEnd() )" << std::endl;
    while (!ior.IsAtReverseEnd())
    {
      std::cout << "    while( !ior.IsAtReverseEndOfLine() )" << std::endl;
      std::cout << "    GetIndex(): " << ior.GetIndex() << std::endl;
      while (!ior.IsAtReverseEndOfLine())
      {
        index0 = ior.GetIndex();
        if (ior.Get() != index0)
        {
          std::cerr << "Values don't correspond to what was stored " << std::endl;
          std::cerr << "Test failed at index ";
          std::cerr << index0 << " value is " << ior.Get() << std::endl;
          return EXIT_FAILURE;
        }
        std::cout << "      --ior;" << std::endl;
        --ior;
        std::cout << "    GetIndex(): " << ior.GetIndex() << std::endl;
      }
      std::cout << "    ior.PreviousLine();" << std::endl;
      ior.PreviousLine();
      std::cout << "    GetIndex(): " << ior.GetIndex() << std::endl;
    }
    std::cout << "   Done ! " << std::endl;
  }

  // Verification
  std::cout << "Verifying const iterator in reverse direction... ";

  ConstIteratorType cor(myImage, region0);

  cor.GoToReverseBegin();
  cor.SetDirection(0); // 0=x, 1=y, 2=z

  while (!cor.IsAtReverseEnd())
  {
    while (!cor.IsAtReverseEndOfLine())
    {
      index0 = cor.GetIndex();
      if (cor.Get() != index0)
      {
        std::cerr << "Values don't correspond to what was stored " << std::endl;
        std::cerr << "Test failed at index ";
        std::cerr << index0 << " value is " << cor.Get() << std::endl;
        return EXIT_FAILURE;
      }
      --cor;
    }
    cor.PreviousLine();
  }
  std::cout << "   Done ! " << std::endl;


  // Verification of GotoBeginOfLine() in the iterator
  {
    std::cout << "Verifying  iterator GoToBeginOfLine()... ";

    ImageType::IndexType start;
    start[0] = 10;
    start[1] = 12;
    start[2] = 14;

    ImageType::SizeType size;
    size[0] = 11;
    size[1] = 12;
    size[2] = 13;

    ImageType::RegionType region;
    region.SetIndex(start);
    region.SetSize(size);

    IteratorType bot(myImage, region);

    bot.SetDirection(0); // 0=x, 1=y, 2=z
    bot.GoToBegin();

    ImageType::IndexType testIndex;
    testIndex = start;
    testIndex[1] += 2; // advance two lines in Y

    bot.NextLine(); // advance two lines in Y
    bot.NextLine();

    ++bot; // advance a bit along the line
    ++bot;
    ++bot;
    ++bot;

    bot.GoToBeginOfLine(); // go back to the begin of the line

    if (bot.GetIndex() != testIndex)
    {
      std::cerr << "GoToBeginOfLine() test failed" << std::endl;
      std::cerr << bot.GetIndex() << " should be" << testIndex << std::endl;
      return EXIT_FAILURE;
    }

    std::cout << "   Done ! " << std::endl;
  }


  // Verification of GotoBeginOfLine() in the const iterator
  {
    std::cout << "Verifying const iterator GoToBeginOfLine()... ";

    ImageType::IndexType start;
    start[0] = 10;
    start[1] = 12;
    start[2] = 14;

    ImageType::SizeType size;
    size[0] = 11;
    size[1] = 12;
    size[2] = 13;

    ImageType::RegionType region;
    region.SetIndex(start);
    region.SetSize(size);

    ConstIteratorType cbot(myImage, region);

    cbot.SetDirection(0); // 0=x, 1=y, 2=z
    cbot.GoToBegin();

    ImageType::IndexType testIndex;
    testIndex = start;
    testIndex[1] += 2; // advance two lines in Y

    cbot.NextLine(); // advance two lines in Y
    cbot.NextLine();

    ++cbot; // advance a bit along the line
    ++cbot;
    ++cbot;
    ++cbot;

    cbot.GoToBeginOfLine(); // go back to the begin of the line

    if (cbot.GetIndex() != testIndex)
    {
      std::cerr << "GoToBeginOfLine() test failed" << std::endl;
      std::cerr << cbot.GetIndex() << " should be" << testIndex << std::endl;
      return EXIT_FAILURE;
    }

    std::cout << "   Done ! " << std::endl;
  }


  // Verification of the Iterator in a subregion of the image
  {
    std::cout << "Verifying Iterator in a Region smaller than the whole image... ";

    ImageType::IndexType start;
    start[0] = 10;
    start[1] = 12;
    start[2] = 14;

    ImageType::SizeType size;
    size[0] = 11;
    size[1] = 12;
    size[2] = 13;

    ImageType::RegionType region;
    region.SetIndex(start);
    region.SetSize(size);

    IteratorType cbot(myImage, region);

    cbot.SetDirection(0); // 0=x, 1=y, 2=z
    cbot.GoToBegin();

    while (!cbot.IsAtEnd())
    {
      while (!cbot.IsAtEndOfLine())
      {
        ImageType::IndexType index = cbot.GetIndex();
        ImageType::PixelType pixel = cbot.Get();

        if (index != pixel)
        {
          std::cerr << "Iterator in region test failed" << std::endl;
          std::cerr << pixel << " should be" << index << std::endl;
          return EXIT_FAILURE;
        }

        ++cbot;
      }
      cbot.NextLine();
    }

    std::cout << "   Done ! " << std::endl;
  }


  // Verification of the Const Iterator in a subregion of the image
  {
    std::cout << "Verifying Const Iterator in a Region smaller than the whole image... ";

    ImageType::IndexType start;
    start[0] = 10;
    start[1] = 12;
    start[2] = 14;

    ImageType::SizeType size;
    size[0] = 11;
    size[1] = 12;
    size[2] = 13;

    ImageType::RegionType region;
    region.SetIndex(start);
    region.SetSize(size);

    ConstIteratorType cbot(myImage, region);

    cbot.SetDirection(0); // 0=x, 1=y, 2=z
    cbot.GoToBegin();

    while (!cbot.IsAtEnd())
    {
      while (!cbot.IsAtEndOfLine())
      {
        ImageType::IndexType index = cbot.GetIndex();
        ImageType::PixelType pixel = cbot.Get();

        if (index != pixel)
        {
          std::cerr << "Iterator in region test failed" << std::endl;
          std::cerr << pixel << " should be" << index << std::endl;
          return EXIT_FAILURE;
        }

        ++cbot;
      }
      cbot.NextLine();
    }

    std::cout << "   Done ! " << std::endl;
  }


  // Verification of the Iterator NextLine() in the middle of a line
  {
    std::cout << "Verifying Iterator NextLine() in the middle of a line";

    ImageType::IndexType start;
    start[0] = 10;
    start[1] = 12;
    start[2] = 14;

    ImageType::SizeType size;
    size[0] = 11;
    size[1] = 12;
    size[2] = 13;

    ImageType::RegionType region;
    region.SetIndex(start);
    region.SetSize(size);

    std::cout << "    IteratorType cbot( myImage, region );" << std::endl;
    IteratorType cbot(myImage, region);

    std::cout << "    cbot.SetDirection( 0 ); // 0=x, 1=y, 2=z" << std::endl;
    cbot.SetDirection(0); // 0=x, 1=y, 2=z
    std::cout << "    cbot.GoToBegin();" << std::endl;
    cbot.GoToBegin();
    std::cout << "    GetIndex(): " << cbot.GetIndex() << std::endl;
    // go to the middle of the first line
    std::cout << "    for(unsigned int i=0; ..." << std::endl;
    for (unsigned int i = 0; i < size[0] / 2; ++i)
    {
      std::cout << "      ++cbot;" << std::endl;
      ++cbot;
      std::cout << "      GetIndex(): " << cbot.GetIndex() << std::endl;
    }

    // go to next line
    std::cout << "    cbot.NextLine();" << std::endl;
    cbot.NextLine();
    std::cout << "    GetIndex(): " << cbot.GetIndex() << std::endl;

    std::cout << "    const ImageType::IndexType testIndex = cbot.Get();" << std::endl;
    const ImageType::IndexType testIndex = cbot.Get();
    std::cout << "    if( cbot.GetIndex() != testIndex )" << std::endl;
    if (cbot.GetIndex() != testIndex)
    {
      std::cerr << "NextLine() test failed" << std::endl;
      std::cerr << cbot.GetIndex() << " should be" << testIndex << std::endl;
      return EXIT_FAILURE;
    }

    std::cout << "   Done ! " << std::endl;
  }

  // Verification of the Iterator PreviousLine() in the middle of a line
  {
    std::cout << "Verifying Iterator PreviousLine() in the middle of a line";

    ImageType::IndexType start;
    start[0] = 10;
    start[1] = 12;
    start[2] = 14;

    ImageType::SizeType size;
    size[0] = 11;
    size[1] = 12;
    size[2] = 13;

    ImageType::RegionType region;
    region.SetIndex(start);
    region.SetSize(size);

    std::cout << "    IteratorType cbot( myImage, region );" << std::endl;
    IteratorType cbot(myImage, region);

    std::cout << "    cbot.SetDirection( 0 ); // 0=x, 1=y, 2=z" << std::endl;
    cbot.SetDirection(0); // 0=x, 1=y, 2=z
    std::cout << "    cbot.GoToBegin();" << std::endl;
    cbot.GoToBegin();
    std::cout << "    GetIndex(): " << cbot.GetIndex() << std::endl;
    // go to the middle of the second line
    std::cout << "    for(unsigned int i=0; ..." << std::endl;
    for (unsigned int i = 0; i < size[0] + size[0] / 2; ++i)
    {
      std::cout << "      ++cbot;" << std::endl;
      ++cbot;
      std::cout << "      if(cbot.IsAtEndOfLine())" << std::endl;
      if (cbot.IsAtEndOfLine())
      {
        std::cout << "        cbot.NextLine();" << std::endl;
        cbot.NextLine();
      }
      std::cout << "    GetIndex(): " << cbot.GetIndex() << std::endl;
    }

    // go to previous line
    std::cout << "    cbot.PreviousLine();" << std::endl;
    cbot.PreviousLine();
    std::cout << "    GetIndex(): " << cbot.GetIndex() << std::endl;

    std::cout << "    const ImageType::IndexType testIndex = cbot.Get();" << std::endl;
    const ImageType::IndexType testIndex = cbot.Get();
    std::cout << "    if( cbot.GetIndex() != testIndex )" << std::endl;
    if (cbot.GetIndex() != testIndex)
    {
      std::cerr << "PreviousLine() test failed" << std::endl;
      std::cerr << cbot.GetIndex() << " should be" << testIndex << std::endl;
      return EXIT_FAILURE;
    }

    std::cout << "   Done ! " << std::endl;
  }

  // Verification of the ConstIterator NextLine() in the middle of a line
  {
    std::cout << "Verifying ConstIterator NextLine() in the middle of a line";

    ImageType::IndexType start;
    start[0] = 10;
    start[1] = 12;
    start[2] = 14;

    ImageType::SizeType size;
    size[0] = 11;
    size[1] = 12;
    size[2] = 13;

    ImageType::RegionType region;
    region.SetIndex(start);
    region.SetSize(size);

    ConstIteratorType cbot(myImage, region);

    cbot.SetDirection(0); // 0=x, 1=y, 2=z
    cbot.GoToBegin();

    // go to the middle of the first line
    for (unsigned int i = 0; i < size[0] / 2; ++i)
    {
      ++cbot;
    }

    // go to next line
    cbot.NextLine();

    const ImageType::IndexType testIndex = cbot.Get();
    if (cbot.GetIndex() != testIndex)
    {
      std::cerr << "NextLine() test failed" << std::endl;
      std::cerr << cbot.GetIndex() << " should be" << testIndex << std::endl;
      return EXIT_FAILURE;
    }

    std::cout << "   Done ! " << std::endl;
  }

  // Verification of the ConstIterator PreviousLine() in the middle of a line
  {
    std::cout << "Verifying ConstIterator PreviousLine() in the middle of a line";

    ImageType::IndexType start;
    start[0] = 10;
    start[1] = 12;
    start[2] = 14;

    ImageType::SizeType size;
    size[0] = 11;
    size[1] = 12;
    size[2] = 13;

    ImageType::RegionType region;
    region.SetIndex(start);
    region.SetSize(size);

    ConstIteratorType cbot(myImage, region);

    cbot.SetDirection(0); // 0=x, 1=y, 2=z
    cbot.GoToBegin();

    // go to the middle of the second line
    for (unsigned int i = 0; i < size[0] + size[0] / 2; ++i)
    {
      ++cbot;
      if (cbot.IsAtEndOfLine())
      {
        cbot.NextLine();
      }
    }

    // go to previous line
    cbot.PreviousLine();

    const ImageType::IndexType testIndex = cbot.Get();
    if (cbot.GetIndex() != testIndex)
    {
      std::cerr << "PreviousLine() test failed" << std::endl;
      std::cerr << cbot.GetIndex() << " should be" << testIndex << std::endl;
      return EXIT_FAILURE;
    }

    std::cout << "   Done ! " << std::endl;
  }

  std::cout << "Test passed" << std::endl;

  return EXIT_SUCCESS;
}
