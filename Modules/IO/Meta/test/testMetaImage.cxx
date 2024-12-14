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

#include <cstdlib>
#include <metaImage.h>
#include "itksys/SystemTools.hxx"
#include "itkMacro.h"
#include "itkMath.h"
#include "itkIOTestHelper.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkTestingComparisonImageFilter.h"
#include "itkMetaImageIO.h"

template <typename PixelType, unsigned int Dimension>
int
ReadWriteCompare(PixelType value, std::string type)
{
  std::cout << "Testing: " << type << '\n';
  using ImageType = itk::Image<PixelType, 3>;
  const char *                      filename = "test.mha";
  typename ImageType::SpacingType   spacing;
  typename ImageType::PointType     origin;
  typename ImageType::DirectionType direction;
  typename ImageType::SizeType      size;
  // Allocate Images
  direction[0][1] = 1;
  direction[1][0] = -1;
  direction[0][0] = 0;
  direction[1][1] = 0;
  for (size_t ii = 0; ii < Dimension; ++ii)
  {
    spacing[ii] = 0.12;
    origin[ii] = 3.2;
    size[ii] = 10;
  }
  const typename ImageType::RegionType region(size);
  const typename ImageType::Pointer    img =
    itk::IOTestHelper::AllocateImageFromRegionAndSpacing<ImageType>(region, spacing);
  { // Fill in entire image
    itk::ImageRegionIterator<ImageType> ri(img, region);
    try
    {
      while (!ri.IsAtEnd())
      {
        ri.Set(value);
        ++ri;
      }
    }
    catch (const itk::ExceptionObject & ex)
    {
      std::cerr << "Error filling array" << ex << '\n';
      return EXIT_FAILURE;
    }
  }
  try
  {
    itk::IOTestHelper::WriteImage<ImageType, itk::MetaImageIO>(img, std::string(filename));
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::string message;
    message = "Problem found while writing image ";
    message += filename;
    message += "\n";
    message += ex.GetLocation();
    message += "\n";
    message += ex.GetDescription();
    std::cerr << message << '\n';
    itk::IOTestHelper::Remove(filename);
    return EXIT_FAILURE;
  }

  typename ImageType::Pointer input;
  try
  {
    input = itk::IOTestHelper::ReadImage<ImageType>(std::string(filename));
  }
  catch (const itk::ExceptionObject & e)
  {
    e.Print(std::cerr);
    itk::IOTestHelper::Remove(filename);
    return EXIT_FAILURE;
  }

  // Now compare the two images
  using DiffType = itk::Testing::ComparisonImageFilter<ImageType, ImageType>;
  const typename DiffType::Pointer diff = DiffType::New();
  diff->SetValidInput(img);
  diff->SetTestInput(input);
  diff->SetDifferenceThreshold(itk::NumericTraits<PixelType>::Zero);
  diff->SetToleranceRadius(0);
  diff->UpdateLargestPossibleRegion();
  if (diff->GetTotalDifference() > 0)
  {
    std::cerr << "Image created and image read are different" << '\n';
    itk::IOTestHelper::Remove(filename);
    return EXIT_FAILURE;
  }
  itk::IOTestHelper::Remove(filename);
  return EXIT_SUCCESS;
}

int
testMetaImage(int, char *[])
{

  MetaImage       tIm(8, 8, 1, 2, MET_CHAR);
  const MetaImage tImCopy(&tIm);

  for (int i = 0; i < 64; ++i)
    tIm.ElementData(i, i);

  for (int i = 0; i < 64; ++i)
  {
    if (itk::Math::NotExactlyEquals(i, tIm.ElementData(i)))
    {
      std::cout << "Assigned Element Values Maintained: FAIL" << '\n';
      return EXIT_FAILURE;
    }
  }

  tIm.Write("test.mha");
  tIm.PrintInfo();

  MetaImage tIm2("test.mha");

  int im2Zero = 0;
  std::cout << "Header size = " << tIm2.HeaderSize() << '\n';
  tIm2.HeaderSize(tIm2.HeaderSize());
  tIm2.Modality(MET_MOD_CT);
  std::cout << "NDims = " << tIm2.NDims() << '\n';
  std::cout << "Modality = " << tIm2.Modality() << '\n';
  std::cout << "DimSize = " << tIm2.DimSize() << '\n';
  std::cout << "Quantity = " << tIm2.Quantity() << '\n';
  std::cout << "SubQuantity = " << tIm2.SubQuantity() << '\n';
  std::cout << "SubQuantity(0) = " << tIm2.SubQuantity(im2Zero) << '\n';
  std::cout << "SequenceID = " << tIm2.SequenceID() << '\n';
  std::cout << "SequenceID[0] = " << tIm2.SequenceID(im2Zero) << '\n';
  auto * sequID = new float[2];
  sequID[0] = 1;
  sequID[1] = 1;
  tIm2.SequenceID(sequID);
  delete[] sequID;
  tIm2.SequenceID(0, 1.0f);

  std::cout << "ElementSizeValid = " << tIm2.ElementSizeValid() << '\n';
  tIm2.ElementSizeValid(tIm2.ElementSizeValid());
  std::cout << "ElementSize = " << tIm2.ElementSize() << '\n';
  std::cout << "ElementSize(0) = " << tIm2.ElementSize(im2Zero) << '\n';

  tIm2.ElementSize(0, 1.0f);
  auto * elmtSize = new float[2];
  elmtSize[0] = 1;
  elmtSize[1] = 2;
  tIm2.ElementSize(elmtSize);
  delete[] elmtSize;

  std::cout << "ElementType = " << tIm2.ElementType() << '\n';
  std::cout << "ElementNumberOfChannels = " << tIm2.ElementNumberOfChannels() << '\n';
  tIm2.ElementNumberOfChannels(tIm2.ElementNumberOfChannels());

  std::cout << "ElementMinMaxValid = " << tIm2.ElementMinMaxValid() << '\n';
  tIm2.ElementMinMaxValid(tIm2.ElementMinMaxValid());

  tIm2.ElementMinMaxRecalc();

  std::cout << "ElementMin = " << tIm2.ElementMin() << '\n';
  std::cout << "ElementMax = " << tIm2.ElementMax() << '\n';

  tIm2.ElementMin(tIm2.ElementMin());
  tIm2.ElementMax(tIm2.ElementMax());

  std::cout << "AutoFreeElementData = " << tIm2.AutoFreeElementData() << '\n';
  std::cout << "ElementDataFileName = " << tIm2.ElementDataFileName() << '\n';

  std::cout << "Element Data: " << tIm2.ElementData() << '\n';

  std::cout << "Testing ConvertElementDataTo: ";
  if (tIm2.ConvertElementDataTo(MET_CHAR, 0, 255))
  {
    std::cout << "[PASSED]" << '\n';
  }
  else
  {
    std::cout << "[FAILED]" << '\n';
    return EXIT_FAILURE;
  }

  tIm2.PrintInfo();
  for (int i = 0; i < 64; ++i)
  {
    if (itk::Math::NotExactlyEquals(i, tIm.ElementData(i)))
    {
      std::cout << "Read Element Values: FAIL" << '\n';
      return EXIT_FAILURE;
    }
  }


  tIm2.AutoFreeElementData(tIm2.AutoFreeElementData());

  // Testing copy
  std::cout << "Testing copy:";
  const MetaImage imCopy(&tIm2);
  std::cout << " [PASSED]" << '\n';


  // testing metaImageUtils
  std::string modality;
  if (!MET_ImageModalityToString(MET_MOD_CT, modality))
  {
    std::cout << "MET_ImageModalityToString: FAIL" << '\n';
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "Modality  = " << modality << '\n';
  }

  // Testing Append function
  std::cout << "Testing Append:";

  if (tIm2.Append("test.mha"))
  {
    std::cout << " [PASSED]" << '\n';
  }
  else
  {
    std::cout << " [FAILED]" << '\n';
  }

  itksys::SystemTools::RemoveFile("test.mha");

  // Testing all pixel types
  if (ReadWriteCompare<unsigned char, 3>(static_cast<unsigned char>(12), "unsigned char"))
  {
    return EXIT_FAILURE;
  }
  if (ReadWriteCompare<char, 3>(static_cast<char>(-8), "char"))
  {
    return EXIT_FAILURE;
  }
  if (ReadWriteCompare<unsigned short, 3>(static_cast<unsigned short>(8192), "unsigned short"))
  {
    return EXIT_FAILURE;
  }
  if (ReadWriteCompare<short, 3>(static_cast<short>(-16384), "short"))
  {
    return EXIT_FAILURE;
  }
  if (ReadWriteCompare<unsigned int, 3>(static_cast<unsigned int>(2718281), "unsigned int"))
  {
    return EXIT_FAILURE;
  }
  if (ReadWriteCompare<int, 3>(static_cast<int>(-3141592), "int"))
  {
    return EXIT_FAILURE;
  }
  if (ReadWriteCompare<unsigned long, 3>(static_cast<unsigned long>(27182818), "unsigned long"))
  {
    return EXIT_FAILURE;
  }
  if (ReadWriteCompare<long, 3>(static_cast<long>(-31415926), "long"))
  {
    return EXIT_FAILURE;
  }
  if (ReadWriteCompare<unsigned long long, 3>(static_cast<unsigned long long>(8589934592ull), "unsigned long long"))
  {
    return EXIT_FAILURE;
  }
  if (ReadWriteCompare<long long, 3>(static_cast<long long>(-8589934592ll), "long long"))
  {
    return EXIT_FAILURE;
  }
  if (ReadWriteCompare<float, 3>(static_cast<float>(1.23456), "float"))
  {
    return EXIT_FAILURE;
  }
  if (ReadWriteCompare<double, 3>(static_cast<double>(7.891011121314), "double"))
  {
    return EXIT_FAILURE;
  }

  std::cout << "[DONE]" << '\n';

  return EXIT_SUCCESS;
}
