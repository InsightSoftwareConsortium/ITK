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
#include "itkArray.h"
#include "itkHDF5ImageIO.h"
#include "itkHDF5ImageIOFactory.h"
#include "itkIOTestHelper.h"
#include "itkMetaDataObject.h"

template <typename TPixel>
int HDF5ReadWriteTest(const char *fileName)
{
  int success(EXIT_SUCCESS);
  typedef typename itk::Image<TPixel,3> ImageType;
  typename ImageType::RegionType imageRegion;
  typename ImageType::SizeType size;
  typename ImageType::IndexType index;
  typename ImageType::SpacingType spacing;
  typename ImageType::PointType origin;
  typename ImageType::DirectionType myDirection;
  for(unsigned i = 0; i < 3; i++)
    {
    size[i] = 5;
    index[i] = 0;
    spacing[i] = 1.0 + static_cast<double>(i);
    origin[i] = static_cast<double>(i) * 5.0;
    }
  imageRegion.SetSize(size);
  imageRegion.SetIndex(index);
  typename ImageType::Pointer im =
    itk::IOTestHelper::AllocateImageFromRegionAndSpacing<ImageType>(imageRegion,spacing);

  itk::Matrix<double> mat;
  mat.SetIdentity();
  // 30deg rotation
  mat[1][1] =
  mat[0][0] = 0.866025403784439;
  mat[0][1] = -0.5;
  mat[1][0] = 0.5;
  im->SetDirection(mat);

  // set origin
  im->SetOrigin(origin);
  //
  // add some unique metadata
  itk::MetaDataDictionary & metaDict(im->GetMetaDataDictionary());
  bool metaDataBool(false);
  itk::EncapsulateMetaData<bool>(metaDict,"TestBool",metaDataBool);

  char metaDataChar('c');
  itk::EncapsulateMetaData<char>(metaDict,"TestChar",metaDataChar);

  uint8_t metaDataUChar('u');
  itk::EncapsulateMetaData<uint8_t>(metaDict,"TestUChar",metaDataUChar);

  short metaDataShort(1);
  itk::EncapsulateMetaData<short>(metaDict,"TestShort",metaDataShort);

  uint16_t metaDataUShort(3);
  itk::EncapsulateMetaData<uint16_t>(metaDict,"TestUShort",metaDataUShort);

  int metaDataInt(5);
  itk::EncapsulateMetaData<int>(metaDict,"TestInt",metaDataInt);

  unsigned int metaDataUInt(7);
  itk::EncapsulateMetaData<unsigned int>(metaDict,"TestUInt",metaDataUInt);

  long metaDataLong(5);
  itk::EncapsulateMetaData<long>(metaDict,"TestLong",metaDataLong);

  unsigned long metaDataULong(7);
  itk::EncapsulateMetaData<unsigned long>(metaDict,"TestULong",metaDataULong);

  float metaDataFloat(1.23456);
  itk::EncapsulateMetaData<float>(metaDict,"TestFloat",metaDataFloat);

  double metaDataDouble(1.23456);
  itk::EncapsulateMetaData<double>(metaDict,"TestDouble",metaDataDouble);

  itk::Array<char> metaDataCharArray(5);
  metaDataCharArray[0] = 'h';
  metaDataCharArray[1] = 'e';
  metaDataCharArray[2] = 'l';
  metaDataCharArray[3] = 'l';
  metaDataCharArray[4] = 'o';
  itk::EncapsulateMetaData<itk::Array<char> >(metaDict,"TestCharArray",metaDataCharArray);

  itk::Array<double> metaDataDoubleArray(5);
  metaDataDoubleArray[0] = 3.0;
  metaDataDoubleArray[1] = 1.0;
  metaDataDoubleArray[2] = 4.0;
  metaDataDoubleArray[3] = 5.0;
  metaDataDoubleArray[4] = 2.0;
  itk::EncapsulateMetaData<itk::Array<double> >(metaDict,"TestDoubleArray",metaDataDoubleArray);

  std::string metaDataStdString("Test std::string");
  itk::EncapsulateMetaData<std::string>(metaDict,"StdString",metaDataStdString);

  //
  // fill image buffer
  vnl_random randgen(12345678);
  itk::ImageRegionIterator<ImageType> it(im,im->GetLargestPossibleRegion());
  for(it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    TPixel pix;
    itk::IOTestHelper::RandomPix(randgen,pix);
    it.Set(pix);
    }
  typename ImageType::Pointer im2;
  try
    {
    itk::IOTestHelper::WriteImage<ImageType,itk::HDF5ImageIO>(im,std::string(fileName));
    im2 = itk::IOTestHelper::ReadImage<ImageType>(std::string(fileName));
    }
  catch(itk::ExceptionObject &err)
    {
    std::cout << "itkHDF5ImageIOTest" << std::endl
              << "Exception Object caught: " << std::endl
              << err << std::endl;
    return EXIT_FAILURE;
    }

  if(im->GetOrigin() != im2->GetOrigin())
    {
    std::cout << "Origin read "
              << im2->GetOrigin() << " doesn't match origin written"
              << im->GetOrigin() << std::endl;
    return EXIT_FAILURE;
    }
  if(im->GetSpacing() != im2->GetSpacing())
    {
    std::cout << "Spacing read "
              << im2->GetSpacing() << " doesn't match spacing written"
              << im->GetSpacing() << std::endl;
    return EXIT_FAILURE;
    }
  if(im->GetDirection() != im2->GetDirection())
    {
    std::cout << "Direction read "
              << im2->GetDirection() << " doesn't match direction written"
              << im->GetDirection() << std::endl;
    return EXIT_FAILURE;
    }
  //
  // Check MetaData
  itk::MetaDataDictionary & metaDict2(im2->GetMetaDataDictionary());

  bool metaDataBool2(false);

  if(!itk::ExposeMetaData<bool>(metaDict2,"TestBool",metaDataBool2) ||
     metaDataBool != metaDataBool2)
    {
    std::cerr << "Failure Reading metaData " << "TestBool "
              << metaDataBool << " " << metaDataBool2
              <<  std::endl;
    success = EXIT_FAILURE;
    }

  char metaDataChar2(0);
  if(!itk::ExposeMetaData<char>(metaDict2,"TestChar",metaDataChar2) ||
     metaDataChar2 != metaDataChar)
    {
    std::cerr << "Failure Reading metaData " << "TestChar "
              << metaDataChar2 << " " << metaDataChar
              <<  std::endl;
    success = EXIT_FAILURE;
    }

  uint8_t metaDataUChar2(0);
  if(!itk::ExposeMetaData<uint8_t>(metaDict2,"TestUChar",metaDataUChar2) ||
     metaDataUChar2 != metaDataUChar)
    {
    std::cerr << "Failure Reading metaData " << "TestUChar "
              << metaDataUChar2 << " " << metaDataUChar
              <<  std::endl;
    success = EXIT_FAILURE;
    }

  short metaDataShort2(-1);
  if(!itk::ExposeMetaData<short>(metaDict2,"TestShort",metaDataShort2) ||
     metaDataShort2 != metaDataShort)
    {
    std::cerr << "Failure Reading metaData " << "TestShort "
              << metaDataShort2 << " " << metaDataShort
              <<  std::endl;
    success = EXIT_FAILURE;
    }

  uint16_t metaDataUShort2(0);
  if(!itk::ExposeMetaData<uint16_t>(metaDict2,"TestUShort",metaDataUShort2) ||
     metaDataUShort2 != metaDataUShort)
    {
    std::cerr << "Failure Reading metaData " << "TestUShort "
              << metaDataUShort2 << " " << metaDataUShort
              <<  std::endl;
    success = EXIT_FAILURE;
    }

  int metaDataInt2(1234);
  if(!itk::ExposeMetaData<int>(metaDict2,"TestInt",metaDataInt2) ||
     metaDataInt2 != metaDataInt)
    {
    std::cerr << "Failure Reading metaData " << "TestInt "
              << metaDataInt2 << " " << metaDataInt
              <<  std::endl;
    success = EXIT_FAILURE;
    }

  unsigned int metaDataUInt2(1234);
  if(!itk::ExposeMetaData<unsigned int>(metaDict2,"TestUInt",metaDataUInt2) ||
     metaDataUInt2 != metaDataUInt)
    {
    std::cerr << "Failure Reading metaData " << "TestUInt "
              << metaDataUInt2 << " " << metaDataUInt
              <<  std::endl;
    success = EXIT_FAILURE;
    }

  long metaDataLong2(0);
  if(!itk::ExposeMetaData<long>(metaDict2,"TestLong",metaDataLong2) ||
     metaDataLong2 != metaDataLong)
    {
    std::cerr << "Failure Reading metaData " << "TestLong "
              << metaDataLong2 << " " << metaDataLong
              <<  std::endl;
    success = EXIT_FAILURE;
    }

  unsigned long metaDataULong2(0);
  if(!itk::ExposeMetaData<unsigned long>(metaDict2,"TestULong",metaDataULong2) ||
     metaDataULong2 != metaDataULong)
    {
    std::cerr << "Failure Reading metaData " << "TestULong "
              << metaDataULong2 << " " << metaDataULong
              <<  std::endl;
    success = EXIT_FAILURE;
    }

  float metaDataFloat2(0.0f);
  if(!itk::ExposeMetaData<float>(metaDict2,"TestFloat",metaDataFloat2) ||
     metaDataFloat2 != metaDataFloat)
    {
    std::cerr << "Failure Reading metaData " << "TestFloat "
              << metaDataFloat2 << " " << metaDataFloat
              <<  std::endl;
    success = EXIT_FAILURE;
    }

  double metaDataDouble2(0.0);
  if(!itk::ExposeMetaData<double>(metaDict2,"TestDouble",metaDataDouble2) ||
     metaDataDouble2 != metaDataDouble)
    {
    std::cerr << "Failure reading metaData " << "TestDouble "
              << metaDataDouble2 << " " << metaDataDouble
              <<  std::endl;
    success = EXIT_FAILURE;
    }

  itk::Array<char> metaDataCharArray2;
  metaDataCharArray2.Fill(itk::NumericTraits<char>::Zero);
  if(!itk::ExposeMetaData<itk::Array<char> >(metaDict2,"TestCharArray",
                                             metaDataCharArray2) ||
     metaDataCharArray2 != metaDataCharArray)
    {
    std::cerr << "Failure reading metaData " << "TestCharArray"
              << metaDataCharArray2 << " " << metaDataCharArray2
              <<  std::endl;
    success = EXIT_FAILURE;
    }

  itk::Array<double> metaDataDoubleArray2;
  metaDataDoubleArray2.Fill(itk::NumericTraits<double>::Zero);
  if(!itk::ExposeMetaData<itk::Array<double> >(metaDict2,"TestDoubleArray",
                                             metaDataDoubleArray2) ||
     metaDataDoubleArray2 != metaDataDoubleArray)
    {
    std::cerr << "Failure reading metaData " << "TestDoubleArray "
              << metaDataDoubleArray2 << " " << metaDataDoubleArray
              <<  std::endl;
    success = EXIT_FAILURE;
    }

  std::string metaDataStdString2("");
  if(!itk::ExposeMetaData<std::string>(metaDict2,"StdString",metaDataStdString2) ||
     metaDataStdString2 != metaDataStdString)
    {
    std::cerr << "Failure reading metaData " << "StdString "
              << metaDataStdString2 << " " << metaDataStdString
              <<  std::endl;
    success = EXIT_FAILURE;
    }

  itk::ImageRegionIterator<ImageType> it2(im2,im2->GetLargestPossibleRegion());
  for(it.GoToBegin(),it2.GoToBegin(); !it.IsAtEnd() && !it2.IsAtEnd(); ++it,++it2)
    {
    if(it.Value() != it2.Value())
      {
      std::cout << "Original Pixel (" << it.Value()
                << ") doesn't match read-in Pixel ("
                << it2.Value() << std::endl;
      success = EXIT_FAILURE;
      break;
      }
    }
  // Remove(fileName);
  return success;
}

int
itkHDF5ImageIOTest(int ac, char * av [] )
{
  std::string prefix("");
  if(ac > 1)
    {
    prefix = *++av;
    --ac;
    itksys::SystemTools::ChangeDirectory(prefix.c_str());
    }
  itk::ObjectFactoryBase::RegisterFactory(itk::HDF5ImageIOFactory::New() );

  int result(0);
  result += HDF5ReadWriteTest<uint8_t>("UCharImage.hdf5");
  result += HDF5ReadWriteTest<float>("FloatImage.hdf5");
  result += HDF5ReadWriteTest<itk::RGBPixel<uint8_t> >("RGBImage.hdf5");
  return result != 0;
}
