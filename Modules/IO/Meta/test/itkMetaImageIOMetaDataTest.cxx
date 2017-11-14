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
#include <iostream>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRandomImageSource.h"
#include "itkMetaDataObject.h"
#include "itkMetaImageIO.h"


#define SPECIFIC_IMAGEIO_MODULE_TEST

/** read an image using ITK -- image-based template */
template <typename TImage>
typename TImage::Pointer ReadImage( const std::string &fileName )
{
  typename TImage::Pointer image;
  typedef itk::ImageFileReader<TImage> ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( fileName.c_str() );

  itk::MetaImageIO::Pointer io = itk::MetaImageIO::New();
  reader->SetImageIO(io);

  try
    {
    reader->Update();
    }
  catch ( itk::ExceptionObject &err )
    {
    std::cout << "Caught an exception: " << std::endl;
    std::cout << err << " " << __FILE__ << " " << __LINE__ << std::endl;
    throw;
    }
  catch ( ... )
    {
    std::cout << "Error while reading in image " << fileName << std::endl;
    throw;
    }
  image = reader->GetOutput();
  return image;
}

/** write an image using ITK */
template <typename ImageType>
void
WriteImage(typename ImageType::Pointer &image,
           const std::string &fileName)
{
  typedef itk::ImageFileWriter<ImageType> WriterType;
  typename  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( fileName.c_str() );

  itk::MetaImageIO::Pointer io = itk::MetaImageIO::New();
  writer->SetImageIO(io);

  writer->SetInput(image);

  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject &err )
    {
    std::cout << "Exception Object caught: " << std::endl;
    std::cout << err << std::endl;
    throw;
    }
  catch ( ... )
    {
    std::cout << "Error while writing in image " << fileName << std::endl;
    throw;
    }
}

template <typename TValue>
bool
Equal(TValue &a, TValue &b)
{
  return a == b;
}
template <>
bool
Equal<double>(double &a, double &b)
{
  double diff(itk::Math::abs(a - b));
  if(diff == 0.0)
    {
    return true;
    }
  // base test roughly on magnitude of
  // arguments.
  diff /= itk::Math::abs(a)+itk::Math::abs(b);
  if(diff > 0.000001)
    {
    return false;
    }
  return true;
}

template <>
bool
Equal<float>(float &a, float &b)
{
  double _a(a); double _b(b);
  return Equal(_a,_b);
}

template <typename TValue>
bool
TestMatch(itk::MetaDataDictionary &dict,
          const char *const key,
          TValue expectedValue)
{
  std::istringstream is;
  std::string stringValue;
  if(!itk::ExposeMetaData<std::string>(dict,key,stringValue))
    {
    std::cerr << "Key " << key << " not found" << std::endl;
    return false;
    }
  TValue nativeValue;
  is.str(stringValue);
  is >> nativeValue;
  if(!Equal<TValue>(nativeValue,expectedValue))
    {
    std::cerr << "Key " << key << " found with unexpected value "
              << nativeValue << std::endl;
    return false;
    }
    std::cout << "Key " << key << " found with expected value "
              << nativeValue << std::endl;
    return true;
}

int
itkMetaImageIOMetaDataTest(int argc, char * argv [] )
{
  if(argc < 2)
    {
    std::cerr << "Usage: metadatatest outputimage" << std::endl;
    return 1;
    }
  // write out an image -- using a random image source, but
  // the image data is irrelevant
  const int Dim(2);

  typedef unsigned char                     PixelType;
  typedef itk::Image<PixelType,Dim>         ImageType;
  typedef itk::RandomImageSource<ImageType> SourceType;

  SourceType::Pointer source = SourceType::New();
  ImageType::SizeValueType size[Dim] = { 32,32 };
  source->SetSize(size);
  source->SetMin(itk::NumericTraits<PixelType>::min());
  source->SetMax(itk::NumericTraits<PixelType>::max());
  source->Update();

  ImageType::Pointer randImage(source->GetOutput());
  itk::MetaDataDictionary &dict(randImage->GetMetaDataDictionary());
  //
  // add an arbitrary key to check whether it persists with the image
  //
  {
  // Add string key
  std::string key("hello"); std::string value("world");
  itk::EncapsulateMetaData<std::string>(dict,key,value);
  }
  {
  // Add double
  std::string key("double"); double value(7.891011);
  itk::EncapsulateMetaData<double>(dict,key,value);
  }
  {
  // Add float
  std::string key("float"); float value(1.23456);
  itk::EncapsulateMetaData<float>(dict,key,value);
  }
  {
  // Add long
  std::string key("long"); long value(-31415926);
  itk::EncapsulateMetaData<int>(dict,key,value);
  }
  {
  // Add unsigned long
  std::string key("unsigned_long"); unsigned long value(27182818);
  itk::EncapsulateMetaData<unsigned long>(dict,key,value);
  }
  {
  // Add long long
  std::string key("long_long"); long long value(-8589934592ll);
  itk::EncapsulateMetaData<long long>(dict,key,value);
  }
  {
  // Add unsigned long long
  std::string key("unsigned_long_long"); unsigned long long value(8589934592ull);
  itk::EncapsulateMetaData<unsigned long long>(dict,key,value);
  }
  {
  // Add int
  std::string key("int"); int value(-3141592);
  itk::EncapsulateMetaData<int>(dict,key,value);
  }
  {
  // Add unsigned int
  std::string key("unsigned_int"); unsigned int value(2718281);
  itk::EncapsulateMetaData<unsigned int>(dict,key,value);
  }
  {
  // Add short
  std::string key("short"); short value(-16384);
  itk::EncapsulateMetaData<short>(dict,key,value);
  }
  {
  // Add short
  std::string key("unsigned_short"); unsigned value(8192);
  itk::EncapsulateMetaData<unsigned short>(dict,key,value);
  }
  {
  // Add char
  std::string key("char"); char value('c');
  itk::EncapsulateMetaData<char>(dict,key,value);
  }
  {
  std::string key("bool"); bool value(true);
  itk::EncapsulateMetaData<bool>(dict,key,value);
  }

  WriteImage<ImageType>(randImage,argv[1]);

  //
  // Read the image just written and check if the key we added
  // persisted with the file.
  ImageType::Pointer randImage2 = ReadImage<ImageType>(argv[1]);

  dict = randImage2->GetMetaDataDictionary();

  std::string value("world");
  if(!TestMatch<std::string>(dict,"hello",value))
    {
    return 1; // error
    }
  // Add double
  if(!TestMatch< double >(dict,"double",7.891011))
    {
    return 1; // error
    }
  // Add float
  if(!TestMatch< float >(dict,"float",1.23456))
    {
    return 1; // error
    }
  // Add long
  if(!TestMatch< long >(dict,"long",-31415926))
    {
    return 1; // error
    }
  // Add unsigned long
  if(!TestMatch< unsigned long >(dict,"unsigned_long",27182818))
    {
    return 1; // error
    }
  // Add long long
  if(!TestMatch< long long >(dict,"long_long",-8589934592ll))
    {
    return 1; // error
    }
  // Add unsigned long long
  if(!TestMatch< unsigned long long >(dict,"unsigned_long_long",8589934592ull))
    {
    return 1; // error
    }
  // Add int
  if(!TestMatch< int >(dict,"int",-3141592))
    {
    return 1; // error
    }
  // Add unsigned int
  if(!TestMatch< unsigned int >(dict,"unsigned_int",2718281))
    {
    return 1; // error
    }
  // Add short
  if(!TestMatch< short >(dict,"short",-16384))
    {
    return 1; // error
    }
  // Add short
  if(!TestMatch< unsigned >(dict,"unsigned_short",8192))
    {
    return 1; // error
    }
  // Add char
  if(!TestMatch< char >(dict,"char",'c'))
    {
    return 1; // error
    }
  // Add unsigned char
  if(!TestMatch<bool >(dict,"bool",true))
    {
    return 1; // error
    }

  return 0;
}
