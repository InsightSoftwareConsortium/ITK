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

#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkVTKImageIO.h"
#include "itkGenerateImageSource.h"

#include <fstream>
#include <iostream>
#include <algorithm>
#include "itkMath.h"

namespace itk
{
/** \class ConstantImageSource
 * Image Source that generates an image with constant pixel value.
 */
template< class TOutputImage >
class ConstantImageSource:public GenerateImageSource< TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ConstantImageSource                 Self;
  typedef ConstantImageSource< TOutputImage > Superclass;
  typedef SmartPointer< Self >                Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ConstantImageSource, GenerateImageSource);

  /** Set the value to fill the image. */
  itkSetMacro(Value, typename TOutputImage::PixelType);

protected:
  ConstantImageSource()
  {
    m_Value = NumericTraits< typename TOutputImage::PixelType >::ZeroValue();
  }
  ~ConstantImageSource() ITK_OVERRIDE {}

  /** Does the real work. */
  virtual void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ConstantImageSource);

  typename TOutputImage::PixelType m_Value;
};

template< class TOutputImage >
void ConstantImageSource< TOutputImage >
::GenerateData()
{
  TOutputImage* out = this->GetOutput();
  out->SetBufferedRegion(out->GetRequestedRegion());
  out->Allocate();

  out->FillBuffer( m_Value );
}

}// end namespace

/**
 * Compares two image regions.
 * Assumes that the region is valid and buffered in both images.
 */
template<class TImage>
bool ImagesEqual(const TImage* img1, const TImage* img2,
        const typename TImage::RegionType& region)
{
  if( !img1->GetBufferedRegion().IsInside(region) ) return false;
  if( !img2->GetBufferedRegion().IsInside(region) ) return false;

  itk::ImageRegionConstIterator<TImage> it1(img1, region);
  itk::ImageRegionConstIterator<TImage> it2(img2, region);

  for(it1.GoToBegin(), it2.GoToBegin(); !it1.IsAtEnd(); ++it1, ++it2)
    {
    if( itk::Math::NotExactlyEquals(it1.Get(), it2.Get()) )
      {
      return false;
      }
    }

  return true;
}
template<class TImage>
bool ImagesEqual(const TImage* img1, const TImage* img2)
{
  return ImagesEqual(img1, img2, img1->GetLargestPossibleRegion());
}

/**
 * Generates an image with constant pixel value 23 (binary pattern 0..010111,
 * little endian) and saves it with streamed writing, reads it non-streamed and
 * compares the original image with the read image.
 */
template<class TScalar, unsigned int TDimension>
int TestStreamWrite(char *file1, unsigned int numberOfStreams = 0)
{
  typedef itk::Image<TScalar,TDimension> ImageType;

  // Create a source object (in this case a constant image).
  typename ImageType::SizeValueType size[TDimension];
  for (unsigned int i = 0; i < TDimension; i++)
    {
    size[i] = 2 << (i + 1);
    }
  typename itk::ConstantImageSource<ImageType>::Pointer constValueImageSource;
  constValueImageSource = itk::ConstantImageSource<ImageType>::New();
  constValueImageSource->SetValue(static_cast<TScalar>(23));
  constValueImageSource->SetSize(size);

  typename ImageType::SpacingValueType spacing[3] = {5.0f, 10.0f, 15.0f};
  typename ImageType::PointValueType origin[3] = {-5.0f, -10.0f, -15.0f};

  constValueImageSource->SetSpacing(spacing);
  constValueImageSource->SetOrigin(origin);

  ImageType* consValueImage = constValueImageSource->GetOutput();

  // Create a mapper (in this case a writer). A mapper
  // is templated on the input type.
  itk::VTKImageIO::Pointer vtkIO;
  vtkIO = itk::VTKImageIO::New();
  vtkIO->SetFileTypeToBinary();

  // Write out the image
  typename itk::ImageFileWriter<ImageType>::Pointer writer;
  writer = itk::ImageFileWriter<ImageType>::New();
  writer->SetInput(consValueImage);
  writer->SetFileName(file1);
  if ( numberOfStreams > 0 )
  {
    writer->SetNumberOfStreamDivisions( numberOfStreams );
  }
  writer->Write();

  // Check if written file is correct
  typename itk::ImageFileReader<ImageType>::Pointer reader;
  reader = itk::ImageFileReader<ImageType>::New();
  reader->SetImageIO(vtkIO);
  reader->SetFileName(file1);

  consValueImage->SetRequestedRegion(consValueImage->GetLargestPossibleRegion());
  consValueImage->Update();
  reader->Update();
  bool imagesEqual = ImagesEqual(consValueImage, reader->GetOutput());

  std::string componentType = itk::ImageIOBase::GetComponentTypeAsString( vtkIO->GetComponentType() );

  if ( !imagesEqual )
    {
    std::cout << "[FAILED] writing (" << componentType << ", dim = " << TDimension
              << ", numberOfStreams = " << numberOfStreams << ")" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED] writing (" << componentType << ", dim = " << TDimension
            << ", numberOfStreams = " << numberOfStreams << ")" << std::endl;
  return EXIT_SUCCESS;
}

/**
 * Generates an image with constant pixel value 23 (binary pattern 0..010111,
 * little endian) and saves it with non-streamed  writing, reads it streamed and
 * compares the original image with the read image.
 */
template<class TScalar, unsigned int TDimension>
int TestStreamRead(char *file1, unsigned int numberOfStreams = 0)
{
  typedef itk::Image<TScalar,TDimension> ImageType;

  // Create a source object (in this case a constant image).
  typename ImageType::SizeValueType size[TDimension];
  for (unsigned int i = 0; i < TDimension; i++)
    {
    size[i] = 2 << (i + 1);
    }
  typename itk::ConstantImageSource<ImageType>::Pointer constValueImageSource;
  constValueImageSource = itk::ConstantImageSource<ImageType>::New();
  constValueImageSource->SetValue(static_cast<TScalar>(23));
  constValueImageSource->SetSize(size);

  typename ImageType::SpacingValueType spacing[3] = {5.0f, 10.0f, 15.0f};
  typename ImageType::PointValueType origin[3] = {-5.0f, -10.0f, -15.0f};

  constValueImageSource->SetSpacing(spacing);
  constValueImageSource->SetOrigin(origin);

  ImageType* consValueImage = constValueImageSource->GetOutput();

  // Create a mapper (in this case a writer). A mapper
  // is templated on the input type.
  itk::VTKImageIO::Pointer vtkIO;
  vtkIO = itk::VTKImageIO::New();
  vtkIO->SetFileTypeToBinary();

  // Write out the image non-streamed
  typename itk::ImageFileWriter<ImageType>::Pointer writer;
  writer = itk::ImageFileWriter<ImageType>::New();
  writer->SetInput(consValueImage);
  writer->SetFileName(file1);
  writer->SetNumberOfStreamDivisions( 1 );
  writer->Write();

  // Check if written file is correct
  typename itk::ImageFileReader<ImageType>::Pointer reader;
  reader = itk::ImageFileReader<ImageType>::New();
  reader->SetImageIO(vtkIO);
  reader->SetFileName(file1);
  if (numberOfStreams > 0)
    {
    reader->UseStreamingOn();
    }

  // Simulate streaming and compares regions
  numberOfStreams = std::max(1u, std::min(static_cast<unsigned int>(size[TDimension-1]), numberOfStreams));
  typename ImageType::SizeValueType width = (size[TDimension-1]+numberOfStreams-1) / numberOfStreams;
  typename ImageType::RegionType totalRegion = consValueImage->GetLargestPossibleRegion();

  ImageType* readImage = reader->GetOutput();
  consValueImage->SetRequestedRegion(totalRegion);
  consValueImage->Update();

  bool imagesEqual = true;
  for (unsigned int i = 0; i < numberOfStreams; ++i)
    {
    typename ImageType::RegionType region(totalRegion);
    region.SetIndex(TDimension-1, region.GetIndex(TDimension-1) + i * width);
    region.SetSize(TDimension-1, width);
    region.Crop(totalRegion);

    readImage->SetRequestedRegion(region);
    readImage->Update();

    if(!ImagesEqual(readImage, consValueImage, region))
      {
          imagesEqual = false;
          break;
      }
    }

  std::string componentType = itk::ImageIOBase::GetComponentTypeAsString( vtkIO->GetComponentType() );

  if ( !imagesEqual )
    {
    std::cout << "[FAILED] reading (" << componentType << ", dim = " << TDimension
              << ", numberOfStreams = " << numberOfStreams << ")" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED] reading (" << componentType << ", dim = " << TDimension
            << ", numberOfStreams = " << numberOfStreams << ")" << std::endl;
  return EXIT_SUCCESS;
}

int itkVTKImageIOStreamTest(int argc, char* argv[] )
{

  if( argc < 2 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  output" << std::endl;
    return EXIT_FAILURE;
    }

  unsigned int numberOfStreams = 2;
  int status = 0;

#define ReadWriteTestMACRO(scalarType) \
  status += TestStreamWrite<scalarType,2>(argv[1], 0); \
  status += TestStreamWrite<scalarType,2>(argv[1], numberOfStreams); \
  status += TestStreamWrite<scalarType,3>(argv[1], 0); \
  status += TestStreamWrite<scalarType,3>(argv[1], numberOfStreams); \
  status += TestStreamRead<scalarType,2>(argv[1], 0); \
  status += TestStreamRead<scalarType,2>(argv[1], numberOfStreams); \
  status += TestStreamRead<scalarType,3>(argv[1], 0); \
  status += TestStreamRead<scalarType,3>(argv[1], numberOfStreams);

  ReadWriteTestMACRO(float)
  ReadWriteTestMACRO(double)
  ReadWriteTestMACRO(unsigned char)
  ReadWriteTestMACRO(char)
  ReadWriteTestMACRO(unsigned short)
  ReadWriteTestMACRO(short)
  ReadWriteTestMACRO(unsigned int)
  ReadWriteTestMACRO(int)
  ReadWriteTestMACRO(unsigned long)
  ReadWriteTestMACRO(long)
  ReadWriteTestMACRO(unsigned long long)
  ReadWriteTestMACRO(long long)

  return status;
}
