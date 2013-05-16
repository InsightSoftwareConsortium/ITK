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
#ifndef __itkMGHImageIOTest_h
#define __itkMGHImageIOTest_h

#include <fstream>
#include <vector>
#include "itkImageRegionIterator.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMGHImageIO.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkRandomImageSource.h"
#include "itkDiffusionTensor3D.h"

template <class TPixelType, unsigned int VImageDimension>
typename itk::Image<TPixelType, VImageDimension>::Pointer
itkMGHImageIOTestGenerateRandomImage(unsigned int size)
{
  typedef itk::Image<TPixelType, VImageDimension> ImageType;

  const double dir1[3] = { 0, -1, 0 };
  const double dir2[3] = { 1, 0, 0 };
  const double dir3[3] = { 0, 0, -1 };

  typename ImageType::DirectionType direction;
  direction.GetVnlMatrix().set_column(0,dir1);
  direction.GetVnlMatrix().set_column(1,dir2);
  direction.GetVnlMatrix().set_column(2,dir3);

  typename ImageType::SizeType sz;
  typename ImageType::SpacingType spacing;
  typename ImageType::PointType origin;
  for (unsigned int i = 0; i < VImageDimension; i++)
    {
    sz[i]      = size;
    spacing[i] = static_cast<float>(i+1);
    origin[i]  = static_cast<float>(i);
    }

  typename itk::RandomImageSource<ImageType>::Pointer source
    = itk::RandomImageSource<ImageType>::New();

  source->SetDirection(direction);
  source->SetSize(sz);
  source->SetOrigin(origin);
  source->SetSpacing(spacing);

  source->Update();
  return (source->GetOutput());
}

//Template specialization for itkDiffusionTensor3D
template<>
itk::Image<itk::DiffusionTensor3D<float>,3>::Pointer
itkMGHImageIOTestGenerateRandomImage< itk::DiffusionTensor3D<float> , 3 >(unsigned int size)
{
  typedef itk::Image<itk::DiffusionTensor3D<float>,3> TensorImageType;
  typedef TensorImageType::Pointer                    TensorImagePointer;

  const double dir1[3] = { 0, -1, 0 };
  const double dir2[3] = { 1, 0, 0 };
  const double dir3[3] = { 0, 0, -1 };

  TensorImageType::DirectionType direction;
  direction.GetVnlMatrix().set_column(0,dir1);
  direction.GetVnlMatrix().set_column(1,dir2);
  direction.GetVnlMatrix().set_column(2,dir3);

  TensorImageType::SizeType      sz;
  TensorImageType::SpacingType   spacing;
  TensorImageType::PointType     origin;
  for(unsigned int i = 0; i < 3; ++i)
    {
    sz[i] = size;
    spacing[i] = static_cast<double>(i + 1);
    origin[i] = static_cast<double>( (i & 1) ? -i : i );
    }

  TensorImagePointer tensorImage = TensorImageType::New();
  tensorImage->SetRegions(sz);
  tensorImage->SetSpacing(spacing);
  tensorImage->SetOrigin(origin);
  tensorImage->Allocate();
  tensorImage->SetDirection(direction);

  itk::DiffusionTensor3D<double>   pix;
  typedef itk::Image<double,3>     ScalarImageType;
  typedef ScalarImageType::Pointer ScalarImagePointer;

  std::vector<ScalarImagePointer> scalarImageVec;
  for(unsigned i = 0; i < pix.Size(); ++i)
    {
    ScalarImagePointer tempImage = itkMGHImageIOTestGenerateRandomImage<double,3>(size);
    scalarImageVec.push_back(tempImage);
    }
  itk::ImageRegionIteratorWithIndex<TensorImageType>
    tensorIt(tensorImage,tensorImage->GetLargestPossibleRegion());
  for(tensorIt.GoToBegin(); !tensorIt.IsAtEnd(); ++tensorIt)
    {
    for(unsigned int i = 0; i < pix.Size(); ++i)
      {
      const TensorImageType::IndexType & index = tensorIt.GetIndex();
      pix.SetNthComponent(i,scalarImageVec[i]->GetPixel(index));
      }
      tensorIt.Set(pix);
    }
  return tensorImage;
}


template<class TPixelType, unsigned int VImageDimension>
int itkMGHImageIOTestReadWriteTest(std::string fn, unsigned int size,
                                    std::string inputFile, bool compression=false)
{
  typedef itk::Image<TPixelType, VImageDimension> ImageType;

  typename itk::ImageFileReader<ImageType>::Pointer reader
    = itk::ImageFileReader<ImageType>::New();
  typename itk::ImageFileWriter<ImageType>::Pointer writer
    = itk::ImageFileWriter<ImageType>::New();

  itk::MGHImageIO::Pointer io = itk::MGHImageIO::New();
  reader->SetImageIO(io);
  writer->SetImageIO(io);

  typename ImageType::Pointer image;

  if (inputFile != "null")
    {
    typename itk::ImageFileReader<ImageType>::Pointer tmpReader
      = itk::ImageFileReader<ImageType>::New();
    tmpReader->SetImageIO(io);
    tmpReader->SetFileName(inputFile.c_str());
    try
      {
      tmpReader->Update();
      std::cout << "DONE READING INPUT IMAGE" << std::endl;
      }
    catch(itk::ExceptionObject &e)
      {
      std::cerr << e << std::endl;
      return EXIT_FAILURE;
      }

    image = tmpReader->GetOutput();
    }
  else
    {
    // Generate a random image.
    image = itkMGHImageIOTestGenerateRandomImage<TPixelType, VImageDimension>(size);
    }

  // Write, then read the image.
  try
    {
    writer->SetFileName(fn.c_str());
    if (compression==true)
      { writer->UseCompressionOn(); }
    else
      { writer->UseCompressionOff();}
    reader->SetFileName(fn.c_str());
    //writer->SetFileName("testDebug.mhd");
    //reader->SetFileName("testDebug.mhd");

    }
  catch(itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

    writer->SetInput(image);

    image->Print(std::cout);
    std::cout << "----------" << std::endl;

  try
    {
    writer->Update();
    std::cout << "DONE WRITING TEST IMAGE" << std::endl;
    reader->Update();
    std::cout << "DONE READING TEST IMAGE" << std::endl;
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "Exception in file reader or writer " << std::endl;
    std::cerr << e.GetDescription() << std::endl;
    std::cerr << e.GetLocation() << std::endl;
    return EXIT_FAILURE;
    }

  // Print the image information.

  reader->GetOutput()->Print(std::cout);
  std::cout << std::endl;

  // Compare input and output images.
  itk::ImageRegionIterator<ImageType> a(image, image->GetRequestedRegion());
  itk::ImageRegionIterator<ImageType> b(reader->GetOutput(),
                                        reader->GetOutput()->GetRequestedRegion());
  for (a.GoToBegin(), b.GoToBegin(); ! a.IsAtEnd(); ++a, ++b)
    {
    if ( b.Get() != a.Get() )
      {
      std::cerr << "At index " << b.GetIndex() << " value " << b.Get() << " should be " << a.Get() << std::endl;
      return EXIT_FAILURE;
      }
    }
  return EXIT_SUCCESS;
}

#endif // __itkMGHImageIOTest_h_
