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
#ifndef itkMGHImageIOTest_h
#define itkMGHImageIOTest_h

#include <fstream>
#include <vector>
#include <iomanip>
#include "itkImageRegionIterator.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMGHImageIO.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkRandomImageSource.h"
#include "itkDiffusionTensor3D.h"

#include "itkEuler3DTransform.h"



template <class TPixelType, unsigned int VImageDimension>
typename itk::Image<TPixelType, VImageDimension>::Pointer
itkMGHImageIOTestGenerateRandomImage(const unsigned int size)
{
  typedef itk::Euler3DTransform<double> EulerTransformType;
  EulerTransformType::Pointer eulerTransform = EulerTransformType::New();
  eulerTransform->SetIdentity();

  // 15 degrees in radians
  const double angleX = 15.0 * std::atan( 1.0 ) / 45.0;
  // 10 degrees in radians
  const double angleY = 10.0 * std::atan( 1.0 ) / 45.0;
  // 5 degrees in radians
  const double angleZ = 5.0 * std::atan( 1.0 ) / 45.0;
  eulerTransform->SetRotation(angleX, angleY, angleZ);
 
  typedef itk::Image<TPixelType, VImageDimension> ImageType;

  typename ImageType::DirectionType permuteDirections;
  const double dir1[3] = { -1.,  0.,  0. };
  const double dir2[3] = {  0.,  0.,  1. };
  const double dir3[3] = {  0., -1.,  0.  };
  permuteDirections.GetVnlMatrix().set_column(0,dir1);
  permuteDirections.GetVnlMatrix().set_column(1,dir2);
  permuteDirections.GetVnlMatrix().set_column(2,dir3);

  typename ImageType::DirectionType direction = eulerTransform->GetMatrix()*permuteDirections;
  
  for (unsigned int i = 0; i < VImageDimension; ++i)
    {
    for (unsigned int j = 0; j < VImageDimension; ++j)
      {
      direction[i][j] = static_cast<float>(direction[i][j]); //Truncate for testing purposes
      }
    }

  typename ImageType::SizeType sz;
  typename ImageType::SpacingType spacing;
  typename ImageType::PointType origin;

  for (unsigned int i = 0; i < VImageDimension; ++i)
    {
    sz[i]      = size;
    spacing[i] = static_cast<float>(i+1.234567);
    origin[i]  = static_cast<float>(1234.5);
    }

  typename itk::RandomImageSource<ImageType>::Pointer source
    = itk::RandomImageSource<ImageType>::New();

  source->SetDirection(direction);
  source->SetSize(sz);
  source->SetOrigin(origin);
  source->SetSpacing(spacing);

  source->Update();
  typename ImageType::Pointer outImage=source->GetOutput();

    {
    itk::MetaDataDictionary & thisDic = outImage->GetMetaDataDictionary();
    //Add meta data to dictionary
    // set TR, Flip, TE, FI, FOV //TODO: Add code that verifies these values
    float fBufTR = 2.0F;
    float fBufFA=89.1F;
    float fBufTE=1.5F;
    float fBufTI=0.75F;
    float fBufFOV=321.0F;
    itk::EncapsulateMetaData<float>(thisDic,
      std::string("TR"), fBufTR);
    // try to read flipAngle
    itk::EncapsulateMetaData<float>(thisDic,
      std::string("FlipAngle"), fBufFA);
    // TE
    itk::EncapsulateMetaData<float>(thisDic,
      std::string("TE"), fBufTE);
    // TI
    itk::EncapsulateMetaData<float>(thisDic,
      std::string("TI"), fBufTI);
    // FOV
    itk::EncapsulateMetaData<float>(thisDic,
      std::string("FoV"), fBufFOV);
    }
  return outImage;
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
    spacing[i] = static_cast<double>(i*3.21 + 1.0);
    origin[i] = static_cast<double>( 1.23456 );
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
bool itkMGHImageIOTestReadWriteTest(std::string fn, unsigned int size,
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

  typename ImageType::Pointer reference_image;

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
    reference_image = tmpReader->GetOutput();
    }
  else
    {
    // Generate a random reference_image.
    reference_image = itkMGHImageIOTestGenerateRandomImage<TPixelType, VImageDimension>(size);
    }

  // Write, then read the reference_image.
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

    writer->SetInput(reference_image);
    reference_image->Print(std::cout);
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

  // Print the reference_image information.

  typename ImageType::Pointer test_image=reader->GetOutput();
  test_image->Print(std::cout);
  std::cout << std::endl;

  bool isFailingPixelValues = false;
  bool isFailingOrigin = false;
  bool isFailingSpacing = false;
  bool isFailingDirection = false;
  // Compare input and output images.
  itk::ImageRegionIterator<ImageType> a(reference_image, reference_image->GetRequestedRegion());
  itk::ImageRegionIterator<ImageType> b(test_image, test_image->GetRequestedRegion());
  for (a.GoToBegin(), b.GoToBegin(); ! a.IsAtEnd(); ++a, ++b)
    {
    if ( b.Get() != a.Get() )
      {
      std::cerr << "At index " << b.GetIndex() << " value " << b.Get() << " should be " << a.Get() << std::endl;
      isFailingPixelValues = true;
      }
    }
  for( int idx = 0 ; idx < 3; ++idx )
    {
    // itk::Math::FloatAlmostEqual( floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat, 4, 0.1f)
    if( ! itk::Math::FloatAlmostEqual( test_image->GetSpacing()[idx],
        reference_image->GetSpacing()[idx], 4, 1e-7 ) )
      {
      isFailingSpacing = true;
      }
    if( ! itk::Math::FloatAlmostEqual( test_image->GetOrigin()[idx],
        reference_image->GetOrigin()[idx], 100000, 1e-1 ) )
      {
      isFailingOrigin = true;
      }
    for( int idx2 = 0; idx2 < 3; ++idx2 )
      {
      if( ! itk::Math::FloatAlmostEqual( test_image->GetDirection()[idx][idx2],
          reference_image->GetDirection()[idx][idx2], 4, 1e-7 ) )
        {
        isFailingDirection = true;
        }
      }
    }
  std::cerr << std::fixed << std::setprecision(10) << std::endl;
  //Report failure dianostics
  if(isFailingSpacing)
    {
    std::cerr << "ERROR:  Invalid Spacing: " 
      << test_image->GetSpacing() <<  " ! = " << reference_image->GetSpacing()
      << std::endl;
    }
  if(isFailingOrigin)
    {
    std::cerr << "ERROR:  Invalid Origin: " 
      << test_image->GetOrigin() <<  " ! = " << reference_image->GetOrigin()
      << std::endl;
    }
  if(isFailingDirection)
    {
    std::cerr << "ERROR:  Invalid Direction: \n" 
      << test_image->GetDirection() <<  " ! = \n" << reference_image->GetDirection()
      << std::endl;
    }
  return ! (isFailingPixelValues || isFailingOrigin || isFailingSpacing || isFailingDirection);
}

#endif //itkMGHImageIOTest_h_
