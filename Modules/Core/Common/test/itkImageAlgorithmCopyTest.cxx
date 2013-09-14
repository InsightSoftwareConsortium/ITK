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
#include "itkTimeProbe.h"
#include "itkImageAlgorithm.h"


template <typename TImage >
static void AverageTestCopy( typename TImage::SizeType &size )
{

  std::cout << "--- For image size: " << size << " ---" << std::endl;

  typedef typename TImage::PixelType PixelType;
  typedef TImage                     ImageType;
  typename ImageType::RegionType     region;


  typename ImageType::IndexType index;
  index.Fill(0);
  region.SetSize(size);
  region.SetIndex(index);

  typename ImageType::Pointer inImage = ImageType::New();
  inImage->SetRegions( region );
  inImage->Allocate();
  inImage->FillBuffer(12);

  typename ImageType::Pointer outImage = ImageType::New();
  outImage->SetRegions( region );
  outImage->Allocate();
  outImage->FillBuffer(12);


  itk::TimeProbe t;

  typedef itk::ImageRegionIterator<ImageType> ImageIterator;

  for ( unsigned int i = 0; i < 10; ++i )
    {
    t.Start();

    ImageIterator outIt( outImage, region );
    ImageIterator sourceIt( inImage, region );

    // walk the output region, and sample the source image
    while ( !outIt.IsAtEnd() )
      {
      // copy the input pixel to the output
      outIt.Set(  sourceIt.Get() );
      ++outIt;
      ++sourceIt;
      }

    t.Stop();
    }

  std::cout << "\tIterator Copy Average Time: " << t.GetMean() << t.GetUnit() << std::endl;


  itk::TimeProbe t2;

  for ( unsigned int i = 0; i < 10; ++i )
    {
    t2.Start();
    memcpy( inImage->GetBufferPointer(), outImage->GetBufferPointer(), region.GetNumberOfPixels()*sizeof( PixelType ) );
    t2.Stop();
    }

  std::cout << "\tmemcpy Copy Average Time: " << t2.GetMean() << t2.GetUnit() << std::endl;


  itk::TimeProbe t3;

  for ( unsigned int i = 0; i < 10; ++i )
    {
    t3.Start();
    itk::ImageAlgorithm::Copy( inImage.GetPointer(), outImage.GetPointer(), region, region );
    t3.Stop();
    }

  std::cout << "\tImageCopy Average Time: " << t3.GetMean() << t3.GetUnit() << std::endl;

  const double referenceTime = t.GetMean();
  const double memCopyTime = t2.GetMean();
  const double imageCopyTime = t3.GetMean();


  std::cout << "== SUMMARY SPEEDUP RESULTS == " << std::endl;
  std::cout << "memcpy is " << referenceTime/memCopyTime << " times faster" << std::endl;
  std::cout << "ImageCopy is " << referenceTime/imageCopyTime << " times faster" << std::endl;
}

int itkImageAlgorithmCopyTest( int, char *[] )
{
  typedef itk::Image<char, 3 > ImageType3D;
  ImageType3D::SizeType size3d;

  size3d.Fill( 16 );
  AverageTestCopy<ImageType3D>( size3d );

  size3d.Fill( 32 );
  AverageTestCopy<ImageType3D>( size3d );

  size3d.Fill( 64 );
  AverageTestCopy<ImageType3D>( size3d );

  size3d.Fill( 128 );
  AverageTestCopy<ImageType3D>( size3d );

  typedef itk::Image<char, 2 > ImageType2D;
  ImageType2D::SizeType size2d;

  size2d.Fill( 16 );
  AverageTestCopy<ImageType2D>( size2d );

  size2d.Fill( 32 );
  AverageTestCopy<ImageType2D>( size2d );

  size2d.Fill( 64 );
  AverageTestCopy<ImageType2D>( size2d );


  return EXIT_SUCCESS;
}
