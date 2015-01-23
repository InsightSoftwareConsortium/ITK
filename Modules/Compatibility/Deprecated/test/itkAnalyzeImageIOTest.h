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
#ifndef itkAnalyzeImageIOTest_h
#define itkAnalyzeImageIOTest_h

#include <fstream>

#include "itksys/SystemTools.hxx"
#include <iostream>

#include "itkAnalyzeImageIOFactory.h"
#include "itkAnalyzeImageIO.h"
#include "itkIOTestHelper.h"
#include <cstdio>
#include "itk_zlib.h"
#include "itkRGBPixel.h"
#include "itkSpatialOrientationAdapter.h"

#if defined(_WIN32) && defined(_MSC_VER)
#include <cstdlib>
#define _unlink unlink
#else
#include <unistd.h>
#endif

#define SPECIFIC_IMAGEIO_MODULE_TEST

const unsigned char RPI=16;        /*Bit pattern 0 0 0  10000*/
const unsigned char LEFT=128;      /*Bit pattern 1 0 0  00000*/
const unsigned char ANTERIOR=64;   /*Bit pattern 0 1 0  00000*/
const unsigned char SUPERIOR=32;   /*Bit pattern 0 0 1  00000*/

template <typename ImageType>
typename ImageType::DirectionType
CORDirCosines()
{
  typename itk::SpatialOrientationAdapter::DirectionType CORdir=
    itk::SpatialOrientationAdapter().ToDirectionCosines(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP);
  typename ImageType::DirectionType dir;
  for(unsigned i = 0; i < ImageType::ImageDimension; i++)
    {
    for(unsigned j = 0; j < ImageType::ImageDimension; j++)
      {
      dir[i][j] = CORdir[i][j];
      }
    }
  if(ImageType::ImageDimension == 2)
    {
    dir[1][1] = 1.0;
    }
  return dir;
}

template <typename T, unsigned VDimension>
int
MakeImage(const std::string & AugmentName)
{
  typedef itk::Image<T, VDimension>         ImageType;

  const std::string filename=std::string(typeid(T).name()) +"_"+AugmentName+"_" +std::string("test.hdr");

  //Allocate Images
  enum { ImageDimension = ImageType::ImageDimension };
  typename ImageType::Pointer img;
  typename ImageType::SizeType size; // = {{10,10,10}};
  typename ImageType::IndexType index; // = {{0,0,0}};

  for(unsigned i = 0; i < VDimension; i++)
    {
    size[i] = 10;
    index[i] = 0;
    }

  typename ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  img = ImageType::New();
  img->SetLargestPossibleRegion( region );
  img->SetBufferedRegion( region );
  img->SetRequestedRegion( region );

  typename ImageType::DirectionType dir = CORDirCosines<ImageType>();

  img->SetDirection(dir);
  img->Allocate();

  { //Fill in entire image
  itk::ImageRegionIterator<ImageType> ri(img,region);
  try
    {
    while(!ri.IsAtEnd())
      {
      ri.Set( RPI );
      ++ri;
      }
    }
  catch ( itk::ExceptionObject & ex )
    {
    std::cerr << "Error filling array" << ex << std::endl;
    return EXIT_FAILURE;
    }
  }

  { //Fill in left half
  typename ImageType::IndexType RPIindex; // = {{0,0,0}};
  typename ImageType::SizeType RPIsize; // = {{5,10,10}};
  unsigned localdims[] = { 5,10,10 };
  for(unsigned i = 0; i < VDimension; i++)
    {
    RPIindex[i] = 0;
    RPIsize[i] = localdims[i];
    }
  typename ImageType::RegionType RPIregion;
  RPIregion.SetSize( RPIsize );
  RPIregion.SetIndex( RPIindex );
  itk::ImageRegionIterator<ImageType > RPIiterator(img,RPIregion);
  while(!RPIiterator.IsAtEnd())
    {
    RPIiterator.Set( RPIiterator.Get() + LEFT );
    ++RPIiterator;
    }
  }

  { //Fill in anterior half
  typename ImageType::IndexType RPIindex;// = {{0,5,0}};
  typename ImageType::SizeType RPIsize; // = {{10,5,10}};
  unsigned localindex[] = { 0, 5, 0 };
  unsigned localdims[] = { 10,5,10 };
  for(unsigned i = 0; i < VDimension; i++)
    {
    RPIindex[i] = localindex[i];
    RPIsize[i] = localdims[i];
    }
  typename ImageType::RegionType RPIregion;
  RPIregion.SetSize( RPIsize );
  RPIregion.SetIndex( RPIindex );
  itk::ImageRegionIterator<ImageType > RPIiterator(img,RPIregion);
  while(!RPIiterator.IsAtEnd())
    {
    RPIiterator.Set( RPIiterator.Get() + ANTERIOR );
    ++RPIiterator;
    }
  }

  if(VDimension > 2)
    {  //Fill in superior half
    typename ImageType::IndexType RPIindex; //= {{0,0,5}};
    typename ImageType::SizeType RPIsize; //= {{10,10,5}};
    unsigned localInd[] = { 0,0,5 };
    unsigned localSize[] = { 10,10,5 };
    for(unsigned i = 0; i < VDimension; i++)
      {
      RPIindex[i] = localInd[i];
      RPIsize[i] = localSize[i];
      }
    typename ImageType::RegionType RPIregion;
    RPIregion.SetSize( RPIsize );
    RPIregion.SetIndex( RPIindex );
    itk::ImageRegionIterator<ImageType > RPIiterator(img,RPIregion);
    while(!RPIiterator.IsAtEnd())
      {
      RPIiterator.Set( RPIiterator.Get() + SUPERIOR );
      ++RPIiterator;
      }
    }

  typedef itk::ImageFileWriter< ImageType >      ImageWriterType;
  typename ImageWriterType::Pointer ImageWriterPointer =
    ImageWriterType::New();

  //Set the output filename
  ImageWriterPointer->SetFileName(filename);
  itk::AnalyzeImageIO::Pointer io = itk::AnalyzeImageIO::New();
  ImageWriterPointer->SetImageIO(io);

  //Attach input image to the writer.
  ImageWriterPointer->SetInput( img );
  //Determine file type and instantiate appropriate ImageIO class if not
  //explicitly stated with SetImageIO, then write to disk.
  try
    {
    ImageWriterPointer->Write();
    }
  catch ( itk::ExceptionObject & ex )
    {
    std::string message;
    message = "Problem found while writing image ";
    message += filename;
    message += "\n";
    message += ex.GetLocation();
    message += "\n";
    message += ex.GetDescription();
    std::cerr << message << std::endl;
    return EXIT_FAILURE;
    }

  typename ImageType::Pointer input;
  typename itk::ImageFileReader<ImageType>::Pointer imageReader =
    itk::ImageFileReader<ImageType>::New();

  imageReader->SetImageIO(io);
  try
    {
    imageReader->SetFileName(filename);
    imageReader->Update();
    input = imageReader->GetOutput();
    }
  catch (itk::ExceptionObject &e)
    {
    e.Print(std::cerr);
    return EXIT_FAILURE;
    }
  itk::IOTestHelper::Remove(filename.c_str());
  return EXIT_SUCCESS;
}

template <typename ImageType>
typename ImageType::Pointer NewRGBImage()
{
  typename ImageType::IndexType index;
  typename ImageType::SizeType size;
  typename ImageType::SpacingType spacing;
  typename ImageType::RegionType region;
  typename ImageType::Pointer rval;
  for(unsigned i = 0; i < ImageType::ImageDimension; i++)
    {
    spacing[i] = 1.0;
    size[i] = 4;
    index[i] = 0;
    }
  region.SetSize(size);
  region.SetIndex(index);
  return rval = itk::IOTestHelper::AllocateImageFromRegionAndSpacing<ImageType>(region, spacing);
}

int WriteAnalyzeTestFiles(const std::string & AugmentName);
#endif
