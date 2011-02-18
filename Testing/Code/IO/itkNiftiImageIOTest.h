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
#ifndef __itkNiftiImageIOTest_h_
#define __itkNiftiImageIOTest_h_

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImageFileReader.h"
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itksys/SystemTools.hxx"
#include "itkImageRegionIterator.h"
#include "itkImageFileWriter.h"
#include "itkImageIOFactory.h"
#include "itkNiftiImageIOFactory.h"
#include "itkNiftiImageIO.h"
#include "itkMetaDataObject.h"
#include "itkIOCommon.h"
#include "itkSpatialOrientationAdapter.h"
#include "itkDiffusionTensor3D.h"
#include "itkAffineTransform.h"
#include "itkVector.h"
#include "itkRGBAPixel.h"
#include "itkRandomImageSource.h"

#include "vnl/vnl_random.h"
#include "vnl/vnl_math.h"

#include "nifti1_io.h"

#include <string.h>
#include <iostream>
#include <fstream>
#include <stdio.h>

static inline int Remove(const char *fname)
{
  return itksys::SystemTools::RemoveFile(fname);
}

template <typename TImage>
typename TImage::Pointer ReadImage( const std::string &fileName,
                                    const bool zeroOrigin = false )
{
  typedef itk::ImageFileReader<TImage> ReaderType;

  typename ReaderType::Pointer reader = ReaderType::New();
  {
  reader->SetFileName( fileName.c_str() );
  reader->SetImageIO(itk::NiftiImageIO::New());
  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "Caught an exception: " << std::endl;
    std::cout << err << " " << __FILE__ << " " << __LINE__ << std::endl;
    throw err;
    }
  catch(...)
    {
    std::cout << "Error while reading in image for patient " << fileName << std::endl;
    throw;
    }
  }
  typename TImage::Pointer image = reader->GetOutput();
  if(zeroOrigin)
    {
    double origin[TImage::ImageDimension];
    for(unsigned int i =0 ; i< TImage::ImageDimension ; i++)
      {
      origin[i]=0;
      }
    image->SetOrigin(origin);
    }
  return image;
}

template <class ImageType>
void
WriteImage(typename ImageType::Pointer &image ,
           const std::string &filename)
{

  typedef itk::ImageFileWriter< ImageType > WriterType;
  typename  WriterType::Pointer writer = WriterType::New();

  writer->SetImageIO(itk::NiftiImageIO::New());

  writer->SetFileName(filename.c_str());

  writer->SetInput(image);

  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject &err) {
  std::cout << "Exception Object caught: " << std::endl;
  std::cout << err << std::endl;
  throw;
  }
}

const unsigned char RPI=16;        /*Bit pattern 0 0 0  10000*/
const unsigned char LEFT=128;      /*Bit pattern 1 0 0  00000*/
const unsigned char ANTERIOR=64;   /*Bit pattern 0 1 0  00000*/
const unsigned char SUPERIOR=32;   /*Bit pattern 0 0 1  00000*/

template <class ImageType>
void SetIdentityDirection(typename ImageType::Pointer &im)
{
  typename ImageType::DirectionType dir;
  dir.SetIdentity();
  im->SetDirection(dir);
}

#define AllocateImageFromRegionAndSpacing(ImageType,rval,region,spacing) \
{ \
  rval = ImageType::New(); \
  SetIdentityDirection<ImageType>(rval);       \
  rval->SetSpacing(spacing); \
  rval->SetRegions(region); \
  rval->Allocate(); \
}
#define AllocateVecImageFromRegionAndSpacing(ImageType,rval,region,spacing,vecLength) \
{ \
  rval = ImageType::New(); \
  rval->SetSpacing(spacing); \
  rval->SetRegions(region); \
  rval->SetVectorLength(vecLength); \
  rval->Allocate(); \
}

template <typename T> int MakeNiftiImage(void)
{
  typedef itk::Image<T, 3> ImageType ;
  const char *filename = "test.nii";
  //Allocate Images
  enum { ImageDimension = ImageType::ImageDimension };
  typename ImageType::Pointer img;
  const typename ImageType::SizeType size = {{10,10,10}};
  typename ImageType::SpacingType spacing;
  spacing[0] = spacing[1] = spacing[2] = 1.0;

  const typename ImageType::IndexType index = {{0,0,0}};
  typename ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );
  AllocateImageFromRegionAndSpacing(ImageType, img, region, spacing);

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
    const typename ImageType::IndexType RPIindex = {{0,0,0}};
    const typename ImageType::SizeType RPIsize = {{5,10,10}};
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
    const typename ImageType::IndexType RPIindex = {{0,5,0}};
    const typename ImageType::SizeType RPIsize = {{10,5,10}};
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
  { //Fill in superior half
    const typename ImageType::IndexType RPIindex = {{0,0,5}};
    const typename ImageType::SizeType RPIsize = {{10,10,5}};
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
  try
    {
    WriteImage<ImageType>(img,std::string(filename));
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
      Remove(filename);
      return EXIT_FAILURE;
    }
  typename ImageType::Pointer input;
  try
    {
    input = ReadImage<ImageType>(std::string(filename));
    }
  catch (itk::ExceptionObject &e)
    {
      e.Print(std::cerr) ;
      Remove(filename);
      return EXIT_FAILURE;
    }
  Remove(filename);
  return EXIT_SUCCESS;
}

template <class ImageType>
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


#endif
