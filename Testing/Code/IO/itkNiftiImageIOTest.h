/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkNiftiImageIOTest.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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
#include "itkSymmetricSecondRankTensor.h"
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

//The WriteTestFiles function writes binary data to disk to ensure that both big and little endian files are available.
//This allows all the data necessary to create the images to be stored in source files rather than have separate reference images.
static int WriteTestFiles(void)
{
#include "LittleEndian_hdr.h"
    struct nifti_1_header NiftiLittleEndian;
    memcpy(&NiftiLittleEndian,LittleEndian_hdr,sizeof(NiftiLittleEndian));
    NiftiLittleEndian.qform_code=NIFTI_XFORM_UNKNOWN;
    NiftiLittleEndian.sform_code=NIFTI_XFORM_UNKNOWN;
    strncpy(NiftiLittleEndian.magic,"ni1\0",4);
#include "LittleEndian_img.h"
#include "BigEndian_hdr.h"
    struct nifti_1_header NiftiBigEndian;
    memcpy(&NiftiBigEndian,BigEndian_hdr,sizeof(NiftiBigEndian));
    NiftiBigEndian.qform_code=NIFTI_XFORM_UNKNOWN;
    NiftiBigEndian.sform_code=NIFTI_XFORM_UNKNOWN;
    strncpy(NiftiBigEndian.magic,"ni1\0",4);
#include "BigEndian_img.h"
    //Force to be Nifti-compliant
  std::ofstream little_hdr("NiftiLittleEndian.hdr", std::ios::binary | std::ios::out);
  if(!little_hdr.is_open())
    return EXIT_FAILURE;
  std::cout << "NiftiLittleEndian written" << std::endl;
  little_hdr.write(reinterpret_cast<const char *>(LittleEndian_hdr),sizeof(LittleEndian_hdr));
  little_hdr.close();
  std::ofstream little_img("NiftiLittleEndian.img", std::ios::binary | std::ios::out);
  if(!little_img.is_open())
    return EXIT_FAILURE;
  little_img.write(reinterpret_cast<const char *>(LittleEndian_img),sizeof(LittleEndian_img));
  little_img.close();
  std::ofstream big_hdr("NiftiBigEndian.hdr", std::ios::binary | std::ios::out);
  if(!big_hdr.is_open())
    return EXIT_FAILURE;
  big_hdr.write(reinterpret_cast<const char *>(BigEndian_hdr),sizeof(BigEndian_hdr));
  big_hdr.close();
  std::ofstream big_img("NiftiBigEndian.img", std::ios::binary | std::ios::out);
  if(!big_img.is_open())
    return EXIT_FAILURE;
  big_img.write(reinterpret_cast<const char *>(BigEndian_img),sizeof(BigEndian_img));
  big_img.close();
  return EXIT_SUCCESS;
}

static void RemoveByteSwapTestFiles(void)
{
  Remove("NiftiLittleEndian.hdr");
  Remove("NiftiLittleEndian.img");
  Remove("NiftiBigEndian.hdr");
  Remove("NiftiBigEndian.img");
}

#define AllocateImageFromRegionAndSpacing(ImageType,rval,region,spacing) \
{ \
  rval = ImageType::New(); \
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

#endif
