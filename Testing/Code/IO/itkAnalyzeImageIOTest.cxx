/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkAnalyzeImageIOTest.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <fstream>
#include "itkImageFileReader.h"
#include "itkImage.h"

#include <itksys/SystemTools.hxx>
#include "itkImageRegionIterator.h"
#include <iostream>
#include <fstream>

#include "itkImageFileWriter.h"
#include "itkImageIOFactory.h"
#include "itkAnalyzeImageIOFactory.h"
#include "itkNiftiImageIOFactory.h"
#include "itkAnalyzeImageIO.h"
#include "itkNiftiImageIO.h"
#include "itkSpatialOrientationAdapter.h"
#include <stdio.h>
#include "itkMetaDataObject.h"
#include "itkIOCommon.h"
#include "itk_zlib.h"

#if defined(_WIN32) && (defined(_MSC_VER) || defined(__BORLANDC__))
#include <stdlib.h>
#define _unlink unlink
#else
#include <unistd.h>
#endif
static inline int Remove(const char *fname)
{
  return unlink(fname);
}

const unsigned char RPI=16;        /*Bit pattern 0 0 0  10000*/
const unsigned char LEFT=128;      /*Bit pattern 1 0 0  00000*/
const unsigned char ANTERIOR=64;   /*Bit pattern 0 1 0  00000*/
const unsigned char SUPERIOR=32;   /*Bit pattern 0 0 1  00000*/

static int WriteTestFiles(const std::string AugmentName)
{
#include "LittleEndian_hdr.h"
#include "LittleEndian_img.h"
#include "BigEndian_hdr.h"
#include "BigEndian_img.h"
  std::string LittleEndianHdrName=AugmentName+"LittleEndian.hdr";
  std::ofstream little_hdr(LittleEndianHdrName.c_str(), std::ios::binary | std::ios::out);
  if(!little_hdr.is_open())
    {
    return EXIT_FAILURE;
    }
  //std::cout << LittleEndianHdrName << " written" << std::endl;
  little_hdr.write(reinterpret_cast<const char *>(LittleEndian_hdr),sizeof(LittleEndian_hdr));
  little_hdr.close();

  std::string LittleEndianZName(AugmentName);
  LittleEndianZName += "LittleEndianZ.hdr";
  std::ofstream  littlez_hdr(LittleEndianZName.c_str(), std::ios::binary | std::ios::out);
  if(!littlez_hdr.is_open())
    {
    return EXIT_FAILURE;
    }
  littlez_hdr.write(reinterpret_cast<const char *>(LittleEndian_hdr),sizeof(LittleEndian_hdr));

  std::string LittleEndianImgName=AugmentName+"LittleEndian.img";
  std::ofstream little_img(LittleEndianImgName.c_str(), std::ios::binary | std::ios::out);
  if(!little_img.is_open())
    {
    return EXIT_FAILURE;
    }
  // write out compressed.
  little_img.write(reinterpret_cast<const char *>(LittleEndian_img),sizeof(LittleEndian_img));
  little_img.close();

  // write out compressed image
  std::string ImageZFilename(AugmentName);
  ImageZFilename += "LittleEndianZ.img.gz";
  gzFile  file_p = ::gzopen( ImageZFilename.c_str(), "wb" );
  if( file_p==NULL )
    {
    return EXIT_FAILURE;
    }
  ::gzwrite(file_p,reinterpret_cast<const char *>(LittleEndian_img),
            sizeof(LittleEndian_img));
  ::gzclose(file_p);

  std::string BigEndianHdrName=AugmentName+"BigEndian.hdr";
  std::ofstream big_hdr(BigEndianHdrName.c_str(), std::ios::binary | std::ios::out);
  if(!big_hdr.is_open())
    {
    return EXIT_FAILURE;
    }
  big_hdr.write(reinterpret_cast<const char *>(BigEndian_hdr),sizeof(BigEndian_hdr));
  big_hdr.close();
  std::string BigEndianImgName=AugmentName+"BigEndian.img";
  std::ofstream big_img(BigEndianImgName.c_str(), std::ios::binary | std::ios::out);
  if(!big_img.is_open())
    {
    return EXIT_FAILURE;
    }
  big_img.write(reinterpret_cast<const char *>(BigEndian_img),sizeof(BigEndian_img));
  big_img.close();
  return EXIT_SUCCESS;
}
static void RemoveByteSwapTestFiles(const std::string & itkNotUsed(AugmentName) )
{
//--//  Remove(AugmentName+"LittleEndian.hdr");
//--//  Remove(AugmentName+"LittleEndian.img");
//--//  Remove(AugmentName+"BigEndian.hdr");
//--//  Remove(AugmentName+"BigEndian.img");
}

static int TestByteSwap(const std::string & AugmentName)
{
  int rval;
  typedef itk::Image<double, 3> ImageType ;
  typedef itk::ImageFileReader< ImageType > ImageReaderType ;
  if(WriteTestFiles(AugmentName) == -1)
    {
      return EXIT_FAILURE;
    }

  ImageType::Pointer little;
  ImageType::Pointer littlez;
  ImageType::Pointer big;

  itk::ImageFileReader<ImageType>::Pointer imageReader =
    itk::ImageFileReader<ImageType>::New();
  try
  {
    imageReader->SetFileName(AugmentName+"LittleEndian.hdr") ;
    imageReader->Update() ;
    little = imageReader->GetOutput() ;

    imageReader->SetFileName(AugmentName+"LittleEndianZ.hdr") ;
    imageReader->Update() ;
    littlez = imageReader->GetOutput() ;

    imageReader->SetFileName(AugmentName+"BigEndian.hdr") ;
    imageReader->Update() ;
    big = imageReader->GetOutput();
    std::cout << "Printing Dictionary" << std::endl;
    big->GetMetaDataDictionary().Print(std::cout);
  }
  catch (itk::ExceptionObject &e)
    {
      e.Print(std::cerr) ;
      RemoveByteSwapTestFiles(AugmentName);
      return EXIT_FAILURE;
    }
  rval = 0;
  try
    {
      itk::ImageRegionConstIterator<ImageType> littleIter(little,
                                                          little->GetLargestPossibleRegion());
      itk::ImageRegionConstIterator<ImageType> littlezIter(littlez,
                                                          littlez->GetLargestPossibleRegion());
      itk::ImageRegionConstIterator<ImageType> bigIter(big,
                                                       big->GetLargestPossibleRegion());
      while(!littleIter.IsAtEnd())
        {
        if(littleIter.Get() != bigIter.Get() || 
           littlezIter.Get() != bigIter.Get())
            break;
          ++littleIter;
          ++littlezIter;
          ++bigIter;
        }
      if(!littleIter.IsAtEnd() || !bigIter.IsAtEnd() || !littlezIter.IsAtEnd())
        rval = -1;
    }
  catch ( itk::ExceptionObject & ex )
    {
      std::cerr << "Error filling array" << ex.GetDescription() << std::endl;
      rval= -1;
    }
  RemoveByteSwapTestFiles(AugmentName);
  return rval;
}

template <typename T, unsigned Dimension> 
int 
MakeImage(const std::string & AugmentName)
{
  typedef itk::Image<T, Dimension> ImageType ;
  typedef itk::ImageFileReader< ImageType > ImageReaderType ;
  const std::string filename=std::string(typeid(T).name()) +"_"+AugmentName+"_" +std::string("test.hdr");
  //Allocate Images
  enum { ImageDimension = ImageType::ImageDimension };
  typename ImageType::Pointer img;
  typename ImageType::SizeType size; // = {{10,10,10}};
  typename ImageType::IndexType index; // = {{0,0,0}};
  for(unsigned i = 0; i < Dimension; i++)
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
  typename itk::SpatialOrientationAdapter::DirectionType CORdir=
    itk::SpatialOrientationAdapter().ToDirectionCosines(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP);
  typename ImageType::DirectionType dir;
  for(unsigned i = 0; i < Dimension; i++)
    {
    for(unsigned j = 0; j < Dimension; j++)
      {
      dir[i][j] = CORdir[i][j];
      }
    }
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
    std::cerr << "Error filling array" << ex.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }
  }
  { //Fill in left half
  typename ImageType::IndexType RPIindex; // = {{0,0,0}};
  typename ImageType::SizeType RPIsize; // = {{5,10,10}};
  unsigned localdims[] = { 5,10,10 };
  for(unsigned i = 0; i < Dimension; i++)
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
  for(unsigned i = 0; i < Dimension; i++)
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
  if(Dimension > 2)
    {  //Fill in superior half
    typename ImageType::IndexType RPIindex; //= {{0,0,5}};
    typename ImageType::SizeType RPIsize; //= {{10,10,5}};
    unsigned localInd[] = { 0,0,5 };
    unsigned localSize[] = { 10,10,5 };
    for(unsigned i = 0; i < Dimension; i++)
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

  //Attach input image to the writer.
  ImageWriterPointer->SetInput( img );
  //Determine file type and instantiate appropriate ImageIO class if not
  //explicitly stated with SetImageIO, then write to disk.
  try {
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
//--//      Remove(filename);
      return EXIT_FAILURE;
    }

  //typedef itk::ImageFileReader< ImageType > ImageReaderType ;
  typename ImageType::Pointer input;
  typename itk::ImageFileReader<ImageType>::Pointer imageReader =
    itk::ImageFileReader<ImageType>::New();
  try
    {
      imageReader->SetFileName(filename) ;
      imageReader->Update() ;
      input = imageReader->GetOutput() ;
    }
  catch (itk::ExceptionObject &e)
    {
      e.Print(std::cerr) ;
//--//      Remove(filename);
      return EXIT_FAILURE;
    }
//--//  Remove(filename);
  return EXIT_SUCCESS;
}

//template int MakeImage<char>();

int itkAnalyzeImageIOTest(int ac, char* av[])
{
  int rval = 0;
  //Have two loops through the code, the first one
  //reads and writes with the legacy AnalyzeIO, and
  //the second reads a writes with the NiftiIO mechanism.
  for(int loops=0;loops<2;loops++)
    {
    std::string AugmentName="NoneGiven";
    if(loops==1)
      {
      itk::ObjectFactoryBase::UnRegisterAllFactories();
      itk::AnalyzeImageIOFactory::RegisterOneFactory();
      //itk::AnalyzeImageIOFactory::Pointer myAnalyzeIOFactory = itk::AnalyzeImageIOFactory::New();
      //itk::ObjectFactoryBase::UnRegisterFactory(myAnalyzeIOFactory.GetPointer());
      AugmentName="Analyze";
      }
    else
      {
      itk::ObjectFactoryBase::UnRegisterAllFactories();
      itk::NiftiImageIOFactory::RegisterOneFactory();
      //itk::NiftiImageIOFactory::Pointer myNiftiIOFactory = itk::NiftiImageIOFactory::New();
      //itk::ObjectFactoryBase::UnRegisterFactory(myNiftiIOFactory.GetPointer());
      AugmentName="Nifti";
      }

    //
    // first argument is passing in the writable directory to do all testing
    if(ac > 1) {
    char *testdir = *++av;
    --ac;
    itksys::SystemTools::ChangeDirectory(testdir);
    }
    if(ac > 1) //This is a mechanism for reading unsigned char images for testing.
      {
      typedef itk::Image<unsigned char, 3> ImageType ;
      ImageType::Pointer input;
      itk::ImageFileReader<ImageType>::Pointer imageReader =
        itk::ImageFileReader<ImageType>::New();
      for(int imagenameindex=1; imagenameindex < ac; imagenameindex++)
        {
        //std::cout << "Attempting to read " << av[imagenameindex] << std::endl;
        try
          {
          imageReader->SetFileName(av[imagenameindex]) ;
          imageReader->Update() ;
          input=imageReader->GetOutput() ;
          }
        catch (itk::ExceptionObject &e)
          {
          e.Print(std::cerr) ;
          rval = 1;
          }
        }
      }
    else //This is the mechanism for doing internal testing of all data types.
      {
      int cur_return;
      cur_return = MakeImage<char,3>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type char" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<unsigned char,3>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type unsigned char" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<short,3>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type short" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<unsigned short,3>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type unsigned short" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<int,3>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type int" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<float,3>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type float" << std::endl;
        rval += cur_return;
        }
      // awaiting a double precision byte swapper
      cur_return = MakeImage<double,3>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type double" << std::endl;
        rval += cur_return;
        }

      cur_return = MakeImage<char,2>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type char" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<unsigned char,2>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type unsigned char" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<short,2>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type short" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<unsigned short,2>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type unsigned short" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<int,2>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type int" << std::endl;
        rval += cur_return;
        }
      cur_return = MakeImage<float,2>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type float" << std::endl;
        rval += cur_return;
        }
      // awaiting a double precision byte swapper
      cur_return = MakeImage<double,2>(AugmentName);
      if(cur_return != 0)
        {
        std::cerr << "Error writing Analyze file type double" << std::endl;
        rval += cur_return;
        }
      rval += TestByteSwap(AugmentName);
      }
    }
  return rval;
}

int itkAnalyzeImageIOTest2(int ac, char* av[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if(ac > 1) {
    char *testdir = *++av;
    --ac;
    itksys::SystemTools::ChangeDirectory(testdir);
  }
  if(ac != 3)
    return EXIT_FAILURE;
  char *arg1 = av[1];
  char *arg2 = av[2];
  int test_success = 0;
  typedef itk::Image<signed short, 3> ImageType ;
  typedef ImageType::Pointer ImagePointer ;
  typedef itk::ImageFileReader< ImageType > ImageReaderType ;

  itk::AnalyzeImageIO::Pointer io = itk::AnalyzeImageIO::New();
  ImageReaderType::Pointer imageReader = ImageReaderType::New();
  ImagePointer input;
  try 
    {
      imageReader->SetImageIO(io);
      imageReader->SetFileName(arg2);
      imageReader->Update();
      input = imageReader->GetOutput();
    }
  catch (itk::ExceptionObject &)
    {
      test_success = 1;
    }

  if(strcmp(arg1, "true") == 0)
    {
      return test_success;
    }
  else
    {
      return !test_success;
    }
}
