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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <fstream>

#include "itksys/SystemTools.hxx"
#include <iostream>

#include "itkAnalyzeImageIOFactory.h"
#include "itkAnalyzeImageIO.h"
#include <stdio.h>
#include "itk_zlib.h"
#include "itkNiftiImageIOTest.h"
#include "itkRGBPixel.h"

#if defined(_WIN32) && defined(_MSC_VER)
#include <stdlib.h>
#define _unlink unlink
#else
#include <unistd.h>
#endif


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
  typedef itk::Image<double, 3>               ImageType;
  typedef itk::ImageFileReader< ImageType >   ImageReaderType;

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
    imageReader->SetFileName(AugmentName+"LittleEndian.hdr");
    imageReader->Update();
    little = imageReader->GetOutput();

    imageReader->SetFileName(AugmentName+"LittleEndianZ.hdr");
    imageReader->Update();
    littlez = imageReader->GetOutput();

    imageReader->SetFileName(AugmentName+"BigEndian.hdr");
    imageReader->Update();
    big = imageReader->GetOutput();
    std::cout << "Printing Dictionary" << std::endl;
    big->GetMetaDataDictionary().Print(std::cout);
    }
  catch (itk::ExceptionObject &e)
    {
    e.Print(std::cerr);
    RemoveByteSwapTestFiles(AugmentName);
    return EXIT_FAILURE;
    }

  rval = 0;

  try
    {
    itk::ImageRegionConstIterator<ImageType> littleIter(little, little->GetLargestPossibleRegion());
    itk::ImageRegionConstIterator<ImageType> littlezIter(littlez, littlez->GetLargestPossibleRegion());
    itk::ImageRegionConstIterator<ImageType> bigIter(big, big->GetLargestPossibleRegion());

    while(!littleIter.IsAtEnd())
      {
      if( littleIter.Get() != bigIter.Get() || littlezIter.Get() != bigIter.Get())
        {
        break;
        }
      ++littleIter;
      ++littlezIter;
      ++bigIter;
      }

    if(!littleIter.IsAtEnd() || !bigIter.IsAtEnd() || !littlezIter.IsAtEnd())
      {
      rval = -1;
      }
    }
  catch ( itk::ExceptionObject & ex )
    {
    std::cerr << "Error filling array" << ex << std::endl;
    rval= -1;
    }
  RemoveByteSwapTestFiles(AugmentName);
  return rval;
}

template <typename T, unsigned Dimension>
int
MakeImage(const std::string & AugmentName)
{
  typedef itk::Image<T, Dimension>                ImageType;
  typedef itk::ImageFileReader< ImageType >       ImageReaderType;

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
//--//      Remove(filename);
    return EXIT_FAILURE;
    }

  typename ImageType::Pointer input;
  typename itk::ImageFileReader<ImageType>::Pointer imageReader =
    itk::ImageFileReader<ImageType>::New();

  try
    {
    imageReader->SetFileName(filename);
    imageReader->Update();
    input = imageReader->GetOutput();
    }
  catch (itk::ExceptionObject &e)
    {
    e.Print(std::cerr);
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

    // copy ac and av in another var so we can reuse it in the second step of the loop
    int ac2 = ac;
    char** av2 = av;

    //
    // first argument is passing in the writable directory to do all testing
    if(ac2 > 1)
      {
      char *testdir = *++av2;
      --ac2;
      itksys::SystemTools::ChangeDirectory(testdir);
      }

    if(ac2 > 1) //This is a mechanism for reading unsigned char images for testing.
      {
      typedef itk::Image<unsigned char, 3> ImageType;
      ImageType::Pointer input;
      itk::ImageFileReader<ImageType>::Pointer imageReader =
        itk::ImageFileReader<ImageType>::New();
      for(int imagenameindex=1; imagenameindex < ac2; imagenameindex++)
        {
        //std::cout << "Attempting to read " << av2[imagenameindex] << std::endl;
        try
          {
          imageReader->SetFileName(av2[imagenameindex]);
          imageReader->Update();
          input=imageReader->GetOutput();
          }
        catch (itk::ExceptionObject &e)
          {
          e.Print(std::cerr);
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
  if(ac > 1)
    {
    char *testdir = *++av;
    --ac;
    itksys::SystemTools::ChangeDirectory(testdir);
    }

  if(ac != 3)
    {
    return EXIT_FAILURE;
    }

  char *arg1 = av[1];
  char *arg2 = av[2];
  int test_success = 0;
  typedef itk::Image<signed short, 3>         ImageType;
  typedef ImageType::Pointer                  ImagePointer;
  typedef itk::ImageFileReader< ImageType >   ImageReaderType;

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


int
TestDegenerateHeaderFiles()
{
  std::string AugmentName("DegenerateHeaderTest");
  if(WriteTestFiles(AugmentName) == -1)
    {
    return EXIT_FAILURE;
    }
  std::string fname(AugmentName);
  fname += "LittleEndian.hdr";
  typedef itk::Image<unsigned short,3> ImageType;
  ImageType::Pointer img;
  std::fstream header(fname.c_str(),std::ios::binary | std::ios::in | std::ios::out);
  if(!header.is_open())
    {
    return EXIT_FAILURE;
    }
  header.seekg(40,std::ios::beg); // go to location of first element of dim array.
  short int zero(0);
  header.write(reinterpret_cast<const char *>(&zero),sizeof(short int));
  header.close();
  int error(0);
  try
    {
    img = ReadImage<ImageType>(fname);
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "Caught an exception: " << std::endl;
    std::cout << err << " " << __FILE__ << " " << __LINE__ << std::endl;
    error++;
    }
  RemoveByteSwapTestFiles(AugmentName);
  return error ? 1 : 0;
}
int itkAnalyzeImageIOBadHeader(int ac, char* av[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if(ac > 1)
    {
    char *testdir = *++av;
    --ac;
    itksys::SystemTools::ChangeDirectory(testdir);
    }
  itk::ObjectFactoryBase::UnRegisterAllFactories();
  itk::AnalyzeImageIOFactory::RegisterOneFactory();
  int result1 = TestDegenerateHeaderFiles();
  int result2(0);
  //  NIfTI explicitly refuses to read analyze 7.5 files
  // I could force it to do so but since by default, it will never
  // be used in that manner without explicitly asking for it, there
  // isn't much point.
#if 0
  itk::ObjectFactoryBase::UnRegisterAllFactories();
  itk::NiftiImageIOFactory::RegisterOneFactory();
  result2 = TestDegenerateHeaderFiles();
#endif
  return !(result1 == 0 && result2 == 0);
}

template <class ImageType>
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
  AllocateImageFromRegionAndSpacing(ImageType, rval, region, spacing);
  return rval;
}

int itkAnalyzeImageIORGBImageTest(int ac, char* av[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if(ac > 1)
    {
    char *testdir = *++av;
    --ac;
    itksys::SystemTools::ChangeDirectory(testdir);
    }
  const unsigned int Dimension = 3;
  typedef itk::RGBPixel<unsigned char> RGBPixelType;
  typedef itk::Image<RGBPixelType, Dimension> RGBImageType;
  RGBImageType::Pointer im(NewRGBImage<RGBImageType>());
  itk::ImageRegionIterator<RGBImageType> it(im,im->GetLargestPossibleRegion());
  RGBImageType::DirectionType dir(CORDirCosines<RGBImageType>());
  im->SetDirection(dir);
  vnl_random randgen(8775070);
  for(it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    RGBPixelType pixel;
    pixel[0] = static_cast<RGBPixelType::ValueType>(randgen.lrand32(0,255));
    pixel[1] = static_cast<RGBPixelType::ValueType>(randgen.lrand32(0,255));
    pixel[2] = static_cast<RGBPixelType::ValueType>(randgen.lrand32(0,255));
    it.Set(pixel);
    }
  int status(EXIT_SUCCESS);
  const std::string filename("RGBImageTest.hdr");
  try
    {
    WriteImage<RGBImageType>(im,filename);
    }
  catch ( itk::ExceptionObject & ex )
    {
      std::string message;
      message = "Problem found while writing ";
      message += filename;
      message += "\n";
      message += ex.GetLocation();
      message += "\n";
      message += ex.GetDescription();
      std::cerr << message << std::endl;
      status = EXIT_FAILURE;
    }
  if(status == EXIT_SUCCESS)
    {
    RGBImageType::Pointer im2;
    try
      {
      im2 = ReadImage<RGBImageType>(filename);
      }
    catch ( itk::ExceptionObject & ex )
      {
      std::string message;
      message = "Problem found while reading ";
      message += filename;
      message += "\n";
      message += ex.GetLocation();
      message += "\n";
      message += ex.GetDescription();
      std::cerr << message << std::endl;
      status = EXIT_FAILURE;
      }
    if(status == EXIT_SUCCESS)
      {
      itk::ImageRegionIterator<RGBImageType> it2(im2,im2->GetLargestPossibleRegion());
      for(it.GoToBegin(),it2.GoToBegin();
          !it.IsAtEnd() && !it2.IsAtEnd();
          ++it,++it2)
        {
        if(it.Value() != it2.Value())
          {
          std::cout << "Pixel "
                    << it2.Value() << " (from disk) != "
                    << it.Value() << " (original image)"
                    << std::endl;
          status = EXIT_FAILURE;
          }
        }
      }
    }
  Remove(filename.c_str());
  return status;
}
