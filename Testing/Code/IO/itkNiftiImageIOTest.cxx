/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkNiftiImageIOTest.cxx
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
#include <string.h>
#include "itkImageFileReader.h"
#include "itkImage.h"

#include <itksys/SystemTools.hxx>
#include "itkImageRegionIterator.h"
#include <iostream>
#include <fstream>
#include <stdio.h>

#include "itkImageFileWriter.h"
#include "itkImageIOFactory.h"
#include "itkNiftiImageIOFactory.h"
#include "itkNiftiImageIO.h"
#include "itkMetaDataObject.h"
#include "itkIOCommon.h"
#include "itkSpatialOrientationAdapter.h"

#include "itkAffineTransform.h"
#include "itkVector.h"
#include <vnl/vnl_math.h>

#include <nifti1_io.h>

static inline int Remove(const char *fname)
{
  return itksys::SystemTools::RemoveFile(fname);
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

static int TestByteSwap(void)
{
  int rval;
  typedef itk::Image<double, 3> ImageType ;
  typedef itk::ImageFileReader< ImageType > ImageReaderType ;
  if(WriteTestFiles() == -1)
    {
      return EXIT_FAILURE;
    }

  ImageType::Pointer little;
  ImageType::Pointer big;

  itk::ImageFileReader<ImageType>::Pointer imageReader =
    itk::ImageFileReader<ImageType>::New();
  try
  {
    imageReader->SetFileName("NiftiLittleEndian.hdr") ;
    imageReader->Update() ;
    little = imageReader->GetOutput() ;
    imageReader->SetFileName("NiftiBigEndian.hdr") ;
    imageReader->Update() ;
    big = imageReader->GetOutput();
    std::cout << "Printing Dictionary" << std::endl;
    big->GetMetaDataDictionary().Print(std::cout);
  }
  catch (itk::ExceptionObject &e)
    {
      e.Print(std::cerr) ;
      RemoveByteSwapTestFiles();
      return EXIT_FAILURE;
    }
  rval = 0;
  try
    {
      itk::ImageRegionConstIterator<ImageType> littleIter(little,
                                                          little->GetLargestPossibleRegion());
      itk::ImageRegionConstIterator<ImageType> bigIter(big,
                                                       big->GetLargestPossibleRegion());
      while(!littleIter.IsAtEnd())
        {
          if(littleIter.Get() != bigIter.Get())
            break;
          ++littleIter;
          ++bigIter;
        }
      if(!littleIter.IsAtEnd() || !bigIter.IsAtEnd())
        rval = -1;
    }
  catch ( itk::ExceptionObject & ex )
    {
      std::cerr << "Error filling array" << ex << std::endl;
      rval= -1;
    }

  RemoveByteSwapTestFiles();
  return rval;
}

template <typename T> int MakeNiftiImage(void)
{
  typedef itk::Image<T, 3> ImageType ;
  typedef itk::ImageFileReader< ImageType > ImageReaderType ;
  const char *filename = "test.nii";
  //Allocate Images
  enum { ImageDimension = ImageType::ImageDimension };
  typename ImageType::Pointer img;
  const typename ImageType::SizeType size = {{10,10,10}};
  const typename ImageType::IndexType index = {{0,0,0}};
  typename ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  img = ImageType::New();
  img->SetLargestPossibleRegion( region );
  img->SetBufferedRegion( region );
  img->SetRequestedRegion( region );
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
      Remove(filename);
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
      Remove(filename);
      return EXIT_FAILURE;
    }
  Remove(filename);
  return EXIT_SUCCESS;
}

//template int MakeNiftiImage<char>();

int itkNiftiImageIOTest(int ac, char* av[])
{
  itk::ObjectFactoryBase::UnRegisterAllFactories();
  itk::NiftiImageIOFactory::RegisterOneFactory();
  int rval = 0;
  //
  // first argument is passing in the writable directory to do all testing
  if(ac > 1) {
    char *testdir = *++av;
    --ac;
    itksys::SystemTools::ChangeDirectory(testdir);
  }
  static bool firstTime = true;
  if(firstTime) 
    {
    itk::ObjectFactoryBase::RegisterFactory(itk::NiftiImageIOFactory::New() );
    firstTime = false;
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
      cur_return = MakeNiftiImage<char>();
      if(cur_return != 0)
        {
          std::cerr << "Error writing Nifti file type char" << std::endl;
          rval += cur_return;
        }
      cur_return = MakeNiftiImage<unsigned char>();
      if(cur_return != 0)
        {
          std::cerr << "Error writing Nifti file type unsigned char" << std::endl;
          rval += cur_return;
        }
      cur_return = MakeNiftiImage<short>();
      if(cur_return != 0)
        {
          std::cerr << "Error writing Nifti file type short" << std::endl;
          rval += cur_return;
        }
      cur_return = MakeNiftiImage<unsigned short>();
      if(cur_return != 0)
        {
          std::cerr << "Error writing Nifti file type unsigned short" << std::endl;
          rval += cur_return;
        }
      cur_return = MakeNiftiImage<int>();
      if(cur_return != 0)
        {
          std::cerr << "Error writing Nifti file type int" << std::endl;
          rval += cur_return;
        }
      cur_return = MakeNiftiImage<float>();
      if(cur_return != 0)
        {
          std::cerr << "Error writing Nifti file type float" << std::endl;
          rval += cur_return;
        }
      // awaiting a double precision byte swapper
      cur_return = MakeNiftiImage<double>();
      if(cur_return != 0)
        {
          std::cerr << "Error writing Nifti file type double" << std::endl;
          rval += cur_return;
        }
      rval += TestByteSwap();
    }
  //Tests added to increase code coverage.
      {
      itk::NiftiImageIOFactory::Pointer MyFactoryTest=itk::NiftiImageIOFactory::New();
      //This was made a protected function.  MyFactoryTest->PrintSelf(std::cout,0);
      }
  return rval;
}

int itkNiftiImageIOTest2(int ac, char* av[])
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

  if((strcmp(arg1, "true") == 0) && WriteTestFiles() == -1)
    {
      return EXIT_FAILURE;
    }



  itk::NiftiImageIO::Pointer io = itk::NiftiImageIO::New();
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


#include "itkRandomImageSource.h"
#include <vnl/vnl_random.h>




template <class ScalarType, unsigned VecLength, unsigned Dimension>
int
TestVectorImage(const std::string fname)
{
  const int dimsize = 2;
  /** Deformation field pixel type. */
  typedef typename itk::Vector<ScalarType,VecLength> FieldPixelType;

  /** Deformation field type. */
  typedef typename itk::Image<FieldPixelType,Dimension> VectorImageType;

  /** file reader type */
  typedef typename itk::ImageFileReader< VectorImageType >  FieldReaderType;

  /** file writer type */
  typedef typename itk::ImageFileWriter< VectorImageType >  FieldWriterType;

  //
  // swizzle up a random vector image.
  typename VectorImageType::Pointer vi = VectorImageType::New();
  typename VectorImageType::RegionType imageRegion;
  typename VectorImageType::SizeType size;
  typename VectorImageType::IndexType index;
  typename VectorImageType::SpacingType spacing;
  typename VectorImageType::PointType origin;
  typename VectorImageType::DirectionType myDirection;
  myDirection.SetIdentity();
  for(unsigned int r=0;r<Dimension;r++)
    {
    for(unsigned int c=0;c<Dimension;c++)
      {
      if(r == (Dimension-1-c))
        {
        myDirection[r][c]=vcl_pow(-1.0,static_cast<double>(r));
        }
      else
        {
        myDirection[r][c]=0.0;
        }
      }
    }

  std::cout << " === Testing VectorLength: " << VecLength << " Image Dimension " << static_cast<int>(Dimension) << std::endl;
  std::cout << "======================== Initialized Direction" << std::endl;
  std::cout << myDirection << std::endl;

  for(unsigned i = 0; i < Dimension; i++)
    {
    size[i] = dimsize;
    index[i] = 0;
    spacing[i] = 1.0;
    origin[i] = 0;
    }

  imageRegion.SetSize(size); 
  imageRegion.SetIndex(index);
  vi->SetRegions(imageRegion);
  vi->SetSpacing(spacing);
  vi->SetOrigin(origin);
  vi->SetDirection(myDirection);
  vi->Allocate();

  vnl_random randgen;

  typedef itk::ImageRegionIterator<VectorImageType> IteratorType;
  typedef itk::ImageRegionConstIterator<VectorImageType> ConstIteratorType;

  int dims[7];
  int _index[7];
  for(unsigned i = 0; i < Dimension; i++)
    {
    dims[i] = size[i];
    }
  for(unsigned i = Dimension; i < 7; i++)
    {
    dims[i] = 1;
    }

  int incr_value=0;
  //  for(fillIt.GoToBegin(); !fillIt.IsAtEnd(); ++fillIt)
  for(int l = 0; l < dims[6]; l++)
    {
    _index[6] = l;
    for(int m = 0; m < dims[5]; m++)
      {
      _index[5] = m;
      for(int n = 0; n < dims[4]; n++)
        {
        _index[4] = n;
        for(int p = 0; p < dims[3]; p++)
          { 
          _index[3] = p;
          for(int i = 0; i < dims[2]; i++)
            {
            _index[2] = i;
            for(int j = 0; j < dims[1]; j++)
              {
              _index[1] = j;
              for(int k = 0; k < dims[0]; k++)
                {
                _index[0] = k;
                FieldPixelType pixel;
                float lowrange(100.00),highrange(200.00);
                for(unsigned int q = 0; q < VecLength; q++)
                  {
                  //pixel[q] = randgen.drand32(lowrange,highrange);
                  pixel[q] = incr_value++;
                  lowrange += 100.0;
                  highrange += 100.0;
                  }
                for(unsigned int q = 0; q < Dimension; q++)
                  {
                  index[q] = _index[q];
                  }
                vi->SetPixel(index,pixel);
                }
              }
            }
          }
        }
      }
    }
  typename FieldWriterType::Pointer writer = FieldWriterType::New();
  writer->SetInput(vi);
  writer->SetFileName(fname.c_str());
  try
    {
    writer->Write();
    }
  catch(itk::ExceptionObject &ex)
    {
    std::string message;
    message = "Problem found while writing image ";
    message += fname; message += "\n";
    message += ex.GetLocation(); message += "\n";
    message += ex.GetDescription(); std::cout << message << std::endl;
    Remove(fname.c_str());
    return EXIT_FAILURE;
    }
  //
  // read it back in.
  typename VectorImageType::Pointer readback;
  typename FieldReaderType::Pointer reader = FieldReaderType::New();
  try
    {
    reader->SetFileName(fname.c_str());
    reader->Update();
    readback = reader->GetOutput();
    }
  catch(itk::ExceptionObject &ex)
    {
    std::string message;
    message = "Problem found while reading image ";
    message += fname; message += "\n";
    message += ex.GetLocation(); message += "\n";
    message += ex.GetDescription(); std::cout << message << std::endl;
    Remove(fname.c_str());
    return EXIT_FAILURE;
    }
  bool same = true;
  if(readback->GetOrigin() != vi->GetOrigin() )
    {
    std::cout << "Origin is different: " << readback->GetOrigin() << " != " << vi->GetOrigin()  << std::endl;
    same = false;
    }
  if(readback->GetSpacing() != vi->GetSpacing() )
    {
    std::cout << "Spacing is different: " << readback->GetSpacing() << " != " << vi->GetSpacing()  << std::endl;
    same = false;
    }
  for(unsigned int r=0;r<Dimension;r++)
    {
    for(unsigned int c=0;c<Dimension;c++)
      {
      if(vcl_abs(readback->GetDirection()[r][c] - vi->GetDirection()[r][c]) > 1e-7 )
        {
        std::cout << "Direction is different:\n " << readback->GetDirection() << "\n != \n" << vi->GetDirection()  << std::endl;
        same = false;
        break;
        }
      }
    }
  std::cout << "Original vector Image  ?=   vector Image read from disk " << std::endl;
  for(int l = 0; l < dims[6]; l++)
    {
    _index[6] = l;
    for(int m = 0; m < dims[5]; m++)
      {
      _index[5] = m;
      for(int n = 0; n < dims[4]; n++)
        {
        _index[4] = n;
        for(int p = 0; p < dims[3]; p++)
          { 
          _index[3] = p;
          for(int i = 0; i < dims[2]; i++)
            {
            _index[2] = i;
            for(int j = 0; j < dims[1]; j++)
              {
              _index[1] = j;
              for(int k = 0; k < dims[0]; k++)
                {
                _index[0] = k;
                FieldPixelType p1,p2;
                for(unsigned int q = 0; q < Dimension; q++)
                  {
                  index[q] = _index[q];
                  }
                p1 = vi->GetPixel(index);
                p2 = readback->GetPixel(index);
                if(p1 != p2)
                  {
                  same = false;
                  std::cout << p1 << " != " << p2 <<  "    ERROR! " << std::endl;
                  }
                else
                  {
                  std::cout << p1 << " == " << p2 << std::endl;
                  }
                }
              }
            }
          }
        }
      }
    }
  if(same)
    {
    Remove(fname.c_str());
    }
  else
    {
    std::cout << "Failing image can be found at: " << fname << std::endl;
    }
  return same ? 0 : EXIT_FAILURE;
}
/** Test writing and reading a Vector Image
 */
int itkNiftiImageIOTest3(int ac, char* av[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if(ac > 1) 
    {
    char *testdir = *++av;
    itksys::SystemTools::ChangeDirectory(testdir);
    }
  else
    {
    return EXIT_FAILURE;
    }
  int success(0);
  success |= TestVectorImage<float,3,1>(std::string("testVectorImage_float_3_1.nii.gz"));
  success |= TestVectorImage<float,3,2>(std::string("testVectorImage_float_3_2.nii.gz"));
  success |= TestVectorImage<float,3,3>(std::string("testVectorImage_float_3_3.nii.gz"));
  success |= TestVectorImage<float,4,3>(std::string("testVectorImage_float_4_3.nii.gz"));
  //TODO: Need to make 4D images work for reading/writing within ITK success |= TestVectorImage<float,4,4>(std::string("testVectorImage_float_4_4.nii.gz"));
  success |= TestVectorImage<double,3,3>(std::string("testVectorImage_double_3_3.nii.gz"));
  return success;
}

typedef itk::Image<unsigned char,3> Test4ImageType;

void
PrintDir(Test4ImageType::DirectionType &dir)
{
  for(unsigned i = 0; i < 3; i++)
    {
    for(unsigned j = 0; j < 3; j++)
      {
      std::cerr << dir[i][j] << " ";
      }
    std::cerr << std::endl;
    }
}

namespace
{
bool Equal(double a, double b)
{
  // actual equality
  double diff = a - b;
  if(diff == 0.0)
    {
    return true;
    }
  // signs match?
  if((a < 0.00 && b >= 0.0) ||
     (b < 0.0 && a >= 0.0))
    {
    return false;
    }
  if(diff < 0.0)
    {
    diff = -diff;
    }
  double avg = (a+b)/2.0;
  if(avg < 0.0)
    {
    avg = - avg;
    }
  if(diff > avg/1000.0)
    {
    return false;
    }
  return true;
}

}
int itkNiftiImageIOTest4(int ac, char* av[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if(ac > 1) 
    {
    char *testdir = *++av;
    itksys::SystemTools::ChangeDirectory(testdir);
    }
  else
    {
    return EXIT_FAILURE;
    }
  
  typedef itk::ImageFileWriter<Test4ImageType> Test4Writer;
  typedef itk::ImageFileReader<Test4ImageType> Test4Reader;
  //
  Test4ImageType::Pointer test4Image = Test4ImageType::New();
  Test4ImageType::RegionType imageRegion;
  Test4ImageType::SizeType size;
  Test4ImageType::IndexType index;
  Test4ImageType::SpacingType spacing;
  Test4ImageType::PointType origin;
  const unsigned dimsize = 2;

  for(unsigned i = 0; i < 3; i++)
    {
    size[i] = dimsize;
    index[i] = 0;
    spacing[i] = 1.0;
    origin[i] = 0;
    }

  imageRegion.SetSize(size); 
  imageRegion.SetIndex(index);
  test4Image->SetRegions(imageRegion);
  test4Image->SetSpacing(spacing);
  test4Image->SetOrigin(origin);
  test4Image->Allocate();
  test4Image->FillBuffer(0);
  Test4ImageType::DirectionType dir;
  dir.SetIdentity();
#if 1
  // arbitrarily rotate the unit vectors to pick random direction
  // cosines;
  vnl_random randgen(8775070);
  typedef itk::AffineTransform<double,3>  TransformType;
  typedef itk::Vector<double,3> AxisType;
  TransformType::Pointer transform = TransformType::New();
  AxisType axis;
  axis[0] = 1.0; axis[1] = 0.0; axis[2] = 0.0;
  transform->Rotate3D(axis,randgen.drand32(0,3.1415926*2.0));
  axis[0] = 0.0; axis[1] = 1.0; axis[2] = 0.0;
  transform->Rotate3D(axis,randgen.drand32(0,3.1415926*2.0));
  axis[0] = 0.0; axis[1] = 0.0; axis[2] = 1.0;
  transform->Rotate3D(axis,randgen.drand32(0,3.1415926*2.0));
  TransformType::MatrixType mat = transform->GetMatrix();
  for(unsigned i = 0; i < 3; i++)
    {
    for(unsigned j = 0; j < 3; j++)
      {
      dir[i][j] = mat[i][j];
      }
    }
  
#else
  dir = 
    itk::SpatialOrientationAdapter().ToDirectionCosines(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLI);
#endif
  test4Image->SetDirection(dir);
  Test4Writer::Pointer writer = Test4Writer::New();
  writer->SetInput(test4Image);
  std::string fname("directionsTest.nii.gz");
  writer->SetFileName(fname.c_str());
  try
    {
    writer->Write();
    }
  catch(itk::ExceptionObject &ex)
    {
    std::string message;
    message = "Problem found while writing image ";
    message += fname; message += "\n";
    message += ex.GetLocation(); message += "\n";
    message += ex.GetDescription(); std::cout << message << std::endl;
    Remove(fname.c_str());
    return EXIT_FAILURE;
    }
  //
  // read it back in.
  Test4ImageType::Pointer readback;
  Test4Reader::Pointer reader = Test4Reader::New();
  reader->SetFileName(fname.c_str());
  try
    {
    reader->Update();
    readback = reader->GetOutput();
    }
  catch(itk::ExceptionObject &ex)
    {
    std::string message;
    message = "Problem found while reading image ";
    message += fname; message += "\n";
    message += ex.GetLocation(); message += "\n";
    message += ex.GetDescription(); std::cout << message << std::endl;
    Remove(fname.c_str());
    return EXIT_FAILURE;
    }
  Remove(fname.c_str());
  Test4ImageType::DirectionType dir2 = readback->GetDirection();

  std::cerr << "Original direction" << std::endl;
  PrintDir(dir);
  std::cerr << "Retrieved direction" << std::endl;
  PrintDir(dir2);

  for(unsigned int i = 0; i < 3; i++)
    {
    for(unsigned int j = 0; j < 3; j++)
      {
      if(!Equal(dir[i][j],dir2[i][j]))
        {
        std::cerr << "difference = " << dir[i][j] - dir2[i][j] << std::endl;
        return EXIT_FAILURE;
        }
      }
    }
  return EXIT_SUCCESS;
}

template <typename PixelType,unsigned typeIndex>
int
SlopeInterceptTest()
{
  //
  // fill out a nifti image and write it.
  const char *filename = "SlopeIntercept.nii";
  nifti_image * niftiImage = nifti_simple_init_nim();
  niftiImage->fname = (char *)malloc(strlen(filename)+1);
  strcpy(niftiImage->fname,filename);
  niftiImage->nifti_type = 1;
  niftiImage->iname = (char *)malloc(strlen(filename)+1);
  strcpy(niftiImage->iname,filename);
  niftiImage->dim[0] =
  niftiImage->ndim = 3;
  niftiImage->nx = niftiImage->dim[1] = 8;
  niftiImage->ny = niftiImage->dim[2] = 8;
  niftiImage->nz = niftiImage->dim[3] = 4;
  niftiImage->nvox = 256;
  niftiImage->dx = niftiImage->pixdim[1] = 
  niftiImage->dy = niftiImage->pixdim[2] = 
  niftiImage->dz = niftiImage->pixdim[3] = 1.0;
  niftiImage->nu = 1;
  niftiImage->datatype = typeIndex;
  niftiImage->nbyper = sizeof(PixelType);
  niftiImage->scl_slope = 1.0/256.0;
  niftiImage->scl_inter = 0.0;
  niftiImage->sform_code = NIFTI_XFORM_SCANNER_ANAT;
  niftiImage->qform_code = NIFTI_XFORM_ALIGNED_ANAT;
  niftiImage->qfac = 1;
  mat44 matrix;
  for(unsigned i = 0; i < 4; i++)
    {
    for(unsigned j = 0; j < 4; j++)
      {
      matrix.m[i][j] = (i == j) ? 1.0 : 0.0;
      }
    }
  niftiImage->qto_xyz = matrix;
  niftiImage->sto_xyz = matrix;
  niftiImage->sto_ijk = matrix;
  niftiImage->qto_ijk = matrix;
  nifti_mat44_to_quatern(matrix,
                         &(niftiImage->quatern_b),
                         &(niftiImage->quatern_c),
                         &(niftiImage->quatern_d),
                         &(niftiImage->qoffset_x),
                         &(niftiImage->qoffset_y),
                         &(niftiImage->qoffset_z),
                         0,
                         0,
                         0,
                         &(niftiImage->qfac));
  niftiImage->data = malloc(sizeof(PixelType) * 256);
  for(unsigned i = 0; i < 256; i++)
    {
    static_cast<PixelType *>(niftiImage->data)[i] = i;
    }
  nifti_image_write(niftiImage);
  nifti_image_free(niftiImage);
  //
  // read the image back in
  typedef typename itk::Image<float,3> ImageType;
  typedef typename itk::ImageFileReader<ImageType> ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(filename);
  try
    {
    reader->Update();
    }
  catch(...)
    {
    Remove(filename);
    return EXIT_FAILURE;
    }
  typename ImageType::Pointer image = reader->GetOutput();
  typedef typename itk::ImageRegionIterator<ImageType> IteratorType;
  IteratorType it(image,image->GetLargestPossibleRegion());
  it.GoToBegin();
  double maxerror = 0.0;
  for(unsigned i = 0; i < 256; i++,++it)
    {
    if(it.IsAtEnd())
      {
      return EXIT_FAILURE;
      }
    if(!Equal(it.Value(),static_cast<float>(i)/256.0))
      {
      //      return EXIT_FAILURE;
      double error = vcl_abs(it.Value() -
                             (static_cast<double>(i)/256.0));
      if(error > maxerror)
        {
        maxerror = error;
        }
      }
    }
  std::cerr << "Max error " << maxerror << std::endl;
  Remove(filename);
  return maxerror > 0.00001 ? EXIT_FAILURE : EXIT_SUCCESS;
}

int itkNiftiImageIOTest5(int ac, char* av[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if(ac > 1) 
    {
    char *testdir = *++av;
    itksys::SystemTools::ChangeDirectory(testdir);
    }
  else
    {
    return EXIT_FAILURE;
    }
  int success(0);
  success |= SlopeInterceptTest<unsigned char,NIFTI_TYPE_UINT8>();
  success |= SlopeInterceptTest<short,NIFTI_TYPE_INT16>();
  success |= SlopeInterceptTest<unsigned short,NIFTI_TYPE_UINT16>();
  success |= SlopeInterceptTest<int,NIFTI_TYPE_INT32>();
  success |= SlopeInterceptTest<unsigned int,NIFTI_TYPE_UINT32>();
  return success;
}
