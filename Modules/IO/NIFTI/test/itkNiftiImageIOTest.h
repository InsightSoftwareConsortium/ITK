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
#ifndef itkNiftiImageIOTest_h
#define itkNiftiImageIOTest_h


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
#include "itkMath.h"

#include "nifti1_io.h"

#include <cstring>
#include <iostream>
#include <fstream>
#include <cstdio>

#include "itkIOTestHelper.h"

const unsigned char RPI=16;        /*Bit pattern 0 0 0  10000*/
const unsigned char LEFT=128;      /*Bit pattern 1 0 0  00000*/
const unsigned char ANTERIOR=64;   /*Bit pattern 0 1 0  00000*/
const unsigned char SUPERIOR=32;   /*Bit pattern 0 0 1  00000*/

template<unsigned int TDimension>
typename itk::ImageBase<TDimension>::DirectionType
PreFillDirection()
{
  typename itk::ImageBase<TDimension>::DirectionType myDirection;
  myDirection.Fill(0.0);
  myDirection.SetIdentity();
  itkGenericExceptionMacro("This template should never be used. Only valid values are given below.");
  return myDirection;
}

template<> itk::ImageBase<1>::DirectionType PreFillDirection<1> ();
template<> itk::ImageBase<2>::DirectionType PreFillDirection<2> ();
template<> itk::ImageBase<3>::DirectionType PreFillDirection<3> ();
template<> itk::ImageBase<4>::DirectionType PreFillDirection<4> ();

template <typename T>
int MakeNiftiImage(void)
{
  typedef itk::Image<T, 3> ImageType;
  const char *filename = "test.nii";
  //Allocate Images
  enum { ImageDimension = ImageType::ImageDimension };
  const typename ImageType::SizeType size = {{10,10,10}};
  typename ImageType::SpacingType spacing;
  spacing[0] = spacing[1] = spacing[2] = 1.0;

  const typename ImageType::IndexType index = {{0,0,0}};
  typename ImageType::RegionType region(index,size);
  typename ImageType::Pointer img =
    itk::IOTestHelper::AllocateImageFromRegionAndSpacing<ImageType>(region, spacing);

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

  typename ImageType::RegionType RPIregion;
  itk::ImageRegionIterator<ImageType >  RPIiterator;

  { //Fill in left half
    const typename ImageType::IndexType RPIindex = {{0,0,0}};
    const typename ImageType::SizeType RPIsize = {{5,10,10}};
    RPIregion = typename ImageType::RegionType(RPIindex, RPIsize);
    RPIiterator = itk::ImageRegionIterator<ImageType >(img,RPIregion);
    while(!RPIiterator.IsAtEnd())
      {
        RPIiterator.Set( RPIiterator.Get() + LEFT );
        ++RPIiterator;
      }
  }
  { //Fill in anterior half
    const typename ImageType::IndexType RPIindex = {{0,5,0}};
    const typename ImageType::SizeType RPIsize =  {{10,5,10}};
    RPIregion = typename ImageType::RegionType(RPIindex, RPIsize);
    RPIiterator = itk::ImageRegionIterator<ImageType > (img,RPIregion);
    while(!RPIiterator.IsAtEnd())
      {
        RPIiterator.Set( RPIiterator.Get() + ANTERIOR );
        ++RPIiterator;
      }
  }
  { //Fill in superior half
    const typename ImageType::IndexType RPIindex = {{0,0,5}};
    const typename ImageType::SizeType RPIsize = {{10,10,5}};
    RPIregion = typename ImageType::RegionType(RPIindex, RPIsize);
    RPIiterator = itk::ImageRegionIterator<ImageType >(img,RPIregion);
    while(!RPIiterator.IsAtEnd())
      {
        RPIiterator.Set( RPIiterator.Get() + SUPERIOR );
        ++RPIiterator;
      }
  }
  {
  // Don't use identity DirectionCosine, Unit Spacing, or Zero Origin
  typename ImageType::DirectionType dc;
  dc[0][0] = 0; dc[0][1] = 1; dc[0][2] = 0;
  dc[1][0] = 0; dc[1][1] = 0; dc[1][2] = 1;
  dc[2][0] = 1; dc[2][1] = 0; dc[2][2] = 0;
  img->SetDirection(dc);
  typename ImageType::SpacingType sp;
  sp[0] = 1.0; sp[1] = 2.0; sp[2] = 3.0;
  img->SetSpacing(sp);
  typename ImageType::PointType og;
  og[0] = -10.0; og[1] = -20.0; og[2] = -30.0;
  img->SetOrigin(og);
  }
  {
  //Set the qform and sfrom codes for the MetaDataDictionary.
  itk::MetaDataDictionary & thisDic = img->GetMetaDataDictionary();
  itk::EncapsulateMetaData< std::string >( thisDic, "qform_code_name", "NIFTI_XFORM_SCANNER_ANAT" );
  itk::EncapsulateMetaData< std::string >( thisDic, "sform_code_name", "NIFTI_XFORM_UNKNOWN" );
  }

  try
    {
    itk::IOTestHelper::WriteImage<ImageType,itk::NiftiImageIO>(img,std::string(filename));
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
      itk::IOTestHelper::Remove(filename);
      return EXIT_FAILURE;
    }
  typename ImageType::Pointer input;
  try
    {
    input = itk::IOTestHelper::ReadImage<ImageType>(std::string(filename));
    // Get the sform and qform codes from the image
    itk::MetaDataDictionary & thisDic = input->GetMetaDataDictionary();
    //std::cout << "DICTIONARY:\n" << std::endl;
    //thisDic.Print( std::cout );
    std::string qform_temp="";
    if ( ! itk::ExposeMetaData< std::string >(thisDic, "qform_code_name", qform_temp) || qform_temp != "NIFTI_XFORM_SCANNER_ANAT" )
      {
      std::cerr << "ERROR: qform code not recovered from file properly: 'NIFTI_XFORM_SCANNER_ANAT' != ." << qform_temp << "'" << std::endl;
      return EXIT_FAILURE;
      }
    std::string sform_temp="";
    if ( ! itk::ExposeMetaData< std::string >(thisDic, "sform_code_name", sform_temp) || sform_temp != "NIFTI_XFORM_UNKNOWN" )
      {
      std::cerr << "ERROR: sform code not recovered from file properly:  'NIFTI_XFORM_UNKNOWN' != '" << sform_temp << "'" << std::endl;
      return EXIT_FAILURE;
      }
    }
  catch (itk::ExceptionObject &e)
    {
    e.Print(std::cerr);
    itk::IOTestHelper::Remove(filename);
    return EXIT_FAILURE;
    }
  itk::IOTestHelper::Remove(filename);
  return EXIT_SUCCESS;
}

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

/* common code for DTi and sym matrix tests
 *
 * Could probably be made to fo the image of vector test as well
 */

/* VS 2015 has a bug when building release with the heavly nested for
 * loops iterating too many times.  This turns off optimization to
 * allow the tests to pass.
*/
#if _MSC_VER == 1900
# pragma optimize( "", off )
#endif

template <typename PixelType, unsigned VDimension>
int
TestImageOfSymMats(const std::string &fname)
{

  const int dimsize = 2;
  /** Deformation field pixel type. */
//  typedef typename itk::DiffusionTenor3D<ScalarType>    PixelType;

  /** Deformation field type. */
  typedef typename itk::Image<PixelType,VDimension>      DtiImageType;

  //
  // swizzle up a random vector image.
  typename DtiImageType::RegionType imageRegion;
  typename DtiImageType::SizeType size;
  typename DtiImageType::IndexType index;
  typename DtiImageType::SpacingType spacing;
  typename DtiImageType::PointType origin;
  // original test case was destined for failure.  NIfTI always writes out 3D
  // orientation.  The only sensible matrices you could pass in would be of the form
  // A B C 0
  // D E F 0
  // E F G 0
  // 0 0 0 1
  // anything in the 4th dimension that didn't follow that form would just come up scrambled.
  //NOTE: Nifti only reports up to 3D images correctly for direction cosigns.  It is implicitly assumed
  //      that the direction for dimensions 4 or greater come diagonal elements including a 1 in the
  //      direction matrix.
  const typename DtiImageType::DirectionType myDirection = PreFillDirection<VDimension>();

  std::cout << " === Testing DtiImageType:  Image Dimension "
            << static_cast<int>(VDimension) << std::endl
            << "======================== Initialized Direction" << std::endl
            << myDirection << std::endl;

  for(unsigned i = 0; i < VDimension; i++)
    {
    size[i] = dimsize;
    index[i] = 0;
    spacing[i] = 1.0;
    origin[i] = 0;
    }

  imageRegion.SetSize(size);
  imageRegion.SetIndex(index);
  typename DtiImageType::Pointer vi =
    itk::IOTestHelper::AllocateImageFromRegionAndSpacing<DtiImageType>(imageRegion, spacing);
  vi->SetOrigin(origin);
  vi->SetDirection(myDirection);

  int dims[7];
  int _index[7];
  for(unsigned i = 0; i < VDimension; i++)
    {
    dims[i] = size[i];
    }
  for(unsigned i = VDimension; i < 7; i++)
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
                PixelType pixel;
                for(unsigned int q = 0; q < pixel.Size(); q++)
                  {
                  //pixel[q] = randgen.drand32(lowrange,highrange);
                  pixel[q] = incr_value++;
                  }
                for(unsigned int q = 0; q < VDimension; q++)
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
  try
    {
    itk::IOTestHelper::WriteImage<DtiImageType,itk::NiftiImageIO>(vi,fname);
    }
  catch(itk::ExceptionObject &ex)
    {
    std::string message;
    message = "Problem found while writing image ";
    message += fname; message += "\n";
    message += ex.GetLocation(); message += "\n";
    message += ex.GetDescription(); std::cout << message << std::endl;
    itk::IOTestHelper::Remove(fname.c_str());
    return EXIT_FAILURE;
    }
  //
  // read it back in.
  typename DtiImageType::Pointer readback;
  try
    {
    readback = itk::IOTestHelper::ReadImage<DtiImageType>(fname);
    }
  catch(itk::ExceptionObject &ex)
      {
    std::string message;
    message = "Problem found while reading image ";
    message += fname; message += "\n";
    message += ex.GetLocation(); message += "\n";
    message += ex.GetDescription(); std::cout << message << std::endl;
    itk::IOTestHelper::Remove(fname.c_str());
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
  for(unsigned int r=0;r<VDimension;r++)
    {
    for(unsigned int c=0;c<VDimension;c++)
      {
      if(std::abs(readback->GetDirection()[r][c] - vi->GetDirection()[r][c]) > 1e-7 )
        {
        std::cout << "Direction is different:\n "
                  << readback->GetDirection() << "\n != \n" << vi->GetDirection()
                  << std::endl;
        same = false;
        break;
      }
    }
    }
  std::cout << "Original Image  ?=   Image read from disk " << std::endl;
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
                PixelType p1,p2;
                for(unsigned int q = 0; q < VDimension; q++)
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
      itk::IOTestHelper::Remove(fname.c_str());
      }
  else
    {
    std::cout << "Failing image can be found at: " << fname << std::endl;
    }
  return same ? 0 : EXIT_FAILURE;
}

extern bool Equal(const double a, const double b);

template <typename RGBPixelType>
int RGBTest(int ac, char *av[])
{
  if(ac > 2)
    {
    char *testdir = *++av;
    itksys::SystemTools::ChangeDirectory(testdir);
    }
  else
    {
    return EXIT_FAILURE;
    }
  char * tmpImage = *++av;
  int success(EXIT_SUCCESS);
  typedef typename itk::Image<RGBPixelType,3> RGBImageType;
  typename RGBImageType::RegionType imageRegion;
  typename RGBImageType::SizeType size;
  typename RGBImageType::IndexType index;
  typename RGBImageType::SpacingType spacing;
  typename RGBImageType::PointType origin;
  typename RGBImageType::DirectionType myDirection;
  for(unsigned i = 0; i < 3; i++)
    {
    size[i] = 5;
    index[i] = 0;
    spacing[i] = 1.0;
    origin[i] = 0;
    }
  imageRegion.SetSize(size);
  imageRegion.SetIndex(index);
  typename RGBImageType::Pointer im =
    itk::IOTestHelper::AllocateImageFromRegionAndSpacing<RGBImageType>(imageRegion,spacing);
  vnl_random randgen(12345678);
  itk::ImageRegionIterator<RGBImageType> it(im,im->GetLargestPossibleRegion());
  for(it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    RGBPixelType pix;
    for(unsigned int i = 0; i < RGBPixelType::Dimension; i++)
      {
      pix[i] = randgen.lrand32(255);
      }
    it.Set(pix);
    }
  typename RGBImageType::Pointer im2;
  try
    {
    itk::IOTestHelper::WriteImage<RGBImageType,itk::NiftiImageIO>(im,std::string(tmpImage));
    im2 = itk::IOTestHelper::ReadImage<RGBImageType>(std::string(tmpImage));
    }
  catch(itk::ExceptionObject &err)
    {
    std::cout << "itkNiftiImageIOTest9" << std::endl
              << "Exception Object caught: " << std::endl
              << err << std::endl;
    return EXIT_FAILURE;
    }
  itk::ImageRegionIterator<RGBImageType> it2(im2,im2->GetLargestPossibleRegion());
  for(it.GoToBegin(),it2.GoToBegin(); !it.IsAtEnd() && !it2.IsAtEnd(); ++it,++it2)
    {
    if(it.Value() != it2.Value())
      {
      std::cout << "Original Pixel (" << it.Value()
                << ") doesn't match read-in Pixel ("
                << it2.Value() << std::endl;
      success = EXIT_FAILURE;
      break;
      }
    }
  itk::IOTestHelper::Remove(tmpImage);
  return success;
}

int WriteNiftiTestFiles(const std::string & prefix);
int TestNiftiByteSwap(const std::string & prefix);
void RemoveNiftiByteSwapTestFiles(const std::string & prefix);

#endif
