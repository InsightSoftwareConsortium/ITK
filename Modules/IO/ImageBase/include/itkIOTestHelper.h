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
#ifndef itkIOTestHelper_h
#define itkIOTestHelper_h
#include "ITKIOImageBaseExport.h"
#include <string>

#include "itksys/SystemTools.hxx"
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "vnl/vnl_random.h"
namespace itk
{
class IOTestHelper
{
public:
  template <typename TImage>
  static typename TImage::Pointer ReadImage( const std::string &fileName,
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
        for(unsigned int i = 0; i < TImage::ImageDimension; i++)
          {
          origin[i]=0;
          }
        image->SetOrigin(origin);
        }
      return image;
    }

  template <typename ImageType,typename ImageIOType>
  static void
  WriteImage(typename ImageType::Pointer &image ,
             const std::string &filename)
    {

      typedef itk::ImageFileWriter< ImageType > WriterType;
      typename  WriterType::Pointer writer = WriterType::New();

      typename ImageIOType::Pointer imageio(ImageIOType::New());

      writer->SetImageIO(imageio);

      writer->SetFileName(filename.c_str());

      writer->SetInput(image);

      try
        {
        writer->Update();
        }
      catch (itk::ExceptionObject &err) {
      std::cerr << "Exception Object caught: " << std::endl
                << err << std::endl;
      throw;
      }
    }

//
// generate random pixels of various types
  static void
  RandomPix(vnl_random &randgen,itk::RGBPixel<unsigned char> &pix)
    {
      for(unsigned int i = 0; i < 3; i++)
        {
        pix[i] = randgen.lrand32(itk::NumericTraits<unsigned char>::max());
        }
    }

  template <typename TPixel>
  static void
  RandomPix(vnl_random &randgen, TPixel &pix)
    {
      pix = randgen.lrand32(itk::NumericTraits<TPixel>::max());
    }

  static void
  RandomPix(vnl_random &randgen, long long &pix)
    {
      pix = randgen.lrand32(itk::NumericTraits<int>::max());
      pix = (pix << 32) | randgen.lrand32();
    }

  static void
  RandomPix(vnl_random &randgen, unsigned long long &pix)
    {
      pix = randgen.lrand32();
      pix = (pix << 32) | randgen.lrand32();
    }

  static void
  RandomPix(vnl_random &randgen, double &pix)
    {
      pix = randgen.drand64(itk::NumericTraits<double>::max());
    }

  static void
  RandomPix(vnl_random &randgen, float &pix)
    {
      pix = randgen.drand64(itk::NumericTraits<float>::max());
    }

  static int Remove(const char *fname)
    {
      return itksys::SystemTools::RemoveFile(fname);
    }

  template <typename ImageType>
  static void SetIdentityDirection(typename ImageType::Pointer &im)
    {
      typename ImageType::DirectionType dir;
      dir.SetIdentity();
      im->SetDirection(dir);
    }

  template <typename ImageType>
  static typename ImageType::Pointer
  AllocateImageFromRegionAndSpacing(const typename ImageType::RegionType &region,
                                    const typename ImageType::SpacingType &spacing)
    {
      typename ImageType::Pointer rval = ImageType::New();
      SetIdentityDirection<ImageType>(rval);
      rval->SetSpacing(spacing);
      rval->SetRegions(region);
      rval->Allocate();
      return rval;
    }
  template <typename ImageType>
  static typename ImageType::Pointer
  AllocateImageFromRegionAndSpacing(const typename ImageType::RegionType &region,
                                    const typename ImageType::SpacingType &spacing,
                                    int vecLength)
    {
      typename ImageType::Pointer rval = ImageType::New();
      rval->SetSpacing(spacing);
      rval->SetRegions(region);
      rval->SetVectorLength(vecLength);
      rval->Allocate();
      return rval;
    }
};
}
#endif // itkIOTestHelper_h
