/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "itkRGBPixel.h"
#include "itkVector.h"
#include <cstdint>
#include <limits>
#include <random>
#include <type_traits>
namespace itk
{
class IOTestHelper
{
public:
  template <typename TImage>
  static typename TImage::Pointer
  ReadImage(const std::string & fileName, const bool zeroOrigin = false, ImageIOBase::Pointer imageio = nullptr)
  {
    using ReaderType = itk::ImageFileReader<TImage>;

    auto reader = ReaderType::New();
    {
      if (imageio)
      {
        reader->SetImageIO(imageio);
      }
      reader->SetFileName(fileName.c_str());
      try
      {
        reader->Update();
      }
      catch (const itk::ExceptionObject & err)
      {
        std::cout << "Caught an exception: " << std::endl;
        std::cout << err << ' ' << __FILE__ << ' ' << __LINE__ << std::endl;
        throw;
      }
      catch (...)
      {
        std::cout << "Error while reading in image for patient " << fileName << std::endl;
        throw;
      }
    }
    typename TImage::Pointer image = reader->GetOutput();
    if (zeroOrigin)
    {
      double origin[TImage::ImageDimension];
      for (unsigned int i = 0; i < TImage::ImageDimension; ++i)
      {
        origin[i] = 0;
      }
      image->SetOrigin(origin);
    }
    return image;
  }

  template <typename ImageType, typename ImageIOType>
  static void
  WriteImage(typename ImageType::Pointer   image,
             const std::string &           filename,
             typename ImageIOType::Pointer imageio = nullptr)
  {
    const bool create_local_io_object{ imageio.IsNull() };
    using WriterType = itk::ImageFileWriter<ImageType>;
    { // Test valid filename writing
      if (create_local_io_object)
      {
        imageio = ImageIOType::New();
      }
      auto writer = WriterType::New();
      writer->SetImageIO(imageio);
      writer->SetFileName(filename);
      writer->SetInput(image);
      try
      {
        writer->Update();
      }
      catch (const itk::ExceptionObject & err)
      {
        std::cerr << "Exception Object caught: " << std::endl << err << std::endl;
        throw;
      }
    }

    {                             // Test if writing to an invalid location causes exception to be thrown:
      imageio = imageio->Clone(); // A new io object is needed because the HDF5 io object is single use.  A new IO
                                  // object is needed to re-initialize the internal state.

      const std::string bad_root_path{ "/a_blatantly_obvious/bad_file_path/that/should/never/exist/on/the/computer/" };
      const std::string bad_filename{ bad_root_path + filename };
      bool              exception_correctly_caught = false;

      auto writer = WriterType::New();
      writer->SetImageIO(imageio);
      writer->SetFileName(bad_filename);
      writer->SetInput(image);
      try
      {
        writer->Update();
      }
      catch (const itk::ExceptionObject & /* err */)
      {
        // This is the correct behavior
        std::cout << "Correctly caught exception for attempting to write to an invalid file." << std::endl;
        exception_correctly_caught = true;
      }
      catch (...)
      {
        itkGenericExceptionMacro("IO library exception not converted to an itk::ExceptionObject.");
      }
      if (!exception_correctly_caught)
      {
        itkGenericExceptionMacro("Invalid file writing path did not throw an exception: " << bad_filename << " with "
                                                                                          << imageio->GetNameOfClass());
      }
    }
  }

  // Sample a value of arithmetic type T uniformly in [0, bound], avoiding
  // implementation-defined narrowing and the high-bits-zero artifact that
  // a single 32-bit engine() output produces for 64-bit integral types.
  template <typename T>
  static T
  BoundedRandom(std::mt19937 & randomNumberEngine, T bound)
  {
    static_assert(std::is_arithmetic_v<T>, "BoundedRandom: T must be arithmetic");
    if constexpr (std::is_floating_point_v<T>)
    {
      std::uniform_real_distribution<T> dist{ static_cast<T>(0), bound };
      return dist(randomNumberEngine);
    }
    else
    {
      using U = std::make_unsigned_t<T>;
      U raw;
      if constexpr (sizeof(T) > sizeof(std::mt19937::result_type))
      {
        // Combine two 32-bit engine outputs to fill the high bits of 64-bit pixel types.
        raw = (static_cast<U>(randomNumberEngine()) << 32) | static_cast<U>(randomNumberEngine());
      }
      else
      {
        raw = static_cast<U>(randomNumberEngine());
      }
      const U bound_u = static_cast<U>(bound);
      // No bound to apply when bound saturates U; bound_u + 1 would wrap to 0
      // and raw % 0 traps with SIGFPE.
      if (bound_u == std::numeric_limits<U>::max())
      {
        return static_cast<T>(raw);
      }
      return static_cast<T>(raw % (bound_u + U{ 1 }));
    }
  }

  template <typename TPixel>
  static void
  RandomPix(std::mt19937 & randomNumberEngine,
            TPixel &       pix,
            double         _max = static_cast<double>(itk::NumericTraits<TPixel>::max()))
  {
    static_assert(std::is_arithmetic_v<TPixel>, "RandomPix: TPixel must be arithmetic");
    pix = BoundedRandom<TPixel>(randomNumberEngine, static_cast<TPixel>(_max));
  }

  template <typename T>
  static void
  RandomPix(std::mt19937 &     randomNumberEngine,
            itk::RGBPixel<T> & pix,
            double             _max = static_cast<double>(itk::NumericTraits<T>::max()))
  {
    for (unsigned int i = 0; i < 3; ++i)
    {
      pix[i] = BoundedRandom<T>(randomNumberEngine, static_cast<T>(_max));
    }
  }

  template <typename T, unsigned int VLength>
  static void
  RandomPix(std::mt19937 &            randomNumberEngine,
            itk::Vector<T, VLength> & pix,
            double                    _max = static_cast<double>(itk::NumericTraits<T>::max()))
  {
    for (unsigned int i = 0; i < VLength; ++i)
    {
      pix[i] = BoundedRandom<T>(randomNumberEngine, static_cast<T>(_max));
    }
  }

  static int
  Remove(const char * fname)
  {
    return static_cast<bool>(itksys::SystemTools::RemoveFile(fname));
  }

  template <typename ImageType>
  static void
  SetIdentityDirection(typename ImageType::Pointer & im)
  {
    typename ImageType::DirectionType dir;
    dir.SetIdentity();
    im->SetDirection(dir);
  }

  template <typename ImageType>
  static typename ImageType::Pointer
  AllocateImageFromRegionAndSpacing(const typename ImageType::RegionType &  region,
                                    const typename ImageType::SpacingType & spacing)
  {
    auto rval = ImageType::New();
    SetIdentityDirection<ImageType>(rval);
    rval->SetSpacing(spacing);
    rval->SetRegions(region);
    rval->Allocate();
    return rval;
  }
  template <typename ImageType>
  static typename ImageType::Pointer
  AllocateImageFromRegionAndSpacing(const typename ImageType::RegionType &  region,
                                    const typename ImageType::SpacingType & spacing,
                                    int                                     vecLength)
  {
    auto rval = ImageType::New();
    rval->SetSpacing(spacing);
    rval->SetRegions(region);
    rval->SetVectorLength(vecLength);
    rval->Allocate();
    return rval;
  }
};
} // namespace itk
#endif // itkIOTestHelper_h
