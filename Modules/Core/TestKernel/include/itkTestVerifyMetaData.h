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
#ifndef itkTestVerifyMetaData_h
#define itkTestVerifyMetaData_h

#include "itkMetaDataDictionary.h"
#include "itkMetaDataObject.h"
namespace itk
{
/* A utility function used for testing, this is not intended to be part of the public interface */
/* Used only to avoid duplicate code in itkMetaDictionaryGTest.cxx and itkHDF5ImageIOTest.cxx */
template <typename T>
int
VerifyMetaDataPrivateTestingUtility(const itk::MetaDataDictionary & metaDict,
                                    const std::string &             key,
                                    const T &                       knownValue)
{
  int status = EXIT_SUCCESS;
  T   exposedValue{};

#if defined ITK_FUTURE_LEGACY_REMOVE
  static_assert(
    !std::is_same_v<itk::Array<char>, T>,
    "Should not use the ambiguous 'char' stored in meta data, because it is not-cross platform consistent.");
  static_assert(
    !std::is_same_v<char, T>,
    "Should not use the ambiguous 'char' stored in meta data, because it is not-cross platform consistent.");
  if (!itk::ExposeMetaData<T>(metaDict, key, exposedValue))
  {
    std::cerr << "Failure ExposeMetaData for key '" << key << "'" << std::endl;
    status = EXIT_FAILURE;
  }
#else
  if constexpr (std::is_same_v<itk::Array<char>, T>)
  {
    // If Encapsulate and Expose Metadata is all in core memory operations,
    // the type of char may be preserved.
    if (!itk::ExposeMetaData<itk::Array<char>>(metaDict, key, exposedValue))
    {
      // If Encapsulate and Expose Metadata is written to disk and
      // possibly shared across platforms, then 'char' may have been
      // stored in an intermediate format (aka file on disk) with explicit
      // signed or unsigned characteristics
      if constexpr (std::is_signed_v<char>)
      {
        itk::Array<signed char> temp_value{};
        if (!itk::ExposeMetaData<itk::Array<signed char>>(metaDict, key, temp_value))
        {
          std::cerr << "Failure ExposeMetaData '" << key << "'" << std::endl;
          status = EXIT_FAILURE;
        }
        exposedValue = temp_value;
      }
      else
      {
        itk::Array<unsigned char> temp_value{};
        if (!itk::ExposeMetaData<itk::Array<unsigned char>>(metaDict, key, temp_value))
        {
          std::cerr << "Failure ExposeMetaData '" << key << "'" << std::endl;
          status = EXIT_FAILURE;
        }
        exposedValue = temp_value;
      }
    }
  }
  else if constexpr (std::is_same_v<char, T>)
  {
    // If Encapsulate and Expose Metadata is all in core memory operations,
    // the type of char may be preserved.
    if (!itk::ExposeMetaData<char>(metaDict, key, exposedValue))
    {
      // If Encapsulate and Expose Metadata is written to disk and
      // possibly shared across platforms, then 'char' may have been
      // stored in an intermediate format (aka file on disk) with explicit
      // signed or unsigned characteristics
      if constexpr (std::is_signed_v<char>)
      {
        signed char temp_value{};

        if (!itk::ExposeMetaData<signed char>(metaDict, key, temp_value))
        {
          std::cerr << "Failure ExposeMetaData '" << key << "'" << std::endl;
          status = EXIT_FAILURE;
        }
        exposedValue = static_cast<T>(temp_value);
      }
      else
      {
        unsigned char temp_value{};
        if (!itk::ExposeMetaData<unsigned char>(metaDict, key, temp_value))
        {
          std::cerr << "Failure ExposeMetaData '" << key << "'" << std::endl;
          status = EXIT_FAILURE;
        }
        exposedValue = static_cast<T>(temp_value);
      }
    }
  }
  else if (!itk::ExposeMetaData<T>(metaDict, key, exposedValue))
  {
    std::cerr << "Failure ExposeMetaData of boolean type '" << key << "'" << std::endl;
    status = EXIT_FAILURE;
  }
#endif


  if constexpr (std::is_floating_point_v<T>)
  {
    if (itk::Math::NotAlmostEquals(exposedValue, knownValue))
    {
      std::cerr << "Incorrect meta value read in for " << key << " '" << exposedValue << "' != '" << knownValue << "'"
                << std::endl;
      status = EXIT_FAILURE;
    }
  }
  else
  {
    if (exposedValue != knownValue)
    {
      std::cerr << "Incorrect meta value read in for " << key << " '" << exposedValue << "' != '" << knownValue << "'"
                << std::endl;
      status = EXIT_FAILURE;
    }
  }
  if (status == EXIT_FAILURE)
  {
    std::cerr << "========================================" << std::endl;
    metaDict.Print(std::cerr);
    std::cerr << "========================================" << std::endl;
  }
  return status;
}
} // namespace itk

#endif // itkTestVerifyMetaData_h
