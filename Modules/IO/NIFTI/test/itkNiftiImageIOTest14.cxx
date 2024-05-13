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

#include "itkNiftiImageIOTest.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

// Test scaling of different spatial or temporal units
// ITK uses mm for space and s for time. NIFTI allows other storage units
// These should be converted to mm and s on output
//
// NIFTI xyzt_units field should be 10 (mm, sec) for all output
int
itkNiftiImageIOTest14(int argc, char * argv[])
{
  // usage: itkNiftiImageIOTest14 test_dir ref_image test_image
  // images should have the same size, spacing, and origin, but may have different units
  if (argc != 4)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " output_test_fn ref_image test_image" << std::endl;
    return EXIT_FAILURE;
  }

  // first arg is the output test filename
  const char * output_test_fn = argv[1];

  // second arg is the reference image in mm and sec
  const char * ref_image_fn = argv[2];

  // third arg is the test image in different units
  const char * test_image_fn = argv[3];

  constexpr unsigned int Dimension = 4;

  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;

  auto ref_image = itk::IOTestHelper::ReadImage<ImageType>(std::string(ref_image_fn));
  auto test_image = itk::IOTestHelper::ReadImage<ImageType>(std::string(test_image_fn));

  // check spacing of ref and test are the same
  const auto ref_spacing = ref_image->GetSpacing();
  const auto test_spacing = test_image->GetSpacing();

  // check size of bounding box of ref and test are the same
  const auto ref_region_size = ref_image->GetLargestPossibleRegion().GetSize();
  const auto test_region_size = test_image->GetLargestPossibleRegion().GetSize();

  // check origin
  const auto ref_origin = ref_image->GetOrigin();
  const auto test_origin = test_image->GetOrigin();

  for (unsigned int i = 0; i < Dimension; i++)
  {
    // check spacing is the same within tolerance
    if (!itk::Math::FloatAlmostEqual(ref_spacing[i], test_spacing[i], 4, 1e-6))
    {
      std::cerr << "Spacing of reference and test images do not match" << std::endl;
      return EXIT_FAILURE;
    }

    // check bounding box is the same
    if (ref_region_size[i] != test_region_size[i])
    {
      std::cerr << "Size of reference and test images do not match" << std::endl;
      return EXIT_FAILURE;
    }

    // check origin is the same within tolerance
    if (!itk::Math::FloatAlmostEqual(ref_origin[i], test_origin[i], 4, 1e-6))
    {
      std::cerr << "Origin of reference and test images do not match" << std::endl;
      return EXIT_FAILURE;
    }
  }

  // set the origin of time to a non-zero value
  auto newOrigin = ImageType::PointType();
  newOrigin[0] = 1.0;
  newOrigin[1] = 1.0;
  newOrigin[2] = 1.0;
  newOrigin[3] = 2.0;
  test_image->SetOrigin(newOrigin);

  itk::IOTestHelper::WriteImage<ImageType, itk::NiftiImageIO>(test_image, output_test_fn);

  bool metadataHasCorrectXYZTTUnits = false;
  bool metadataHasCorrectToffset = false;
  bool imageHasCorrectTimeOrigin = false;

  try
  {
    // read the image back in
    ImageType::Pointer image_from_disk = itk::IOTestHelper::ReadImage<ImageType>(output_test_fn);

    // check the metadata for xyzt_units and toffset
    itk::MetaDataDictionary & dictionary = image_from_disk->GetMetaDataDictionary();

    std::string toffset;
    if (!itk::ExposeMetaData<std::string>(dictionary, "toffset", toffset))
    {
      std::cerr << "toffset not found in metadata" << std::endl;
    }
    if (itk::Math::FloatAlmostEqual(std::stod(toffset), 2.0, 4, 1e-6))
    {
      metadataHasCorrectToffset = true;
    }
    else
    {
      std::cerr << "toffset: " << std::stod(toffset) << " is not set correctly in metadata" << std::endl;
      metadataHasCorrectToffset = false;
    }

    auto read_origin = test_image->GetOrigin();

    if (itk::Math::FloatAlmostEqual(read_origin[3], 2.0, 4, 1e-6))
    {
      imageHasCorrectTimeOrigin = true;
    }
    else
    {
      std::cerr << "Time origin not read back correctly from toffset field" << std::endl;
      imageHasCorrectTimeOrigin = false;
    }

    std::string xyzt_units;
    if (!itk::ExposeMetaData<std::string>(dictionary, "xyzt_units", xyzt_units))
    {
      std::cerr << "xyzt_units not found in metadata" << std::endl;
    }
    if (xyzt_units == "10")
    {
      metadataHasCorrectXYZTTUnits = true;
    }
    else
    {
      std::cerr << "xyzt_units not set correctly in metadata" << std::endl;
      metadataHasCorrectXYZTTUnits = false;
    }
  }
  catch (...)
  {
    std::cerr << "Exception caught while reading image back in" << std::endl;
    throw;
  }

  if (metadataHasCorrectXYZTTUnits && metadataHasCorrectToffset && imageHasCorrectTimeOrigin)
  {
    return EXIT_SUCCESS;
  }
  return EXIT_FAILURE;
}
