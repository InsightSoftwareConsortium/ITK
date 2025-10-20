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

#include <fstream>
#include "itkExtractImageFilter.h"
#include "itkImageDuplicator.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkJoinSeriesImageFilter.h"
#include "itkNrrdImageIO.h"
#include "itkPermuteAxesImageFilter.h"
#include "itkTestingMacros.h"

// Test writing of time-varying displacement field into NRRD file.

// Generate a sequence of 2D or 3D displacement field images
template <class ImageType>
void
GenerateImageSequence(int numberOfTimePoints, std::vector<typename ImageType::Pointer> & imageList)
{
  // Region
  typename ImageType::SizeType size;
  size[0] = 8;
  size[1] = 12;
  if (ImageType::SizeType::Dimension > 2)
  {
    size[2] = 10;
  }
  typename ImageType::IndexType  start{};
  typename ImageType::RegionType region = { start, size };
  // Origin
  typename ImageType::PointType inOrigin; // 3D vector
  inOrigin[0] = 10.0;
  inOrigin[1] = 20.0;
  if (ImageType::PointType::Dimension > 2)
  {
    inOrigin[2] = 15.0;
  }
  // Direction (matrix indices: column, row)
  typename ImageType::DirectionType inDirection; // 3x3 matrix
  inDirection[0][0] = 0.94;
  inDirection[0][1] = 0.24;
  inDirection[1][0] = -0.10;
  inDirection[1][1] = 0.87;
  if (ImageType::DirectionType::RowDimensions > 2)
  {
    inDirection[0][2] = -0.24;
    inDirection[1][2] = 0.49;
    inDirection[2][0] = 0.33;
    inDirection[2][1] = -0.43;
    inDirection[2][2] = 0.84;
  }
  // Spacing
  typename ImageType::SpacingType inSpacing; // 3D vector
  inSpacing[0] = 1.2;
  inSpacing[1] = 2.3;
  if (ImageType::SpacingType::Dimension > 2)
  {
    inSpacing[2] = 4.1;
  }
  for (int timePoint = 0; timePoint < numberOfTimePoints; ++timePoint)
  {
    typename ImageType::Pointer image = ImageType::New();
    image->SetRegions(region);
    image->Allocate();

    // Fill each image with a different constant value for testing
    image->FillBuffer(typename ImageType::PixelType(timePoint * 2));

    // Set common geometry
    image->SetOrigin(inOrigin);
    image->SetDirection(inDirection);
    image->SetSpacing(inSpacing);
    // Add to list
    imageList.push_back(image);
  }
}

//----------------------------------------------------------------------------
template <class PixelType, int SpaceDimension>
void
WriteImageSequenceFile(std::string                                                            filename,
                       std::vector<typename itk::Image<PixelType, SpaceDimension>::Pointer> & imageList,
                       bool                                                                   listAxisFastest = false)
{
  typedef itk::Image<PixelType, SpaceDimension>     ImageType;
  typedef itk::Image<PixelType, SpaceDimension + 1> ImageSequenceType;

  typedef itk::JoinSeriesImageFilter<ImageType, ImageSequenceType> JoinImageFilterType;
  typename JoinImageFilterType::Pointer                            joinImageFilter = JoinImageFilterType::New();
  for (auto image : imageList)
  {
    joinImageFilter->PushBackInput(image);
  }

  typename ImageSequenceType::Pointer joinedImage;
  unsigned int                        listAxis;
  if (!listAxisFastest)
  {
    // axis order: 0 1 2 3
    listAxis = SpaceDimension;
    joinImageFilter->Update();
    joinedImage = joinImageFilter->GetOutput();
  }
  else
  {
    // axis order: 3 0 1 2

    joinImageFilter->Update();
    joinedImage = joinImageFilter->GetOutput();

    listAxis = 0;
    using PermuteFilterType = itk::PermuteAxesImageFilter<ImageSequenceType>;
    typename PermuteFilterType::Pointer permuteFilter = PermuteFilterType::New();
    permuteFilter->SetInput(joinImageFilter->GetOutput());

    // Define new axis order: e.g., swap axis 0 and 2 (X <-> Z)
    constexpr unsigned int                                ImageSequenceDimension = SpaceDimension + 1;
    itk::FixedArray<unsigned int, ImageSequenceDimension> order;
    order[0] = SpaceDimension;
    for (unsigned int i = 1; i < ImageSequenceDimension; ++i)
    {
      order[i] = i - 1;
    }
    permuteFilter->SetOrder(order);
    permuteFilter->Update();

    joinedImage = permuteFilter->GetOutput();

    // Permute filter does not reorder the origin, so we need to do it manually
    typename ImageSequenceType::PointType oldOrigin = joinedImage->GetOrigin();
    typename ImageSequenceType::PointType newOrigin;
    for (unsigned int i = 0; i < ImageSequenceDimension; ++i)
    {
      newOrigin[i] = oldOrigin[order[i]];
    }
    joinedImage->SetOrigin(newOrigin);
  }

  // Save additional metadata into NRRD header that is needed for correct image interpretation
  itk::MetaDataDictionary & dictionary = joinedImage->GetMetaDataDictionary();

  // NRRD axis 0 kind is set from pixel type (covariant-vector)
  // NRRD axis 1, 2, 3 are spatial, no need to modify them
  // NRRD axis 4 (ITK axis 3) must be set to "list" (setting to time or domain would assign direction and then no
  // per-axis unit could be specified)
  std::string listAxisStr = std::to_string(listAxis); // 3 for 5D image (sequence of 3D displacement fields), 2 for
                                                      // 4D image (sequence of 2D displacement fields)
  itk::EncapsulateMetaData<std::string>(dictionary, "NRRD_kinds[" + listAxisStr + "]", "list");
  // NRRD axis 4 (ITK axis 3) is set to time to indicate this is a time sequence
  itk::EncapsulateMetaData<std::string>(dictionary, "NRRD_labels[" + listAxisStr + "]", "time");
  // NRRD axis 4 (ITK axis 3) is set to seconds (unit of spatial axes can be set in NRRD "space units" tag)
  itk::EncapsulateMetaData<std::string>(dictionary, "NRRD_units[" + listAxisStr + "]", "s");

  // Set intent code indicating this is a displacement field (comes from Nifti heritage as a de facto standard)
  itk::EncapsulateMetaData<std::string>(
    dictionary, "intent_code", "1006"); // 1006 NIFTI_INTENT_DISPVECT (Displacement field)

  // Write to file
  typedef itk::ImageFileWriter<ImageSequenceType> ImageWriterType;
  typename ImageWriterType::Pointer               writer = ImageWriterType::New();
  writer->SetImageIO(itk::NrrdImageIO::New());
  writer->SetInput(joinedImage);
  writer->SetFileName(filename);
  writer->Update();
}

//----------------------------------------------------------------------------
void
CheckImageSequenceFileHeader(std::string  filename,
                             unsigned int SpaceDimension,
                             unsigned int expectedDimensionInFile,
                             std::string  expectedPixelType,
                             unsigned int expectedComponents,
                             bool         useNonListExtensionAsPixel)
{
  // Create an NRRD image IO object
  using ImageIOType = itk::NrrdImageIO;
  ImageIOType::Pointer imageIO = ImageIOType::New();
  if (useNonListExtensionAsPixel)
  {
    imageIO->SetAxesReorderToUseNonListRangeAxisAsPixel();
  }

  // Read metadata
  if (!imageIO->CanReadFile(filename.c_str()))
  {
    throw itk::InvalidArgumentError("Cannot read file: " + filename);
  }
  imageIO->SetFileName(filename);
  imageIO->ReadImageInformation(); // Read only the header information

  // Read the relevant header information
  itk::MetaDataDictionary  thisDic = imageIO->GetMetaDataDictionary();
  std::vector<std::string> keys = thisDic.GetKeys();

  std::cout << "---------------------------------------------------------" << std::endl;
  std::cout << "Checking NRRD file: " << filename << std::endl;
  std::cout << "Metadata: " << std::endl;
  for (const std::string & key : keys)
  {
    // Print all key/value pairs from metadata dictionary for debugging
    std::string value;
    itk::ExposeMetaData<std::string>(thisDic, key, value);
    std::cout << "  " << key << "=" << value << std::endl;
  }

  std::cout << "Image information:" << std::endl;
  unsigned int dimensionInFile = imageIO->GetNumberOfDimensions();
  std::cout << "  Dimensions: " << dimensionInFile << std::endl;
  if (dimensionInFile != expectedDimensionInFile)
  {
    throw itk::InvalidArgumentError("Unexpected number of dimensions in NRRD file: " + std::to_string(dimensionInFile) +
                                    " != " + std::to_string(expectedDimensionInFile));
  }
  for (unsigned int i = 0; i < expectedDimensionInFile; ++i)
  {
    std::cout << "  Size of dimension " << i << ": " << imageIO->GetDimensions(i) << std::endl;
  }
  std::cout << "  Pixel Type: " << imageIO->GetPixelTypeAsString(imageIO->GetPixelType()) << std::endl;
  std::cout << "  Component Type: " << imageIO->GetComponentTypeAsString(imageIO->GetComponentType()) << std::endl;

  std::string listAxis;
  if (dimensionInFile == SpaceDimension)
  {
    // list dimension is pixel components
    listAxis = "pixel";
  }
  else
  {
    // list dimension is after space dimension
    listAxis = std::to_string(SpaceDimension);

    // check axis kind
    std::string listAxisKind;
    itk::ExposeMetaData<std::string>(thisDic, "NRRD_kinds[" + listAxis + "]", listAxisKind);
    std::cout << "  NRRD axis " << listAxis << " kind: " << listAxisKind << std::endl;
    if (listAxisKind != "list")
    {
      throw itk::InvalidArgumentError("Unexpected NRRD axis kind for axis " + listAxis + ": " + listAxisKind +
                                      " != list");
    }
  }

  std::string listAxisLabel;
  itk::ExposeMetaData<std::string>(thisDic, "NRRD_labels[" + listAxis + "]", listAxisLabel);
  std::string listAxisUnit;
  itk::ExposeMetaData<std::string>(thisDic, "NRRD_units[" + listAxis + "]", listAxisUnit);
  std::cout << "  NRRD axis " << listAxis << " label: " << listAxisLabel << std::endl;
  std::cout << "  NRRD axis " << listAxis << " unit: " << listAxisUnit << std::endl;

  if (listAxisLabel != "time")
  {
    throw itk::InvalidArgumentError("Unexpected NRRD axis label for axis " + listAxis + ": " + listAxisLabel +
                                    " != time");
  }
  if (listAxisUnit != "s")
  {
    throw itk::InvalidArgumentError("Unexpected NRRD axis unit for axis " + listAxis + ": " + listAxisUnit + " != s");
  }
  if (imageIO->GetPixelTypeAsString(imageIO->GetPixelType()) != expectedPixelType)
  {
    throw itk::InvalidArgumentError(
      "Unexpected pixel type in NRRD file: " + imageIO->GetPixelTypeAsString(imageIO->GetPixelType()) +
      " != " + expectedPixelType);
  }
  if (imageIO->GetNumberOfComponents() != expectedComponents)
  {
    throw itk::InvalidArgumentError(
      "Unexpected number of components in NRRD file: " + std::to_string(imageIO->GetNumberOfComponents()) +
      " != " + std::to_string(expectedComponents));
  }
}

//----------------------------------------------------------------------------
template <class PixelType, int SpaceDimension>
void
ReadImageSequenceFile(std::string                                                            filename,
                      std::vector<typename itk::Image<PixelType, SpaceDimension>::Pointer> & imageList)
{
  constexpr unsigned int ImageSequenceDimension = SpaceDimension + 1;
  using ImageSequenceType = itk::Image<PixelType, ImageSequenceDimension>;
  using ImageType = itk::Image<PixelType, SpaceDimension>;

  // Read image from file
  using ReaderType = itk::ImageFileReader<ImageSequenceType>;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(filename);
  reader->Update();
  typename ImageSequenceType::ConstPointer image = reader->GetOutput();

  // Extract frames from image
  using ExtractImageFilterType = itk::ExtractImageFilter<ImageSequenceType, ImageType>;
  typename ExtractImageFilterType::Pointer extractImageFilter = ExtractImageFilterType::New();
  extractImageFilter->SetInput(image);

  // Set up extraction region
  typename ImageSequenceType::RegionType fullRegion = image->GetLargestPossibleRegion();
  typename ImageSequenceType::SizeType   extractionSize = fullRegion.GetSize();
  const int                              listDimIdx = SpaceDimension;
  int                                    numberOfTimePoints = extractionSize[listDimIdx];
  // Collapse sequence dimension when extracting frame
  extractionSize[listDimIdx] = 0;

  // Get image frames into list
  imageList.clear();
  for (int timePoint = 0; timePoint < numberOfTimePoints; ++timePoint)
  {
    typename ImageSequenceType::RegionType extractionRegion;
    typename ImageSequenceType::IndexType  extractionIndex = extractionRegion.GetIndex();
    extractionIndex[listDimIdx] = timePoint;
    extractionRegion = { extractionIndex, extractionSize };
    extractImageFilter->SetDirectionCollapseToSubmatrix();
    extractImageFilter->SetExtractionRegion(extractionRegion);
    extractImageFilter->Update();
    // Deep-copy the frame
    {
      using DuplicatorType = itk::ImageDuplicator<ImageType>;
      typename DuplicatorType::Pointer duplicator = DuplicatorType::New();
      duplicator->SetInputImage(extractImageFilter->GetOutput());
      duplicator->Update();
      imageList.push_back(duplicator->GetOutput());
    }
  }
}

//----------------------------------------------------------------------------
template <class PixelType, int SpaceDimension>
void
ReadImageFile(std::string filename, typename itk::VectorImage<PixelType, SpaceDimension>::Pointer & image)
{
  using ImageType = itk::VectorImage<PixelType, SpaceDimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(filename);
  reader->Update();
  image = reader->GetOutput();
}

//----------------------------------------------------------------------------
int
itkNrrd5dVectorImageReadWriteTest(int argc, char * argv[])
{
  if (argc < 7)
  {
    std::cerr << "Usage: " << argc << itkNameOfTestExecutableMacro(argv)
              << " 2d-double-vector.seq.nrrd 3d-double-vector.seq.nrrd"
              << " 2d-displacement-field-vector.seq.nrrd 3d-displacement-field-vector.seq.nrrd"
              << " 2d-color-image-vector.seq.nrrd 3d-color-image-vector.seq.nrrd\n";
    return EXIT_FAILURE;
  }

  // In the first two tests pixel type is scalar.
  // We test if we can read and write an image sequence with the list axis as the slowest axis and fastest axis.

  bool useNonListExtensionAsPixel = true;

  // 2D scalar image sequence, written and read with list as the slowest axis
  {
    const char *                                  filename = argv[1];
    constexpr int                                 SpaceDimension = 2;
    const std::string                             expectedPixelType = "scalar";
    typedef double                                PixelType;
    typedef itk::Image<PixelType, SpaceDimension> ImageType;
    std::vector<ImageType::Pointer>               imageListToWrite;

    GenerateImageSequence<ImageType>(20, imageListToWrite);
    WriteImageSequenceFile<PixelType, SpaceDimension>(filename, imageListToWrite);
    CheckImageSequenceFileHeader(
      filename, SpaceDimension, SpaceDimension + 1, expectedPixelType, 1, useNonListExtensionAsPixel);
    std::vector<typename itk::Image<PixelType, SpaceDimension>::Pointer> imageListRead;
    ReadImageSequenceFile<PixelType, SpaceDimension>(filename, imageListRead);
  }

  // 3D scalar image sequence, written and read with list as the slowest axis
  {
    const char *                                  filename = argv[2];
    constexpr int                                 SpaceDimension = 3;
    const std::string                             expectedPixelType = "scalar";
    typedef double                                PixelType;
    typedef itk::Image<PixelType, SpaceDimension> ImageType;
    std::vector<ImageType::Pointer>               imageListToWrite;

    GenerateImageSequence<ImageType>(20, imageListToWrite);
    WriteImageSequenceFile<PixelType, SpaceDimension>(filename, imageListToWrite);
    CheckImageSequenceFileHeader(
      filename, SpaceDimension, SpaceDimension + 1, expectedPixelType, 1, useNonListExtensionAsPixel);
    std::vector<typename itk::Image<PixelType, SpaceDimension>::Pointer> imageListRead;
    ReadImageSequenceFile<PixelType, SpaceDimension>(filename, imageListRead);
  }

  // 3D scalar image sequence, written with list as the fastest axis, read with list as the slowest axis
  {
    const char *                                  filename = argv[2];
    constexpr int                                 SpaceDimension = 3;
    const std::string                             expectedPixelType = "scalar";
    typedef double                                PixelType;
    typedef itk::Image<PixelType, SpaceDimension> ImageType;
    std::vector<ImageType::Pointer>               imageListToWrite;
    const int                                     numberOfTimePoints = 20;

    GenerateImageSequence<ImageType>(numberOfTimePoints, imageListToWrite);
    WriteImageSequenceFile<PixelType, SpaceDimension>(filename, imageListToWrite, true);
    CheckImageSequenceFileHeader(
      filename, SpaceDimension, SpaceDimension + 1, expectedPixelType, 1, useNonListExtensionAsPixel);

    // Read image from file into a single image with vector pixel type
    typedef itk::VectorImage<double, SpaceDimension> ImageTypeToRead;
    ImageTypeToRead::Pointer                         imageRead;
    ReadImageFile<double, SpaceDimension>(filename, imageRead);
    if (imageRead->GetNumberOfComponentsPerPixel() != numberOfTimePoints)
    {
      std::cerr << "Error reading image with vector pixel type, expected " << numberOfTimePoints << " components, got "
                << imageRead->GetNumberOfComponentsPerPixel() << std::endl;
      return EXIT_FAILURE;
    }
  }

  // Read list axis as fastest axis (default axis mapping strategy)
  useNonListExtensionAsPixel = false;

  // 2D scalar image sequence, written and read with list as the fastest axis
  {
    const char *                                         filename = argv[1];
    constexpr int                                        SpaceDimension = 2;
    const std::string                                    expectedPixelType = "vector";
    const int                                            numberOfTimePoints = 20;
    typedef double                                       PixelTypeToWrite;
    typedef itk::Image<PixelTypeToWrite, SpaceDimension> ImageTypeToWrite;
    std::vector<ImageTypeToWrite::Pointer>               imageListToWrite;

    GenerateImageSequence<ImageTypeToWrite>(numberOfTimePoints, imageListToWrite);
    WriteImageSequenceFile<PixelTypeToWrite, SpaceDimension>(filename, imageListToWrite);
    CheckImageSequenceFileHeader(
      filename, SpaceDimension, SpaceDimension, expectedPixelType, numberOfTimePoints, useNonListExtensionAsPixel);

    // Read image from file into a single image with vector pixel type
    typedef itk::VectorImage<double, SpaceDimension> ImageTypeToRead;
    ImageTypeToRead::Pointer                         imageRead;
    ReadImageFile<double, SpaceDimension>(filename, imageRead);
    if (imageRead->GetNumberOfComponentsPerPixel() != numberOfTimePoints)
    {
      std::cerr << "Error reading image with vector pixel type, expected " << numberOfTimePoints << " components, got "
                << imageRead->GetNumberOfComponentsPerPixel() << std::endl;
      return EXIT_FAILURE;
    }
  }

  // 3D scalar image sequence, written and read with list as the fastest axis
  {
    const char *                                         filename = argv[2];
    constexpr int                                        SpaceDimension = 3;
    const std::string                                    expectedPixelType = "vector";
    const int                                            numberOfTimePoints = 20;
    typedef double                                       PixelTypeToWrite;
    typedef itk::Image<PixelTypeToWrite, SpaceDimension> ImageTypeToWrite;
    std::vector<ImageTypeToWrite::Pointer>               imageListToWrite;

    GenerateImageSequence<ImageTypeToWrite>(numberOfTimePoints, imageListToWrite);
    WriteImageSequenceFile<PixelTypeToWrite, SpaceDimension>(filename, imageListToWrite);
    CheckImageSequenceFileHeader(
      filename, SpaceDimension, SpaceDimension, expectedPixelType, numberOfTimePoints, useNonListExtensionAsPixel);

    // Read image from file into a single image with vector pixel type
    typedef itk::VectorImage<double, SpaceDimension> ImageTypeToRead;
    ImageTypeToRead::Pointer                         imageRead;
    ReadImageFile<double, SpaceDimension>(filename, imageRead);
    if (imageRead->GetNumberOfComponentsPerPixel() != numberOfTimePoints)
    {
      std::cerr << "Error reading image with vector pixel type, expected " << numberOfTimePoints << " components, got "
                << imageRead->GetNumberOfComponentsPerPixel() << std::endl;
      return EXIT_FAILURE;
    }
  }

  // 3D scalar image time sequence, written time as the fastest axis, read with time as slowest axis
  {
    const char *                                         filename = argv[2];
    constexpr int                                        SpaceDimension = 3;
    const std::string                                    expectedPixelType = "vector";
    const int                                            numberOfTimePoints = 20;
    typedef double                                       PixelTypeToWrite;
    typedef itk::Image<PixelTypeToWrite, SpaceDimension> ImageTypeToWrite;
    std::vector<ImageTypeToWrite::Pointer>               imageListToWrite;

    GenerateImageSequence<ImageTypeToWrite>(numberOfTimePoints, imageListToWrite);
    WriteImageSequenceFile<PixelTypeToWrite, SpaceDimension>(filename, imageListToWrite);
    CheckImageSequenceFileHeader(
      filename, SpaceDimension, SpaceDimension, expectedPixelType, numberOfTimePoints, useNonListExtensionAsPixel);

    // Read image from file into a single image with vector pixel type
    typedef itk::VectorImage<double, SpaceDimension> ImageTypeToRead;
    ImageTypeToRead::Pointer                         imageRead;
    ReadImageFile<double, SpaceDimension>(filename, imageRead);
    if (imageRead->GetNumberOfComponentsPerPixel() != numberOfTimePoints)
    {
      std::cerr << "Error reading image with vector pixel type, expected " << numberOfTimePoints << " components, got "
                << imageRead->GetNumberOfComponentsPerPixel() << std::endl;
      return EXIT_FAILURE;
    }
  }

  // 2D gradient field sequence
  {
    const char *                                         filename = argv[3];
    constexpr int                                        SpaceDimension = 2;
    const std::string                                    expectedPixelType = "covariant_vector";
    typedef itk::CovariantVector<double, SpaceDimension> PixelType;
    typedef itk::Image<PixelType, SpaceDimension>        ImageType;
    std::vector<ImageType::Pointer>                      imageListToWrite;

    GenerateImageSequence<ImageType>(20, imageListToWrite);
    WriteImageSequenceFile<PixelType, SpaceDimension>(filename, imageListToWrite);
    CheckImageSequenceFileHeader(
      filename, SpaceDimension, SpaceDimension + 1, expectedPixelType, SpaceDimension, useNonListExtensionAsPixel);
    std::vector<typename itk::Image<PixelType, SpaceDimension>::Pointer> imageListRead;
    ReadImageSequenceFile<PixelType, SpaceDimension>(filename, imageListRead);
  }

  // 3D displacement field sequence
  {
    const char *                                  filename = argv[4];
    constexpr int                                 SpaceDimension = 3;
    const std::string                             expectedPixelType = "vector";
    typedef itk::Vector<double, SpaceDimension>   PixelType;
    typedef itk::Image<PixelType, SpaceDimension> ImageType;
    std::vector<ImageType::Pointer>               imageListToWrite;

    GenerateImageSequence<ImageType>(20, imageListToWrite);
    WriteImageSequenceFile<PixelType, SpaceDimension>(filename, imageListToWrite);
    CheckImageSequenceFileHeader(
      filename, SpaceDimension, SpaceDimension + 1, expectedPixelType, SpaceDimension, useNonListExtensionAsPixel);
    std::vector<typename itk::Image<PixelType, SpaceDimension>::Pointer> imageListRead;
    ReadImageSequenceFile<PixelType, SpaceDimension>(filename, imageListRead);
  }

  // 2D color image (RGB) sequence
  {
    const char *                                  filename = argv[5];
    constexpr int                                 SpaceDimension = 2;
    const std::string                             expectedPixelType = "rgb";
    const unsigned int                            expectedComponents = 3; // RGB has 3 components
    typedef itk::RGBPixel<double>                 PixelType;
    typedef itk::Image<PixelType, SpaceDimension> ImageType;
    std::vector<ImageType::Pointer>               imageListToWrite;

    GenerateImageSequence<ImageType>(20, imageListToWrite);
    WriteImageSequenceFile<PixelType, SpaceDimension>(filename, imageListToWrite);
    CheckImageSequenceFileHeader(
      filename, SpaceDimension, SpaceDimension + 1, expectedPixelType, expectedComponents, useNonListExtensionAsPixel);
    std::vector<typename itk::Image<PixelType, SpaceDimension>::Pointer> imageListRead;
    ReadImageSequenceFile<PixelType, SpaceDimension>(filename, imageListRead);
  }

  // 3D color image (RGBA) sequence
  {
    const char *                                  filename = argv[6];
    constexpr int                                 SpaceDimension = 3;
    const std::string                             expectedPixelType = "rgba";
    const unsigned int                            expectedComponents = 4; // RGBA has 4 components
    typedef itk::RGBAPixel<double>                PixelType;
    typedef itk::Image<PixelType, SpaceDimension> ImageType;
    std::vector<ImageType::Pointer>               imageListToWrite;

    GenerateImageSequence<ImageType>(20, imageListToWrite);
    WriteImageSequenceFile<PixelType, SpaceDimension>(filename, imageListToWrite);
    CheckImageSequenceFileHeader(
      filename, SpaceDimension, SpaceDimension + 1, expectedPixelType, expectedComponents, useNonListExtensionAsPixel);
    std::vector<typename itk::Image<PixelType, SpaceDimension>::Pointer> imageListRead;
    ReadImageSequenceFile<PixelType, SpaceDimension>(filename, imageListRead);
  }

  // Cross-checks (make sure incorrect files cannot be read)

  // 2D displacement field can be distinguished from 3D
  {
    const char *      filename = argv[3];
    constexpr int     SpaceDimension = 3;
    const std::string expectedPixelType = "covariant_vector";

    bool expectedErrorCaught = false;
    try
    {
      CheckImageSequenceFileHeader(
        filename, SpaceDimension, SpaceDimension + 1, expectedPixelType, SpaceDimension, useNonListExtensionAsPixel);
    }
    catch (itk::InvalidArgumentError & e)
    {
      std::cout << "Caught expected error: " << e.what() << std::endl;
      expectedErrorCaught = true;
    }
    if (!expectedErrorCaught)
    {
      std::cerr << "Expected error not caught when checking reading 2D displacement field as 3D image." << std::endl;
      return EXIT_FAILURE;
    }
  }

  // RGB color can be distinguished from displacement field
  {
    const char *      filename = argv[3];
    constexpr int     SpaceDimension = 3;
    const std::string expectedPixelType = "rgb";

    bool expectedErrorCaught = false;
    try
    {
      CheckImageSequenceFileHeader(
        filename, SpaceDimension, SpaceDimension + 1, expectedPixelType, 3, useNonListExtensionAsPixel);
    }
    catch (itk::InvalidArgumentError & e)
    {
      std::cout << "Caught expected error: " << e.what() << std::endl;
      expectedErrorCaught = true;
    }
    if (!expectedErrorCaught)
    {
      std::cerr << "Expected error not caught when checking reading 2D displacement field as 3D image." << std::endl;
      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}
