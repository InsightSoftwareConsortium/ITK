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

#include "itkWindowedSincInterpolateImageFunction.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "itkResampleImageFilter.h"
#include "itkRGBPixel.h"
#include "itkTileConfiguration.h"


template <typename PixelType, unsigned Dimension>
int
CreateGroundTruth(char *                        inFilename,
                  const std::vector<unsigned> & montageSize,
                  const std::vector<double> &   overlap,
                  const std::string &           outDir)
{
  using ImageType = itk::Image<PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(inFilename);
  reader->Update();
  typename ImageType::Pointer  inImage = reader->GetOutput();
  typename ImageType::SizeType size = inImage->GetLargestPossibleRegion().GetSize();
  typename ImageType::SizeType tileSize;

  // calculate tile sizes in index space
  std::vector<double> tileSizes(Dimension);
  constexpr double    slopeFactor = 0.40; // varMax as fraction of overlap (MUST be < 0.50)
  std::vector<double> varMax(Dimension);
  itk::SizeValueType  totalTiles = 1;
  for (unsigned d = 0; d < Dimension; d++)
  {
    tileSizes[d] = size[d] / (montageSize[d] + overlap[d] - montageSize[d] * overlap[d]);
    tileSize[d] = std::round(tileSizes[d]);
    varMax[d] = slopeFactor * tileSizes[d] * overlap[d];
    totalTiles *= montageSize[d];
  }

  using TileConfig = itk::TileConfiguration<Dimension>;
  TileConfig regular, randomized;
  for (unsigned d = 0; d < Dimension; d++)
  {
    regular.AxisSizes[d] = montageSize[d];
    randomized.AxisSizes[d] = montageSize[d];
  }
  regular.Tiles.resize(regular.LinearSize());
  randomized.Tiles.resize(randomized.LinearSize());


  using MersenneTwister = itk::Statistics::MersenneTwisterRandomVariateGenerator;
  typename MersenneTwister::Pointer randomizer = MersenneTwister::New();
  randomizer->SetSeed(1983); // make the pseudorandom seqence fully reproducible

  // calculate tile positions
  for (itk::SizeValueType linearIndex = 0; linearIndex < totalTiles; linearIndex++)
  {
    typename TileConfig::TileIndexType ind = regular.LinearIndexToNDIndex(linearIndex);
    regular.Tiles[linearIndex].FileName = "tile_" + std::to_string(linearIndex) + ".nrrd";
    randomized.Tiles[linearIndex].FileName = regular.Tiles[linearIndex].FileName;

    for (unsigned d = 0; d < Dimension; d++)
    {
      double regularPosition = ind[d] * (1 - overlap[d]) * tileSizes[d];
      regular.Tiles[linearIndex].Position[d] = regularPosition;
      randomized.Tiles[linearIndex].Position[d] = regularPosition;
      if (ind[d] > 0 && ind[d] < montageSize[d] - 1) // not at the outer perimeter of the montage
      {
        randomized.Tiles[linearIndex].Position[d] += randomizer->GetUniformVariate(-varMax[d], varMax[d]);
      }
    }
  }

  regular.Write(outDir + "TileConfiguration.txt");
  randomized.Write(outDir + "TileConfiguration.GroundTruth.txt");

  // now resample the tiles
  using ResampleFilterType = itk::ResampleImageFilter<ImageType, ImageType>;
  typename ResampleFilterType::Pointer resampleFilter = ResampleFilterType::New();
  resampleFilter->SetOutputDirection(inImage->GetDirection());
  resampleFilter->SetOutputSpacing(inImage->GetSpacing());
  resampleFilter->SetInput(inImage);
  resampleFilter->SetSize(tileSize);

  using SincType = itk::WindowedSincInterpolateImageFunction<ImageType, 5>;
  typename SincType::Pointer sinc = SincType::New();
  resampleFilter->SetInterpolator(sinc); // replace default linear interpolator
  using WriterType = itk::ImageFileWriter<ImageType>;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetInput(resampleFilter->GetOutput());

  for (itk::SizeValueType linearIndex = 0; linearIndex < totalTiles; linearIndex++)
  {
    itk::ContinuousIndex<double, Dimension> cInd;
    for (unsigned d = 0; d < Dimension; d++)
    {
      cInd[d] = randomized.Tiles[linearIndex].Position[d];
    }
    typename ImageType::PointType p;
    inImage->TransformContinuousIndexToPhysicalPoint(cInd, p);
    resampleFilter->SetOutputOrigin(p);
    std::cout << "Resampling " << randomized.Tiles[linearIndex].FileName << std::flush;
    resampleFilter->Update();

    std::cout << " Writing " << std::flush;
    writer->SetFileName(outDir + randomized.Tiles[linearIndex].FileName);
    writer->Update();
    std::cout << " Done" << std::endl;
  }

  return EXIT_SUCCESS;
}

template <typename PixelType>
int
CreateGroundTruth(int argc, char * argv[], unsigned dimension)
{
  std::string outputPath = argv[2];
  if (outputPath.back() != '/' && outputPath.back() != '\\')
  {
    outputPath += '/';
  }

  std::vector<unsigned> montageSize(dimension, 10);
  if (argc >= 3 + int(dimension))
  {
    for (unsigned i = 0; i < dimension; i++)
    {
      montageSize[i] = std::stoul(argv[3 + i]);
    }
  }

  std::vector<double> overlap(dimension, 0.1);
  if (argc >= 3 + 2 * int(dimension))
  {
    for (unsigned i = 0; i < dimension; i++)
    {
      overlap[i] = std::stod(argv[3 + dimension + i]) / 100.0;
    }
  }

  if (dimension == 2)
  {
    return CreateGroundTruth<PixelType, 2>(argv[1], montageSize, overlap, outputPath);
  }
  else if (dimension == 3)
  {
    return CreateGroundTruth<PixelType, 3>(argv[1], montageSize, overlap, outputPath);
  }
  else
  {
    std::cerr << "Only dimensions 2 and 3 are supported!" << std::endl;
    return EXIT_FAILURE;
  }
}

template <typename ComponentType>
int
CreateGroundTruth(int argc, char * argv[], unsigned dimension, itk::IOPixelEnum pixelType)
{
  if (pixelType == itk::IOPixelEnum::RGB) // possibly add RGBA
  {
    return CreateGroundTruth<itk::RGBPixel<ComponentType>>(argc, argv, dimension);
  }
  else
  {
    return CreateGroundTruth<ComponentType>(argc, argv, dimension);
  }
}

int
itkMontageTruthCreator(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0] << " <inputImage> <outputDirectory> [montageSize] [overlap%]" << std::endl;
    std::cerr << "Example:" << argv[0] << " slice2D.png ./outGT/ 8 6 15.0 10.0" << std::endl;
    std::cerr << "Example:" << argv[0] << " volume3D.nrrd ./outGT/ 8 6 4 15.0 10.0 5.0" << std::endl;
    return EXIT_FAILURE;
  }

  try
  {
    itk::ImageIOBase::Pointer imageIO = itk::ImageIOFactory::CreateImageIO(argv[1], itk::IOFileModeEnum::ReadMode);
    imageIO->SetFileName(argv[1]);
    imageIO->ReadImageInformation();
    unsigned                   dim = imageIO->GetNumberOfDimensions();
    const itk::IOComponentEnum cType = imageIO->GetComponentType();

    if (cType == itk::IOComponentEnum::UCHAR)
    {
      return CreateGroundTruth<unsigned char>(argc, argv, dim, imageIO->GetPixelType());
    }
    else if (cType == itk::IOComponentEnum::USHORT)
    {
      return CreateGroundTruth<unsigned short>(argc, argv, dim, imageIO->GetPixelType());
    }
    else if (cType == itk::IOComponentEnum::SHORT)
    {
      return CreateGroundTruth<short>(argc, argv, dim, imageIO->GetPixelType());
    }
    else
    {
      std::cerr << "Unsupported component type: " << itk::ImageIOBase::GetComponentTypeAsString(cType) << std::endl;
    }
  }
  catch (itk::ExceptionObject & exc)
  {
    std::cerr << exc;
  }
  catch (std::runtime_error & exc)
  {
    std::cerr << exc.what();
  }
  catch (...)
  {
    std::cerr << "Unknown error has occurred" << std::endl;
  }
  return EXIT_FAILURE;
}
