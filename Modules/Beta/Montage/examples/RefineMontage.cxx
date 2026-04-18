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

#include "itkImageFileReader.h"
#include "itkTileConfiguration.h"
#include "itkTileMontage.h"

#include "itksys/SystemTools.hxx"

template <typename TImage>
typename TImage::Pointer
ReadImage(const char * filename)
{
  using ReaderType = itk::ImageFileReader<TImage>;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(filename);
  reader->Update();
  return reader->GetOutput();
}

// taken from test/itkMontageTestHelper.hxx and simplified
template <unsigned Dimension, typename PixelType, typename AccumulatePixelType>
void
refineMontage(const itk::TileConfiguration<Dimension> & stageTiles,
              itk::TileConfiguration<Dimension> &       actualTiles,
              const std::string &                       inputPath)
{
  using TileConfig = itk::TileConfiguration<Dimension>;
  using TransformType = itk::TranslationTransform<itk::SpacePrecisionType, Dimension>;
  using ScalarImageType = itk::Image<PixelType, Dimension>;
  typename ScalarImageType::SpacingType sp;
  sp.Fill(1.0);

  // instantiate and invoke the montaging class
  using MontageType = itk::TileMontage<ScalarImageType>;
  typename MontageType::Pointer montage = MontageType::New();
  montage->SetMontageSize(stageTiles.AxisSizes);
  for (size_t t = 0; t < stageTiles.LinearSize(); t++)
  {
    std::string                       filename = inputPath + stageTiles.Tiles[t].FileName;
    typename ScalarImageType::Pointer image = ReadImage<ScalarImageType>(filename.c_str());
    typename TileConfig::PointType    origin = stageTiles.Tiles[t].Position;

    // tile configurations are in pixel (index) coordinates
    // so we convert them into physical ones
    sp = image->GetSpacing();
    for (unsigned d = 0; d < Dimension; d++)
    {
      origin[d] *= sp[d];
    }
    image->SetOrigin(origin);

    montage->SetInputTile(t, image);
  }
  montage->Update(); // calculate registration transforms

  // update resulting tile configuration
  static_assert(std::is_same<typename TileConfig::TileIndexType, typename MontageType::TileIndexType>::value,
                "TileIndexType must be the same in itk::Montage and itk::TileConfiguration");
  for (size_t t = 0; t < actualTiles.LinearSize(); t++)
  {
    typename MontageType::TileIndexType  ind = actualTiles.LinearIndexToNDIndex(t);
    const TransformType *                regTr = montage->GetOutputTransform(ind);
    const itk::Vector<double, Dimension> regPos = regTr->GetOffset();
    // transform physical into index shift and modify original position
    for (unsigned d = 0; d < Dimension; d++)
    {
      actualTiles.Tiles[t].Position[d] = stageTiles.Tiles[t].Position[d] - regPos[d] / sp[d];
    }
  }
}

template <unsigned Dimension>
int
mainHelper(std::string inputPath, std::string inFile, std::string outFile)
{
  itk::TileConfiguration<Dimension> stageTiles;
  stageTiles.Parse(inFile);
  itk::TileConfiguration<Dimension> actualTiles = stageTiles; // result - only positions need to be updated

  std::string               firstFilename = inputPath + stageTiles.Tiles[0].FileName;
  itk::ImageIOBase::Pointer imageIO =
    itk::ImageIOFactory::CreateImageIO(firstFilename.c_str(), itk::IOFileModeEnum::ReadMode);
  imageIO->SetFileName(firstFilename);
  imageIO->ReadImageInformation();

  const unsigned numDimensions = imageIO->GetNumberOfDimensions();
  if (numDimensions != Dimension)
  {
    itkGenericExceptionMacro("Tile configuration has dimension " << Dimension << ", but image\n"
                                                                 << firstFilename << "\nhas dimension "
                                                                 << numDimensions);
  }

  const itk::IOComponentEnum componentType = imageIO->GetComponentType();
  switch (componentType)
  {
    case itk::IOComponentEnum::UCHAR:
      refineMontage<Dimension, unsigned char, unsigned int>(stageTiles, actualTiles, inputPath);
      break;
    case itk::IOComponentEnum::USHORT:
      refineMontage<Dimension, unsigned short, double>(stageTiles, actualTiles, inputPath);
      break;
    case itk::IOComponentEnum::SHORT:
      refineMontage<Dimension, short, double>(stageTiles, actualTiles, inputPath);
      break;
    default: // instantiating too many types leads to long compilation time and big executable
      itkGenericExceptionMacro(
        "Only unsigned char, unsigned short and short are supported as pixel component types! Trying to montage "
        << itk::ImageIOBase::GetComponentTypeAsString(componentType));
  }

  actualTiles.Write(outFile);

  return EXIT_SUCCESS;
}

int
main(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cout << "Usage: " << std::endl;
    std::cout << argv[0] << " <inputTileConfiguration> [outputTileConfiguration]" << std::endl;
    return EXIT_FAILURE;
  }

  std::string inputPath = itksys::SystemTools::GetFilenamePath(argv[1]);
  if (!inputPath.empty()) // a path was given in addition to file name
  {
    inputPath += '/';
  }

  std::string outputFilename = "TileConfiguration.registered.txt";
  if (argc > 2)
  {
    outputFilename = argv[2];
  }
  std::string outPath = itksys::SystemTools::GetFilenamePath(outputFilename);
  if (!outPath.empty()) // a path was given in addition to file name
  {
    outPath += '/';
    if (outPath != inputPath)
    {
      std::cout << "Warning: the output path differs from the input path:\n";
      std::cout << "In:  " << inputPath << "\n";
      std::cout << "Out  " << outPath << "\n";
      std::cout << "Image filenames will not contain paths (either relative or absolute)." << std::endl;
    }
  }
  else
  {
    outputFilename = inputPath + outputFilename;
  }

  try
  {
    unsigned dim;
    itk::TileConfiguration<2>::TryParse(argv[1], dim);

    switch (dim)
    {
      case 2:
        return mainHelper<2>(inputPath, argv[1], outputFilename);
      case 3:
        return mainHelper<3>(inputPath, argv[1], outputFilename);
      default:
        std::cerr << "Only dimensions 2 and 3 are supported. You are attempting to montage dimension " << dim;
        return EXIT_FAILURE;
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
