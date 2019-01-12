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

#include "itkMockMontageHelper.hxx"
#include "itkMontageTestHelper.hxx"
#include "itkParseTileConfiguration.h"
#include "itkRGBPixel.h"

int itkMontageTest2D(int argc, char* argv[])
{
  if ( argc < 4 )
    {
    std::cerr << "Usage: " << argv[0] << " <directoryWtihInputData> <montageTSV> <mockTSV>";
    std::cerr << " [ varyPaddingMethods peakMethod loadIntoMemory streamSubdivisions ]" << std::endl;
    return EXIT_FAILURE;
    }

  bool varyPaddingMethods = true;
  if ( argc > 4 )
    {
    varyPaddingMethods = std::stoi( argv[4] );
    }
  int peakMethod = -1;
  if ( argc > 5 )
    {
    peakMethod = std::stoi( argv[5] );
    }
  bool loadIntoMemory = false;
  if ( argc > 6 )
    {
    loadIntoMemory = std::stoi( argv[6] );
    }
  unsigned streamSubdivisions = 1;
  if ( argc > 7 )
    {
    streamSubdivisions = std::stoul( argv[7] );
    }

  constexpr unsigned Dimension = 2;
  std::string inputPath = argv[1];
  if ( inputPath.back() != '/' && inputPath.back() != '\\' )
    {
    inputPath += '/';
    }

  itk::TileLayout2D stageTiles = itk::ParseTileConfiguration2D( inputPath + "TileConfiguration.txt" );
  itk::TileLayout2D actualTiles = itk::ParseTileConfiguration2D( inputPath + "TileConfiguration.registered.txt" );

  itk::ImageIOBase::Pointer imageIO = itk::ImageIOFactory::CreateImageIO(
    ( inputPath + stageTiles[0][0].FileName ).c_str(), itk::ImageIOFactory::ReadMode );
  imageIO->SetFileName( inputPath + stageTiles[0][0].FileName );
  imageIO->ReadImageInformation();
  const itk::ImageIOBase::IOPixelType pixelType = imageIO->GetPixelType();

  int r1, r2;
  if (pixelType == itk::ImageIOBase::IOPixelType::RGB)
    {
    r1 = montageTest< itk::RGBPixel< unsigned char >, itk::RGBPixel< unsigned int > >(
      stageTiles, actualTiles, inputPath, argv[2],
      varyPaddingMethods, peakMethod, loadIntoMemory, streamSubdivisions );
    r2 = mockMontageTest< unsigned char >(
        stageTiles, actualTiles, inputPath, argv[3], varyPaddingMethods );
    }
  else
    {
    r1 = montageTest< unsigned short, double >(
      stageTiles, actualTiles, inputPath, argv[2],
      varyPaddingMethods, peakMethod, loadIntoMemory, streamSubdivisions );
    r2 = mockMontageTest< unsigned short >(
        stageTiles, actualTiles, inputPath, argv[3], varyPaddingMethods );
    }

  if ( r1 == EXIT_FAILURE || r2 == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
