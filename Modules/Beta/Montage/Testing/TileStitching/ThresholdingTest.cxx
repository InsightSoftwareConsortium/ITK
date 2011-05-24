/*=========================================================================
 *
 *  Copyright Kitware Inc.
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


#include "vtkMicroscopyTileStitcher.h"
#include "vtkMicroscopyTileStitcherConfig.h"
#include "vtkMicroscopyTileConfigParser.h"
#include "vtkMicroscopyImageType.h"
#include "vtkSmartPointer.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRegisterThresholdedImageFilter.h"
#include "itkMicroscopyTileReader.h"

#include <iostream>

namespace {
template <class TPixel, unsigned int Dimension> class ProcesserCreator
{
public:
  typedef itk::Image<TPixel, Dimension>              ImageType;
  typedef typename ImageType::Pointer                ImagePointerType;
  typedef typename ImageType::SizeType               ImageSizeType;
  typedef typename ImageType::IndexType              ImageIndexType;
  typedef typename ImageType::RegionType             ImageRegionType;
  typedef typename ImageType::PointType              ImagePointType;
  typedef itk::MicroscopyTileReader<ImageType>       ReaderType;
  typedef typename ReaderType::Pointer               ReaderPointerType;
  typedef itk::ImageFileWriter<ImageType>            WriterType;
  typedef typename WriterType::Pointer               WriterPointerType;
  typedef itk::RegisterThresholdedImageFilter<TPixel, Dimension> FilterType;

  //
  //  stitch image pair
  //
  static void Process(vtkMicroscopyTileConfigParser* parser,
                     std::string fileNamePre)
    {
    int index = 1;
    parser->InitTraverse();
    do 
      {
      std::stringstream filename;
      filename << fileNamePre << index << ".tif";
      std::cout << "Processing " << index << "-th image and saving output to "
                << filename.str() << " ... ";
      
      // read image
      ReaderPointerType reader = ReaderType::New();
      std::vector<std::string> fileNames = parser->GetCurrentTileImageFileNames();
      reader->SetFileNames(fileNames);
      try
        {
        reader->Update();
        }
      catch (itk::ExceptionObject& e)
        {
        std::cout << "Error reading tile " << index << " " << e.what() << std::endl;
        continue;
        }
      ImagePointerType image = reader->GetOutputImage();

      // process image      
      typename FilterType::Pointer filter = FilterType::New();
      try
        {
        if (vtkMicroscopyTileStitcherConfig::GetInstance()->GetNormalizeFlag())
          {
          image = filter->NormalizeImage(image);
          }
        image = filter->BlurImage(image);
        image = filter->ThresholdImage(image);
        }
      catch (itk::ExceptionObject& e)
        {
        std::cout << "Error processing tile " << index << " " << e.what() << std::endl;
        continue;
        }

      // write output image
      WriterPointerType writer = WriterType::New();
      writer->SetFileName(filename.str());
      writer->SetInput(image);
      try
        {
        writer->Update();
        }
      catch (itk::ExceptionObject& e)
        {
        std::cout << "Error writing tile " << index << " " << e.what() << std::endl;
        continue;
        }
      
      index++;
      std::cout << "done" << std::endl;
      }
    while (parser->MoveToNextTile());
    }
  }; // end class
} // end namespace

#define ImageProcessMacro(ComponentType, Dimension, Parser, FilePre)                 \
  if (Dimension == 2)                                               \
    {                                                               \
    ProcesserCreator<ComponentType, 2>::Process(Parser, FilePre);  \
    }                                                               \
  else                                                              \
    {                                                               \
    ProcesserCreator<ComponentType, 3>::Process(Parser, FilePre);  \
    }

int main( int argc, char *argv[] )
{
  //inputs and output are passed as command line arguments
  if (argc < 7)
    {
    std::cout << "Usage: " << std::endl;
    std::cout << argv[0] << " ConfigFileName OutputImagePre NormalizationFlag "
              << "GaussianBlurSigma LowerThresholdRatio UpperThresholdRatio"
              << std::endl;
    return EXIT_FAILURE;
    }

  std::string configFileName = argv[1];
  std::string outputFileNamePre = argv[2];
  int normalizeFlag = atoi(argv[3]);
  double sigma = atof(argv[4]);
  double lowerThreshRatio = atof(argv[5]);
  double upperThreshRatio = atof(argv[6]);

  // get parameter setting
  vtkMicroscopyTileStitcherConfig* config =
    vtkMicroscopyTileStitcherConfig::GetInstance();
  config->SetNormalizeFlag(normalizeFlag);
  config->SetThresholdFlag(1);
  config->SetGaussianBlurSigma(sigma);
  config->SetLowerThresholdRatio(lowerThreshRatio);
  config->SetUpperThresholdRatio(upperThreshRatio);

  // parse config file
  vtkSmartPointer<vtkMicroscopyTileConfigParser> parser =
    vtkSmartPointer<vtkMicroscopyTileConfigParser>::New();
  parser->SetConfigFileName(configFileName.c_str());
  parser->Update();
  if (!parser->ParsedSuccessfully())
    {
    std::cout << "Error parsing config file." << std::endl;
    return 1;
    }

  // get image info  
  vtkMicroscopy::ComponentType componentType = parser->GetImageComponentType();
  const unsigned int           dimension     = parser->GetImageDimension();

  if (dimension != 2 && dimension != 3)
    {
    std::cout << "Only 2D or 3D tiles are supported." << std::endl;
    return 1;
    }

  // process input tiles
  switch (componentType)
    {
    case vtkMicroscopy::UNSIGNED_CHAR:
      ImageProcessMacro(unsigned char, dimension, parser, outputFileNamePre);
      break;
    case vtkMicroscopy::SHORT:
      ImageProcessMacro(short, dimension, parser, outputFileNamePre);
      break;
    case vtkMicroscopy::UNSIGNED_SHORT:
      ImageProcessMacro(unsigned short, dimension, parser, outputFileNamePre);
      break;
    default:
      std::cout << "Unrecognized component type." << std::endl;
      return 1;
    }

  return 0;
}
