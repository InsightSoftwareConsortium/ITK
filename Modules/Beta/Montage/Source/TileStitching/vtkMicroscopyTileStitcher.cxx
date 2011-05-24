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
#include "vtkMicroscopyTileConfigParser.h"
#include "vtkMicroscopyTileStitcherConfig.h"
#include "vtkObjectFactory.h"
#include "vtkSmartPointer.h"
#include "vtkMicroscopyImageType.h"
#include "vtkGlobalPositionOptimizer.h"

#include "itkImage.h"
#include "itkMicroscopyTileReader.h"
#include "itkMicroscopyTileStitcher.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkRGBPixel.h"
#include "itkIntTypes.h"

vtkCxxRevisionMacro(vtkMicroscopyTileStitcher, "$Revision: 429 $");
vtkStandardNewMacro( vtkMicroscopyTileStitcher );

namespace {
template <class TPixel, unsigned int Dimension>
class TileProcesserCreator
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
  typedef itk::MicroscopyTileStitcher<TPixel, Dimension>     StitcherType;

  //
  //  stitch image pair
  //
  static int Process(vtkMicroscopyTileConfigParser* parser)
    {
    parser->InitTraverse();
    while (parser->MoveToNextTile())
      {
      // prepare current/moving image
      int currentTileId[3];
      parser->GetCurrentTileId(currentTileId);
      ReaderPointerType movingReader = ReaderType::New();
      std::vector<std::string> movingTileFileNames = parser->GetCurrentTileImageFileNames();
      movingReader->SetFileNames(movingTileFileNames);
      try
        {
        movingReader->Update();
        }
      catch (itk::ExceptionObject& e)
        {
        std::cout << "Error reading moving tile " << e.what() << std::endl;
        continue;
        }

      double spacing[3];
      parser->GetImageSpacing(spacing);

      double recordedMovingOrigin[3];
      parser->GetRecordedCurrentTileOrigin(recordedMovingOrigin);
      movingReader->GetOutputImage()->SetOrigin(recordedMovingOrigin);
      movingReader->GetOutputImage()->SetSpacing(spacing);

      // foreach previous/fixed image that is (1) a neighbor of current/moving image
      // and (2) has been visited before, we register to compute the offset.
      std::vector<std::vector<int> > prevIds = parser->GetPreviousNeighborTileIds();
      for (size_t index = 0; index < prevIds.size(); index++)
        {
        int previousTileId[3];
        for (int i = 0; i < 3; i++)
          {
          previousTileId[i] = prevIds[index][i];
          }

        std::cout << "Register overlapping area of [" << currentTileId[0] << ", "
          << currentTileId[1] << ", " << currentTileId[2] << "] and ["
          << previousTileId[0] << ", " << previousTileId[1] << ", "
          << previousTileId[2] << "]: ";

        std::vector<std::string> fixedTileFileNames =
          parser->GetTileImageFileNames(previousTileId);


        // prepare fixed image
        ReaderPointerType fixedReader = ReaderType::New();
        fixedReader->SetFileNames(fixedTileFileNames);
        try
          {
          fixedReader->Update();
          }
        catch (itk::ExceptionObject& e)
          {
          std::cout << "Error reading fixed tile " << e.what() << std::endl;
          continue;
          }
        double recordedFixedOrigin[3];
        parser->GetRecordedTileOrigin(previousTileId, recordedFixedOrigin);
        fixedReader->GetOutputImage()->SetOrigin(recordedFixedOrigin);
        fixedReader->GetOutputImage()->SetSpacing(spacing);

        // register fixed and moving images
        typename StitcherType::Pointer stitcher = StitcherType::New();
        stitcher->SetFixedImage(fixedReader->GetOutputImage());
        stitcher->SetMovingImage(movingReader->GetOutputImage());
        if (previousTileId[2] != currentTileId[2])
          {
          stitcher->SetPreprocessFlag(1);
          }
        try
          {
          stitcher->Update();
          }
        catch (itk::ExceptionObject& e)
          {
          std::cout << "Error stitch tiles " << e.what() << std::endl;
          continue;
          }

        typename StitcherType::StitchingParameterType
          parameters = stitcher->GetStitchingParameters();
        if (parameters.size() != static_cast<size_t>(parser->GetImageDimension()))
          {
          std::cout << "Expect stitcher to return parameters of size = "
            << parser->GetImageDimension() << " but get size = "
            << parameters.size() << " instead." << std::endl;
          continue;
          }

        // compute origin of the moving tile
        double fixedOrigin[3], movingOrigin[3];
        parser->GetTileOrigin(previousTileId, fixedOrigin);
        for (int i = 0; i < parser->GetImageDimension(); i++)
          {
          if (i < parser->GetGridDimension())
            {
            movingOrigin[i] = fixedOrigin[i] - recordedFixedOrigin[i]
                            + recordedMovingOrigin[i] - parameters[i];
            }
          else
            {
            movingOrigin[i] = recordedMovingOrigin[i];
            }
          }

        // update tile origin and offet in the config file.
        parser->SetCurrentTileOrigin(movingOrigin);
        double offset[3];
        for (int i = 0; i < parser->GetImageDimension(); i++)
          {
          if (i < parser->GetGridDimension())
            {
            offset[i] = movingOrigin[i] - fixedOrigin[i];
            }
          else
            {
            offset[i] = 0;
            }
          }
        parser->RecordCurrentTilePairOffset(currentTileId, previousTileId, offset);

        } // end for
      } // end while
    return 1;
    }

  //
  // Output stitching result
  //
  static int Write(vtkMicroscopyTileConfigParser* parser, std::string& filename, TPixel background)
    {
    int size[3];
    double spacing[3], length[3];
    parser->GetImageSpacing(spacing);
    parser->GetImageSize(size);
    length[0] = spacing[0] * size[0];
    length[1] = spacing[1] * size[1];
    length[2] = spacing[2] * size[2];

    double origin[3], corner[3], tempOrigin[3], tempCorner[3];
    parser->InitTraverse();
    parser->GetCurrentTileOrigin(origin);
    corner[0] = origin[0] + length[0];
    corner[1] = origin[1] + length[1];
    corner[2] = origin[2] + length[2];
    while (parser->MoveToNextTile())
      {
      parser->GetCurrentTileOrigin(tempOrigin);
      for (int i = 0; i < 3; i++)
        {
        tempCorner[i] = tempOrigin[i] + length[i];
        if (spacing[i] > 0)
          {
          if (tempOrigin[i] < origin[i])
            {
            origin[i] = tempOrigin[i];
            }
          if (tempCorner[i] > corner[i])
            {
            corner[i] = tempCorner[i];
            }
          }
        else
          {
          if (tempOrigin[i] > origin[i])
            {
            origin[i] = tempOrigin[i];
            }
          if (tempCorner[i] < corner[i])
            {
            corner[i] = tempCorner[i];
            }
          }
        }
      }

    ImageSizeType newSize;
    ImageIndexType newStart;
    for (unsigned int i = 0; i < Dimension; i++)
      {
      newStart[i] = 0;
      newSize[i] = floor((corner[i] - origin[i]) / spacing[i] + 0.5) + 1;
      }
    ImageRegionType newRegion;
    newRegion.SetSize(newSize);
    newRegion.SetIndex(newStart);

    ImagePointerType newImage = ImageType::New();
    newImage->SetOrigin(origin);
    newImage->SetSpacing(spacing);
    newImage->SetRegions(newRegion);
    newImage->Allocate();
    newImage->FillBuffer(background);

    parser->InitTraverse();
    do
      {
      ReaderPointerType reader = ReaderType::New();
      reader->SetFileNames(parser->GetCurrentTileImageFileNames());
      try
        {
        reader->Update();
        }
      catch (itk::ExceptionObject& e)
        {
        std::cout << "Error reading tile " << e.what() << std::endl;
        continue;
        }
      double org[3];
      parser->GetCurrentTileOrigin(org);
      reader->GetOutputImage()->SetOrigin(org);
      reader->GetOutputImage()->SetSpacing(spacing);
      ImagePointerType image = reader->GetOutputImage();

      ImageIndexType index, newIndex;
      ImagePointType point;
      itk::ImageRegionIteratorWithIndex<ImageType> iter(image,
        image->GetLargestPossibleRegion());
      typedef typename ImageIndexType::IndexValueType  IndexValueType;
      while (!iter.IsAtEnd())
        {
        index = iter.GetIndex();
        image->TransformIndexToPhysicalPoint(index, point);
        newImage->TransformPhysicalPointToIndex(point, newIndex);
        for (unsigned int i = 0; i < Dimension; i++)
          {
          if (newIndex[i] < 0)
            {
            newIndex[i] = 0;
            }
          if (newIndex[i] > static_cast<IndexValueType>(newSize[i] - 1))
            {
            newIndex[i] = static_cast<IndexValueType>(newSize[i] - 1);
            }
          }
        TPixel value = iter.Get();
        newImage->SetPixel(newIndex, value);
        ++iter;
        }
      }
    while (parser->MoveToNextTile());

    WriterPointerType writer =  WriterType::New();
    writer->SetInput(newImage);
    writer->SetFileName(filename);
    try
      {
      writer->Update();
      }
    catch (itk::ExceptionObject& e)
      {
      std::cerr << "Error writing stitched tile " << e.what() << std::endl;
      return 0;
      }

    return 1;
    }
};

}

/** This macro will instantiate the pipeline for a reader creator for a
 * particular image type */
#define TileProcessMacro(ComponentType, Dimension)                  \
  if (Dimension == 2)                                               \
    {                                                               \
    TileProcesserCreator<ComponentType, 2>::Process(this->Parser);  \
    }                                                               \
  else                                                              \
    {                                                               \
    TileProcesserCreator<ComponentType, 3>::Process(this->Parser);  \
    }

/** This macro will instantiate the pipeline for a writer creator for a
 * particular image type */
#define TileWriteMacro(PixelType, Dimension, Background)                           \
  if (Dimension == 2)                                                              \
    {                                                                              \
    TileProcesserCreator<PixelType, 2>::Write(this->Parser, this->OutputFileName,  \
                                              Background);              \
    }                                                                              \
  else                                                                             \
    {                                                                              \
    TileProcesserCreator<PixelType, 3>::Write(this->Parser, this->OutputFileName,  \
                                              Background);              \
    }

//----------------------------------------------------------------------------
vtkMicroscopyTileStitcher::vtkMicroscopyTileStitcher()
{
  this->Parser = vtkSmartPointer<vtkMicroscopyTileConfigParser>::New();
  this->OutputFileName = "";
}

//----------------------------------------------------------------------------
vtkMicroscopyTileStitcher::~vtkMicroscopyTileStitcher()
{
}

//-----------------------------------------------------------------------------
void vtkMicroscopyTileStitcher::SetConfigFileName(const vtkStdString & filename)
{
  this->Parser->SetConfigFileName(filename.c_str());
}

//-----------------------------------------------------------------------------
void vtkMicroscopyTileStitcher::GlobalOptimization()
{
  int gridSize[3];
  this->Parser->GetGridSize(gridSize);

  vtkSmartPointer<vtkGlobalPositionOptimizer> optimizer =
    vtkSmartPointer<vtkGlobalPositionOptimizer>::New();
  optimizer->SetGridSize(gridSize);
  optimizer->SetInitialPositionParameters(this->Parser->GetOriginVector());
  optimizer->SetInitialOffsetParameters(this->Parser->GetOffsetVector());
  optimizer->SetOffsetIdToPositionIdsMap(this->Parser->GetOffsetIdToTileIdsMap());

  std::cout << "=====starting position==============" << std::endl;
  std::vector<double> vec = this->Parser->GetOriginVector();
  for (size_t i = 0; i < vec.size(); i+=3)
    {
    std::cout << i/3 << ": " << vec[i] << " " << vec[i+1] << " " << vec[i+2] << std::endl;
    }
  std::cout << "====================================" << std::endl;

  std::cout << "start global optimization ...... ";
  optimizer->Update();
  std::cout << "done." << std::endl;

  this->Parser->SetOriginVector(optimizer->GetOptimizedPositionParameters());

  std::cout << "=====ending position================" << std::endl;
  vec = optimizer->GetOptimizedPositionParameters();
  for (size_t i = 0; i < vec.size(); i+=3)
    {
    std::cout << i/3 << ": " << vec[i] << " " << vec[i+1] << " " << vec[i+2] << std::endl;
    }
  std::cout << "====================================" << std::endl;
}

//-----------------------------------------------------------------------------
void vtkMicroscopyTileStitcher::Update()
{
  this->Parser->Update();
  if (!this->Parser->ParsedSuccessfully())
    {
    return;
    }

  this->Parser->SetScanOrder(vtkMicroscopyTileStitcherConfig::GetInstance()->GetStitchOrder());

  vtkMicroscopy::ComponentType componentType = this->Parser->GetImageComponentType();
  vtkMicroscopy::PixelType     pixelType     = this->Parser->GetImagePixelType();
  const unsigned int           dimension     = this->Parser->GetImageDimension();

  if (dimension != 2 && dimension != 3)
    {
    vtkErrorMacro("Only 2D or 3D tiles are supported.");
    return;
    }

  //
  // Register input tiles
  //
  switch (componentType)
    {
    case vtkMicroscopy::UNSIGNED_CHAR:
      TileProcessMacro(unsigned char, dimension);
      break;
    case vtkMicroscopy::SHORT:
      TileProcessMacro(short, dimension);
      break;
    case vtkMicroscopy::UNSIGNED_SHORT:
      TileProcessMacro(unsigned short, dimension);
      break;
    default:
      vtkErrorMacro("Unrecognized component type.");
      break;
    }

  //
  //  Global optimization
  //
  this->GlobalOptimization();

  //
  // Output stitched image
  //
  switch (componentType)
    {
    case vtkMicroscopy::UNSIGNED_CHAR:
      if (pixelType == vtkMicroscopy::RGB)
        {
        unsigned char bv[3] = {0, 0, 0};
        TileWriteMacro(itk::RGBPixel<unsigned char>, dimension, bv);
        }
      else
        {
        TileWriteMacro(unsigned char, dimension, 0);
        }
      break;
    case vtkMicroscopy::SHORT:
      if (pixelType == vtkMicroscopy::RGB)
        {
        short bv[3] = {0, 0, 0};
        TileWriteMacro(itk::RGBPixel<short>, dimension, bv);
        }
      else
        {
        TileWriteMacro(short, dimension, 0);
        }
      break;
    case vtkMicroscopy::UNSIGNED_SHORT:
      if (pixelType == vtkMicroscopy::RGB)
        {
        unsigned short bv[3] = {0, 0, 0};
        TileWriteMacro(itk::RGBPixel<unsigned short>, dimension, bv);
        }
      else
        {
        TileWriteMacro(unsigned short, dimension, 0);
        }
      break;
    default:
      vtkErrorMacro("Unrecognized component type.");
      break;
    }

}
