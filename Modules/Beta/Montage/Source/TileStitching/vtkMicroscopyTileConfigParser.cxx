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

#include "vtkMicroscopyTileConfigParser.h"
#include "vtkObjectFactory.h"
#include "vtksys/SystemTools.hxx"
#include "vtkXMLDataParser.h"
#include "vtkSmartPointer.h"

#include "itkImageIOBase.h"
#include "itkImageIOFactory.h"

vtkCxxRevisionMacro(vtkMicroscopyTileConfigParser, "$Revision: 429 $");
vtkStandardNewMacro( vtkMicroscopyTileConfigParser );

//----------------------------------------------------------------------------
vtkMicroscopyTileConfigParser::vtkMicroscopyTileConfigParser()
{
  this->ImageComponentType = vtkMicroscopy::UNKNOWNCOMPONENTTYPE;
  this->ImagePixelType = vtkMicroscopy::UNKNOWNPIXELTYPE;
  this->ImageDimension = 0;
  this->GridSize[0] = this->GridSize[1] = this->GridSize[2] = 0;
  this->CurrentTileId[0] = this->CurrentTileId[1] = this->CurrentTileId[2] = 0;
  this->ScanOrder = vtkMicroscopy::UNKNOWNSCANORDERTYPE;
  this->SuccessFlag = false;
}

//----------------------------------------------------------------------------
vtkMicroscopyTileConfigParser::~vtkMicroscopyTileConfigParser()
{
}

//-----------------------------------------------------------------------------
void vtkMicroscopyTileConfigParser::SetCurrentTileOrigin(double origin[3])
{
  int tileId[3];
  this->GetCurrentTileId(tileId);
  this->SetTileOrigin(tileId, origin);
}

//-----------------------------------------------------------------------------
void vtkMicroscopyTileConfigParser::SetTileOrigin(int tileId[3], double origin[3])
{
  int id = this->Get1DTileId(tileId);
  std::map<int, vtkstd::vector<double> >::iterator it =
    this->TileOriginMap.find(id);
  if (it != this->TileOriginMap.end())
    {
    vtkstd::vector<double> org;
    org.push_back(origin[0]);
    org.push_back(origin[1]);
    org.push_back(origin[2]);
    this->TileOriginMap[id] = org;
    }
}

//-----------------------------------------------------------------------------
void vtkMicroscopyTileConfigParser::GetCurrentTileOrigin(double origin[3])
{
  int tileId[3];
  this->GetCurrentTileId(tileId);
  this->GetTileOrigin(tileId, origin);
}

//-----------------------------------------------------------------------------
void vtkMicroscopyTileConfigParser::GetTileOrigin(int tileId[3], double origin[3])
{
  int id = this->Get1DTileId(tileId);
  std::map<int, vtkstd::vector<double> >::iterator it =
    this->TileOriginMap.find(id);
  if (it != this->TileOriginMap.end())
    {
    vtkstd::vector<double> org = this->TileOriginMap[id];
    origin[0] = org[0];
    origin[1] = org[1];
    origin[2] = org[2];
    }
}

//-----------------------------------------------------------------------------
void vtkMicroscopyTileConfigParser::GetRecordedCurrentTileOrigin(double origin[3])
{
  int tileId[3];
  this->GetCurrentTileId(tileId);
  this->GetRecordedTileOrigin(tileId, origin);
}

//-----------------------------------------------------------------------------
void vtkMicroscopyTileConfigParser::GetRecordedTileOrigin(int tileId[3], double origin[3])
{
  int id = this->Get1DTileId(tileId);
  std::map<int, vtkstd::vector<double> >::iterator it =
    this->RecordedTileOriginMap.find(id);
  if (it != this->RecordedTileOriginMap.end())
    {
    vtkstd::vector<double> org = this->RecordedTileOriginMap[id];
    origin[0] = org[0];
    origin[1] = org[1];
    origin[2] = org[2];
    }
}

//-----------------------------------------------------------------------------
std::vector<std::string> vtkMicroscopyTileConfigParser::
GetTileImageFileNames(int tileId[3])
{
  std::vector<std::string> stringList;
  int id = this->Get1DTileId(tileId);
  std::map<int, std::vector<std::string> >::iterator it =
    this->ImageFileNameMap.find(id);
  if (it != this->ImageFileNameMap.end())
    {
    stringList = it->second;
    }
  return stringList;
}

//-----------------------------------------------------------------------------
std::vector<std::string> vtkMicroscopyTileConfigParser::GetCurrentTileImageFileNames()
{
  int tileId[3];
  this->GetCurrentTileId(tileId);
  return this->GetTileImageFileNames(tileId);
}

//-----------------------------------------------------------------------------
void vtkMicroscopyTileConfigParser::InitTraverse()
{
  this->CurrentTileId[0] = 0;
  this->CurrentTileId[1] = 0;
  this->CurrentTileId[2] = 0;
}

//-----------------------------------------------------------------------------
int vtkMicroscopyTileConfigParser::MoveToNextTile()
{
  int order[3];
  this->GetScanOrder(order);

  if (++this->CurrentTileId[order[0]] == this->GridSize[order[0]])
    {
    this->CurrentTileId[order[0]] = 0;
    if (++this->CurrentTileId[order[1]] == this->GridSize[order[1]])
      {
      this->CurrentTileId[order[1]] = 0;
      if (++this->CurrentTileId[order[2]] == this->GridSize[order[2]])
        {
        this->InitTraverse();
        }
      }
    }
  return this->Get1DTileId(this->CurrentTileId);
}

//-----------------------------------------------------------------------------
void vtkMicroscopyTileConfigParser::MoveToTile(int tileId[3])
{
  if ( tileId[0] < 0 || tileId[0] >= this->GridSize[0]
    || tileId[1] < 0 || tileId[1] >= this->GridSize[1]
    || tileId[2] < 0 || tileId[2] >= this->GridSize[2] )
    {
    vtkErrorMacro("Input tile id outside tile dimension.")
    return;
    }
  this->CurrentTileId[0] = tileId[0];
  this->CurrentTileId[1] = tileId[1];
  this->CurrentTileId[2] = tileId[2];
}

//-----------------------------------------------------------------------------
void vtkMicroscopyTileConfigParser::GetCurrentTileId(int tileId[3])
{
  tileId[0] = this->CurrentTileId[0];
  tileId[1] = this->CurrentTileId[1];
  tileId[2] = this->CurrentTileId[2];
}

//-----------------------------------------------------------------------------
void vtkMicroscopyTileConfigParser::GetScanOrder(int order[3])
{
  switch (this->ScanOrder)
    {
    case vtkMicroscopy::ROWFIRST:
      order[0] = 0; order[1] = 1; order[2] = 2;
      break;
    case vtkMicroscopy::COLUMNFIRST:
      order[0] = 1; order[1] = 0; order[2] = 2;
      break;
    case vtkMicroscopy::DEPTHFIRST:
    default:
      order[0] = 2; order[1] = 0; order[2] = 1;
    }
}

//-----------------------------------------------------------------------------
std::vector<std::vector<int> >
vtkMicroscopyTileConfigParser::GetPreviousNeighborTileIds()
{
  std::vector<std::vector<int> > tileIds;

  int tileId[3];
  this->GetCurrentTileId(tileId);
  for (int i = 0; i < 3; i++)
    {
    std::vector<int> id3d;
    if (--tileId[i] >= 0)
      {
      id3d.push_back(tileId[0]);
      id3d.push_back(tileId[1]);
      id3d.push_back(tileId[2]);
      tileIds.push_back(id3d);
      }
    tileId[i]++;
    }

  return tileIds;
}

//-----------------------------------------------------------------------------
int vtkMicroscopyTileConfigParser::Get1DTileId(int tileId[3])
{
  std::vector<int> id3;
  id3.push_back(tileId[0]);
  id3.push_back(tileId[1]);
  id3.push_back(tileId[2]);
  std::map<std::vector<int>, int>::iterator it =
    this->TileId3DTo1DMap.find(id3);
  if (it != this->TileId3DTo1DMap.end())
    {
    return it->second;
    }
  return -1;
}

//-----------------------------------------------------------------------------
void vtkMicroscopyTileConfigParser::Get3DTileId(int tileId1, int tileId3[3])
{
  std::map<int, std::vector<int> >::iterator it =
    this->TileId1DTo3DMap.find(tileId1);
  if (it != this->TileId1DTo3DMap.end())
    {
    tileId3[0] = it->second[0]; tileId3[1] = it->second[1]; tileId3[2] = it->second[2];
    }
  else
    {
    tileId3[0] = -1;  tileId3[1] = -1;  tileId3[2] = -1;
    }
}

//-----------------------------------------------------------------------------
bool vtkMicroscopyTileConfigParser::IsCurrentTile(int tileId[3])
{
  return ( tileId[0] == this->CurrentTileId[0]
        && tileId[1] == this->CurrentTileId[1]
        && tileId[2] == this->CurrentTileId[2] );
}

//-----------------------------------------------------------------------------
void vtkMicroscopyTileConfigParser::SetConfigFileName(const char* filename)
{
  this->ConfigFileName = filename;
  std::string file;
  vtksys::SystemTools::SplitProgramPath(filename, this->InputPath, file);
  this->SuccessFlag = false;
}

//-----------------------------------------------------------------------------
void vtkMicroscopyTileConfigParser::Update()
{
  this->SuccessFlag = false;
  if (!vtksys::SystemTools::FileExists(this->ConfigFileName.c_str()))
    {
    vtkErrorMacro("Config file does not exist.");
    return;
    }

  vtkSmartPointer<vtkXMLDataParser> parser = vtkSmartPointer<vtkXMLDataParser>::New();
  parser->SetFileName(this->ConfigFileName.c_str());
  if (!parser->Parse())
    {
    vtkErrorMacro("Error parsing config file.");
    return;
    }

  // Get the root element
  vtkXMLDataElement* root = parser->GetRootElement();
  if (!root)
    {
    vtkErrorMacro("Error parsing config file -- root.");
    return;
    }

  // Process gridConfig--the first nested element of root.
  vtkXMLDataElement* gridConfig = root->FindNestedElementWithName("gridConfig");
  if (!gridConfig)
    {
    vtkErrorMacro("Error parsing config file -- gridConfig");
    return;
    }
  gridConfig->GetScalarAttribute("dimension", this->GridDimension);
  gridConfig->GetVectorAttribute("size", this->GridDimension, this->GridSize);
  for (int i = this->GridDimension; i < 3; i++)
    {
    this->GridSize[i] = 1;
    }

  // Process imageConfig--the second nested element of root.
  double physicalSize[3];
  vtkXMLDataElement* imageConfig = root->FindNestedElementWithName("imageConfig");
  if (!imageConfig)
    {
    vtkErrorMacro("Error parsing config file -- imageConfig.");
    return;
    }
  imageConfig->GetScalarAttribute("dimension", this->ImageDimension);
  imageConfig->GetVectorAttribute("physicalSize", this->ImageDimension, physicalSize);
  imageConfig->GetVectorAttribute("pixelSize", this->ImageDimension, this->ImageSize);
  for (int i = 0; i < this->ImageDimension; i++)
    {
    this->ImageSpacing[i] = physicalSize[i] / this->ImageSize[i];
    }
  for (int i = this->ImageDimension; i < 3; i++)
    {
    this->ImageSize[i] = 1;
    this->ImageSpacing[i] = 0.0;
    }

  // Process stacks--the only nested element of imageConfig.
  // Note: this field is optional. It is provided when the dataset contains 3D
  // tiles that are represented by sets of 2D image files.
  std::vector<double> depths;
  vtkXMLDataElement* stacks = imageConfig->FindNestedElementWithName("stacks");
  if (stacks)
    {
    // Process the nested planes (different scanning depths) of the stacks.
    double focus = 0.0;
    for (int i = 0; i < stacks->GetNumberOfNestedElements(); i++)
      {
      vtkXMLDataElement* plane = stacks->GetNestedElement(i);
      if (!plane)
        {
        vtkErrorMacro("Error parsing config file -- plane.");
        return;
        }
      plane->GetScalarAttribute("focus", focus);
      depths.push_back(focus);
      }

    if (depths.size() != static_cast<size_t>(this->ImageSize[2]))
      {
      vtkErrorMacro("Number of stack plane does not match specified image dimension.");
      return;
      }

    double zSpacing = (depths[depths.size() - 1] - depths[0]) / (depths.size() - 1);
    if (fabs(zSpacing - this->ImageSpacing[2]) > vtkMicroscopy::EPSILON)
      {
      vtkErrorMacro("The z spacing extracted from stack planes does not match "
        "specified image spacing.");
      return;
      }
    }

  // Go back to the nested elements of root. Starting from the third element (other
  // than gridConfig and imageConfig) they should all be image files.
  // Note: for 3D tiles containing multiple stack planes, we will stack the set of
  // 2D images taken at same x-y grid, but different depth into 3D.
  int numberOfImages = root->GetNumberOfNestedElements() - vtkMicroscopy::FILE_NODE_OFFSET;
  int numberOfImagesPerTile = 1;
  if (!depths.empty())
    {
    numberOfImagesPerTile = depths.size();
    }
  int numberOfTiles = numberOfImages / numberOfImagesPerTile;
  if (numberOfTiles*numberOfImagesPerTile - numberOfImages)
    {
    vtkErrorMacro("Error parsing config file -- plane and tile number mismatch.");
    return;
    }

  for (int tileId = 0; tileId < numberOfTiles; tileId++)
    {
    std::vector<std::string> fileNameList;
    int grid[3];
    double center[3];
    for (int imageId = 0; imageId < numberOfImagesPerTile; imageId++)
      {
      int xmlId = tileId * numberOfImagesPerTile + imageId + vtkMicroscopy::FILE_NODE_OFFSET;
      vtkXMLDataElement* image = root->GetNestedElement(xmlId);
      if (!image)
        {
        vtkErrorMacro("Error parsing config file -- image.");
        return;
        }
      fileNameList.push_back(this->InputPath + "/" + image->GetAttribute("fname"));

      if (!imageId)
        {
        image->GetVectorAttribute("center", 3, center);
        image->GetVectorAttribute("grid", this->GridDimension, grid);
        }
      }
    this->ImageFileNameMap[tileId] = fileNameList;

    vtkstd::vector<double> origin;
    for (int i = 0; i < 3; i++)
      {
      origin.push_back(center[i] -  physicalSize[i] / 2.0);
      }
    this->RecordedTileOriginMap[tileId] = origin;

    vtkstd::vector<int> gridId;
    for (int i = 0; i < this->GridDimension; i++)
      {
      gridId.push_back(grid[i]);
      }
    for (int i = this->GridDimension; i < 3; i++)
      {
      gridId.push_back(0);
      }
    this->TileId1DTo3DMap[tileId] = gridId;
    this->TileId3DTo1DMap[gridId] = tileId;

    // check image component type once on the first image.
    if (!tileId)
      {
      this->CheckImageComponentType(fileNameList[0]);
      }
    }

  this->TileOriginMap = this->RecordedTileOriginMap;

  this->SuccessFlag = true;
}

//-----------------------------------------------------------------------------
void vtkMicroscopyTileConfigParser::CheckImageComponentType(std::string & filename)
{
  typedef itk::ImageIOBase::IOComponentType  ITKComponentType;
  typedef itk::ImageIOBase::IOPixelType      ITKPixelType;

  itk::ImageIOBase::Pointer imageIO =
    itk::ImageIOFactory::CreateImageIO( filename.c_str(),
                                        itk::ImageIOFactory::ReadMode );
  if( !imageIO )
    {
    this->ImageComponentType = vtkMicroscopy::UNKNOWNCOMPONENTTYPE;
    this->ImagePixelType = vtkMicroscopy::UNKNOWNPIXELTYPE;
    vtkErrorMacro("No ImageIO recognized the fileformat of the filename");
    return;
    }

  try
    {
    imageIO->SetFileName( filename );
    imageIO->ReadImageInformation();
    }
  catch (itk::ExceptionObject & e)
    {
    vtkErrorMacro("Error reading image information " << e.what());
    }

  ITKPixelType pixelType = imageIO->GetPixelType();
  switch( pixelType )
    {
    case itk::ImageIOBase::SCALAR:
      this->ImagePixelType = vtkMicroscopy::SCALAR;
      break;
    case itk::ImageIOBase::VECTOR:
      this->ImagePixelType = vtkMicroscopy::VECTOR;
      break;
    case itk::ImageIOBase::RGB:
      this->ImagePixelType = vtkMicroscopy::RGB;
      break;
    default:
      this->ImagePixelType = vtkMicroscopy::UNKNOWNPIXELTYPE;
      vtkErrorMacro("File with unsupported pixel type");
    }

  ITKComponentType componentType = imageIO->GetComponentType();
  switch (componentType)
    {
    case itk::ImageIOBase::UCHAR:
      this->ImageComponentType = vtkMicroscopy::UNSIGNED_CHAR;
      break;
    case itk::ImageIOBase::SHORT:
      this->ImageComponentType = vtkMicroscopy::SHORT;
      break;
    case itk::ImageIOBase::USHORT:
      this->ImageComponentType = vtkMicroscopy::UNSIGNED_SHORT;
      break;
    case itk::ImageIOBase::INT:
      this->ImageComponentType = vtkMicroscopy::SHORT;
      break;
    case itk::ImageIOBase::FLOAT:
      this->ImageComponentType = vtkMicroscopy::FLOAT;
      break;
    case itk::ImageIOBase::DOUBLE:
      this->ImageComponentType = vtkMicroscopy::DOUBLE;
      break;
    default:
      this->ImageComponentType = vtkMicroscopy::UNKNOWNCOMPONENTTYPE;
      vtkErrorMacro("File with unsupported component type");
    }
}

//-----------------------------------------------------------------------------
bool vtkMicroscopyTileConfigParser::ParsedSuccessfully()
{
  return this->SuccessFlag;
}

//-----------------------------------------------------------------------------
void vtkMicroscopyTileConfigParser::RecordCurrentTilePairOffset(
        int movingTileId[3], int fixedTileId[3], double offset[3])
{
  int numberOfOffsets = static_cast<int>(this->OffsetVector.size() / 3);
  std::vector<int> idVec(2);
  idVec[0] = this->Get1DTileId(fixedTileId);
  idVec[1] = this->Get1DTileId(movingTileId);
  this->OffsetIdToTileIdsMap[numberOfOffsets] = idVec;

  this->OffsetVector.push_back(offset[0]);
  this->OffsetVector.push_back(offset[1]);
  this->OffsetVector.push_back(offset[2]);
}

//-----------------------------------------------------------------------------
const std::map<int, std::vector<int> > &
vtkMicroscopyTileConfigParser::GetOffsetIdToTileIdsMap()
{
  return this->OffsetIdToTileIdsMap;
}

//-----------------------------------------------------------------------------
const std::vector<double> & vtkMicroscopyTileConfigParser::GetOffsetVector()
{
  return this->OffsetVector;
}

//-----------------------------------------------------------------------------
const std::vector<double> & vtkMicroscopyTileConfigParser::GetOriginVector()
{
  // TileOriginMap holds elements with key id from 0 to numberOfTiles - 1
  this->OriginVector.clear();
  int id = 0;
  std::map<int, vtkstd::vector<double> >::iterator it =
    this->TileOriginMap.find(id);
  while (it != this->TileOriginMap.end())
    {
    this->OriginVector.push_back(it->second[0]);
    this->OriginVector.push_back(it->second[1]);
    this->OriginVector.push_back(it->second[2]);

    it = this->TileOriginMap.find(++id);
    }

  return this->OriginVector;
}

//-----------------------------------------------------------------------------
void vtkMicroscopyTileConfigParser
::SetOriginVector(const std::vector<double> & orgVector)
{
  this->OriginVector = orgVector;

  // TileOriginMap holds elements with key id from 0 to numberOfTiles - 1
  int id = 0;
  std::map<int, vtkstd::vector<double> >::iterator it =
    this->TileOriginMap.find(id);
  while (it != this->TileOriginMap.end())
    {
    std::vector<double> origin(3);
    origin[0] = orgVector[3*id];
    origin[1] = orgVector[3*id+1];
    origin[2] = orgVector[3*id+2];
    it->second = origin;

    it = this->TileOriginMap.find(++id);
    }
}
