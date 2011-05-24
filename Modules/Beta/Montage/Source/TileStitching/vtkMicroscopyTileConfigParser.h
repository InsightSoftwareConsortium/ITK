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

#ifndef __vtkMicroscopyTileConfigParser_h
#define __vtkMicroscopyTileConfigParser_h

#include <vtkAlgorithm.h>
#include "vtkMicroscopyImageType.h"
#include "vtkRegistrationConfigure.h"

#include <vector>
#include <map>

class VTK_SimpleImageReconstruction_EXPORT vtkMicroscopyTileConfigParser : public vtkAlgorithm
{
public:
  static vtkMicroscopyTileConfigParser* New();
  vtkTypeRevisionMacro(vtkMicroscopyTileConfigParser,vtkAlgorithm);

  vtkGetMacro(ImageComponentType, vtkMicroscopy::ComponentType);

  vtkGetMacro(ImagePixelType, vtkMicroscopy::PixelType);

  vtkGetMacro(ImageDimension, int);

  vtkGetVectorMacro(ImageSpacing, double, 3);

  vtkGetVectorMacro(ImageSize, int, 3);

  vtkGetMacro(GridDimension, int);

  vtkGetVectorMacro(GridSize, int, 3);

  void GetRecordedTileOrigin(int tileId[3], double origin[3]);
  void GetRecordedCurrentTileOrigin(double origin[3]);
  void GetTileOrigin(int tileId[3], double origin[3]);
  void GetCurrentTileOrigin(double origin[3]);
  void SetCurrentTileOrigin(double origin[3]);

  void GetCurrentTileId(int tileId[3]);
  std::vector<std::vector<int> > GetPreviousNeighborTileIds();

  void RecordCurrentTilePairOffset(int movingTileId[3], int fixedTileId[3], double offset[3]);

  // return single-item vector for 2D tiles and multi-item vector for 3D tiles.
  std::vector<std::string> GetCurrentTileImageFileNames();
  std::vector<std::string> GetTileImageFileNames(int tileId[3]);

  void InitTraverse();
  int MoveToNextTile();

  virtual void Update();
  bool ParsedSuccessfully();

  void SetConfigFileName(const char* filename);

  vtkGetMacro(ScanOrder, int);
  vtkSetMacro(ScanOrder, int);

  const std::map<int, std::vector<int> > &   GetOffsetIdToTileIdsMap();
  const std::vector<double> &                GetOffsetVector();

  const std::vector<double> &   GetOriginVector();
  void SetOriginVector(const std::vector<double> & orgVector);

protected:

  vtkMicroscopyTileConfigParser();
  ~vtkMicroscopyTileConfigParser();

  void Get3DTileId(int tileId1, int tileId3[3]);
  int Get1DTileId(int tileId[3]);
  void GetScanOrder(int order[3]);
  bool IsCurrentTile(int tileId[3]);
  void CheckImageComponentType(std::string & filename);
  void SetTileOrigin(int tileId[3], double origin[3]);
  void MoveToTile(int tileId[3]);

private:
  vtkMicroscopyTileConfigParser(const vtkMicroscopyTileConfigParser&);   // Not implemented.
  void operator=(const vtkMicroscopyTileConfigParser&);  // Not implemented.

  vtkMicroscopy::PixelType             ImagePixelType;
  vtkMicroscopy::ComponentType         ImageComponentType;
  int                                  GridDimension;
  int                                  GridSize[3];
  int                                  ImageDimension;
  std::string                          InputPath;
  std::string                          ConfigFileName;
  double                               ImageSpacing[3];
  double                               ImageOrigin[3];
  int                                  ImageSize[3];
  int                                  ScanOrder;
  int                                  CurrentTileId[3];
  bool                                 SuccessFlag;

  std::map<int, std::vector<std::string> >  ImageFileNameMap;
  std::map<int, std::vector<double> >       RecordedTileOriginMap;
  std::map<int, std::vector<double> >       TileOriginMap;
  std::map<int, std::vector<int> >          TileId1DTo3DMap;
  std::map<std::vector<int>, int>           TileId3DTo1DMap;
  std::map<int, std::vector<int> >          OffsetIdToTileIdsMap;
  std::vector<double>                       OffsetVector;
  std::vector<double>                       OriginVector;
};

#endif
