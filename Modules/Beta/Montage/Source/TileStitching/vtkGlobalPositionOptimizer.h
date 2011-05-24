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

#ifndef __vtkGlobalPositionOptimizer_h
#define __vtkGlobalPositionOptimizer_h

#include <vtkProcessObject.h>
#include "vtkRegistrationConfigure.h"

#include <map>
#include <vector>

class VTK_SimpleImageReconstruction_EXPORT vtkGlobalPositionOptimizer : public vtkProcessObject
{
public:
  vtkTypeRevisionMacro(vtkGlobalPositionOptimizer, vtkProcessObject);

  // Description:
  // Prints information about the state of the filter
  void PrintSelf(ostream& os, vtkIndent indent);

  // Description:
  // Creates a new instance
  static vtkGlobalPositionOptimizer *New();

  // Description:
  // Get the optimized position parameters of the image tiles.
  const std::vector<double> & GetOptimizedPositionParameters();

  // Description:
  // Set the initial position parameter of the image tiles.
  void SetInitialPositionParameters(const std::vector<double>& parameters);

  // Description:
  // Set the initial offset parameters (computed from regitration) between
  // adjacent image tiles.
  void SetInitialOffsetParameters(const std::vector<double>& parameters);

  // Description:
  // Set the map from offset id to position id pair. An offset is computed
  // from a pair of adjacent tiles.
  void SetOffsetIdToPositionIdsMap(const std::map<int, std::vector<int> >& idMap);

  // Description:
  // Return a string describing the end condition of the optimizer
  std::string GetOptimizerEndCondition(int code);

  // Descriptio:
  // Execution
  virtual void Update();

  vtkSetVectorMacro(GridSize, int, 3);
  vtkGetVectorMacro(GridSize, int, 3);

  vtkSetMacro(DebugFlag, bool);
  vtkGetMacro(DebugFlag, bool);

protected:
  vtkGlobalPositionOptimizer();
  ~vtkGlobalPositionOptimizer();

  // compute the number of positions (tiles) from grid size
  unsigned int GetNumberOfPositions();

  // compute the number of offsets (tile pairs) from grid size
  unsigned int GetNumberOfOffsets();

  // get the number of valid dimensions (size > 1)
  unsigned int GetNumberOfValidDimensions();

private:
  // the parameters describing the origin of tiles
  std::vector<double> PositionParameters;

  // the parameters describing the offsets between adjacent tile pairs
  std::vector<double> OffsetParameters;

  // the map from offset id to adjacent tile id pair.
  std::map<int, std::vector<int> > OffsetIdToPositionIdsMap;

  int GridSize[3];

  bool DebugFlag;

  vtkGlobalPositionOptimizer
    (const vtkGlobalPositionOptimizer&); // Not Implemented
  void operator=
    (const vtkGlobalPositionOptimizer&); // Not Implemented
};

#endif
