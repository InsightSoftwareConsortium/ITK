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

#ifndef __vtkMicroscopyTileStitcher_h
#define __vtkMicroscopyTileStitcher_h

#include <vtkAlgorithm.h>
#include <vtkSmartPointer.h>
#include <vtkStdString.h>
#include "vtkMicroscopyImageType.h"
#include "vtkRegistrationConfigure.h"

class vtkMicroscopyTileConfigParser;

class VTK_SimpleImageReconstruction_EXPORT vtkMicroscopyTileStitcher : public vtkAlgorithm
{
public:
  static vtkMicroscopyTileStitcher* New();
  vtkTypeRevisionMacro(vtkMicroscopyTileStitcher, vtkAlgorithm);

  void SetConfigFileName(const vtkStdString & filename);

  vtkSetMacro(OutputFileName, vtkStdString);
  vtkGetMacro(OutputFileName, vtkStdString);

  virtual void Update();

protected:
  vtkMicroscopyTileStitcher();
  ~vtkMicroscopyTileStitcher();

  void GlobalOptimization();

private:
  vtkMicroscopyTileStitcher(const vtkMicroscopyTileStitcher&);   // Not implemented.
  void operator=(const vtkMicroscopyTileStitcher&);  // Not implemented.

  vtkSmartPointer<vtkMicroscopyTileConfigParser>       Parser;
  vtkStdString                                         OutputFileName;
};

#endif
