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

#ifndef __vtkMicroscopyTileStitcherConfig_h
#define __vtkMicroscopyTileStitcherConfig_h

#include "vtkObject.h"
#include "vtkRegistrationConfigure.h"

class VTK_SimpleImageReconstruction_EXPORT vtkMicroscopyTileStitcherConfig : public vtkObject
{
public:
  vtkTypeRevisionMacro(vtkMicroscopyTileStitcherConfig,vtkObject);
  void PrintSelf(ostream& os, vtkIndent indent);
  static vtkMicroscopyTileStitcherConfig* New();

  // Description:
  // Return the vtkMicroscopyTileStitcherConfig instance
  static vtkMicroscopyTileStitcherConfig* GetInstance();

  vtkSetMacro(RegistrationMethod, int);
  vtkGetMacro(RegistrationMethod, int);

  vtkSetMacro(StitchOrder, int);
  vtkGetMacro(StitchOrder, int);

  vtkSetMacro(NormalizeFlag, int);
  vtkGetMacro(NormalizeFlag, int);

  vtkSetMacro(ThresholdFlag, int);
  vtkGetMacro(ThresholdFlag, int);

  vtkSetMacro(GaussianBlurSigma, double);
  vtkGetMacro(GaussianBlurSigma, double);

  vtkSetMacro(LowerThresholdRatio, double);
  vtkGetMacro(LowerThresholdRatio, double);

  vtkSetMacro(UpperThresholdRatio, double);
  vtkGetMacro(UpperThresholdRatio, double);

  //BTX
  enum
    {
    UNKNOWN,
    PHASE_CORRELATION,
    THRESHOLD_COMPONENT
    };
  //ETX

protected:
  vtkMicroscopyTileStitcherConfig();
  virtual ~vtkMicroscopyTileStitcherConfig();

private:
  // Singleton management functions.
  static void ClassInitialize();
  static void ClassFinalize();

  int     RegistrationMethod;
  int     StitchOrder;
  int     NormalizeFlag;
  int     ThresholdFlag;
  double  GaussianBlurSigma;
  double  LowerThresholdRatio;
  double  UpperThresholdRatio;

  //BTX
  friend class vtkMicroscopyTileStitcherConfigInitializer;
  //ETX

  vtkMicroscopyTileStitcherConfig(const vtkMicroscopyTileStitcherConfig&);  // Not implemented.
  void operator=(const vtkMicroscopyTileStitcherConfig&);  // Not implemented.
};

//----------------------------------------------------------------------------
//BTX
class VTK_SimpleImageReconstruction_EXPORT vtkMicroscopyTileStitcherConfigInitializer
{
public:
  vtkMicroscopyTileStitcherConfigInitializer();
  ~vtkMicroscopyTileStitcherConfigInitializer();
};

// This instance will show up in any translation unit that uses
// vtkMicroscopyTileStitcherConfig or that has a singleton.  It will make sure
// vtkMicroscopyTileStitcherConfig is initialized before it is used and finalized when
// it is done being used.
static vtkMicroscopyTileStitcherConfigInitializer vtkMicroscopyTileStitcherConfigInitializerInstance;
//ETX

#endif // __vtkMicroscopyTileStitcherConfig_h
