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

#include "vtkMicroscopyTileStitcherConfig.h"
#include "vtkObjectFactory.h"

vtkCxxRevisionMacro(vtkMicroscopyTileStitcherConfig, "$Revision$");
vtkStandardNewMacro(vtkMicroscopyTileStitcherConfig);

//----------------------------------------------------------------------------
// This MUST be default initialized to zero by the compiler and is
// therefore not initialized here.  The ClassInitialize and
// ClassFinalize methods handle this instance.
static vtkMicroscopyTileStitcherConfig* vtkMicroscopyTileStitcherConfigInstance;

//----------------------------------------------------------------------------
// Must NOT be initialized.  Default initialization to zero is
// necessary.
unsigned int vtkMicroscopyTileStitcherConfigCount;

//----------------------------------------------------------------------------
vtkMicroscopyTileStitcherConfigInitializer::vtkMicroscopyTileStitcherConfigInitializer()
{
  if(++vtkMicroscopyTileStitcherConfigCount == 1)
    {
    vtkMicroscopyTileStitcherConfig::ClassInitialize();
    }
}

//----------------------------------------------------------------------------
vtkMicroscopyTileStitcherConfigInitializer::~vtkMicroscopyTileStitcherConfigInitializer()
{
  if(--vtkMicroscopyTileStitcherConfigCount == 0)
    {
    vtkMicroscopyTileStitcherConfig::ClassFinalize();
    }
}

//----------------------------------------------------------------------------
vtkMicroscopyTileStitcherConfig::vtkMicroscopyTileStitcherConfig()
{
  this->RegistrationMethod = 0;
  this->StitchOrder = 0;
  this->NormalizeFlag = 0;
  this->ThresholdFlag = 0;
  this->LowerThresholdRatio = 0.0;
  this->UpperThresholdRatio = 1.0;
  this->GaussianBlurSigma = 0.0;
}

//----------------------------------------------------------------------------
vtkMicroscopyTileStitcherConfig::~vtkMicroscopyTileStitcherConfig()
{
}

//----------------------------------------------------------------------------
vtkMicroscopyTileStitcherConfig* vtkMicroscopyTileStitcherConfig::GetInstance()
{
  return vtkMicroscopyTileStitcherConfigInstance;
}


//----------------------------------------------------------------------------
void vtkMicroscopyTileStitcherConfig::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);
}

//----------------------------------------------------------------------------
void vtkMicroscopyTileStitcherConfig::ClassInitialize()
{
  // Allocate the singleton
  vtkMicroscopyTileStitcherConfigInstance = vtkMicroscopyTileStitcherConfig::New();
}

//----------------------------------------------------------------------------
void vtkMicroscopyTileStitcherConfig::ClassFinalize()
{
  // We are done with the singleton.  Delete it and reset the pointer.
  vtkMicroscopyTileStitcherConfigInstance->Delete();
  vtkMicroscopyTileStitcherConfigInstance = 0;
}

