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
#include "vtkSmartPointer.h"

int main( int argc, char *argv[] )
{

  //inputs and output are passed as command line arguments
  if (argc < 4)
    {
    std::cout << "Usage: " << std::endl;
    std::cout << argv[0] << " ConfigFileName OutputFileName RegistrationMethod"
              << std::endl;
    return EXIT_FAILURE;
    }
  std::string configFileName = argv[1];
  std::string outputFileName = argv[2];
  int registrationMethod = atoi(argv[3]);

  vtkMicroscopyTileStitcherConfig* config =
    vtkMicroscopyTileStitcherConfig::GetInstance();
  config->SetRegistrationMethod(registrationMethod);

  if (registrationMethod == vtkMicroscopyTileStitcherConfig::THRESHOLD_COMPONENT)
    {
    if (argc < 8)
      {
      std::cout << "Usage when registration method is set to 2: " << std::endl;
      std::cout << argv[0] << " ConfigFileName OutputFileName RegistrationMethod "
        << " NormalizeFlag GaussianBlurSigma LowerThresholdRatio UpperThresholdRatio"
        << std::endl;
      return EXIT_FAILURE;
      }
    int normalizeFlag = atoi(argv[4]);
    double sigma = atof(argv[5]);
    double lowerThreshRatio = atof(argv[6]);
    double upperThreshRatio = atof(argv[7]);

    config->SetStitchOrder(1);
    config->SetNormalizeFlag(normalizeFlag);
    config->SetThresholdFlag(1);
    config->SetGaussianBlurSigma(sigma);
    config->SetLowerThresholdRatio(lowerThreshRatio);
    config->SetUpperThresholdRatio(upperThreshRatio);
    }

  vtkSmartPointer<vtkMicroscopyTileStitcher> stitcher =
    vtkSmartPointer<vtkMicroscopyTileStitcher>::New();
  stitcher->SetConfigFileName(configFileName);
  stitcher->SetOutputFileName(outputFileName);
  stitcher->Update();

  return 0;
}
