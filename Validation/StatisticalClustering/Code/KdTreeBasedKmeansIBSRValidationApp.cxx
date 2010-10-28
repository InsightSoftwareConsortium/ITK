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
#include "itkWin32Header.h"

#include "OptionList.h"
#include "ValidationSampleGenerator.h"
#include "ParameterTable.h"

#include "KdTreeBasedKmeansValidationApp.h"

void print_usage()
{
  std::cout << "kmeans clustering 1.0 (17. Dec. 2001)" << std::endl ;

  std::cout << "usage: KdTreeBasedKmeansIBSRValidationApp --images file"  << std::endl ;
  std::cout << "       --mask file --bucket-size int --iteration int"  << std::endl ;
  std::cout << "       --slice-offset int " << std::endl ;
  std::cout << "       --parameters file " << std::endl ;
  std::cout << "       --result file " << std::endl ;

  std::cout << "" << std::endl ;

  std::cout << "--image file" << std::endl ;
  std::cout << "        image file name with intesnity values [meta image format]"
            << std::endl ;

  std::cout << "--mask file" << std::endl ;
  std::cout << "        class label image file name that will have the class labels for pixels"
            << std::endl ;
  std::cout << "        in the target image file [meta image format]"  << std::endl ;

  std::cout << "--slice-offset int" << std::endl ;
  std::cout << "--bucket-size int" << std::endl ;
  std::cout << "--iteration int" << std::endl ;
  std::cout << "--parameters file" << std::endl ;
  std::cout << "        data file has initial parameters for each class" << std::endl ;
  std::cout << "" << std::endl ;

  std::cout << "example: KdTreeBasedKmeansIBSRValidationApp --images 20Normals_T1_brain/110_3.mhd"
            << std::endl ;
  std::cout << "         --mask 20Normals_T1_seg/110_3.mhd" << std::endl ;
  std::cout << "         --bucket-size 10 --slice-offset 0 --iteration 200" << std::endl ;
}

int main(int argc, char* argv[])
{
  namespace stat = itk::Statistics ;

  if (argc <= 1)
    {
      print_usage() ;
      exit(0) ;
    }

  KdTreeBasedKmeansValidationApp< unsigned char, 1 > app ;

  app.SetCommandLineOptions(argc, argv) ;

  std::vector< unsigned int > selectedClasses ;
  selectedClasses.push_back(128) ;
  selectedClasses.push_back(192) ;
  selectedClasses.push_back(254) ;

  app.SetSelectedClasses(selectedClasses) ;
  app.Run() ;

  return 0 ;
}

