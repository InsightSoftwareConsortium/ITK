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

#include "itkNiftiImageIOTest.h"

/** Test writing and reading a Vector Image
 */
int itkNiftiImageIOTest7(int ac, char* av[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if(ac > 1)
    {
    char *testdir = *++av;
    itksys::SystemTools::ChangeDirectory(testdir);
    }
  else
    {
    return EXIT_FAILURE;
    }
  int success(0);
  success |= TestImageOfSymMats<itk::DiffusionTensor3D<float>,1>( std::string("testDtiImage_float_1.nii.gz"));
  success |= TestImageOfSymMats<itk::DiffusionTensor3D<float>,2>( std::string("testDtiImage_float_2.nii.gz"));
  success |= TestImageOfSymMats<itk::DiffusionTensor3D<float>,3>( std::string("testDtiImage_float_3.nii.gz"));
  success |= TestImageOfSymMats<itk::DiffusionTensor3D<float>,4>( std::string("testDtiImage_float_4.nii.gz"));
  success |= TestImageOfSymMats<itk::DiffusionTensor3D<double>,3>(std::string("testDtiImage_double_3.nii.gz"));
  return success;
}
