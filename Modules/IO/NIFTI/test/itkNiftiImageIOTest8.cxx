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

int itkNiftiImageIOTest8(int ac, char *av[])
{
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

  //only 3,4,5,6 as supported in itkImageIOBase
  success |= TestImageOfSymMats<itk::SymmetricSecondRankTensor<float,3>,1>(std::string("testSymImage_float_2_1.nii.gz"));
  success |= TestImageOfSymMats<itk::SymmetricSecondRankTensor<float,3>,2>(std::string("testSymImage_float_2_2.nii.gz"));
  success |= TestImageOfSymMats<itk::SymmetricSecondRankTensor<float,3>,3>(std::string("testSymImage_float_2_3.nii.gz"));
  success |= TestImageOfSymMats<itk::SymmetricSecondRankTensor<float,3>,4>(std::string("testSymImage_float_2_4.nii.gz"));

//only test some of the others
  success |= TestImageOfSymMats<itk::SymmetricSecondRankTensor<float,4>,3>(std::string("testSymImage_float_3_3.nii.gz"));
  success |= TestImageOfSymMats<itk::SymmetricSecondRankTensor<float,5>,3>(std::string("testSymImage_float_3_3.nii.gz"));
  success |= TestImageOfSymMats<itk::SymmetricSecondRankTensor<float,6>,3>(std::string("testSymImage_float_3_3.nii.gz"));

  return success;
}
