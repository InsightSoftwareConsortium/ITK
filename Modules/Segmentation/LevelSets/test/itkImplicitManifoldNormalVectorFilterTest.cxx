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

#include <iostream>
#include "itkSparseFieldFourthOrderLevelSetImageFilter.h"
#include "itkImplicitManifoldNormalVectorFilter.h"
#include "itkNormalVectorDiffusionFunction.h"

int itkImplicitManifoldNormalVectorFilterTest(int, char* [] )
{
  using InputImageType = itk::Image  <float, 2>;
  using NodeType = itk::NormalBandNode <InputImageType>;
  using OutputImageType = itk::SparseImage <NodeType, 2>;
  using FilterType = itk::ImplicitManifoldNormalVectorFilter<InputImageType,
    OutputImageType>;
  using FunctionType = itk::NormalVectorDiffusionFunction<OutputImageType>;

  InputImageType::Pointer im_init = InputImageType::New();
  InputImageType::RegionType r;
  InputImageType::SizeType   sz = {{50, 50}};
  InputImageType::IndexType  idx = {{0,0}};
  r.SetSize(sz);
  r.SetIndex(idx);
  im_init->SetLargestPossibleRegion(r);
  im_init->SetBufferedRegion(r);
  im_init->SetRequestedRegion(r);
  im_init->Allocate();

  InputImageType::IndexType index;
  for ( index[0]=0; index[0] < 50; index[0]++ )
    for ( index[1]=0; index[1] < 50; index[1]++ )
      {
      im_init->SetPixel (index, static_cast<float>(index[0]));
      }

  FilterType::Pointer filter = FilterType::New();
  FunctionType::Pointer function = FunctionType::New();
  filter->SetInput(im_init);
  filter->SetNormalFunction(function);
  filter->SetIsoLevelLow (15.0);
  filter->SetIsoLevelHigh (35.0);
  filter->SetMaxIteration (100);
  filter->SetMinVectorNorm (0.001);

  std::cout<<"Max iteration = "<<(filter->GetMaxIteration())<<"\n";
  std::cout<<"IsoLevelLow = "<<(filter->GetIsoLevelLow())<<"\n";
  std::cout<<"IsoLevelHigh = "<<(filter->GetIsoLevelHigh())<<"\n";
  std::cout<<"MinVectorNorm = "<<(filter->GetMinVectorNorm())<<"\n";
  std::cout<<"UnsharpMaskingFlag = "<<(filter->GetUnsharpMaskingFlag())<<"\n";
  std::cout<<"UnsharpMaskingWeight = "
           <<(filter->GetUnsharpMaskingWeight())<<"\n";
  std::cout<<"Precomputeflag = "<<(filter->GetPrecomputeFlag())<<"\n";

  filter->Print(std::cout);
  function->Print(std::cout);
  try {
  filter->Update();
  }
  catch (itk::ExceptionObject &e)
    {
      std::cerr << e << std::endl;
    }

  return EXIT_SUCCESS;
}
