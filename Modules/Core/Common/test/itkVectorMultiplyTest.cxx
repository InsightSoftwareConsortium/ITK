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
#include "itkVectorImage.h"
#include "itkMultiplyImageFilter.h"

int itkVectorMultiplyTest(int , char * [])
{
  typedef itk::VectorImage<double,3> VecImageType;
  typedef itk::MultiplyImageFilter<VecImageType,VecImageType,VecImageType> MultFilterType;

  VecImageType::SizeType size;
  size[0] = size[1] = size[2] = 2;

  VecImageType::Pointer images[2];
  MultFilterType::Pointer mult = MultFilterType::New();
  for(unsigned int i = 0; i < 2; ++i)
    {
    images[i] = VecImageType::New();
    images[i]->SetRegions(size);
    images[i]->SetVectorLength(3);
    images[i]->Allocate();
    VecImageType::PixelType pix;
    pix[0] = pix[1] = pix[2]= i+1;
    images[i]->FillBuffer(pix);
    mult->SetInput(i,images[i]);
    }
  mult->Update();
  VecImageType::Pointer result = mult->GetOutput();
}
