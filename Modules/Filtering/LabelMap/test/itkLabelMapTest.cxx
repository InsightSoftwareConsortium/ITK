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
#include "itkLabelMap.h"
#include "itkLabelObject.h"

int itkLabelMapTest(int argc, char * argv[])
{

  if( argc != 1 )
    {
    std::cerr << "usage: " << argv[0] << "" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 3;

  typedef itk::LabelObject< unsigned long, dim > LabelObjectType;
  typedef LabelObjectType::IndexType             IndexType;
  typedef itk::LabelMap< LabelObjectType >       LabelMapType;
  typedef LabelMapType::RegionType               RegionType;
  typedef LabelMapType::SizeType                 SizeType;
  typedef LabelMapType::LabelObjectVectorType    LabelObjectVectorType;
  typedef LabelMapType::LabelVectorType          LabelVectorType;

  LabelMapType::Pointer map = LabelMapType::New();

  SizeType sizeIn;
  sizeIn[0] = 10;
  sizeIn[1] = 20;
  sizeIn[2] = 30;
  map->SetRegions( sizeIn );
  map->Allocate();

  RegionType regionIn;
  regionIn.SetSize(sizeIn);

  RegionType regionOut;
  regionOut = map->GetRequestedRegion();
  itkAssertOrThrowMacro ( (regionOut == regionIn), "SetRegions (size) failed");
  map->Initialize();

  IndexType index;
  index[0] = 1;
  index[1] = 3;
  index[2] = 5;

  sizeIn[0] = 100;
  sizeIn[1] = 200;
  sizeIn[2] = 300;

  regionIn.SetIndex(index);
  regionIn.SetSize(sizeIn);
  map->SetRegions( regionIn );
  map->Allocate();

  regionOut = map->GetRequestedRegion();
  itkAssertOrThrowMacro ( (regionOut == regionIn), "SetRegions (region) failed");

  LabelMapType::Pointer mapGraft = LabelMapType::New();
  mapGraft->Graft( map );
  regionOut = mapGraft->GetRequestedRegion();
  itkAssertOrThrowMacro ( (regionOut == regionIn), "Graft failed");

  mapGraft->SetBackgroundValue(255);
  itkAssertOrThrowMacro( (mapGraft->GetBackgroundValue() == 255), "Set/GetBackground failed.");

  LabelObjectType::Pointer lo = LabelObjectType::New();
  lo->SetLabel(1);

  IndexType idx;
  idx[0] = 1;
  idx[1] = 20;
  idx[2] = 30;
  lo->AddLine( idx, 10 );

  idx[0] = 5;
  lo->AddLine( idx, 10 );

  idx[0] = 5;
  idx[1] = 0;
  idx[2] = 0;
  lo->AddLine( idx, 10 );

  idx[0] = 5;
  idx[1] = 0;
  idx[2] = 1;
  lo->AddLine( idx, 10 );

  idx[0] = 5;
  idx[1] = 1;
  idx[2] = 0;
  lo->AddLine( idx, 10 );

  idx[0] = 0;
  idx[1] = 1;
  idx[2] = 1;
  lo->AddLine( idx, 10 );

  idx[0] = 10;
  idx[1] = 1;
  idx[2] = 1;
  lo->AddLine( idx, 1 );

  LabelObjectType::Pointer loCheck;

  map->AddLabelObject(lo);
  loCheck = map->GetLabelObject(idx);
  itkAssertOrThrowMacro ( (loCheck == lo), "Add/GetLabelObject failed");

  LabelObjectVectorType loVector;
  loVector = map->GetLabelObjects();
  itkAssertOrThrowMacro ( (loVector[0] == lo), "GetLabelObjects failed");

  LabelVectorType loLabels;
  loLabels = map->GetLabels();
  itkAssertOrThrowMacro ( (loLabels[0] == 1), "GetLabels failed");

  LabelObjectType::Pointer lo2 = LabelObjectType::New();

  idx[0] = 1;
  idx[1] = 21;
  idx[2] = 30;
  lo2->AddLine( idx, 10 );


  map->PushLabelObject(lo2);
  loCheck = map->GetLabelObject(idx);
  itkAssertOrThrowMacro ( (loCheck == lo2), "Push/GetLabelObject failed");

  idx[0] = 1;
  idx[1] = 21;
  idx[2] = 31;
  map->SetPixel( idx, 3 );

  idx[0] = 1;
  idx[1] = 21;
  idx[2] = 32;
  map->SetLine( idx, 11, 3);

  idx[0] = 2;
  idx[1] = 21;
  idx[2] = 32;
  itkAssertOrThrowMacro ( (map->GetPixel(idx) == 3), "SetPixel/SetLine/GetPixel failed");

  itkAssertOrThrowMacro ( (map->GetNumberOfLabelObjects() == 3), "GetNumberOfLabelObjects failed");
  itkAssertOrThrowMacro ( map->HasLabel(1), "HasLabel failed");

  loCheck = map->GetNthLabelObject(1);
  itkAssertOrThrowMacro ( (loCheck == lo2), "GetNthLabelObject failed");

  itkAssertOrThrowMacro ( (map->GetPixel(idx) == 3), "SetPixel/SetLine/GetPixel failed");

  map->Print(std::cout);

  map->PrintLabelObjects();

  map->Optimize();

  map->RemoveLabelObject( lo );
  map->RemoveLabel(2);
  itkAssertOrThrowMacro ( (map->GetNumberOfLabelObjects() == 1), "Remove LabelObject/Label failed");
  map->ClearLabels();
  itkAssertOrThrowMacro ( (map->GetNumberOfLabelObjects() == 0), "ClearLabels failed");

  return EXIT_SUCCESS;
}
