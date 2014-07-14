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

int itkLabelMapTest2(int argc, char * argv[])
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

  LabelMapType::Pointer map = LabelMapType::New();

  SizeType sizeIn;
  sizeIn[0] = 10;
  sizeIn[1] = 20;
  sizeIn[2] = 30;
  map->SetRegions( sizeIn );
  map->Allocate();

  RegionType regionIn;
  regionIn.SetSize(sizeIn);

  RegionType regionOut = map->GetRequestedRegion();
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

  map->SetBackgroundValue(255);

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

  map->AddLabelObject(lo);

  LabelObjectType::Pointer lo2 = LabelObjectType::New();

  idx[0] = 1;
  idx[1] = 21;
  idx[2] = 30;
  lo2->AddLine( idx, 10 );

  map->PushLabelObject(lo2);

  map->SetPixel( idx, 255 );
  itkAssertOrThrowMacro ( (map->GetPixel(idx) == 255), "SetPixel( idx, 255 ) - Failed 1");
  itkAssertOrThrowMacro ( (map->GetNumberOfLabelObjects() == 2), "SetPixel( idx, 255 ) - Failed 1");

  idx[0] = 11;
  idx[1] = 21;
  idx[2] = 30;
  map->SetPixel( idx, 255 );
  itkAssertOrThrowMacro ( (map->GetPixel(idx) == 255), "SetPixel( idx, 255 ) - Failed 2");
  itkAssertOrThrowMacro ( (map->GetNumberOfLabelObjects() == 2), "SetPixel( idx, 255 ) - Failed 2");

  idx[0] = 10;
  idx[1] = 21;
  idx[2] = 30;
  map->SetPixel( idx, 255 );
  itkAssertOrThrowMacro ( (map->GetPixel(idx) == 255), "SetPixel( idx, 255 ) - Failed 3");
  itkAssertOrThrowMacro ( (map->GetNumberOfLabelObjects() == 2), "SetPixel( idx, 255 ) - Failed 3");

  idx[0] = 5;
  idx[1] = 21;
  idx[2] = 30;
  map->SetPixel( idx, 1 );
  itkAssertOrThrowMacro ( (map->GetPixel(idx) == 1), "SetPixel( idx, 1 ) failed");
  itkAssertOrThrowMacro ( (map->GetNumberOfLabelObjects() == 2), "SetPixel( idx, 1 ) failed");

  map->SetPixel( idx, 255 );
  itkAssertOrThrowMacro ( (map->GetPixel(idx) == 255), "SetPixel( idx, 255 ) - Failed 4");
  itkAssertOrThrowMacro ( (map->GetNumberOfLabelObjects() == 2), "SetPixel( idx, 1 ) - Failed 4");

  map->SetPixel( idx, 255 );
  itkAssertOrThrowMacro ( (map->GetPixel(idx) == 255), "SetPixel( idx, 255 ) - Failed 5");
  itkAssertOrThrowMacro ( (map->GetNumberOfLabelObjects() == 2), "SetPixel( idx, 1 ) - Failed 5");

  map->SetPixel( idx, 3 );
  itkAssertOrThrowMacro ( (map->GetPixel(idx) == 3), "SetPixel( idx, 3 ) failed");
  itkAssertOrThrowMacro ( (map->GetNumberOfLabelObjects() == 3), "SetPixel( idx, 1 ) failed");

  map->SetPixel( idx, 2 );
  itkAssertOrThrowMacro ( (map->GetPixel(idx) == 2), "SetPixel( idx, 2 ) failed");
  itkAssertOrThrowMacro ( (map->GetNumberOfLabelObjects() == 2), "SetPixel( idx, 2 ) failed");

  idx[0] = 0;
  idx[1] = 21;
  idx[2] = 30;

  for( int k = 0; k < 15; k++, idx[0]++ )
    {
    map->SetPixel( idx, 255 );
    }

  itkAssertOrThrowMacro ( (map->GetNumberOfLabelObjects() == 1), "Remove Label Object 2");

  map->Optimize();

  map->ClearLabels();

  return EXIT_SUCCESS;
}
