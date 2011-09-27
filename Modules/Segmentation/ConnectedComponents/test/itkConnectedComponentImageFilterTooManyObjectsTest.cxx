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

#include "itkConnectedComponentImageFilter.h"
#include "itkSimpleFilterWatcher.h"

int itkConnectedComponentImageFilterTooManyObjectsTest(int itkNotUsed(argc), char*[] itkNotUsed(argv))
{

  typedef   unsigned char  PixelType;
  const     unsigned int   Dimension = 2;
  typedef itk::Image< PixelType, Dimension >  ImageType;

  // create a test input image with more objects in it than what the output type
  // can handle - 255
  ImageType::Pointer img = ImageType::New();
  ImageType::SizeType size;
  size.Fill( 512 );
  img->SetRegions( size );
  img->Allocate();
  img->FillBuffer( 0 );
  for( int x=0; x<512; x+=2 )
    {
    ImageType::IndexType idx;
    idx[0] = x;
    for( int y=0; y<512; y+=2 )
      {
      idx[1] = y;
      img->SetPixel( idx, 255 );
      }
    }

  typedef itk::ConnectedComponentImageFilter< ImageType, ImageType > FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( img );
  itk::SimpleFilterWatcher watcher(filter);

  try
    {
    filter->Update();
    // no exception - that's not normal
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "exception caught:" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_SUCCESS;
    }

  return EXIT_FAILURE;
}
