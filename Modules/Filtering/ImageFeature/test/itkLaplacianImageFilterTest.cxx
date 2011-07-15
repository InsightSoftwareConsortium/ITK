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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include "itkImage.h"
#include <iostream>
#include "itkLaplacianImageFilter.h"
#include "itkNullImageToImageFilterDriver.hxx"
#include "itkVector.h"
#include "itkFilterWatcher.h"

inline std::ostream& operator<<(std::ostream &o, const itk::Vector<float, 3> &v)
{
  o << "["<< v[0] << " " << v[1] << " " << v[2] << "]";
  return o;
}

int itkLaplacianImageFilterTest(int , char * [] )
{
  try
    {
    typedef itk::Image<float, 2> ImageType;

      // Set up filter
      itk::LaplacianImageFilter<ImageType, ImageType>::Pointer
        filter =
        itk::LaplacianImageFilter<ImageType, ImageType>::New();

      FilterWatcher watch(filter);

      // Run Test
      itk::Size<2> sz;
      sz[0] = 100 ; //atoi(argv[1]);
      sz[1] = 100 ; // atoi(argv[2]);
      //      sz[2] = 10;//atoi(argv[3]);
      //      sz[3] = 5;//atoi(argv[4]);
      itk::NullImageToImageFilterDriver< ImageType, ImageType > test1;
      test1.SetImageSize(sz);
      test1.SetFilter(filter.GetPointer());
      test1.Execute();

      // verify the fix for Bug: 788
      // The following code should throw an exception and not crash.
      filter->SetInput(NULL);
      bool exceptionSeen = false;
      try
        {
        filter->Update();
        }
      catch(itk::ExceptionObject &err)
        {
        exceptionSeen = true;
        std::cout << "Expected exception: " << std::endl;
        std::cout << err << std::endl;
        std::cout << " was received OK" << std::endl;
        }
      if( !exceptionSeen )
        {
        std::cerr << "Expected exception was not thrown" << std::endl;
        return EXIT_FAILURE;
        }
    }
  catch(itk::ExceptionObject &err)
    {
      (&err)->Print(std::cerr);
      return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
