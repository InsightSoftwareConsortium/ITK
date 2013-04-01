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
#include "itkZeroCrossingImageFilter.h"
#include "itkNullImageToImageFilterDriver.hxx"

inline std::ostream& operator<<(std::ostream &o, const itk::Vector<float, 3> &v)
{
  o << "["<< v[0] << " " << v[1] << " " << v[2] << "]";
  return o;
}

int itkZeroCrossingImageFilterTest(int, char * [] )
{
  try
    {
      typedef itk::Image<float, 2> ImageType;

      // Set up filter
      itk::ZeroCrossingImageFilter<ImageType, ImageType>::Pointer
        filter =
        itk::ZeroCrossingImageFilter<ImageType, ImageType>::New();

      std::cout << "filter: " << filter;
      // Run Test
      itk::Size<2> sz;
      sz[0] = 100;
      sz[1] = 100;

      itk::NullImageToImageFilterDriver< ImageType, ImageType > test1;
      test1.SetImageSize(sz);
      test1.SetFilter(filter.GetPointer());
      test1.Execute();
    }
  catch(itk::ExceptionObject &err)
    {
      (&err)->Print(std::cerr);
      return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
