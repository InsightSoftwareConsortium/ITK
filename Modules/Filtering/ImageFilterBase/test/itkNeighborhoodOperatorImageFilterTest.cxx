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
#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkDerivativeOperator.h"
#include "itkNullImageToImageFilterDriver.hxx"

int itkNeighborhoodOperatorImageFilterTest(int , char *[] )
{
  try
    {
      typedef  itk::Image<float, 3> ImageType;
      // Set up operator
      itk::DerivativeOperator<float, 3> oper;
      oper.SetOrder(2);
      oper.SetDirection(2);
      oper.CreateDirectional();

      // Set up filter
      itk::NeighborhoodOperatorImageFilter<ImageType, ImageType>::Pointer filter
        = itk::NeighborhoodOperatorImageFilter<ImageType, ImageType>::New();
      filter->SetOperator(oper);

      // Run Test
      itk::Size<3> sz;
      sz[0]=257;
      sz[1]=252;
      sz[2]=5;
      itk::NullImageToImageFilterDriver< ImageType, ImageType >  test1;
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
