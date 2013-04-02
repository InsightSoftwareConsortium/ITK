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
#include "itkVectorNeighborhoodOperatorImageFilter.h"
#include "itkDerivativeOperator.h"
#include "itkNullImageToImageFilterDriver.hxx"

int
itkVectorNeighborhoodOperatorImageFilterTest(
int itkNotUsed(argc),
char * itkNotUsed(argv) [] )
{
  try
    {
      typedef  itk::Vector<double, 2>    VectorType;
      typedef  VectorType::ValueType     ScalarValueType;
      typedef  itk::Image<VectorType, 3> ImageType;
      // Set up operator
      itk::DerivativeOperator<ScalarValueType, 3> oper;
      oper.SetOrder(2);
      oper.SetDirection(2);
      oper.CreateDirectional();

      // Set up filter
      itk::VectorNeighborhoodOperatorImageFilter<ImageType, ImageType>::Pointer filter
        = itk::VectorNeighborhoodOperatorImageFilter<ImageType, ImageType>::New();
      filter->SetOperator(oper);

      // Run Test
      itk::Size<3> sz;
      sz[0]=128;
      sz[1]=128;
      sz[2]=2;
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
