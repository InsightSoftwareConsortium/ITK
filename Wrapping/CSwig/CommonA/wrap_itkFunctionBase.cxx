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
#include "itkImage.h"
#include "itkFunctionBase.h"
#include "itkArray.h"
#include "itkContinuousIndex.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkFunctionBase);
  namespace wrappers
  {

    ITK_WRAP_OBJECT2(FunctionBase, image::F2, double, itkFunctionBaseIF2D);
    ITK_WRAP_OBJECT2(FunctionBase, image::F3, double, itkFunctionBaseIF3D);
    ITK_WRAP_OBJECT2(FunctionBase, image::US2, double, itkFunctionBaseIUS2D);
    ITK_WRAP_OBJECT2(FunctionBase, image::US3, double, itkFunctionBaseIUS3D);
    ITK_WRAP_OBJECT2(FunctionBase, double, double, itkFunctionBaseDD);

    namespace point
    {
    typedef ::itk::Point< float,  2 > F2;
    typedef ::itk::Point< float,  3 > F3;
    typedef ::itk::Point< double, 2 > D2;
    typedef ::itk::Point< double, 3 > D3;
    }
    // wrap FunctionBase
    ITK_WRAP_OBJECT2(FunctionBase, point::F2, double, itkFunctionBasePF2D);
    ITK_WRAP_OBJECT2(FunctionBase, point::F3, double, itkFunctionBasePF3D);
    ITK_WRAP_OBJECT2(FunctionBase, point::D2, double, itkFunctionBasePD2D);
    ITK_WRAP_OBJECT2(FunctionBase, point::D3, double, itkFunctionBasePD3D);


    // the following types are needed for the BSplineInterpolationWeightFunction
    namespace continuousIndex
    {
    typedef ::itk::ContinuousIndex< float,  2 > F2;
    typedef ::itk::ContinuousIndex< float,  3 > F3;
    typedef ::itk::ContinuousIndex< double, 2 > D2;
    typedef ::itk::ContinuousIndex< double, 3 > D3;
    }

    namespace array
    {
    typedef ::itk::Array< double > D;
    typedef ::itk::Array< float  > F;
    }

    ITK_WRAP_OBJECT2(FunctionBase, continuousIndex::F2, array::D, itkFunctionBaseCIF2AD);
    ITK_WRAP_OBJECT2(FunctionBase, continuousIndex::F3, array::D, itkFunctionBaseCIPF3AD);
    ITK_WRAP_OBJECT2(FunctionBase, continuousIndex::D2, array::D, itkFunctionBaseCIPD2AD);
    ITK_WRAP_OBJECT2(FunctionBase, continuousIndex::D3, array::D, itkFunctionBaseCIPD3AD);

  }
}

#endif
