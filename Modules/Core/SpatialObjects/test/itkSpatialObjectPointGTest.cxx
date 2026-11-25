/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

// First include the header files to be tested:
#include "itkContourSpatialObjectPoint.h"
#include "itkDTITubeSpatialObjectPoint.h"
#include "itkLineSpatialObjectPoint.h"
#include "itkSpatialObjectPoint.h"
#include "itkSurfaceSpatialObjectPoint.h"
#include "itkTubeSpatialObjectPoint.h"

#include <type_traits> // For is_nothrow_move_constructible_v and is_nothrow_move_assignable_v.


// Note: we cannot assert nothrow_move_constructible, because SpatialObjectPoint internally has an `std::map`, which may
// not be nothrow_move_constructible, at least in C++17.
static_assert(std::is_move_constructible_v<itk::ContourSpatialObjectPoint<>>);
static_assert(std::is_move_constructible_v<itk::DTITubeSpatialObjectPoint<>>);
static_assert(std::is_move_constructible_v<itk::LineSpatialObjectPoint<>>);
static_assert(std::is_move_constructible_v<itk::SpatialObjectPoint<>>);
static_assert(std::is_move_constructible_v<itk::SurfaceSpatialObjectPoint<>>);
static_assert(std::is_move_constructible_v<itk::TubeSpatialObjectPoint<>>);

static_assert(std::is_nothrow_move_assignable_v<itk::ContourSpatialObjectPoint<>>);
static_assert(std::is_nothrow_move_assignable_v<itk::DTITubeSpatialObjectPoint<>>);
static_assert(std::is_nothrow_move_assignable_v<itk::LineSpatialObjectPoint<>>);
static_assert(std::is_nothrow_move_assignable_v<itk::SpatialObjectPoint<>>);
static_assert(std::is_nothrow_move_assignable_v<itk::SurfaceSpatialObjectPoint<>>);
static_assert(std::is_nothrow_move_assignable_v<itk::TubeSpatialObjectPoint<>>);
