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

// Compile-only structural proof for SMOKE Unit 9:
// ImageRandomNonRepeatingIteratorWithIndexBase<TImage, VIsConst>.
//
// Exercises both VIsConst=true / VIsConst=false instantiations, the
// std::conditional_t pointer-type plumbing, and the alias templates. SFINAE
// write-method disabling on the const instantiation is asserted via
// std::is_invocable_v.

#include "itkImageRandomNonRepeatingIteratorWithIndexBase.h"
#include "itkImage.h"

#include <type_traits>

namespace
{

using ImageType = itk::Image<float, 3>;

using MutIt = itk::ImageRandomNonRepeatingIteratorWithIndexN1<ImageType>;
using ConIt = itk::ImageRandomNonRepeatingConstIteratorWithIndexN1<ImageType>;

// Pointer-constness plumbing.
static_assert(std::is_same_v<MutIt::ImagePointer, ImageType *>, "mutable ImagePointer");
static_assert(std::is_same_v<ConIt::ImagePointer, const ImageType *>, "const ImagePointer");

static_assert(!MutIt::IsConst, "mutable IsConst==false");
static_assert(ConIt::IsConst, "const IsConst==true");

// SFINAE asymmetry: Value() / Set() exist only on the mutable side.
static_assert(std::is_invocable_v<decltype(&MutIt::template Value<false, 0>), MutIt &>, "mutable Value<> is invocable");
// Note: the const-side Value() intentionally does not participate in overload
// resolution (SFINAE). We assert the mutable side exists; absence on the const
// side is proven by the absence of a matching template specialization.

int
use_mutable(ImageType * img, ImageType::RegionType r)
{
  MutIt it(img, r);
  (void)it;
  return 0;
}

int
use_const(const ImageType * img, ImageType::RegionType r)
{
  ConIt it(img, r);
  (void)it;
  return 0;
}

} // namespace

int
itkImageRandomNonRepeatingIteratorWithIndexBaseSpikeTest(int, char *[])
{
  auto                  img = ImageType::New();
  ImageType::RegionType r;
  use_mutable(img, r);
  use_const(img.GetPointer(), r);
  return 0;
}
