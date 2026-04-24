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

// Compile-only test for the NeighborhoodIteratorBase design spike.
// Exercises:
//   * Both VIsConst=true and VIsConst=false instantiations of the unified
//     NeighborhoodIteratorBase and ShapedNeighborhoodIteratorBase templates.
//   * The std::conditional_t pointer-type plumbing (via static_assert).
//   * The non-const -> const converting ctor.
//   * Negative checks (no-compile lines documented in comments) for the
//     reverse conversion and for SetPixel on const instantiations.

#include "itkNeighborhoodIteratorBase.h"
#include "itkImage.h"

#include <type_traits>

namespace
{

using ImageType = itk::Image<float, 2>;

using MutIt = itk::NeighborhoodIterator2<ImageType>;
using ConIt = itk::ConstNeighborhoodIterator2<ImageType>;

using MutShaped = itk::ShapedNeighborhoodIterator2<ImageType>;
using ConShaped = itk::ConstShapedNeighborhoodIterator2<ImageType>;

// --- Pointer-type plumbing (the whole point of std::conditional_t) ----------
static_assert(std::is_same_v<MutIt::ImagePointer, ImageType *>,
              "Non-const iterator must hold a non-const image pointer.");
static_assert(std::is_same_v<ConIt::ImagePointer, const ImageType *>,
              "Const iterator must hold a const image pointer.");

static_assert(std::is_same_v<MutIt::InternalPixelPointer, ImageType::InternalPixelType *>,
              "Non-const iterator must hold non-const pixel pointers.");
static_assert(std::is_same_v<ConIt::InternalPixelPointer, const ImageType::InternalPixelType *>,
              "Const iterator must hold const pixel pointers.");

// --- IsConst exposure -------------------------------------------------------
static_assert(MutIt::IsConst == false);
static_assert(ConIt::IsConst == true);
static_assert(MutShaped::IsConst == false);
static_assert(ConShaped::IsConst == true);

// --- Converting ctor: non-const -> const must be available -----------------
static_assert(std::is_constructible_v<ConIt, const MutIt &>,
              "Non-const NeighborhoodIterator must be implicitly convertible to its const sibling.");
static_assert(std::is_constructible_v<ConShaped, const MutShaped &>,
              "Non-const ShapedNeighborhoodIterator must be implicitly convertible to its const sibling.");

// --- Converting ctor: const -> non-const must NOT be available -------------
// This is the structural guarantee the modernization buys us. A const
// iterator cannot be silently laundered back into a writable one.
static_assert(!std::is_constructible_v<MutIt, const ConIt &>,
              "Const NeighborhoodIterator must NOT be convertible to a non-const sibling.");
static_assert(!std::is_constructible_v<MutShaped, const ConShaped &>,
              "Const ShapedNeighborhoodIterator must NOT be convertible to a non-const sibling.");

// --- Inner-iterator constness (Shaped) -------------------------------------
static_assert(std::is_same_v<MutShaped::Iterator::OuterPointer, MutShaped *>);
static_assert(std::is_same_v<MutShaped::ConstIterator::OuterPointer, const MutShaped *>);
static_assert(std::is_same_v<ConShaped::ConstIterator::OuterPointer, const ConShaped *>);

// --- Negative tests (documented; would fail to compile if uncommented) -----
//
// 1. SetPixel on a ConstNeighborhoodIterator2 fires a static_assert with a
//    readable message:
//        ConIt c;
//        c.SetPixel(0, 1.0f);
//        // error: SetPixel() is not available on a const NeighborhoodIterator.
//
// 2. Non-const Begin() on a ConstShapedNeighborhoodIterator2 is SFINAE'd
//    away:
//        ConShaped cs;
//        auto it = cs.Begin();           // returns ConstIterator (OK)
//        ConShaped::Iterator wit = cs.Begin();  // error: no matching overload
//
// 3. Set() on a ConstIterator (Shaped inner) fires a static_assert:
//        MutShaped ms;
//        auto it = static_cast<const MutShaped &>(ms).Begin(); // -> ConstIterator
//        it.Set(1.0f);  // error: Set() requires non-const iterator
//
// 4. Implicit upcast from ConIt to MutIt is forbidden (static_assert above).

} // namespace

int
itkNeighborhoodIteratorBaseSpikeTest(int, char *[])
{
  // All assertions above are compile-time; if this TU compiled, the spike's
  // structural design is sound. Runtime body is intentionally empty.
  return 0;
}
