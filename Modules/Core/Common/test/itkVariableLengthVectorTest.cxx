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
#include "itkVariableLengthVector.h"

#define ASSERT(cond, text) \
  if (!(cond))             \
    std::cout << __FILE__ << ":" << __LINE__ << ":" << "Assertion failed: " << #cond << ": " << (text) << std::endl;

int itkVariableLengthVectorTest(int, char*[])
{
  typedef itk::VariableLengthVector<float>   FloatVariableLengthVectorType;
  typedef itk::VariableLengthVector<double>  DoubleVariableLengthVectorType;

  FloatVariableLengthVectorType f( 3 );
  f[0]=1.0; f[1] = 2.0; f[2] = 3.0;
  DoubleVariableLengthVectorType g( 3 );
  g[0]=4.0; g[1] = 5.0; g[2] = 6.0;
  FloatVariableLengthVectorType h;
  h  = g + f;
  g  = h++;
  h -= 1.1;
  h *= 2.0;
  h /= 2.0;
  h += g;
  h -= g;
  h  = g - h;
  h  = -h;

  std::cout << h << std::endl;  // should be [-1.1 -1.1 -1.1]

  h = ( FloatVariableLengthVectorType ) g;
  if( h!= static_cast< FloatVariableLengthVectorType >( g ) )
    {
    std::cerr << "Casts: [FAILED]" << std::endl;
    }

  {
  double *d = new double[3];
  d[0] = 0.1; d[1] = 0.2; d[2] = 0.3;
    {
    DoubleVariableLengthVectorType x( d, 3, false );
    }
    {
    DoubleVariableLengthVectorType x( d, 3, false );
    if( (d[0] != 0.1) || (x[0] != 0.1) )
      {
      std::cerr << "Memory management(1): [FAILED]" << std::endl;
      }
    std::cout << x << std::endl;
    x.SetSize( 5 , false);
    x[3] = 3.0;
    x[4] = 4.0;
    std::cout << d[0] << "->" << x << std::endl;
    if( (d[0] != 0.1) || (x[0] != 0.1) ) // increase length but preserve existing data
      {
      std::cerr << "Memory management(2): [FAILED]" << std::endl;
      }
    x.SetSize( 2 , false); // reduce length but preserve existing data
    std::cout << x << std::endl;
    if( (x.GetSize() != 2) || (d[0] != 0.1) || (x[0] != 0.1) )
      {
      std::cerr << "Memory management(3): [FAILED]" << std::endl;
      }
    x.SetSize( 5 , true); // increase size, destroy data.
    x.SetSize( 7 , true); // increase size, destroy data.
    x.SetSize( 6 , true); // decrease size, destroy data.
    }

  // Tests for SetSize(size, allocation policy, values keeping policy)
    {
    DoubleVariableLengthVectorType ref( d, 3, false );
    ASSERT( ref.IsAProxy(), "Unexpected Reference VLV value")
    ASSERT( (ref[0] == 0.1) && (d[0] == 0.1), "Unexpected Reference VLV value")

    DoubleVariableLengthVectorType x( d, 3, false );
    ASSERT( x.IsAProxy(), "Unexpected VLV value")
    ASSERT( (x[0] == 0.1) && (x[0] == 0.1), "Unexpected VLV value")

    // ===[ Keep old values
    // ---[ Shrink To Fit
    x.SetSize(5, DoubleVariableLengthVectorType::ShrinkToFit(), DoubleVariableLengthVectorType::KeepOldValues());
    ASSERT( ! x.IsAProxy(), "After resizing a proxy, it shall not be a proxy anymore")
    ASSERT( ref[0]==x[0] && ref[1]==x[1] && ref[2]==x[2], "Old Values shall have been kept")
    x[3] = 3.0;
    x[4] = 4.0;
    double * start = &x[0];

    x.SetSize(3, DoubleVariableLengthVectorType::ShrinkToFit(), DoubleVariableLengthVectorType::KeepOldValues());
    ASSERT( ! x.IsAProxy(), "After resizing, it shall never be a proxy");
    ASSERT( x == ref, "Values haven't been preserved");
    ASSERT(&x[0] != start, "ShrinkToFit shall induce a resizing");
    start = &x[0];
    x.SetSize(3, DoubleVariableLengthVectorType::ShrinkToFit(), DoubleVariableLengthVectorType::KeepOldValues());
    ASSERT(&x[0] == start, "ShrinkToFit on the same size shall not induce a reallocation");

    // ---[ Don't Shrink To Fit
    x.SetSize(5, DoubleVariableLengthVectorType::DontShrinkToFit(), DoubleVariableLengthVectorType::KeepOldValues());
    ASSERT( ! x.IsAProxy(), "After resizing, it shall never be a proxy");
    ASSERT( ref[0]==x[0] && ref[1]==x[1] && ref[2]==x[2], "Old Values shall have been kept")
    ASSERT(&x[0] != start, "DontShrinkToFit shall induce a resizing when the size grows");
    x[3] = 3.0;
    x[4] = 4.0;
    start = &x[0];

    x.SetSize(3, DoubleVariableLengthVectorType::DontShrinkToFit(), DoubleVariableLengthVectorType::KeepOldValues());
    ASSERT( ! x.IsAProxy(), "After resizing, it shall never be a proxy");
    ASSERT( ref == x, "Old Values shall have been kept")
    ASSERT(&x[0] == start, "DontShrinkToFit shall not induce a resizing when the size diminishes");
    start = &x[0];

    x.SetSize(3, DoubleVariableLengthVectorType::DontShrinkToFit(), DoubleVariableLengthVectorType::KeepOldValues());
    ASSERT( ! x.IsAProxy(), "After resizing, it shall never be a proxy");
    ASSERT( ref == x, "Old Values shall have been kept")
    ASSERT(&x[0] == start, "DontShrinkToFit shall not induce a resizing when the size stays the same");

    // ---[ Always Reallocate
    x.SetSize(5, DoubleVariableLengthVectorType::AlwaysReallocate(), DoubleVariableLengthVectorType::KeepOldValues());
    ASSERT( ! x.IsAProxy(), "After resizing, it shall never be a proxy");
    ASSERT( ref[0]==x[0] && ref[1]==x[1] && ref[2]==x[2], "Old Values shall have been kept")
    ASSERT(&x[0] != start, "AlwaysReallocate shall induce a reallocation when resizing");
    start = &x[0];

    x.SetSize(3, DoubleVariableLengthVectorType::AlwaysReallocate(), DoubleVariableLengthVectorType::KeepOldValues());
    ASSERT( ! x.IsAProxy(), "After resizing, it shall never be a proxy");
    ASSERT( ref[0]==x[0] && ref[1]==x[1] && ref[2]==x[2], "Old Values shall have been kept")
    ASSERT(&x[0] != start, "AlwaysReallocate shall induce a reallocation when resizing");
    start = &x[0];

    x.SetSize(3, DoubleVariableLengthVectorType::AlwaysReallocate(), DoubleVariableLengthVectorType::KeepOldValues());
    ASSERT( ! x.IsAProxy(), "After resizing, it shall never be a proxy");
    ASSERT( ref[0]==x[0] && ref[1]==x[1] && ref[2]==x[2], "Old Values shall have been kept")
    ASSERT(&x[0] != start, "AlwaysReallocate shall induce a reallocation when resizing, even with the same size");
    start = &x[0];

    // ===[ Don't keep old values
    // ---[ ShrinkToFit
    x.SetSize(5, DoubleVariableLengthVectorType::ShrinkToFit(), DoubleVariableLengthVectorType::DumpOldValues());
    ASSERT(&x[0] != start, "ShrintToFit(bigger) => reallocate");
    ASSERT( ref[0] != x[0], "Resize + Dump Old Value --> Old values shall be lost");
    x[0] = ref[0];
    start = &x[0];

    x.SetSize(3, DoubleVariableLengthVectorType::ShrinkToFit(), DoubleVariableLengthVectorType::DumpOldValues());
    ASSERT(&x[0] != start, "ShrintToFit(smaller) => reallocate");
    ASSERT( ref[0] != x[0], "Resize + Dump Old Value --> Old values shall be lost");
    x[0] = ref[0];
    start = &x[0];

    x.SetSize(5, DoubleVariableLengthVectorType::DontShrinkToFit(), DoubleVariableLengthVectorType::DumpOldValues());
    ASSERT(&x[0] != start, "DontShrintToFit(bigger) => reallocate");
    ASSERT( ref[0] != x[0], "Resize + Dump Old Value --> Old values shall be lost");
    x[0] = ref[0];
    start = &x[0];
    }

  // Test on assignments
    {
    // We won't be able to test that old values are dumped.
    // Only when reallocations will be avoided.
    DoubleVariableLengthVectorType ref1(3);
    ref1[0] = 0.1; ref1[1] = 0.2; ref1[2] = 0.3;
    DoubleVariableLengthVectorType ref2(3);
    ref2[0] = 1.1; ref2[1] = 1.2; ref2[2] = 1.3;
    DoubleVariableLengthVectorType ref4(4);
    ref4[0] = 1.1; ref4[1] = 1.2; ref4[2] = 1.3; ref4[3] = 1.4;

    DoubleVariableLengthVectorType x;
    ASSERT(x != ref1, "New VLV shall be empty");

    x = ref1;
    ASSERT(x == ref1, "Ref1 is expected to be copied into x");
    double * start = &x[0];

    x = ref2;
    ASSERT(x == ref2, "Ref2 is expected to be copied into x");
    ASSERT(start == &x[0], "Assignment doesn't imply reallocation when the new size is identical to the current one");

    x = ref4;
    ASSERT(x == ref4, "Ref4 is expected to be copied into x");
    ASSERT(start != &x[0], "Assignment implies reallocation when the current size is insufficient to hold the new value");
    start = &x[0];

    x = ref1;
    ASSERT(x == ref1, "Ref1 is expected to be copied into x");
    ASSERT(start == &x[0], "Assignment doesn't imply reallocation when the current size is enough");

    // NB: From here, x=ref4; will induce a reallocation even if enough memory has already
    // been allocated.
    }

  // Test FastAssign
    {
    DoubleVariableLengthVectorType ref1(3);
    ref1[0] = 0.1; ref1[1] = 0.2; ref1[2] = 0.3;
    DoubleVariableLengthVectorType ref2(3);
    ref2[0] = 1.1; ref2[1] = 1.2; ref2[2] = 1.3;
    DoubleVariableLengthVectorType ref4(4);
    ref4[0] = 1.1; ref4[1] = 1.2; ref4[2] = 1.3; ref4[3] = 1.4;

    DoubleVariableLengthVectorType x(3);
    // FastAssign pre conditions
    assert(x.GetSize() == ref1.GetSize());
    assert(!x.IsAProxy());
    double * start = &x[0];
    x.FastAssign(ref1);
    ASSERT(start == &x[0], "FastAssign shall never reallocate");
    ASSERT(x == ref1, "FastAssign shall ... assign VLVs");

    assert(x.GetSize() == ref2.GetSize());
    x.FastAssign(ref2);
    ASSERT(start == &x[0], "FastAssign shall never reallocate");
    ASSERT(x == ref2, "FastAssign shall ... assign VLVs");

    // As ref4.GetSize() is different from x.GetSize(),
    //    x.FastAssign(ref4);
    // is an invalid instruction: Indeed FastAssign preconditions are not met.
    }

  delete[] d;
  }

  std::cout << "[PASSED]" << std::endl;

  return EXIT_SUCCESS;

}
