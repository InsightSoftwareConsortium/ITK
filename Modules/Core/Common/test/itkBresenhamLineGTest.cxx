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

#include "itkBresenhamLine.h"
#include "itkGTest.h"

#include <iostream>

TEST(BresenhamLine, BuildLineFromVector)
{
  // Test BuildLine(Vector, distance)
  itk::Vector<float, 2> v;
  v[0] = 1;
  v[1] = 1;

  itk::BresenhamLine<2>       line;
  std::vector<itk::Offset<2>> offsets = line.BuildLine(v, 4);

  EXPECT_EQ(offsets.size(), 4u);

  for (int i = 0; i < 4; ++i)
  {
    const itk::Offset<2> expectedOffset{ { i, i } };
    ITK_EXPECT_VECTOR_NEAR(offsets[i], expectedOffset, 0);
  }
}

TEST(BresenhamLine, BuildLineFromIndices)
{
  // Test BuildLine(Index, Index)
  itk::Index<2> p0;
  p0[0] = 0;
  p0[1] = 0;

  itk::Index<2> p1;
  p1[0] = 39;
  p1[1] = 39;

  itk::BresenhamLine<2>      line;
  std::vector<itk::Index<2>> indices = line.BuildLine(p0, p1);

  EXPECT_EQ(indices.size(), 40u);

  for (int i = 0; i < 40; ++i)
  {
    const itk::Index<2> expectedIndex{ { i, i } };
    ITK_EXPECT_VECTOR_NEAR(indices[i], expectedIndex, 0);
  }

  std::cout << "Test Passed !" << std::endl;
}

TEST(BresenhamLine, FinishAtEndIndex)
{
  // Test BuildLine(Index, Index) to finish at the specified end index
  const itk::Index<3> start{ { 0, 0, 0 } }, end{ { 250, 250, 1 } };

  itk::BresenhamLine<3>            line;
  const std::vector<itk::Index<3>> indices = line.BuildLine(start, end);
  EXPECT_EQ(indices.back(), end);
}

TEST(BresenhamLine, FinishAtEndIndexNegativeDirection)
{
  // Test BuildLine(Index, Index) with reversed endpoints (negative direction)
  const itk::Index<3> start{ { 250, 250, 1 } }, end{ { 0, 0, 0 } };

  itk::BresenhamLine<3>            line;
  const std::vector<itk::Index<3>> indices = line.BuildLine(start, end);
  EXPECT_EQ(indices.front(), start);
  EXPECT_EQ(indices.back(), end);
}

TEST(BresenhamLine, AxisAligned2D)
{
  // Horizontal and vertical lines must hit exact endpoints
  itk::BresenhamLine<2> line;

  const auto horiz = line.BuildLine(itk::Index<2>{ { 0, 5 } }, itk::Index<2>{ { 10, 5 } });
  EXPECT_EQ(horiz.size(), 11u);
  EXPECT_EQ(horiz.front(), (itk::Index<2>{ { 0, 5 } }));
  EXPECT_EQ(horiz.back(), (itk::Index<2>{ { 10, 5 } }));
  for (const auto & idx : horiz)
  {
    EXPECT_EQ(idx[1], 5);
  }

  const auto vert = line.BuildLine(itk::Index<2>{ { 3, 0 } }, itk::Index<2>{ { 3, 7 } });
  EXPECT_EQ(vert.size(), 8u);
  EXPECT_EQ(vert.front(), (itk::Index<2>{ { 3, 0 } }));
  EXPECT_EQ(vert.back(), (itk::Index<2>{ { 3, 7 } }));
  for (const auto & idx : vert)
  {
    EXPECT_EQ(idx[0], 3);
  }
}

TEST(BresenhamLine, SinglePixelLine)
{
  // p0 == p1: line should contain exactly one pixel
  itk::BresenhamLine<3> line;
  const itk::Index<3>   p{ { 5, 10, 15 } };

  const auto indices = line.BuildLine(p, p);
  EXPECT_EQ(indices.size(), 1u);
  EXPECT_EQ(indices[0], p);
}

TEST(BresenhamLine, Connectivity2D)
{
  // Every consecutive pair must be 8-connected (Chebyshev distance <= 1)
  itk::BresenhamLine<2> line;
  const auto            indices = line.BuildLine(itk::Index<2>{ { 0, 0 } }, itk::Index<2>{ { 100, 37 } });

  for (size_t i = 1; i < indices.size(); ++i)
  {
    itk::IndexValueType maxStep = 0;
    for (unsigned int d = 0; d < 2; ++d)
    {
      const auto s = static_cast<itk::IndexValueType>(itk::Math::Absolute(indices[i][d] - indices[i - 1][d]));
      if (s > maxStep)
      {
        maxStep = s;
      }
    }
    EXPECT_LE(maxStep, 1) << "Gap between pixels " << i - 1 << " and " << i;
  }
}

TEST(BresenhamLine, Connectivity3D)
{
  // Every consecutive pair must be 26-connected (Chebyshev distance <= 1)
  itk::BresenhamLine<3> line;
  const auto            indices = line.BuildLine(itk::Index<3>{ { 0, 0, 0 } }, itk::Index<3>{ { 50, 80, 30 } });

  for (size_t i = 1; i < indices.size(); ++i)
  {
    itk::IndexValueType maxStep = 0;
    for (unsigned int d = 0; d < 3; ++d)
    {
      const auto s = static_cast<itk::IndexValueType>(itk::Math::Absolute(indices[i][d] - indices[i - 1][d]));
      if (s > maxStep)
      {
        maxStep = s;
      }
    }
    EXPECT_LE(maxStep, 1) << "Gap between pixels " << i - 1 << " and " << i;
  }
}

TEST(BresenhamLine, PixelCountMatchesChebyshev)
{
  // Number of pixels should be Chebyshev distance + 1
  itk::BresenhamLine<3> line;

  struct TestCase
  {
    itk::Index<3> p0, p1;
  };
  const TestCase cases[] = { { { { 0, 0, 0 } }, { { 10, 0, 0 } } },
                             { { { 0, 0, 0 } }, { { 10, 10, 0 } } },
                             { { { 0, 0, 0 } }, { { 10, 10, 10 } } },
                             { { { 0, 0, 0 } }, { { 100, 50, 25 } } },
                             { { { 5, 5, 5 } }, { { 5, 5, 5 } } } };

  for (const auto & tc : cases)
  {
    itk::IndexValueType chebyshev = 0;
    for (unsigned int d = 0; d < 3; ++d)
    {
      const auto dist = static_cast<itk::IndexValueType>(itk::Math::Absolute(tc.p1[d] - tc.p0[d]));
      if (dist > chebyshev)
      {
        chebyshev = dist;
      }
    }
    const auto indices = line.BuildLine(tc.p0, tc.p1);
    EXPECT_EQ(indices.size(), static_cast<size_t>(chebyshev + 1));
  }
}

TEST(BresenhamLine, ReverseSymmetry)
{
  // Forward and reverse lines must have same length and hit same endpoints
  itk::BresenhamLine<3> line;
  const itk::Index<3>   a{ { 10, 20, 3 } }, b{ { 200, 50, 80 } };

  const auto forward = line.BuildLine(a, b);
  const auto reverse = line.BuildLine(b, a);

  ASSERT_FALSE(forward.empty());
  ASSERT_FALSE(reverse.empty());
  ASSERT_EQ(forward.size(), reverse.size());
  EXPECT_EQ(forward.front(), a);
  EXPECT_EQ(forward.back(), b);
  EXPECT_EQ(reverse.front(), b);
  EXPECT_EQ(reverse.back(), a);

  // Note: pixel-by-pixel reversal symmetry (forward[i] == reverse[N-1-i])
  // is NOT guaranteed by integer Bresenham for all directions. The error
  // accumulator tie-breaking can produce slightly different intermediate
  // pixels when starting from opposite ends. Both paths are valid
  // rasterizations of the same line segment — they have the same length,
  // same endpoints, and maintain the same connectivity invariant.
}

TEST(BresenhamLine, SteepDiagonal3D)
{
  // Strongly diagonal line that triggered the original bug
  itk::BresenhamLine<3> line;
  const itk::Index<3>   start{ { 0, 0, 0 } }, end{ { 250, 250, 1 } };

  const auto fwd = line.BuildLine(start, end);
  EXPECT_EQ(fwd.front(), start);
  EXPECT_EQ(fwd.back(), end);

  const auto rev = line.BuildLine(end, start);
  EXPECT_EQ(rev.front(), end);
  EXPECT_EQ(rev.back(), start);

  EXPECT_EQ(fwd.size(), rev.size());
}

// --- Tests below address gaps identified from OpenCV, scikit-image, ---
// --- and Foley/van Dam "Computer Graphics: Principles and Practice" ---

TEST(BresenhamLine, AllEightOctants2D)
{
  // Foley/van Dam: every Bresenham implementation must handle all 8 octants.
  // scikit-image tests horizontal, vertical, and diagonal directions.
  itk::BresenhamLine<2> line;

  struct OctantCase
  {
    itk::Index<2> p0, p1;
  };
  // One representative line per octant, plus axis-aligned cases
  const OctantCase cases[] = {
    { { { 0, 0 } }, { { 10, 0 } } },   // +X (right)
    { { { 0, 0 } }, { { -10, 0 } } },  // -X (left)
    { { { 0, 0 } }, { { 0, 10 } } },   // +Y (down)
    { { { 0, 0 } }, { { 0, -10 } } },  // -Y (up)
    { { { 0, 0 } }, { { 10, 3 } } },   // octant 0: shallow +X +Y
    { { { 0, 0 } }, { { 3, 10 } } },   // octant 1: steep +X +Y
    { { { 0, 0 } }, { { -3, 10 } } },  // octant 2: steep -X +Y
    { { { 0, 0 } }, { { -10, 3 } } },  // octant 3: shallow -X +Y
    { { { 0, 0 } }, { { -10, -3 } } }, // octant 4: shallow -X -Y
    { { { 0, 0 } }, { { -3, -10 } } }, // octant 5: steep -X -Y
    { { { 0, 0 } }, { { 3, -10 } } },  // octant 6: steep +X -Y
    { { { 0, 0 } }, { { 10, -3 } } },  // octant 7: shallow +X -Y
  };

  for (const auto & tc : cases)
  {
    const auto indices = line.BuildLine(tc.p0, tc.p1);

    // Endpoints
    EXPECT_EQ(indices.front(), tc.p0);
    EXPECT_EQ(indices.back(), tc.p1);

    // Connectivity: Chebyshev distance <= 1 between consecutive pixels
    for (size_t i = 1; i < indices.size(); ++i)
    {
      for (unsigned int d = 0; d < 2; ++d)
      {
        const auto step = static_cast<itk::IndexValueType>(itk::Math::Absolute(indices[i][d] - indices[i - 1][d]));
        EXPECT_LE(step, 1);
      }
    }

    // Correct pixel count
    itk::IndexValueType chebyshev = 0;
    for (unsigned int d = 0; d < 2; ++d)
    {
      const auto dist = static_cast<itk::IndexValueType>(itk::Math::Absolute(tc.p1[d] - tc.p0[d]));
      if (dist > chebyshev)
      {
        chebyshev = dist;
      }
    }
    EXPECT_EQ(indices.size(), static_cast<size_t>(chebyshev + 1));
  }
}

TEST(BresenhamLine, NoDuplicatePixels)
{
  // Foley/van Dam: every pixel in the output must be unique.
  itk::BresenhamLine<3> line;

  struct TestCase
  {
    itk::Index<3> p0, p1;
  };
  const TestCase cases[] = {
    { { { 0, 0, 0 } }, { { 50, 30, 10 } } },
    { { { 0, 0, 0 } }, { { 10, 10, 10 } } },
    { { { 0, 0, 0 } }, { { 100, 1, 0 } } },
    { { { 5, 5, 5 } }, { { -5, -5, -5 } } },
  };

  for (const auto & tc : cases)
  {
    const auto indices = line.BuildLine(tc.p0, tc.p1);
    for (size_t i = 0; i < indices.size(); ++i)
    {
      for (size_t j = i + 1; j < indices.size(); ++j)
      {
        EXPECT_NE(indices[i], indices[j]) << "Duplicate pixel at positions " << i << " and " << j;
      }
    }
  }
}

TEST(BresenhamLine, SingleStepAdjacentPixels)
{
  // Foley/van Dam: lines between adjacent pixels (1-step).
  itk::BresenhamLine<2> line2d;
  itk::BresenhamLine<3> line3d;

  // 2D: all 8 neighbors
  const itk::Index<2> origin2d{ { 5, 5 } };
  const itk::Index<2> neighbors2d[] = { { { 6, 5 } }, { { 6, 6 } }, { { 5, 6 } }, { { 4, 6 } },
                                        { { 4, 5 } }, { { 4, 4 } }, { { 5, 4 } }, { { 6, 4 } } };
  for (const auto & nb : neighbors2d)
  {
    const auto indices = line2d.BuildLine(origin2d, nb);
    EXPECT_EQ(indices.size(), 2u);
    EXPECT_EQ(indices[0], origin2d);
    EXPECT_EQ(indices[1], nb);
  }

  // 3D: face, edge, and corner neighbors
  const itk::Index<3> origin3d{ { 5, 5, 5 } };
  const itk::Index<3> neighbors3d[] = {
    { { 6, 5, 5 } }, // face
    { { 6, 6, 5 } }, // edge
    { { 6, 6, 6 } }, // corner
    { { 4, 5, 5 } }, // face (negative)
    { { 4, 4, 4 } }, // corner (negative)
  };
  for (const auto & nb : neighbors3d)
  {
    const auto indices = line3d.BuildLine(origin3d, nb);
    EXPECT_EQ(indices.size(), 2u);
    EXPECT_EQ(indices[0], origin3d);
    EXPECT_EQ(indices[1], nb);
  }
}

TEST(BresenhamLine, NegativeCoordinates)
{
  // scikit-image tests line from (-1,-1) to (2,2).
  // Foley/van Dam: lines crossing quadrant boundaries.
  itk::BresenhamLine<2> line2d;
  itk::BresenhamLine<3> line3d;

  // Line entirely in negative coordinates
  {
    const auto indices = line2d.BuildLine(itk::Index<2>{ { -10, -10 } }, itk::Index<2>{ { -1, -1 } });
    EXPECT_EQ(indices.size(), 10u);
    EXPECT_EQ(indices.front(), (itk::Index<2>{ { -10, -10 } }));
    EXPECT_EQ(indices.back(), (itk::Index<2>{ { -1, -1 } }));
  }

  // Line crossing origin
  {
    const auto indices = line2d.BuildLine(itk::Index<2>{ { -5, -3 } }, itk::Index<2>{ { 5, 3 } });
    EXPECT_EQ(indices.front(), (itk::Index<2>{ { -5, -3 } }));
    EXPECT_EQ(indices.back(), (itk::Index<2>{ { 5, 3 } }));
    EXPECT_EQ(indices.size(), 11u);
  }

  // 3D negative
  {
    const auto indices = line3d.BuildLine(itk::Index<3>{ { -20, -10, -5 } }, itk::Index<3>{ { 20, 10, 5 } });
    EXPECT_EQ(indices.front(), (itk::Index<3>{ { -20, -10, -5 } }));
    EXPECT_EQ(indices.back(), (itk::Index<3>{ { 20, 10, 5 } }));
    EXPECT_EQ(indices.size(), 41u);
  }
}

TEST(BresenhamLine, AxisAligned3D)
{
  // Foley/van Dam: axis-aligned lines along each individual axis in 3D.
  itk::BresenhamLine<3> line;

  // Along X
  {
    const auto indices = line.BuildLine(itk::Index<3>{ { 0, 5, 10 } }, itk::Index<3>{ { 20, 5, 10 } });
    EXPECT_EQ(indices.size(), 21u);
    for (const auto & idx : indices)
    {
      EXPECT_EQ(idx[1], 5);
      EXPECT_EQ(idx[2], 10);
    }
  }
  // Along Y
  {
    const auto indices = line.BuildLine(itk::Index<3>{ { 3, 0, 7 } }, itk::Index<3>{ { 3, 15, 7 } });
    EXPECT_EQ(indices.size(), 16u);
    for (const auto & idx : indices)
    {
      EXPECT_EQ(idx[0], 3);
      EXPECT_EQ(idx[2], 7);
    }
  }
  // Along Z
  {
    const auto indices = line.BuildLine(itk::Index<3>{ { 1, 2, 0 } }, itk::Index<3>{ { 1, 2, 30 } });
    EXPECT_EQ(indices.size(), 31u);
    for (const auto & idx : indices)
    {
      EXPECT_EQ(idx[0], 1);
      EXPECT_EQ(idx[1], 2);
    }
  }
}

TEST(BresenhamLine, SteepVsShallowSlopes)
{
  // Foley/van Dam: explicit steep (|dy|>|dx|) vs shallow (|dx|>|dy|) cases.
  itk::BresenhamLine<2> line;

  // Shallow slope: dx=20, dy=3
  {
    const auto indices = line.BuildLine(itk::Index<2>{ { 0, 0 } }, itk::Index<2>{ { 20, 3 } });
    EXPECT_EQ(indices.size(), 21u);
    EXPECT_EQ(indices.front(), (itk::Index<2>{ { 0, 0 } }));
    EXPECT_EQ(indices.back(), (itk::Index<2>{ { 20, 3 } }));

    // Dominant axis is X: every step must advance X by exactly 1
    for (size_t i = 1; i < indices.size(); ++i)
    {
      EXPECT_EQ(indices[i][0] - indices[i - 1][0], 1);
    }
  }

  // Steep slope: dx=3, dy=20
  {
    const auto indices = line.BuildLine(itk::Index<2>{ { 0, 0 } }, itk::Index<2>{ { 3, 20 } });
    EXPECT_EQ(indices.size(), 21u);
    EXPECT_EQ(indices.front(), (itk::Index<2>{ { 0, 0 } }));
    EXPECT_EQ(indices.back(), (itk::Index<2>{ { 3, 20 } }));

    // Dominant axis is Y: every step must advance Y by exactly 1
    for (size_t i = 1; i < indices.size(); ++i)
    {
      EXPECT_EQ(indices[i][1] - indices[i - 1][1], 1);
    }
  }
}

TEST(BresenhamLine, VeryLongLine)
{
  // OpenCV tests very long lines (34204 to 46400). Foley/van Dam warns
  // about accumulation error on long lines.
  itk::BresenhamLine<2> line;
  const itk::Index<2>   p0{ { 0, 0 } }, p1{ { 10000, 7777 } };

  const auto indices = line.BuildLine(p0, p1);

  // Correct endpoints
  EXPECT_EQ(indices.front(), p0);
  EXPECT_EQ(indices.back(), p1);

  // Correct count
  EXPECT_EQ(indices.size(), 10001u);

  // Connectivity: no gaps anywhere in the line
  for (size_t i = 1; i < indices.size(); ++i)
  {
    for (unsigned int d = 0; d < 2; ++d)
    {
      const auto s = static_cast<itk::IndexValueType>(itk::Math::Absolute(indices[i][d] - indices[i - 1][d]));
      EXPECT_LE(s, 1) << "Gap at pixel " << i;
    }
  }

  // Monotonicity: X must be non-decreasing since dx > 0
  for (size_t i = 1; i < indices.size(); ++i)
  {
    EXPECT_GE(indices[i][0], indices[i - 1][0]);
  }
}
