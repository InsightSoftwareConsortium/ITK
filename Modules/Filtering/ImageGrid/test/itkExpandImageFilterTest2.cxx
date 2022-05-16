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

#include <iostream>

#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkExpandImageFilter.h"
#include "itkMath.h"
#include "itkVectorImage.h"
#include "itkVariableLengthVector.h"
#include "itkImageRegionIteratorWithIndex.h"

using PixelType = double;
using VectorImage1D = itk::VectorImage<PixelType, 1>;
using VectorImage3D = itk::VectorImage<PixelType, 3>;


/// Testing ExpandImageFilter with VectorImage

///
/// Generates the value of the test image at a particular index so it is initialized in a single iterator pass.
///
template <typename TVectorImage>
typename TVectorImage::PixelType
GetPattern(const typename TVectorImage::IndexType & index,
           const typename TVectorImage::SizeType &  size,
           unsigned int                             nImages)
{
  typename TVectorImage::PixelType ans(nImages);
  int                              d = TVectorImage::SizeType::Dimension;

  int volume = 1;
  for (int j = 0; j < d; ++j)
  {
    volume *= size[j];
  }

  int    x = index[0] + 1;
  double coeff = size[0];
  for (int j = 1; j < d; ++j)
  {
    x += index[j] * coeff;
    coeff *= size[j];
  }

  for (unsigned int k = 0; k < nImages; ++k)
  {
    ans[k] = volume * k + x;
  }

  return ans;
}

template VectorImage1D::PixelType
GetPattern<VectorImage1D>(const VectorImage1D::IndexType &, const VectorImage1D::SizeType &, unsigned int);
template VectorImage3D::PixelType
GetPattern<VectorImage3D>(const VectorImage3D::IndexType &, const VectorImage3D::SizeType &, unsigned int);

///
/// Pretty prints the values of the small 1D test image.  Each "channel" is printed on a separate line in order
///
template <typename TVectorImage>
std::string
PrintTestImage1D(const TVectorImage * img)
{
  std::string                     ans = "";
  unsigned int                    nImages = img->GetVectorLength();
  typename TVectorImage::SizeType size = img->GetLargestPossibleRegion().GetSize();

  for (unsigned int i = 0; i < nImages; ++i)
  {
    typename TVectorImage::IndexType index = TVectorImage::IndexType::Filled(0);
    ans.append("\n");
    for (unsigned int j = 0; j < size[0]; ++j)
    {
      index.SetElement(0, j);
      ans.append(std::to_string(img->GetPixel(index)[i]) + " ");
    }
  }

  return ans;
}

template std::string
PrintTestImage1D<VectorImage1D>(const VectorImage1D *);

///
/// Pretty prints the values of the small 3D test image.  Prints in dimension 0 and 1 as a grid, a space separates
/// dimension 2 slices, and a double space separates "channels".
template <typename TVectorImage>
std::string
PrintTestImage3D(const TVectorImage * img)
{
  std::string                     ans = "";
  unsigned int                    nImages = img->GetVectorLength();
  typename TVectorImage::SizeType size = img->GetLargestPossibleRegion().GetSize();

  for (unsigned int i = 0; i < nImages; ++i)
  {
    typename TVectorImage::IndexType index = TVectorImage::IndexType::Filled(0);
    ans.append("\n\n\n");
    for (unsigned int j = 0; j < size[2]; ++j)
    {
      ans.append("\n\n");
      for (unsigned int k = 0; k < size[1]; ++k)
      {
        ans.append("\n");
        for (unsigned int m = 0; m < size[0]; ++m)
        {
          index.SetElement(0, m);
          index.SetElement(1, k);
          index.SetElement(2, j);
          ans.append(std::to_string(img->GetPixel(index)[i]) + " ");
        }
      }
    }
  }

  return ans;
}

template std::string
PrintTestImage3D<VectorImage3D>(const VectorImage3D *);


///
/// Create image by traversing channels in dimension starting from 1 and
/// incrementing by 1 every pixel.  So, every pixel is uniquely valued from 0 to image volume * number of
/// images/channels - 1;
///
template <typename TVectorImage>
typename TVectorImage::Pointer
GetVectorTestImage(const typename TVectorImage::SizeType &         size,
                   const typename TVectorImage::VectorLengthType & nImages)
{
  auto                              ans = TVectorImage::New();
  typename TVectorImage::RegionType region;

  region.SetSize(size);
  ans->SetLargestPossibleRegion(region);
  ans->SetBufferedRegion(region);
  ans->SetVectorLength(
    nImages); // a collection of nImages with dimension TVectorImage::ImageDimension and pixel type TPixel
  ans->Allocate();

  itk::ImageRegionIteratorWithIndex<TVectorImage> iter(ans, ans->GetLargestPossibleRegion());
  for (iter.GoToBegin(); !iter.IsAtEnd(); ++iter)
  {

    iter.Set(GetPattern<TVectorImage>(iter.GetIndex(), size, nImages));
  }

  return ans;
}
template VectorImage1D::Pointer
GetVectorTestImage<VectorImage1D>(const VectorImage1D::SizeType &, const VectorImage1D::VectorLengthType &);

template VectorImage3D::Pointer
GetVectorTestImage<VectorImage3D>(const VectorImage3D::SizeType &, const VectorImage3D::VectorLengthType &);

std::string
DoubleToStringArray(double * a, unsigned int size)
{
  std::string ans = "";
  for (unsigned int i = 0; i < size; ++i)
  {
    ans.append(std::to_string(a[i]) + " ");
  }

  return ans;
}

int
itkExpandImageFilterTest2(int, char *[])
{
  // return EXIT_FAILURE;
  int statusValue = EXIT_SUCCESS;

  /// Test 1D: A 5 pixel long 1D image with 2 channels.  Using a NearestNeighborInterpolator for simplicity.  Expanding
  /// by 2.
  VectorImage1D::SizeType size1D = { { 5 } };

  VectorImage1D::Pointer input1D = GetVectorTestImage<VectorImage1D>(size1D, 2);
  std::cout << "Output input1D:" << std::endl;
  std::cout << PrintTestImage1D<VectorImage1D>(input1D) << std::endl;

  using Expander1DType = itk::ExpandImageFilter<VectorImage1D, VectorImage1D>;
  auto expander1D = Expander1DType::New();

  using Interpolator1DType = itk::NearestNeighborInterpolateImageFunction<VectorImage1D, double>;
  auto interpolator1D = Interpolator1DType::New();

  expander1D->SetInterpolator(interpolator1D);
  unsigned int factors1[1] = { 2 };
  expander1D->SetInput(input1D);
  expander1D->SetExpandFactors(factors1);
  expander1D->Update();
  VectorImage1D::Pointer output1D = expander1D->GetOutput();

  std::cout << "Output 1D: \n";
  std::cout << PrintTestImage1D<VectorImage1D>(output1D) << "\n";

  auto s1 = output1D->GetLargestPossibleRegion().GetSize()[0];
  if (s1 != 10)
  {
    std::cout << "Expected 1D image size 10, actual: " << s1;
    statusValue = EXIT_FAILURE;
  }

  double                   slice1[10] = { 6, 6, 7, 7, 8, 8, 9, 9, 10, 10 };
  double                   sliceOut1[10] = {};
  bool                     b1 = true;
  VectorImage1D::IndexType idx1 = VectorImage1D::IndexType::Filled(0);
  for (int i = 0; i < 10; ++i)
  {
    idx1[0] = i;
    sliceOut1[i] = output1D->GetPixel(idx1)[1];
    b1 = b1 && itk::Math::FloatAlmostEqual(slice1[i], sliceOut1[i]); // nearest neighbor, don't need epsilon
  }
  if (!b1)
  {
    std::cout << "Expected 1D image channel 2: " << DoubleToStringArray(slice1, 10)
              << "\nActual: " << DoubleToStringArray(sliceOut1, 10) << "\n";
    statusValue = EXIT_FAILURE;
  }

  /// Test 3D: a 3 x 3 4-channel image.  Like above, incremental pixel values along each channel, dim 0, dim 1, dim 2.
  /// Channel 1 values are 1-27, Channel 2 is 28-54, etc.  Expanding by 2 along dim 1.
  VectorImage3D::SizeType size3D = { { 3, 3, 3 } };
  VectorImage3D::Pointer  input3D = GetVectorTestImage<VectorImage3D>(size3D, 4);

  std::cout << "Output input3D:" << std::endl;
  std::cout << PrintTestImage3D<VectorImage3D>(input3D) << std::endl;

  using Expander3DType = itk::ExpandImageFilter<VectorImage3D, VectorImage3D>;
  auto expander3D = Expander3DType::New();

  using Interpolator3DType = itk::NearestNeighborInterpolateImageFunction<VectorImage3D, double>;
  auto interpolator3D = Interpolator3DType::New();

  expander3D->SetInterpolator(interpolator3D);
  unsigned int factors3[3] = { 1, 2, 1 };
  expander3D->SetInput(input3D);
  expander3D->SetExpandFactors(factors3);
  expander3D->Update();

  VectorImage3D::Pointer output3D = expander3D->GetOutput();

  std::cout << "Output 3D: \n";
  std::cout << PrintTestImage3D<VectorImage3D>(output3D) << "\n";

  VectorImage3D::SizeType s2 = output3D->GetLargestPossibleRegion().GetSize();
  double                  d3[3] = { 3, 6, 3 };
  double                  d4[3] = {};
  bool                    b2 = true;
  for (int i = 0; i < 3; ++i)
  {
    d4[i] = s2[i];
    b2 = b2 && itk::Math::FloatAlmostEqual(d3[i], d4[i]);
  }
  if (!b2)
  {
    std::cout << "Expected 3D image size: " << DoubleToStringArray(d3, 3) << "\n"
              << "Actual:" << DoubleToStringArray(d4, 3) << "\n";
    statusValue = EXIT_FAILURE;
  }

  double                   slice3[6] = { 38, 38, 41, 41, 44, 44 };
  double                   slice3Out[6] = {};
  VectorImage3D::IndexType idx2;
  idx2[0] = 1;
  idx2[2] = 1;
  bool b3 = true;
  for (int i = 0; i < 6; ++i)
  {
    idx2[1] = i;
    slice3Out[i] = output3D->GetPixel(idx2)[1];
    b3 = b3 && itk::Math::FloatAlmostEqual(slice3[i], slice3Out[i]);
  }

  if (!b3)
  {
    std::cout << "Expected 3D image (1,0:9,1) Channel 2: " << DoubleToStringArray(slice3, 6)
              << "\nActual: " << DoubleToStringArray(slice3Out, 6) << "\n";
    statusValue = EXIT_FAILURE;
  }
  return statusValue;
}
