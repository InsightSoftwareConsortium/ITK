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

// Multi-scenario robustness test for ParallelSparseFieldLevelSetImageFilter's
// inter-phase synchronization. Exercises the rewritten barrier under:
//   1. Work-unit-count sweep (1, 2, 4, 8, 11, 16, 32) -- must not deadlock,
//      and the resulting image must always be a finite, plausible final
//      embedding (summary in a sane range; no NaN/Inf).
//   2. Determinism: repeated identical runs at one work-unit count must
//      produce bit-identical output images.
//   3. Rapid-fire reuse: invoking the algorithm repeatedly with varied
//      work-unit counts must not deadlock the barrier on re-creation.
// Each scenario exercises the LOAD_BALANCE_ITERATION_FREQUENCY=30 path by
// running 100 iterations of sphere-to-cube morphology.
//
// Cross-work-unit output is not bit-identical: the per-thread ResolveTimeStep
// reduction picks slightly different timesteps when the per-thread active-layer
// partitioning changes, and that propagates through the evolution trajectory.
// That sensitivity is inherent to the algorithm, not a sync bug.

#include "itkLevelSetFunction.h"
#include "itkParallelSparseFieldLevelSetImageFilter.h"
#include "itkTestingMacros.h"

#include <algorithm>
#include <cmath>
#include <iostream>
#include <vector>

namespace PSFLSIFR
{

constexpr unsigned int DIM = 32;
constexpr int          RADIUS = DIM / 4;

float
sphere(unsigned int x, unsigned int y, unsigned int z)
{
  float dis = (x - float{ DIM } / 2.0f) * (x - float{ DIM } / 2.0f) +
              (y - float{ DIM } / 2.0f) * (y - float{ DIM } / 2.0f) +
              (z - float{ DIM } / 2.0f) * (z - float{ DIM } / 2.0f);
  dis = RADIUS - std::sqrt(dis);
  return -dis;
}

float
cube(unsigned int x, unsigned int y, unsigned int z)
{
  const float X = itk::Math::Absolute(x - float{ DIM } / 2.0f);
  const float Y = itk::Math::Absolute(y - float{ DIM } / 2.0f);
  const float Z = itk::Math::Absolute(z - float{ DIM } / 2.0f);
  float       dis = -std::sqrt((X - RADIUS) * (X - RADIUS) + (Y - RADIUS) * (Y - RADIUS) + (Z - RADIUS) * (Z - RADIUS));
  if (!((X > RADIUS) && (Y > RADIUS) && (Z > RADIUS)))
  {
    dis = RADIUS - (std::max(std::max(X, Y), Z));
  }
  return -dis;
}

void
fill_image(itk::Image<float, 3> * im, float (*f)(unsigned int, unsigned int, unsigned int))
{
  itk::Image<float, 3>::IndexType idx;
  for (unsigned int x = 0; x < DIM; ++x)
  {
    idx[0] = x;
    for (unsigned int y = 0; y < DIM; ++y)
    {
      idx[1] = y;
      for (unsigned int z = 0; z < DIM; ++z)
      {
        idx[2] = z;
        im->SetPixel(idx, f(x, y, z));
      }
    }
  }
}

class MorphFunction : public itk::LevelSetFunction<itk::Image<float, 3>>
{
public:
  using Self = MorphFunction;
  using Superclass = itk::LevelSetFunction<itk::Image<float, 3>>;
  using Pointer = itk::SmartPointer<Self>;
  itkOverrideGetNameOfClassMacro(MorphFunction);
  itkNewMacro(Self);

  void
  SetDistanceTransform(itk::Image<float, 3> * d)
  {
    m_DistanceTransform = d;
  }

protected:
  ~MorphFunction() override = default;
  MorphFunction()
  {
    RadiusType r;
    r[0] = r[1] = r[2] = 1;
    Superclass::Initialize(r);
  }

private:
  itk::Image<float, 3>::Pointer m_DistanceTransform;
  ScalarValueType
  PropagationSpeed(const NeighborhoodType & nbh, const FloatOffsetType &, GlobalDataStruct *) const override
  {
    return m_DistanceTransform->GetPixel(nbh.GetIndex());
  }
};

class MorphFilter : public itk::ParallelSparseFieldLevelSetImageFilter<itk::Image<float, 3>, itk::Image<float, 3>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MorphFilter);
  using Self = MorphFilter;
  using Pointer = itk::SmartPointer<Self>;
  itkOverrideGetNameOfClassMacro(MorphFilter);
  itkNewMacro(Self);
  itkSetMacro(Iterations, unsigned int);

  void
  SetDistanceTransform(itk::Image<float, 3> * im)
  {
    auto * func = dynamic_cast<MorphFunction *>(this->GetDifferenceFunction().GetPointer());
    if (func == nullptr)
    {
      itkGenericExceptionMacro("MorphFunction cast failed");
    }
    func->SetDistanceTransform(im);
  }

protected:
  ~MorphFilter() override = default;
  MorphFilter()
  {
    auto p = MorphFunction::New();
    p->SetPropagationWeight(-1.0);
    p->SetAdvectionWeight(0.0);
    p->SetCurvatureWeight(1.0);
    this->SetDifferenceFunction(p);
  }

private:
  unsigned int m_Iterations{ 0 };
  bool
  Halt() override
  {
    return this->GetElapsedIterations() == m_Iterations;
  }
};

using ImageType = itk::Image<float, 3>;

ImageType::Pointer
make_init_image()
{
  auto                  im = ImageType::New();
  ImageType::SizeType   sz{ DIM, DIM, DIM };
  ImageType::RegionType r{ sz };
  im->SetRegions(r);
  im->Allocate();
  fill_image(im, sphere);
  return im;
}

ImageType::Pointer
make_target_image()
{
  auto                  im = ImageType::New();
  ImageType::SizeType   sz{ DIM, DIM, DIM };
  ImageType::RegionType r{ sz };
  im->SetRegions(r);
  im->Allocate();
  fill_image(im, cube);
  // Squash level sets everywhere but near the zero set (matches the
  // baseline driver test).
  itk::ImageRegionIterator<ImageType> it(im, im->GetRequestedRegion());
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    it.Value() = it.Value() / std::sqrt((5.0f + itk::Math::sqr(it.Value())));
  }
  return im;
}

// Returns the output image after iterating `iterations` times with the given
// work-unit count.
ImageType::Pointer
run_one(unsigned int workUnits, unsigned int iterations)
{
  auto init = make_init_image();
  auto target = make_target_image();
  auto mf = MorphFilter::New();
  mf->SetDistanceTransform(target);
  mf->SetIterations(iterations);
  mf->SetInput(init);
  mf->SetNumberOfWorkUnits(workUnits);
  mf->SetNumberOfLayers(3);
  mf->SetIsoSurfaceValue(0.0);
  mf->Update();
  ImageType::Pointer out = mf->GetOutput();
  out->DisconnectPipeline();
  return out;
}

// Returns RMS-like summary of an image (deterministic reduction).
double
image_summary(ImageType * img)
{
  double                                   sum = 0.0;
  itk::ImageRegionConstIterator<ImageType> it(img, img->GetRequestedRegion());
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    sum += static_cast<double>(it.Get()) * static_cast<double>(it.Get());
  }
  return std::sqrt(sum / img->GetRequestedRegion().GetNumberOfPixels());
}

// Bit-identical comparison.
bool
images_identical(ImageType * a, ImageType * b)
{
  if (a->GetRequestedRegion() != b->GetRequestedRegion())
  {
    return false;
  }
  itk::ImageRegionConstIterator<ImageType> ai(a, a->GetRequestedRegion());
  itk::ImageRegionConstIterator<ImageType> bi(b, b->GetRequestedRegion());
  for (ai.GoToBegin(), bi.GoToBegin(); !ai.IsAtEnd(); ++ai, ++bi)
  {
    if (ai.Get() != bi.Get())
    {
      return false;
    }
  }
  return true;
}

} // namespace PSFLSIFR

int
itkParallelSparseFieldLevelSetImageFilterRobustnessTest(int, char *[])
{
  using namespace PSFLSIFR;

  int testStatus = EXIT_SUCCESS;

  constexpr unsigned int kIterations = 100;

  // ----- Scenario 1: work-unit-count sweep. Must not deadlock at any
  // count. Output must be a finite plausible embedding -- no NaN, no
  // Inf, summary in a sane range (the sphere->cube morph should not
  // blow up regardless of partitioning).
  std::cout << "--- Scenario 1: work-unit sweep ---" << std::endl;
  const std::vector<unsigned int> sweep{ 1, 2, 4, 8, 11, 16, 32 };
  for (const unsigned int wu : sweep)
  {
    std::cout << "  workUnits=" << wu << std::flush;
    auto         out = run_one(wu, kIterations);
    const double summary = image_summary(out);
    std::cout << " summary=" << summary << std::endl;
    ITK_TEST_EXPECT_TRUE_STATUS_VALUE(std::isfinite(summary), testStatus);
    ITK_TEST_EXPECT_TRUE_STATUS_VALUE(summary > 0.0 && summary < 100.0, testStatus);
  }

  // ----- Scenario 2: determinism at fixed work-unit count. Repeated
  // runs of the same configuration must produce bit-identical output.
  std::cout << "--- Scenario 2: determinism (workUnits=11 x3) ---" << std::endl;
  auto run0 = run_one(11, kIterations);
  for (unsigned int rep = 1; rep < 3; ++rep)
  {
    auto       runN = run_one(11, kIterations);
    const bool identical = images_identical(run0, runN);
    ITK_TEST_EXPECT_TRUE_STATUS_VALUE(identical, testStatus);
    if (!identical)
    {
      std::cerr << "  run " << rep << " differs from run 0" << std::endl;
    }
  }

  // ----- Scenario 3: rapid-fire repeated invocations across changing
  // work-unit counts. The Iterate() entry must re-create the barrier
  // each call; this scenario fails if barrier reset is broken.
  std::cout << "--- Scenario 3: rapid-fire across work-unit counts ---" << std::endl;
  for (unsigned int rep = 0; rep < 8; ++rep)
  {
    const unsigned int wu = sweep[rep % sweep.size()];
    auto               out = run_one(wu, 30); // shorter
    ITK_TEST_EXPECT_TRUE_STATUS_VALUE(out.IsNotNull(), testStatus);
    std::cout << "  rep " << rep << " workUnits=" << wu << " summary=" << image_summary(out) << std::endl;
  }

  std::cout << "Test finished." << std::endl;
  return testStatus;
}
