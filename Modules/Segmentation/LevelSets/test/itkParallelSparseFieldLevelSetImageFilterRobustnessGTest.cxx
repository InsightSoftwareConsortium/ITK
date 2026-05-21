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

// Robustness test: the neighbor-only inter-phase sync deadlocks under
// pool/TBB MultiThreader backends when work units exceed worker-pool size.
// Only same-work-unit-count runs produce bit-identical output. See the PR
// for the full analysis and scenario rationale.

#include "gtest/gtest.h"

#include "itkLevelSetFunction.h"
#include "itkParallelSparseFieldLevelSetImageFilter.h"

#include <algorithm>
#include <chrono>
#include <cmath>
#include <condition_variable>
#include <future>
#include <iostream>
#include <mutex>
#include <thread>
#include <vector>

namespace PSFLSIFR
{

constexpr unsigned int DIM = 32;
constexpr int          RADIUS = DIM / 4;

// Abort the process if body does not finish within the deadline, so a
// ParallelizeArray dispatch deadlock fails the test instead of hanging the
// driver. The watchdog runs on its own OS thread, outside the work-unit pool
// that the deadlock pins.
template <typename TFunctor>
void
runWithDeadline(unsigned int seconds, TFunctor && body)
{
  std::mutex              m;
  std::condition_variable cv;
  bool                    done = false;
  std::thread             watchdog([&] {
    std::unique_lock<std::mutex> lk(m);
    if (!cv.wait_for(lk, std::chrono::seconds(seconds), [&] { return done; }))
    {
      std::cerr << "Deadline exceeded (" << seconds << "s): ParallelizeArray dispatch deadlock\n";
      std::abort();
    }
  });
  body();
  {
    const std::lock_guard<std::mutex> lk(m);
    done = true;
  }
  cv.notify_all();
  watchdog.join();
}

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
  itk::ImageRegionIterator<ImageType> it(im, im->GetRequestedRegion());
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    it.Value() = it.Value() / std::sqrt((5.0f + itk::Math::sqr(it.Value())));
  }
  return im;
}

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

// Scenario 1: repeatedly cycle work-unit counts to amplify the rare per-run
// pool-starvation deadlock into a near-certain per-test failure.
TEST(ParallelSparseFieldLevelSetRobustness, SweepRepeat)
{
  using namespace PSFLSIFR;
  runWithDeadline(30, [] {
    const std::vector<unsigned int> sweep{ 1, 2, 4, 8, 11, 16, 32 };
    constexpr unsigned int          kSweepRepeats = 20;
    constexpr unsigned int          kSweepIterations = 30;
    for (unsigned int rep = 0; rep < kSweepRepeats; ++rep)
    {
      for (const unsigned int wu : sweep)
      {
        auto         out = run_one(wu, kSweepIterations);
        const double summary = image_summary(out);
        EXPECT_TRUE(std::isfinite(summary));
        EXPECT_GT(summary, 0.0);
        EXPECT_LT(summary, 100.0);
      }
    }
  });
}

// Scenario 2: repeated runs at a fixed work-unit count must be bit-identical.
TEST(ParallelSparseFieldLevelSetRobustness, Determinism)
{
  using namespace PSFLSIFR;
  runWithDeadline(30, [] {
    auto run0 = run_one(11, 100);
    for (unsigned int rep = 1; rep < 3; ++rep)
    {
      auto runN = run_one(11, 100);
      EXPECT_TRUE(images_identical(run0, runN)) << "run " << rep << " differs from run 0";
    }
  });
}

// Scenario 3: eight concurrent std::async pipelines (88 work units) contend
// for the core-bounded worker pool, probing the dispatch-starvation deadlock.
TEST(ParallelSparseFieldLevelSetRobustness, ConcurrentMultiPipeline)
{
  using namespace PSFLSIFR;
  runWithDeadline(30, [] {
    constexpr unsigned int kConcurrentReps = 6;
    constexpr unsigned int kConcurrentPipelines = 8;
    for (unsigned int rep = 0; rep < kConcurrentReps; ++rep)
    {
      std::vector<std::future<ImageType::Pointer>> futures;
      futures.reserve(kConcurrentPipelines);
      for (unsigned int p = 0; p < kConcurrentPipelines; ++p)
      {
        futures.emplace_back(std::async(std::launch::async, [] { return run_one(11, 60); }));
      }
      for (auto & f : futures)
      {
        ImageType::Pointer out = f.get();
        EXPECT_TRUE(out.IsNotNull());
      }
    }
  });
}
