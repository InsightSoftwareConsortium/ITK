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

#include "itkLevelSetFunction.h"
#include "itkParallelSparseFieldLevelSetImageFilter.h"

#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

/*
 * This test exercises the dense p.d.e. solver framework
 * itkParallelSparseFieldLevelSetImageFilter and the three-dimensional level
 * set surface modeling function object.  It shows how to subclass
 * the ParallelSparseFieldLevelSetImageFilter and the LevelSetFunction to
 * create a very simple level set surface evolution application.
 *
 * This application morphs a sphere to a cube using the level-set surface
 * modeling framework.  Speed function input to the level-set equation
 * is the cube distance transform.
 *
 */

namespace PSFLSIFT
{ // local namespace for helper functions

const unsigned int HEIGHT = (64);
const unsigned int WIDTH = (64);
const unsigned int DEPTH = (64);

const int RADIUS = (std::min(std::min(HEIGHT, WIDTH), DEPTH) / 4);

// Distance transform function for a sphere
float
sphere(unsigned int x, unsigned int y, unsigned int z)
{
  float dis;
  dis = (x - static_cast<float>(WIDTH) / 2.0) * (x - static_cast<float>(WIDTH) / 2.0) +
        (y - static_cast<float>(HEIGHT) / 2.0) * (y - static_cast<float>(HEIGHT) / 2.0) +
        (z - static_cast<float>(DEPTH) / 2.0) * (z - static_cast<float>(DEPTH) / 2.0);
  dis = RADIUS - std::sqrt(dis);
  return (-dis);
}

// Distance transform function for a cube
float
cube(unsigned int x, unsigned int y, unsigned int z)
{
  float X, Y, Z;
  X = itk::Math::abs(x - static_cast<float>(WIDTH) / 2.0);
  Y = itk::Math::abs(y - static_cast<float>(HEIGHT) / 2.0);
  Z = itk::Math::abs(z - static_cast<float>(DEPTH) / 2.0);
  float dis;
  if (!((X > RADIUS) && (Y > RADIUS) && (Z > RADIUS)))
  {
    dis = RADIUS - (std::max(std::max(X, Y), Z));
  }
  else
  {
    dis = -sqrt((X - RADIUS) * (X - RADIUS) + (Y - RADIUS) * (Y - RADIUS) + (Z - RADIUS) * (Z - RADIUS));
  }
  return (-dis);
}

// Evaluates a function at each pixel in the itk volume
void
evaluate_function(itk::Image<float, 3> * im, float (*f)(unsigned int, unsigned int, unsigned int))
{
  itk::Image<float, 3>::IndexType idx;
  for (unsigned int x = 0; x < WIDTH; ++x)
  {
    idx[0] = x;
    for (unsigned int y = 0; y < HEIGHT; ++y)
    {
      idx[1] = y;
      for (unsigned int z = 0; z < HEIGHT; ++z)
      {
        idx[2] = z;
        im->SetPixel(idx, f(x, y, z));
      }
    }
  }
}

/**
 * \class MorphFunction
 * Subclasses LevelSetFunction, supplying the "PropagationSpeed" term.
 *
 * See LevelSetFunction for more information.
 */
class MorphFunction : public itk::LevelSetFunction<itk::Image<float, 3>>
{
public:
  void
  SetDistanceTransform(itk::Image<float, 3> * d)
  {
    m_DistanceTransform = d;
  }

  using Self = MorphFunction;

  using Superclass = itk::LevelSetFunction<itk::Image<float, 3>>;
  using RadiusType = Superclass::RadiusType;
  using GlobalDataStruct = Superclass::GlobalDataStruct;

  /**
   * Smart pointer support for this class.
   */
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(MorphFunction, LevelSetFunction);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

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
  PropagationSpeed(const NeighborhoodType & neighborhood, const FloatOffsetType &, GlobalDataStruct *) const override
  {
    itk::Index<3> idx = neighborhood.GetIndex();
    return m_DistanceTransform->GetPixel(idx);
  }
};

class MorphFilter : public itk::ParallelSparseFieldLevelSetImageFilter<itk::Image<float, 3>, itk::Image<float, 3>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MorphFilter);

  using Self = MorphFilter;

  /**
   * Smart pointer support for this class.
   */
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(MorphFilter, ParallelSparseFieldLevelSetImageFilter);

  /**
   * Method for creation through the object factory.
   */
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
    m_Iterations = 0;
  }

private:
  unsigned int m_Iterations;

  bool
  Halt() override
  {
    if (this->GetElapsedIterations() == m_Iterations)
      return true;
    else
      return false;
  }
};

} // end namespace PSFLSIFT

int
itkParallelSparseFieldLevelSetImageFilterTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " OutputImage [InitImage [TargetImage]]\n";
    return EXIT_FAILURE;
  }

  using ImageType = itk::Image<float, 3>;

  constexpr int n = 100;                // Number of iterations
  constexpr int numberOfWorkUnits = 11; // Number of work units to be used

  auto im_init = ImageType::New();
  auto im_target = ImageType::New();

  ImageType::RegionType r;
  ImageType::SizeType   sz = { { PSFLSIFT::HEIGHT, PSFLSIFT::WIDTH, PSFLSIFT::DEPTH } };
  ImageType::IndexType  idx = { { 0, 0, 0 } };
  r.SetSize(sz);
  r.SetIndex(idx);

  ImageType::PointType     origin;
  ImageType::SpacingType   spacing;
  ImageType::DirectionType direction;
  origin[0] = 1.0;
  origin[1] = 10.0;
  origin[2] = 100.0;
  spacing[0] = 1.0;
  spacing[1] = 2.0;
  spacing[2] = 3.0;
  direction.SetIdentity();
  direction(1, 1) = -1.0;

  im_init->SetRegions(r);

  im_init->SetOrigin(origin);
  im_init->SetSpacing(spacing);
  im_init->SetDirection(direction);

  im_target->SetRegions(r);

  im_target->SetOrigin(origin);
  im_target->SetSpacing(spacing);
  im_target->SetDirection(direction);

  im_init->Allocate();
  im_target->Allocate();

  PSFLSIFT::evaluate_function(im_init, PSFLSIFT::sphere);
  PSFLSIFT::evaluate_function(im_target, PSFLSIFT::cube);

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  if (argc > 2)
  {
    writer->SetInput(im_init);
    writer->SetFileName(argv[2]);
    writer->Update();
  }
  if (argc > 3)
  {
    writer->SetInput(im_target);
    writer->SetFileName(argv[3]);
    writer->Update();
  }
  itk::ImageRegionIterator<ImageType> itr(im_target, im_target->GetRequestedRegion());

  // Squash level sets everywhere but near the zero set.
  for (itr.GoToBegin(); !itr.IsAtEnd(); ++itr)
  {
    itr.Value() = itr.Value() / std::sqrt((5.0f + itk::Math::sqr(itr.Value())));
  }

  PSFLSIFT::MorphFilter::Pointer mf = PSFLSIFT::MorphFilter::New();
  mf->SetDistanceTransform(im_target);
  mf->SetIterations(n);
  mf->SetInput(im_init);
  mf->SetNumberOfWorkUnits(numberOfWorkUnits);
  mf->SetNumberOfLayers(3);

  try
  {
    mf->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << e << std::endl;
  }

  mf->GetOutput()->Print(std::cout);

  writer->SetInput(mf->GetOutput());
  writer->SetFileName(argv[1]);
  writer->Update();

  std::cout << mf << std::endl << std::flush;

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
