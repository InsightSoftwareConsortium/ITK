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
#include "itkDenseFiniteDifferenceImageFilter.h"


/*
 * This test exercises the dense p.d.e. solver framework
 * itkDenseFiniteDifferenceImageFilter and the two-dimensional level
 * set surface modeling function object.  It shows how to subclass
 * the DenseFiniteDifferenceImageFilter and the LevelSetFunction to
 * create a very simple level set surface evolution application.
 *
 * This application morphs a circle to a square using the level-set surface
 * modeling framework.  Speed function input to the level-set equation
 * is the square distance transform.
 *
 */

namespace LSFT
{ // local namespace for helper test functions

const unsigned int HEIGHT = (256);
const unsigned int WIDTH = (256);

#define RADIUS (std::min(HEIGHT, WIDTH) / 4)

// Distance transform function for circle
float
circle(unsigned int x, unsigned int y)
{
  float dis;
  dis = (x - static_cast<float>(WIDTH) / 2.0) * (x - static_cast<float>(WIDTH) / 2.0) +
        (y - static_cast<float>(HEIGHT) / 2.0) * (y - static_cast<float>(HEIGHT) / 2.0);
  dis = RADIUS - std::sqrt(dis);
  return dis;
}

// Distance transform function for square
float
square(unsigned int x, unsigned int y)
{
  float X, Y;
  X = itk::Math::abs(x - static_cast<float>(WIDTH) / 2.0);
  Y = itk::Math::abs(y - static_cast<float>(HEIGHT) / 2.0);
  float dis;
  if (!((X > RADIUS) && (Y > RADIUS)))
    dis = RADIUS - std::max(X, Y);
  else
    dis = -std::sqrt((X - RADIUS) * (X - RADIUS) + (Y - RADIUS) * (Y - RADIUS));
  return dis;
}

// Evaluates a function at each pixel in the itk image
void
evaluate_function(itk::Image<float, 2> * im, float (*f)(unsigned int, unsigned int))

{
  itk::Image<float, 2>::IndexType idx;
  for (unsigned int x = 0; x < WIDTH; ++x)
  {
    idx[0] = x;
    for (unsigned int y = 0; y < HEIGHT; ++y)
    {
      idx[1] = y;
      im->SetPixel(idx, f(x, y));
    }
  }
}


/**
 * \class MorphFunction
 * Subclasses LevelSetFunction, supplying the "PropagationSpeed" term.
 *
 * See LevelSetFunction for more information.
 */
class MorphFunction : public itk::LevelSetFunction<itk::Image<float, 2>>
{
public:
  void
  SetDistanceTransform(itk::Image<float, 2> * d)
  {
    m_DistanceTransform = d;
  }

  using Self = MorphFunction;

  using Superclass = itk::LevelSetFunction<itk::Image<float, 2>>;
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
    r[0] = r[1] = 1;
    Superclass::Initialize(r);
  }

private:
  itk::Image<float, 2>::Pointer m_DistanceTransform;
  ScalarValueType
  PropagationSpeed(const NeighborhoodType & neighborhood, const FloatOffsetType &, GlobalDataStruct *) const override
  {
    itk::Index<2> idx = neighborhood.GetIndex();
    return m_DistanceTransform->GetPixel(idx);
  }
};


class MorphFilter : public itk::DenseFiniteDifferenceImageFilter<itk::Image<float, 2>, itk::Image<float, 2>>
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
  itkTypeMacro(MorphFilter, DenseFiniteDifferenceImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  itkSetMacro(Iterations, unsigned int);

  void
  SetDistanceTransform(itk::Image<float, 2> * im)
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

} // end of namespace LSFT

int
itkLevelSetFunctionTest(int, char *[])
{
  using ImageType = itk::Image<float, 2>;

  constexpr int n = 100; // Number of iterations

  auto im_init = ImageType::New();
  auto im_target = ImageType::New();

  ImageType::RegionType r;
  ImageType::SizeType   sz = { { LSFT::HEIGHT, LSFT::WIDTH } };
  ImageType::IndexType  idx = { { 0, 0 } };
  r.SetSize(sz);
  r.SetIndex(idx);

  im_init->SetRegions(r);

  im_target->SetRegions(r);

  im_init->Allocate();
  im_target->Allocate();

  LSFT::evaluate_function(im_init, LSFT::circle);
  LSFT::evaluate_function(im_target, LSFT::square);

  itk::ImageRegionIterator<ImageType> itr(im_target, im_target->GetRequestedRegion());

  // Squash level sets everywhere but near the zero set.
  for (itr.GoToBegin(); !itr.IsAtEnd(); ++itr)
  {
    itr.Value() = itr.Value() / std::sqrt((5.0f + itk::Math::sqr(itr.Value())));
  }

  LSFT::MorphFilter::Pointer mf = LSFT::MorphFilter::New();
  mf->SetDistanceTransform(im_target);
  mf->SetIterations(n);
  mf->SetInput(im_init);

  mf->Update();

  return EXIT_SUCCESS;
}
