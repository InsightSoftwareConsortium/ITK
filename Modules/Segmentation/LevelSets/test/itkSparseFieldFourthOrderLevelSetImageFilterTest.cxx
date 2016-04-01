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

#include "itkSparseFieldFourthOrderLevelSetImageFilter.h"
#include <iostream>

/*
 * This test exercises the SparseFieldFourthOrderLevelSetImageFilter
 * framework. A 2D image of a square is created and passed as input to the
 * filter which performs 500 iterations. This application will perform
 * isotropic fourth order diffusion on the input; therefore, the square will
 * morph towards a circle. The classes tested are the following:
 *
 * SparseImage
 * FiniteDifferenceSparseImageFilter
 * FiniteDifferenceSparseImageFunction
 * ImplicitManifoldNormalDiffusionFilter
 * NormalVectorFunctionBase
 * NormalVectorDiffusionFunction
 * LevelSetFunctionWithRefitTerm
 * SparseFieldFourthOrderLevelSetImageFilter
 *
 */

namespace SFFOLSIFT {  // local namespace for helper functions

const unsigned int HEIGHT = (128);
const unsigned int WIDTH  = (128);

#define RADIUS (std::min(HEIGHT, WIDTH)/4)

// Distance transform function for square
float square(unsigned x, unsigned y)
{
    float X, Y;
    X = std::fabs(x - (float)WIDTH/2.0);
    Y = std::fabs(y - (float)HEIGHT/2.0);
    float dis;
    if (!((X > RADIUS)&&(Y > RADIUS)))
      dis = RADIUS - std::max(X, Y);
    else
      dis = -std::sqrt((X - RADIUS)*(X - RADIUS) +  (Y - RADIUS)*(Y - RADIUS));
    return(dis);
}

// Evaluates a function at each pixel in the itk image
void evaluate_function(itk::Image<float, 2> *im,
                       float (*f)(unsigned int, unsigned int) )

{
  itk::Image<float, 2>::IndexType idx;
  for (unsigned int x = 0; x < WIDTH; ++x)
    {
      idx[0] = x;
      for (unsigned int y = 0; y < HEIGHT; ++y)
        {
          idx[1] = y;
          im->SetPixel(idx, f(x, y) );
        }
    }
}

} // end namespace

namespace itk {
template <typename TInputImage, typename TOutputImage>
class IsotropicDiffusionLevelSetFilter
  : public SparseFieldFourthOrderLevelSetImageFilter <TInputImage, TOutputImage>
{
public:
  typedef IsotropicDiffusionLevelSetFilter Self;
  typedef SparseFieldFourthOrderLevelSetImageFilter <TInputImage,
                                                     TOutputImage>
                                           Superclass;
  typedef SmartPointer<Self>               Pointer;
  typedef SmartPointer<const Self>         ConstPointer;

  itkTypeMacro(IsotropicDiffusionLevelSetFilter,SparseFieldFourthOrderLevelSetImageFilter);
  itkNewMacro (Self);

  typedef typename Superclass::SparseImageType SparseImageType;
  typedef LevelSetFunctionWithRefitTerm <TOutputImage,SparseImageType> FunctionType;
  typedef typename FunctionType::RadiusType RadiusType;

protected:
  typename FunctionType::Pointer m_Function;
  IsotropicDiffusionLevelSetFilter()
  {
    RadiusType radius;
    for (unsigned int j=0; j<TInputImage::ImageDimension;j++)
      {
      radius[j] = 1;
      }

    m_Function=FunctionType::New();
    this->SetLevelSetFunction(m_Function);
    this->SetNumberOfLayers(this->GetMinimumNumberOfLayers());

    this->SetMaxNormalIteration(10);
    this->SetMaxRefitIteration(40);
    m_Function->Initialize(radius);
    this->SetNormalProcessType (0);

    m_Function->Print(std::cout);
  }

  virtual bool Halt () ITK_OVERRIDE
  {
    if (this->GetElapsedIterations() == 50)
      {
      return true;
      }
    else
      {
      return false;
      }
  }
};

} // end namespace itk

int itkSparseFieldFourthOrderLevelSetImageFilterTest(int, char* [] )
{
  typedef itk::Image<float, 2> ImageType;

  ImageType::Pointer im_init = ImageType::New();

  ImageType::RegionType r;
  ImageType::SizeType   sz = {{SFFOLSIFT::HEIGHT, SFFOLSIFT::WIDTH}};
  ImageType::IndexType  idx = {{0,0}};
  r.SetSize(sz);
  r.SetIndex(idx);

  im_init->SetLargestPossibleRegion(r);
  im_init->SetBufferedRegion(r);
  im_init->SetRequestedRegion(r);
  im_init->Allocate();

  SFFOLSIFT::evaluate_function(im_init, SFFOLSIFT::square);
  typedef itk::IsotropicDiffusionLevelSetFilter<ImageType, ImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();

  filter->SetInput(im_init);
  std::cout<<"MaxRefitIteration = "<<(filter->GetMaxRefitIteration())<<"\n";
  std::cout<<"MaxNormalIteration = "<<(filter->GetMaxNormalIteration())<<"\n";
  filter->SetCurvatureBandWidth (4);
  std::cout<<"CurvatureBandWidth= "<<(filter->GetCurvatureBandWidth())<<"\n";
  filter->SetRMSChangeNormalProcessTrigger(0.001);
  std::cout<<"RMS change trigger = "
           <<(filter->GetRMSChangeNormalProcessTrigger())<<"\n";
  std::cout<<"Normal process type = "<<(filter->GetNormalProcessType())<<"\n";
  std::cout<<"Conductance = "<<(filter->GetNormalProcessConductance())<<"\n";
  std::cout<<"Unsharp flag = "<<(filter->GetNormalProcessUnsharpFlag())<<"\n";
  std::cout<<"Unsharp weight = "
           <<(filter->GetNormalProcessUnsharpWeight())<<"\n";

  filter->Update();
  filter->Print(std::cout);
  std::cout<<"Passed.\n";
  return EXIT_SUCCESS;
}
