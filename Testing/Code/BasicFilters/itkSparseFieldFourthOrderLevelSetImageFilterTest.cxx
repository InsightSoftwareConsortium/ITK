#include "itkImageRegionIterator.h"
#include "itkSparseFieldFourthOrderLevelSetImageFilter.h"
#include "itkLevelSetFunctionWithRefitTerm.h"
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

const unsigned int HEIGHT = (128);
const unsigned int WIDTH  = (128);

#define RADIUS (vnl_math_min(HEIGHT, WIDTH)/4)

// Distance transform function for square
float square(unsigned x, unsigned y)
{
    float X, Y;
    X = ::fabs(x - (float)WIDTH/2.0);
    Y = ::fabs(y - (float)HEIGHT/2.0);
    float dis;
    if (!((X > RADIUS)&&(Y > RADIUS)))
      dis = RADIUS - vnl_math_max(X, Y);
    else
      dis = -sqrt((X - RADIUS)*(X - RADIUS) +  (Y - RADIUS)*(Y - RADIUS));
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

namespace itk {
template <class TInputImage, class TOutputImage>
class ITK_EXPORT IsotropicDiffusionLevelSetFilter
  : public SparseFieldFourthOrderLevelSetImageFilter <TInputImage, TOutputImage>
{
public:
  typedef IsotropicDiffusionLevelSetFilter Self;
  typedef SparseFieldFourthOrderLevelSetImageFilter <TInputImage,
                                                     TOutputImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

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
    for (unsigned int j=0; j<ImageDimension;j++)
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
  }
  
  virtual bool Halt ()
  {
    if (this->GetElapsedIterations()==100) return true;
    else return false;
  }
};

} // end namespace itk

int itkSparseFieldFourthOrderLevelSetImageFilterTest(int, char* [] )
{
  typedef itk::Image<float, 2> ImageType;
  
  ImageType::Pointer im_init = ImageType::New();
  
  ImageType::RegionType r;
  ImageType::SizeType   sz = {{HEIGHT, WIDTH}};
  ImageType::IndexType  idx = {{0,0}};
  r.SetSize(sz);
  r.SetIndex(idx);

  im_init->SetLargestPossibleRegion(r);
  im_init->SetBufferedRegion(r);
  im_init->SetRequestedRegion(r);
  im_init->Allocate();

  evaluate_function(im_init, square);
  typedef itk::IsotropicDiffusionLevelSetFilter<ImageType, ImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();

  filter->SetInput(im_init);
  std::cout<<"Starting processing.\n";
  filter->Update();
  std::cout<<"Passed.\n";
  return 0;
}
