/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetFunctionTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImageRegionIterator.h"
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
const unsigned int HEIGHT = (256);
const unsigned int WIDTH  = (256);

#define RADIUS (vnl_math_min(HEIGHT, WIDTH)/4)

// Distance transform function for circle
float circle(unsigned x, unsigned y)
{
  float dis;
    dis = (x - (float)WIDTH/2.0)*(x - (float)WIDTH/2.0)
         + (y - (float)HEIGHT/2.0)*(y - (float)HEIGHT/2.0);
    dis = RADIUS - sqrt(dis);
    return(dis);
}

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

/**
 * \class MorphFunction
 * Subclasses LevelSetFunction, supplying the ``PropagationSpeed'' term.
 * 
 * See LevelSetFunction for more information.
 */
class MorphFunction : public LevelSetFunction< Image<float, 2> >
{
public:
  void SetDistanceTransform (Image<float, 2> *d)
    { m_DistanceTransform = d; }
  
  typedef MorphFunction Self;

  typedef LevelSetFunction< Image<float, 2> > Superclass;
  typedef Superclass::RadiusType RadiusType;
  
   /** 
   * Smart pointer support for this class.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro( MorphFunction, LevelSetFunction );
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

protected:
  ~MorphFunction() {}

  MorphFunction()
    {
      RadiusType r;
      r[0] = r[1] = 1;
      Superclass::Initialize(r);
    }

private:
  Image<float, 2>::Pointer m_DistanceTransform;

  virtual ScalarValueType PropagationSpeed(
                            const NeighborhoodType& neighborhood,
                            const FloatOffsetType &
                          ) const
    {
      Index<2> idx = neighborhood.GetIndex();
      return m_DistanceTransform->GetPixel(idx);
    }

  virtual ScalarValueType PropagationSpeed(const BoundaryNeighborhoodType
                               &neighborhood, const FloatOffsetType &
                                           ) const
    {
      Index<2> idx = neighborhood.GetIndex();
      return m_DistanceTransform->GetPixel(idx);
    }
};


class MorphFilter : public
DenseFiniteDifferenceImageFilter< Image<float, 2>, Image<float, 2> >
{
public:
  typedef MorphFilter Self;
  
  /** 
   * Smart pointer support for this class.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro( MorphFunction, LevelSetFunction );
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  itkSetMacro(Iterations, unsigned int);

  void SetDistanceTransform(Image<float, 2> *im)
    {
      ((MorphFunction *)(this->GetDifferenceFunction().GetPointer()))
        ->SetDistanceTransform(im);
    }


protected:
  ~MorphFilter() {}
  MorphFilter()
    {
      MorphFunction::Pointer p = MorphFunction::New();
      p->SetPropagationWeight(-1.0);
      p->SetAdvectionWeight(0.0);
      p->SetCurvatureWeight(1.0);
      this->SetDifferenceFunction(p);
      m_Iterations = 0;
    }
  MorphFilter(const Self &) {}
  
private:
  unsigned int m_Iterations; 
  
  virtual bool Halt()
    {
      if (this->GetElapsedIterations() == m_Iterations) return true;
      else return false;
    }  
};

} // end namespace itk
int itkLevelSetFunctionTest(int, char**)
{
  typedef itk::Image<float, 2> ImageType;
  
  const int n = 200;  // Number of iterations
  
  ImageType::Pointer im_init = ImageType::New();
  ImageType::Pointer im_target = ImageType::New();
  
  ImageType::RegionType r;
  ImageType::SizeType   sz = {{HEIGHT, WIDTH}};
  ImageType::IndexType  idx = {{0,0}};
  r.SetSize(sz);
  r.SetIndex(idx);

  im_init->SetLargestPossibleRegion(r);
  im_init->SetBufferedRegion(r);
  im_init->SetRequestedRegion(r);

  im_target->SetLargestPossibleRegion(r);
  im_target->SetBufferedRegion(r);
  im_target->SetRequestedRegion(r);

  im_init->Allocate();
  im_target->Allocate();

  evaluate_function(im_init, circle);
  evaluate_function(im_target, square);

  itk::ImageRegionIterator<ImageType> itr(im_target,
                                          im_target->GetRequestedRegion());

  // Squash level sets everywhere but near the zero set.
  for (itr = itr.Begin(); ! itr.IsAtEnd(); ++itr)
  {
    itr.Value() = itr.Value() /vnl_math_sqrt((5.0f +vnl_math_sqr(itr.Value())));
  
  }
 
  itk::MorphFilter::Pointer mf = itk::MorphFilter::New();
  mf->SetDistanceTransform(im_target);
  mf->SetIterations(n);
  mf->SetInput(im_init);

  mf->Update();

 return 0;
}
