/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSet2DEquationTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include "itkImageRegionIterator.h"
#include "itkLevelSetEquation.h"
#include "itkLevelSet2DEquation.h"
#include "itkDenseFiniteDifferenceImageFilter.h"
//#include "itkRawImageWriter.h"


/*
 * This test exercises the dense p.d.e. solver framework
 * itkDenseFiniteDifferenceImageFilter and the two-dimensional level
 * set surface modeling function object.  It shows how to subclass
 * the DenseFiniteDifferenceImageFilter and the LevelSet2DEquation to
 * create a very simple level set surface evolution application.
 * 
 * This application morphs a circle to a square using the level-set surface
 * modeling framework.  Speed function input to the level-set equation
 * is the square distance transform.
 *
 */
const int HEIGHT = (256);
const int WIDTH  = (256);

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
 * \class MorphEquation
 * Subclasses LevelSet2DEquation, supplying the ``PropagationSpeed'' term.
 * 
 * See LevelSetEquation for more information.
 */
class MorphEquation : public LevelSet2DEquation< Image<float, 2> >
{
public:
  void SetDistanceTransform (Image<float, 2> *d)
    { m_DistanceTransform = d; }
  
  typedef MorphEquation Self;

  typedef LevelSet2DEquation< Image<float, 2> > Superclass;
  typedef Superclass::RadiusType RadiusType;
  
   /** 
   * Smart pointer support for this class.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro( MorphEquation, LevelSet2DEquation );
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

protected:
  ~MorphEquation() {}

  MorphEquation()
    {
      RadiusType r;
      r[0] = r[1] = 1;
      Superclass::Initialize(r);
    }

private:
  Image<float, 2>::Pointer m_DistanceTransform;

  virtual ScalarValueType PropagationSpeed(
                            const NeighborhoodType& neighborhood,
                            const FloatOffsetType
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
  itkTypeMacro( MorphEquation, LevelSet2DEquation );
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  itkSetMacro(Iterations, unsigned int);

  void SetDistanceTransform(Image<float, 2> *im)
    {
      ((MorphEquation *)(this->GetDifferenceEquation().GetPointer()))
        ->SetDistanceTransform(im);
    }


protected:
  ~MorphFilter() {}
  MorphFilter()
    {
      MorphEquation::Pointer p = MorphEquation::New();
      p->SetPropagationWeight(-1.0);
      p->SetAdvectionWeight(0.0);
      p->SetCurvatureWeight(1.0);
      this->SetDifferenceEquation(p);

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
int main()
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

// Uncomment the following lines to see the input and target images

//  itk::RawImageWriter<ImageType>::Pointer writer
//    = itk::RawImageWriter<ImageType>::New();
//  writer->SetFileName("im_target.raw");
//  writer->SetInput(im_target);
//  writer->Write();

//  writer->SetFileName("im_init.raw");
//  writer->SetInput(im_init);
//  writer->Write();
 
  itk::MorphFilter::Pointer mf = itk::MorphFilter::New();
  mf->SetDistanceTransform(im_target);
  mf->SetIterations(n);
  mf->SetInput(im_init);

// Uncomment the following line to set up for multi-processing.
  //  mf->SetNumberOfThreads(2);

 mf->Update();

// Uncomment the following lines to see the output image
//  writer->SetFileName("final.raw");
//  writer->SetInput(mf->GetOutput());
//  writer->Write();


 return 0;
  
}
