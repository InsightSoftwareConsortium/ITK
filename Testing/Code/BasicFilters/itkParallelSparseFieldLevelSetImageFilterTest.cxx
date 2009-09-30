/*=========================================================================
  
Program:   Insight Segmentation & Registration Toolkit
Module:    itkParallelSparseFieldLevelSetImageFilterTest.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include "itkImageRegionIterator.h"
#include "itkLevelSetFunction.h"
#include "itkParallelSparseFieldLevelSetImageFilter.h"

#include "itkImageFileWriter.h"

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
const unsigned int HEIGHT = (64);
const unsigned int WIDTH  = (64);
const unsigned int DEPTH  = (64);

const int RADIUS= (vnl_math_min (vnl_math_min(HEIGHT, WIDTH), DEPTH) / 4);

// Distance transform function for a sphere
float sphere(unsigned int x, unsigned int y, unsigned int z)
{
  float dis;
  dis
    = (x - (float)WIDTH /2.0)*(x - (float)WIDTH /2.0)
    + (y - (float)HEIGHT/2.0)*(y - (float)HEIGHT/2.0)
    + (z - (float)DEPTH /2.0)*(z - (float)DEPTH /2.0) ;
  dis = RADIUS - vcl_sqrt(dis);
  return(-dis);
}

// Distance transform function for a cube
float cube(unsigned int x, unsigned int y, unsigned int z)
{
  float X, Y, Z;
  X = vcl_fabs(x - (float)WIDTH /2.0);
  Y = vcl_fabs(y - (float)HEIGHT/2.0);
  Z = vcl_fabs(z - (float)DEPTH /2.0);
  float dis;
  if (!((X > RADIUS)&&(Y > RADIUS)&&(Z>RADIUS)))
    {
    dis = RADIUS - (vnl_math_max (vnl_math_max(X, Y), Z));
    }
  else
    {
    dis = -sqrt((X - RADIUS)*(X - RADIUS) + (Y - RADIUS)*(Y - RADIUS) + (Z - RADIUS)*(Z - RADIUS));
    }
  return(-dis);
}

// Evaluates a function at each pixel in the itk volume
void evaluate_function(itk::Image<float, 3> *im,
                       float (*f)(unsigned int, unsigned int, unsigned int) )
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
        im->SetPixel (idx, f(x, y, z) );
        }
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
class MorphFunction : public LevelSetFunction< Image<float, 3> >
{
public:
  void SetDistanceTransform (Image<float, 3> *d)
  {
    m_DistanceTransform = d;
  }
  
  typedef MorphFunction Self;

  typedef LevelSetFunction< Image<float, 3> > Superclass;
  typedef Superclass::RadiusType RadiusType;
  typedef Superclass::GlobalDataStruct GlobalDataStruct;
  
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
    r[0] = r[1] = r[2] = 1;
    Superclass::Initialize(r);
  }

private:
  Image<float, 3>::Pointer m_DistanceTransform;
  virtual ScalarValueType PropagationSpeed(
    const NeighborhoodType& neighborhood,
    const FloatOffsetType &,
    GlobalDataStruct * 
    ) const
  {
    Index<3> idx = neighborhood.GetIndex();
    return m_DistanceTransform->GetPixel(idx);
  }
};

class MorphFilter : public
ParallelSparseFieldLevelSetImageFilter< Image<float, 3>, Image<float, 3> >
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

  void SetDistanceTransform(Image<float, 3> *im)
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
  MorphFilter(const Self &); // purposely not implemented
  
private:
  unsigned int m_Iterations; 
  
  virtual bool Halt()
  {
    if (this->GetElapsedIterations() == m_Iterations) return true;
    else return false;
  }  
};

} // end namespace itk

int itkParallelSparseFieldLevelSetImageFilterTest(int argc, char* argv[])
{
  if (argc < 2)
    {
    std::cerr << "Usage: " << argv[0] << " OutputImage [InitImage [TargetImage]]\n";
    return EXIT_FAILURE;
    }

  typedef itk::Image<float, 3> ImageType;
  
  const int n = 100;  // Number of iterations
  const int numOfThreads= 3; // Number of threads to be used
  
  ImageType::Pointer im_init = ImageType::New();
  ImageType::Pointer im_target = ImageType::New();
  
  ImageType::RegionType r;
  ImageType::SizeType   sz = {{HEIGHT, WIDTH, DEPTH}};
  ImageType::IndexType  idx = {{0,0,0}};
  r.SetSize(sz);
  r.SetIndex(idx);
  
  ImageType::PointType origin;
  ImageType::SpacingType spacing;
  ImageType::DirectionType direction;
  origin[0] = 1.0; origin[1] = 10.0; origin[2] = 100.0;
  spacing[0] = 1.0; spacing[1] = 2.0; spacing[2] = 3.0;
  direction.SetIdentity();
  direction(1,1) = -1.0;

  im_init->SetLargestPossibleRegion(r);
  im_init->SetBufferedRegion(r);
  im_init->SetRequestedRegion(r);

  im_init->SetOrigin(origin);
  im_init->SetSpacing(spacing);
  im_init->SetDirection(direction);
  
  im_target->SetLargestPossibleRegion(r);
  im_target->SetBufferedRegion(r);
  im_target->SetRequestedRegion(r);

  im_target->SetOrigin(origin);
  im_target->SetSpacing(spacing);
  im_target->SetDirection(direction);
  
  im_init->Allocate();
  im_target->Allocate();
  
  evaluate_function(im_init, sphere);
  evaluate_function(im_target, cube);
  
  typedef  itk::ImageFileWriter<  ImageType  > WriterType;
  WriterType::Pointer writer = WriterType::New();
  if (argc > 2)
    {
    writer->SetInput (im_init);
    writer->SetFileName (argv[2]);
    writer->Update ();
    }
  if (argc > 3)
    {
    writer->SetInput (im_target);
    writer->SetFileName (argv[3]);
    writer->Update ();
    }
  itk::ImageRegionIterator<ImageType> itr(im_target,
                                          im_target->GetRequestedRegion());
  
  // Squash level sets everywhere but near the zero set.
  for (itr = itr.Begin(); ! itr.IsAtEnd(); ++itr)
    {
    itr.Value() = itr.Value() /vcl_sqrt((5.0f +vnl_math_sqr(itr.Value())));
    }
  
  itk::MorphFilter::Pointer mf = itk::MorphFilter::New();
  mf->SetDistanceTransform(im_target);
  mf->SetIterations(n);
  mf->SetInput(im_init);
  mf->SetNumberOfThreads(numOfThreads);
  mf->SetNumberOfLayers(3);
  
  try
    {
    mf->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    }
  
  mf->GetOutput()->Print(std::cout);

  writer->SetInput (mf->GetOutput());
  writer->SetFileName (argv[1]);
  writer->Update();
  
  std::cout << mf << std::endl << std::flush;
  
  std::cout << "Passed !" << std::endl << std::flush;
  
  return EXIT_SUCCESS;
}
