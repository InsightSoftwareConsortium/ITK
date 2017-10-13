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

/**
 * Demonstration of filter dispatching.  The general problem is to provide
 * filters with specialized implementations for each dimension, but in
 * only one class, and without partial specialization.  Consider a filter
 * with separate methods for 2-d, 3-d, and N-d implementations.  We would
 * like to dispatch a call to the correct implementation from a single
 * Execute() call.
 *
 * The basic challenge here is that a filter that has been instantiated as
 * dimension 3, for example, still has execute methods for 2, 3, and N
 * dimensions.  If the Execute(2-d) is instantiated, it may have compiler
 * errors because it assumes that the class is instantiated for 2 dimensions.
 * This means we can't have an Execute() method which tests the dimension
 * at run-time and calls the appropriate Execute(*-d) method because it will
 * instantiate all the Execute(*-d) methods, when only one will compile
 * properly.
 *
 * The solution presented below will allow a single Execute() call to
 * instantiate and call only the correct Execute(*-d) method for a particular
 * filter instantiation.
 */

#include "itkImageToImageFilter.h"
#include <sstream>

/**
 * \class An example filter.
 */
template <typename TInputImage, typename TOutputImage>
class ExampleImageFilter:
  public itk::ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ExampleImageFilter Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef itk::ImageToImageFilter<TInputImage, TOutputImage> Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef itk::SmartPointer<Self>       Pointer;
  typedef itk::SmartPointer<const Self> ConstPointer;

  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;
  enum { ImageDimension = InputImageType::ImageDimension };

  void Update(void) ITK_OVERRIDE;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

protected:
  ExampleImageFilter() {}
  ExampleImageFilter(const Self&) {}
  void operator=(const Self&) {}
  virtual ~ExampleImageFilter() ITK_OVERRIDE {}

private:
  /**
   * Dispatch class base allows automatic use of general implementation
   * when no specific dispatch rules match.
   */
  struct DispatchBase {};

  /**
   * Dispatch control class simply holds information in its template
   * parameter(s) that is used to control which Execute() method is chosen.
   */
  template <unsigned long V>
  struct Dispatch: public DispatchBase {};

  void Execute(const DispatchBase&);
  void Execute(Dispatch<2>);
  void Execute(Dispatch<3>);
  void Execute(Dispatch<0>);
};


/**
 * Filter update method called by the user of the filter.
 * This just makes the call to the real method by instantiating the
 * appropriate Dispatch control class.
 */
template <typename TInputImage, typename TOutputImage>
void ExampleImageFilter<TInputImage, TOutputImage>
::Update(void)
{
  this->Execute(Dispatch<ImageDimension>());
}


/**
 * General N-dimensional implementation of example filter.
 * The Dispatch parameter is not used.  It is just used to control
 * instantiation.
 */
template <typename TInputImage, typename TOutputImage>
void ExampleImageFilter<TInputImage, TOutputImage>
::Execute(const DispatchBase&)
{
  std::cout << "General N-d Execute() has been called." << std::endl;

  // Make sure the correct Execute() method has been called.
  if((ImageDimension == 2) || (ImageDimension == 3))
    {
    std::ostringstream err;
    err << "Error: N-d filter implementation called for "
        << ImageDimension
        << "-d filter, even though specific implementation exists."
        << std::endl;
    throw std::string(err.str().c_str());
    }
}


/**
 * 2-dimensional implementation of example filter.
 * The Dispatch parameter is not used.  It is just used to control
 * instantiation.
 */
template <typename TInputImage, typename TOutputImage>
void ExampleImageFilter<TInputImage, TOutputImage>
::Execute(Dispatch<2>)
{
  std::cout << "2d-specific Execute() has been called." << std::endl;

  // Make sure the correct Execute() method has been called.
  if(ImageDimension != 2)
    {
    std::ostringstream err;
    err << "Error: 2-d filter implementation called for "
        << ImageDimension
        << "-d filter." << std::endl;
    throw std::string(err.str().c_str());
    }
}


/**
 * 3-dimensional implementation of example filter.
 * The Dispatch parameter is not used.  It is just used to control
 * instantiation.
 */
template <typename TInputImage, typename TOutputImage>
void ExampleImageFilter<TInputImage, TOutputImage>
::Execute(Dispatch<3>)
{
  std::cout << "3d-specific Execute() has been called." << std::endl;

  // Make sure the correct Execute() method has been called.
  if(ImageDimension != 3)
    {
    std::ostringstream err;
    err << "Error: 3-d filter implementation called for "
        << ImageDimension
        << "-d filter." << std::endl;
    throw std::string(err.str().c_str());
    }
}


/**
 * Zero-dimensional implementation of filter.  This should never be called,
 * or even instantiated.  If a compiler instantiates this, the test will
 * fail to compile.
 */
template <typename TInputImage, typename TOutputImage>
void ExampleImageFilter<TInputImage, TOutputImage>
::Execute(Dispatch<0>)
{
  // this_should_not_have_been_instantiated();
  throw std::string("The 0-Dispatch method should not have been called.");
}


/**
 * Filter dispatch test creates several ExampleImageFilter instantiations
 * and calls them to check if the dispatch rules are working correctly.
 */
int itkFilterDispatchTest(int, char* [] )
{
  bool passed = true;

  // Define an image of each dimension.
  typedef itk::Image<float, 2> Image2d;
  typedef itk::Image<float, 3> Image3d;
  typedef itk::Image<float, 4> Image4d;
  typedef itk::Image<float, 5> Image5d;

  // Define a filter of each dimension.
  typedef ExampleImageFilter<Image2d, Image2d>  Filter2d;
  typedef ExampleImageFilter<Image3d, Image3d>  Filter3d;
  typedef ExampleImageFilter<Image4d, Image4d>  Filter4d;
  typedef ExampleImageFilter<Image5d, Image5d>  Filter5d;

  // Instantiate a filter of each dimension.
  Filter2d::Pointer filter2d = Filter2d::New();
  Filter3d::Pointer filter3d = Filter3d::New();
  Filter4d::Pointer filter4d = Filter4d::New();
  Filter5d::Pointer filter5d = Filter5d::New();

  // Try running each of the filters.  If the wrong Execute() method is
  // invoked by one of these calls, a std::string() exception will be
  // thrown with the error description.
  try
    {
    std::cout << "Executing 2-d filter: ";
    filter2d->Update();

    std::cout << "Executing 3-d filter: ";
    filter3d->Update();

    std::cout << "Executing 4-d filter: ";
    filter4d->Update();

    std::cout << "Executing 5-d filter: ";
    filter5d->Update();
    }
  catch (std::string & err)
    {
    std::cout << err;
    passed = false;
    }

  if(passed)
    {
    std::cout << "The test has passed." << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "The test has failed." << std::endl;
    return EXIT_FAILURE;
    }
}
