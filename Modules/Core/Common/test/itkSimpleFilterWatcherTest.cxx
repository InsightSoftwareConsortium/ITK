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

#include <iostream>
#include "itkSimpleFilterWatcher.h"
#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
  namespace Function
  {
    template< typename TInput, typename TOutput >
    class TanHelper
    {
    public:
      TanHelper() {}
      ~TanHelper() {}
      bool operator!=(const TanHelper & rhs) const
      {
        return this != & rhs;
      }

      /* NOTE: operator== purposefully NOT defined. It is not required
       * as part of the defined specification for a Function.
      bool operator==(const TanHelper & other) const
      {
        return !( *this != other );
      }
      */

      inline TOutput operator()(const TInput & A) const
      { return (TOutput)std::tan( (double)A ); }
    };
  }

  template< typename TInputImage, typename TOutputImage >
  class TanHelperImageFilter:
    public UnaryFunctorImageFilter< TInputImage, TOutputImage,
                             Function::TanHelper< typename TInputImage::PixelType,
                                            typename TOutputImage::PixelType >   >
  {
  public:
    /** Standard class typedefs. */
    typedef TanHelperImageFilter Self;
    typedef UnaryFunctorImageFilter<
      TInputImage, TOutputImage,
      Function::TanHelper< typename TInputImage::PixelType,
                     typename TOutputImage::PixelType > >  Superclass;

    typedef SmartPointer< Self >       Pointer;
    typedef SmartPointer< const Self > ConstPointer;

    /** Method for creation through the object factory. */
    itkNewMacro(Self);

    /** Runtime information support. */
    itkTypeMacro(TanHelperImageFilter,
                 UnaryFunctorImageFilter);

  #ifdef ITK_USE_CONCEPT_CHECKING
    // Begin concept checking
    itkConceptMacro( InputConvertibleToDoubleCheck,
                     ( Concept::Convertible< typename TInputImage::PixelType, double > ) );
    itkConceptMacro( DoubleConvertibleToOutputCheck,
                     ( Concept::Convertible< double, typename TOutputImage::PixelType > ) );
    // End concept checking
  #endif
  protected:
    TanHelperImageFilter() {}
    virtual ~TanHelperImageFilter() ITK_OVERRIDE {}
  private:
    ITK_DISALLOW_COPY_AND_ASSIGN(TanHelperImageFilter);
  };

}

int itkSimpleFilterWatcherTest (int, char*[])
{
  // Test out the code
  typedef itk::SimpleFilterWatcher        WatcherType;
  typedef itk::Image<char,3>              ImageType;
  typedef itk::TanHelperImageFilter<
    ImageType, ImageType>                 FilterType;
  FilterType::Pointer filter = FilterType::New();
  const char * comment = "comment";

  // Test constructor that takes a ProcessObject.
  WatcherType watcher1( filter, comment );

  // Test copy constructor.
  WatcherType watcher2( watcher1 );
  if ( watcher1.GetNameOfClass() != watcher2.GetNameOfClass()
       || watcher1.GetProcess() != watcher2.GetProcess()
       || watcher1.GetSteps() != watcher2.GetSteps()
       || watcher1.GetIterations() != watcher2.GetIterations()
       || watcher1.GetQuiet() != watcher2.GetQuiet()
       || watcher1.GetComment() != watcher2.GetComment() )
    //|| watcher1.GetTimeProbe() != watcher2.GetTimeProbe() )
    {
    std::cout << "Copy constructor failed." << std::endl;
    return EXIT_FAILURE;
    }

  // Test default constructor.
  WatcherType watcher3;

  // Test assignment operator.
  watcher3 = watcher2;
  if ( watcher3.GetNameOfClass() != watcher2.GetNameOfClass()
       || watcher3.GetProcess() != watcher2.GetProcess()
       || watcher3.GetSteps() != watcher2.GetSteps()
       || watcher3.GetIterations() != watcher2.GetIterations()
       || watcher3.GetQuiet() != watcher2.GetQuiet()
       || watcher3.GetComment() != watcher2.GetComment() )
    //|| watcher3.GetTimeProbe() != watcher2.GetTimeProbe() )
    {
    std::cout << "Operator= failed." << std::endl;
    return EXIT_FAILURE;
    }

  // Test GetNameOfClass().
  std::string name = watcher3.GetNameOfClass();
  if ( name != filter->GetNameOfClass() )
    {
    std::cout << "GetNameOfClass failed. Expected: "
              << filter->GetNameOfClass()
              << " but got: "
              << watcher3.GetNameOfClass() << std::endl;
    return EXIT_FAILURE;
    }

  // Test Quiet functions.
  watcher3.QuietOff();
  watcher3.QuietOn();
  watcher3.SetQuiet( false );
  if ( watcher3.GetQuiet() != false )
    {
    std::cout << "GetQuiet() failed." << std::endl;
    return EXIT_FAILURE;
    }

  // Test comment.
  if ( watcher3.GetComment() != comment )
    {
    std::cout << "GetComment() failed." << std::endl;
    return EXIT_FAILURE;
    }

  // Check Pointer
  if ( watcher3.GetProcess() != filter )
    {
    std::cout << "GetProcess() failed." << std::endl;
    return EXIT_FAILURE;
    }

  //Test the SetFunctor operation
  filter->SetFunctor(itk::Function::TanHelper< ImageType::PixelType, ImageType::PixelType >() );

  // Return success.
  std::cout << "SimpleFilterWatcher test PASSED ! " << std::endl;
  return EXIT_SUCCESS;

}
