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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "itkImage.h"
#include "itkImageFunction.h"
#include "vcl_limits.h"
#include "itkMath.h"

using namespace itk;
namespace {

template< class TInputImage, class TCoordRep = double >
class ITK_EXPORT TestImageFunction:
  public ImageFunction< TInputImage,
                        ITK_TYPENAME NumericTraits<
                          typename TInputImage::PixelType >::RealType,
                        TCoordRep >
{
public:

  /** Standard class typedefs. */
  typedef TestImageFunction                   Self;
  typedef ImageFunction< TInputImage,
                         typename NumericTraits<
                         typename TInputImage::PixelType >::RealType,
                         TCoordRep >
                                              Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(TestImageFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType     OutputType;

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType      IndexType;
  typedef typename Superclass::IndexValueType IndexValueType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Evaluate the function at specified Point position.*/
  virtual OutputType Evaluate(const PointType & itkNotUsed(point) ) const
    {
    OutputType result(0);
    return result;
    }

  /** Evaluate the function at specified Index position. */
  virtual OutputType EvaluateAtIndex(const IndexType & itkNotUsed(index) ) const
    {
    OutputType result(0);
    return result;
    }

  /** Evaluate the function at specified ContinuousIndex position. */
  virtual OutputType EvaluateAtContinuousIndex( const ContinuousIndexType & itkNotUsed(index) ) const
    {
    OutputType result(0);
    return result;
    }

protected:
  TestImageFunction(){};
  ~TestImageFunction(){};
  void PrintSelf(std::ostream & os, Indent indent) const
   {
   this->Superclass::PrintSelf(os, indent);
   }

private:
  TestImageFunction(const Self &); //purposely not implemented
  void operator=(const Self &);                 //purposely not implemented

};

}//namespace

/****************************************/

int itkImageFunctionTest( int , char*[] )
{

  bool result = EXIT_SUCCESS;

  const   unsigned int                                  Dimension = 3;
  typedef float                                         PixelType;
  typedef Image< PixelType, Dimension >                 ImageType;
  typedef ImageType::RegionType                         RegionType;
  typedef RegionType::SizeType                          SizeType;
  typedef ImageType::IndexType                          IndexType;

  typedef float                                         CoordRepType;
  typedef ContinuousIndex<CoordRepType, Dimension>      ContinuousIndexType;
  typedef Point<CoordRepType, Dimension>                PointType;

  typedef NumericTraits<IndexType::IndexValueType>
                                              IndexNumericTraits;
  typedef NumericTraits<ContinuousIndexType::ValueType>
                                              ContinuousIndexNumericTraits;
  typedef NumericTraits<PointType::ValueType>
                                              PointNumericTraits;


  typedef TestImageFunction< ImageType, CoordRepType >  FunctionType;

  ImageType::Pointer image = ImageType::New();

  IndexType start;
  start.Fill( 1 );
  SizeType size;
  size[0] = 3;
  size[1] = 4;
  size[2] = 5;
  RegionType region;
  region.SetSize( size );
  region.SetIndex( start );

  image->SetRegions( region );
  image->Allocate();

  ImageType::PointType     origin;
  ImageType::SpacingType   spacing;
  origin.Fill( 0.0 );
  spacing.Fill( 1.0 );

  image->SetOrigin( origin );
  image->SetSpacing( spacing );

  image->Print( std::cout );

  FunctionType::Pointer function = FunctionType::New();

  /* Test SetInputImage & Accessors */
  function->SetInputImage( image );
  IndexType endIndex = function->GetEndIndex();
  IndexType startIndex = function->GetStartIndex();
  ContinuousIndexType endIndexC = function->GetEndContinuousIndex();
  ContinuousIndexType startIndexC = function->GetStartContinuousIndex();

  for ( unsigned int j = 0; j < Dimension; j++ )
    {
    if( startIndex[j]    != start[0]            ||
          endIndex[j]    !=
            static_cast<IndexType::IndexValueType>(start[0]+size[j] -1) ||
          startIndexC[j] != start[0]-0.5        ||
            endIndexC[j] != start[0]+size[j] -0.5 )
        {
        std::cout << "Error in SetInputImage with index bounds." << std::endl;
        return EXIT_FAILURE;
        }
    }

  /* GetInputImage */
  ImageType::ConstPointer returnedImage = function->GetInputImage();
  if( returnedImage.IsNull() )
    {
    std::cout << "Error with GetInputImage." << std::endl;
    return EXIT_FAILURE;
    }

  /* Evaluate* methods - skip since they're pure virtual. */

  /*
   * IsInsideBuffer methods.
   */

  /* IsInsideBuffer with Integer index type */
  IndexType index;
  index = startIndex;
  if( !function->IsInsideBuffer( index ) )
    {
    std::cout << "Error with IsInsideBuffer 1." << std::endl;
    result = EXIT_FAILURE;
    }
  index = endIndex;
  if( !function->IsInsideBuffer( index ) )
    {
    std::cout << "Error with IsInsideBuffer 2." << std::endl;
    result = EXIT_FAILURE;
    }
  index[0] = startIndex[0]-1;
  if( function->IsInsideBuffer( index ) )
    {
    std::cout << "Error with IsInsideBuffer 3. Expected false." << std::endl;
    result = EXIT_FAILURE;
    }
  index[0] = IndexNumericTraits::max();
  if( function->IsInsideBuffer( index ) )
    {
    std::cout << "Error with IsInsideBuffer 4. Expected false." << std::endl;
    result = EXIT_FAILURE;
    }
  if( IndexNumericTraits::is_signed )
    {
    index[0] = IndexNumericTraits::min();
    if( function->IsInsideBuffer( index ) )
      {
      std::cout << "Error with IsInsideBuffer 5. Expected false." << std::endl;
      result = EXIT_FAILURE;
      }
    }

  /* IsInsideBuffer with Continuous index type */
  ContinuousIndexType indexC;
  indexC = startIndexC;
  if( !function->IsInsideBuffer( indexC ) )
    {
    std::cout << "Error with IsInsideBuffer 1C." << std::endl;
    result = EXIT_FAILURE;
    }
  indexC[0] = endIndexC[0] - 0.1;
  indexC[1] = endIndexC[1] - 0.1;
  indexC[2] = endIndexC[2] - 0.1;
  if( !function->IsInsideBuffer( indexC ) )
    {
    std::cout << "Error with IsInsideBuffer 2C." << std::endl;
    result = EXIT_FAILURE;
    }
  indexC[0] = startIndexC[0]-1;
  if( function->IsInsideBuffer( indexC ) )
    {
    std::cout << "Error with IsInsideBuffer 3C. Expected false." << std::endl
              << "  indexC: " << indexC << std::endl
              << "  start/end continuous indecies: "
              << startIndexC << " " << endIndexC << std::endl;
    result = EXIT_FAILURE;
    }
  indexC[0] = ContinuousIndexNumericTraits::max();
  if( function->IsInsideBuffer( indexC ) )
    {
    std::cout << "Error with IsInsideBuffer 4C. Expected false." << std::endl;
    result = EXIT_FAILURE;
    }
  indexC[0] = ContinuousIndexNumericTraits::min();
  if( function->IsInsideBuffer( indexC ) )
    {
    std::cout << "Error with IsInsideBuffer 5C. Expected false." << std::endl;
    result = EXIT_FAILURE;
    }
  if( ContinuousIndexNumericTraits::has_quiet_NaN )
    {
    indexC[0] = ContinuousIndexNumericTraits::quiet_NaN();
    if( function->IsInsideBuffer( indexC ) )
      {
      std::cout << "Error with IsInsideBuffer 6C. Expected false." << std::endl;
      result = EXIT_FAILURE;
      }
    }
  /* Note that signaling_NaN seems to simply wrap quiet_NaN */
  if( ContinuousIndexNumericTraits::has_signaling_NaN )
    {
    indexC[0] = ContinuousIndexNumericTraits::signaling_NaN();
    if( function->IsInsideBuffer( indexC ) )
      {
      std::cout << "Error with IsInsideBuffer 7C. Expected false." << std::endl;
      result = EXIT_FAILURE;
      }
    }
  indexC[0] = ContinuousIndexNumericTraits::infinity();
  if( function->IsInsideBuffer( indexC ) )
    {
    std::cout << "Error with IsInsideBuffer 8C. Expected false." << std::endl;
    result = EXIT_FAILURE;
    }

  /* IsInsideBuffer with Point type */
  PointType point;
  point.Fill(1);
  if( !function->IsInsideBuffer( point ) )
    {
    std::cout << "Error with IsInsideBuffer 1P." << std::endl;
    result = EXIT_FAILURE;
    }
  point[0] = endIndexC[0] - 0.1;
  point[1] = endIndexC[1] - 0.1;
  point[2] = endIndexC[2] - 0.1;
  if( !function->IsInsideBuffer( point ) )
    {
    std::cout << "Error with IsInsideBuffer 2P." << std::endl;
    result = EXIT_FAILURE;
    }
  point[0] = startIndex[0]-1;
  if( function->IsInsideBuffer( point ) )
    {
    std::cout << "Error with IsInsideBuffer 3P. Expected false." << std::endl;
    result = EXIT_FAILURE;
    }
  point[0] = PointNumericTraits::max();
  if( function->IsInsideBuffer( point ) )
    {
    std::cout << "Error with IsInsideBuffer 4P. Expected false." << std::endl;
    result = EXIT_FAILURE;
    }
  point[0] = PointNumericTraits::min();
  if( function->IsInsideBuffer( point ) )
    {
    std::cout << "Error with IsInsideBuffer 5P. Expected false." << std::endl;
    result = EXIT_FAILURE;
    }
  if( PointNumericTraits::has_quiet_NaN )
    {
    std::cout << "Testing IsInsideBuffer(point) with quiet_NaN." << std::endl;
    point[0] = PointNumericTraits::quiet_NaN();
    if( function->IsInsideBuffer( point ) )
      {
      std::cout << "Error with IsInsideBuffer 6P. Expected false." << std::endl;
      result = EXIT_FAILURE;
      }
    }
  if( PointNumericTraits::has_signaling_NaN )
    {
    std::cout << "Testing IsInsideBuffer(point) with signaling_NaN."
              << std::endl;
    point[0] = PointNumericTraits::signaling_NaN();
    if( function->IsInsideBuffer( point ) )
      {
      std::cout << "Error with IsInsideBuffer 7P. Expected false." << std::endl;
      result = EXIT_FAILURE;
      }
    }
  point[0] = PointNumericTraits::infinity();
  if( function->IsInsideBuffer( point ) )
    {
    std::cout << "Error with IsInsideBuffer 8P. Expected false." << std::endl;
    result = EXIT_FAILURE;
    }

  /* NaN behavior
   * Experimenting. Can be removed before final merge.
   * Issue with ImageRegion::IsInside is that it's casting to integer and
   * rounding, and thus NaN's get converted to a large negative int and
   * are only caught indirectly. */

  //GetLargestPossibleRegion().IsInside(indexC) seems to be catching the NaN with
  // the same logic as old ImageFunction::IsInsideBuffer...why?
  std::cout << "NaN test..." << std::endl;
  indexC.Fill(1);
  //NOTE that 0,0,0 is NOT inside.
  if( image->GetLargestPossibleRegion().IsInside(indexC) )
    std::cout << "1,1,1 IsInside" << std::endl;
  else
    std::cout << "1,1,1 Is not inside !" << std::endl;

  indexC[0] = ContinuousIndexNumericTraits::quiet_NaN();
  if( image->GetLargestPossibleRegion().IsInside(indexC) )
    std::cout << "NaN,1,1 IsInside" << std::endl;
  else
    std::cout << "NaN,1,1 Is not inside" << std::endl;

  std::cout << "NaN < -1 = " << (indexC[0] < -1.0) << std::endl;
  std::cout << "NaN > -1 = " << (indexC[0] > -1.0) << std::endl;
  std::cout << "!(NaN < 1) = " << !(indexC[0] < 1.0) << std::endl;
  std::cout << "!(NaN > 1) = " << !(indexC[0] > 1.0) << std::endl;

  float NaN = ContinuousIndexNumericTraits::quiet_NaN();
  std::cout << "Math::RoundHalfIntegerUp< float >(NaN) < static_cast<float> (1): "
    << ( Math::RoundHalfIntegerUp< float >(NaN) < static_cast<float> (1) )
    << std::endl;
  std::cout << "Math::RoundHalfIntegerUp< float >(NaN) > static_cast<float> (1): "
    << ( Math::RoundHalfIntegerUp< float >(NaN) > static_cast<float> (1) )
    << std::endl;
  std::cout << "RoundHalfIntegerUp(Nan): " << Math::RoundHalfIntegerUp< CoordRepType >(NaN) << std::endl;
  CoordRepType rf = Math::RoundHalfIntegerUp< CoordRepType >(NaN);
  std::cout << "CoordRepType = RoundHalfIntegerUp(NaN): " << rf << std::endl;
  long rl = Math::RoundHalfIntegerUp< CoordRepType >(NaN);
  std::cout << "long type = RoundHalfIntegerUp(NaN): " << rl << std::endl;
  std::cout << "static_cast<long>( NaN ): " << static_cast<long> (NaN) << std::endl;
  std::cout << "NumericTraits<ImageType::RegionType::IndexValueType>::min(): "
    << NumericTraits<ImageType::RegionType::IndexValueType>::min() << std::endl;
  std::cout << "CoordRepType min(): " << ContinuousIndexNumericTraits::min() << std::endl;
  std::cout << "...end NaN tests." << std::endl << std::endl;

  /* ConvertPointToNearestIndex
   * With region origin of 0,0,0 and spacing of 1,1,1,
   * this is straight-forward */
  point[0]=1.1;
  point[1]=1.2;
  point[2]=1.3;
  function->ConvertPointToNearestIndex( point, index );
  if( index[0] != 1 || index[1] != 1 || index[2] != 1 )
    {
    std::cout << "Error with ConvertPointToNearestIndex." << std::endl
              << "point: " << point << " index: " << index << std::endl;
    result = EXIT_FAILURE;
    }

  /* Test with NaN to see what happens */
  if( PointNumericTraits::has_quiet_NaN )
    {
    point[0] = PointNumericTraits::quiet_NaN();
    //NOTE: this calls Image::TransformPhysicalPointToContinuousIndex
    // but doesn't check return for true/false, and doesn't return
    // true or false.
    function->ConvertPointToNearestIndex( point, index );
    std::cout << "ConvertPointToNearestIndex with quiet_NaN: "
              << index << std::endl;
    }

  /* Test with infinity to see what happens */
  point[0] = PointNumericTraits::infinity();
  function->ConvertPointToNearestIndex( point, index );
  std::cout << "ConvertPointToNearestIndex with infinity: "
            << index << std::endl;

  /* ConvertPointToContinuousIndex */
  point[0]=1.1;
  point[1]=1.2;
  point[2]=1.3;
  function->ConvertPointToContinuousIndex( point, indexC );
  std::cout << "ConvertPointToContinuousIndex." << std::endl
            << "  point: " << point << " indexC: " << indexC << std::endl;
  if( indexC[0] != point[0] || indexC[1] != point[1] || indexC[2] != point[2] )
    {
    std::cout << "Error with ConvertPointToContinuousIndex." << std::endl;
    result = EXIT_FAILURE;
    }

  /* ConvertContinuousIndexToNearestIndex */
  indexC[0]=1.1;
  indexC[1]=1.2;
  indexC[2]=1.3;
  function->ConvertContinuousIndexToNearestIndex( indexC, index );
  std::cout << "ConvertContinuousIndexToNearestIndex." << std::endl
            << "  indexC: " << indexC << " index: " << index << std::endl;
  if( index[0] != 1 || index[1] != 1 || index[2] != 1 )
    {
    std::cout << "Error with ConvertContinuousIndexToNearestIndex." << std::endl;
    result = EXIT_FAILURE;
    }

  /* Test with NaN to see what happens */
  if( ContinuousIndexNumericTraits::has_quiet_NaN )
    {
    indexC[0] = ContinuousIndexNumericTraits::quiet_NaN();
    function->ConvertContinuousIndexToNearestIndex( indexC, index );
    std::cout << "ConvertContinuousIndexToNearestIndex with quiet_NaN:" << std::endl
              << "  indexC: " << indexC << " index: " << index << std::endl;
    }

  return result;
}
