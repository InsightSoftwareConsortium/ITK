/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDummyImageToListAdaptorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <time.h>

#include "itkFixedArray.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkPixelTraits.h"

#include "itkImageToListAdaptor.h"
#include "itkScalarToArrayCastImageFilter.h"

#include "itkImage.h"
#include "itkObject.h"
#include "itkMacro.h"
#include "itkPixelTraits.h"

template< class TImage >
class DefaultMeasurementVectorSpecification
{
public:
  typedef typename TImage::PixelType MeasurementVectorType ;
} ; // end of class

template< class TImage >
class FixedArrayMeasurementVectorSpecification
{
public:
  typedef typename TImage::PixelType PixelType ;
  typedef typename itk::PixelTraits< PixelType >::ValueType MeasurementType ;
  itkStaticConstMacro( MeasurementVectorSize, unsigned int, 
                       itk::PixelTraits< PixelType >::Dimension ) ;
  typedef typename itk::FixedArray< MeasurementType, MeasurementVectorSize >
  MeasurementVectorType ;
} ; // end of class

template < class TImage, 
           class TMeasurementVectorSpecification = 
           DefaultMeasurementVectorSpecification< TImage > >
class ITK_EXPORT DummyImageToListAdaptor : 
//   public itk::Statistics::ListSampleBase< 
//   itk::FixedArray< typename MyPixelTraits< typename TImage::PixelType >::ValueType, 
//                    MyPixelTraits< typename TImage::PixelType >::Dimension > >
  public itk::Object
{
public:
  typedef DummyImageToListAdaptor Self ;
  typedef itk::Object Superclass ;

  typedef itk::SmartPointer< Self > Pointer ;
  typedef itk::SmartPointer< const Self > ConstPointer ;

  itkTypeMacro( DummyImageToListAdaptor, Object ) ;
  itkNewMacro( Self ) ;

  typedef typename TImage::PixelType PixelType ;
  typedef typename itk::PixelTraits< PixelType >::ValueType MeasurementType ;
  itkStaticConstMacro( MeasurementVectorSize, unsigned int, 
                       itk::PixelTraits< PixelType >::Dimension ) ;

  typedef typename TMeasurementVectorSpecification::MeasurementVectorType MeasurementVectorType ;

  void SetImage( TImage* image ) ;

  MeasurementVectorType& GetMeasurementVector(int id) ;
  
protected:
  DummyImageToListAdaptor() ;
  
  virtual ~DummyImageToListAdaptor() {}

private:
  DummyImageToListAdaptor(const Self&) ; 
  void operator=(const Self&) ;

  typename TImage::Pointer m_Image ;
  typename TImage::PixelContainer* m_ImageBuffer ;
  bool m_UseBuffer ;
} ;


template< class TImage, class TMeasurementVectorSpecification >
DummyImageToListAdaptor< TImage, TMeasurementVectorSpecification >
::DummyImageToListAdaptor()
{
  m_Image = 0 ;
}

template< class TImage, class TMeasurementVectorSpecification >
void
DummyImageToListAdaptor< TImage, TMeasurementVectorSpecification >
::SetImage( TImage* image )
{
  m_Image = image ;
  m_ImageBuffer = m_Image->GetPixelContainer() ;
  if ( strcmp( m_Image->GetNameOfClass(), "Image" ) != 0 ) 
    {
    m_UseBuffer = false ;
    }
  else
    { 
    m_UseBuffer = true ;
    }
}

template< class TImage, class TMeasurementVectorSpecification >
DummyImageToListAdaptor< TImage, TMeasurementVectorSpecification >::MeasurementVectorType&
DummyImageToListAdaptor< TImage, TMeasurementVectorSpecification >
::GetMeasurementVector(int id)
{
  if( m_UseBuffer )
    {
    return *(reinterpret_cast< MeasurementVectorType* >
             ( &(*m_ImageBuffer)[id] ) ) ;
    }
  else
    {
    return *(reinterpret_cast< MeasurementVectorType* >
             ( &(m_Image->GetPixel( m_Image->ComputeIndex( id ) ) ) ) ) ;
    }
}

int itkDummyImageToListAdaptorTest(int, char* [] ) 
{
  typedef int ScalarPixelType ;
  typedef itk::FixedArray< ScalarPixelType, 1 > ArrayPixelType ;
  typedef itk::Vector< ScalarPixelType, 2 > VectorPixelType ;

  typedef itk::Image< ScalarPixelType, 3 > ScalarImageType ;
  typedef itk::Image< ArrayPixelType, 3 > ArrayImageType ;
  typedef itk::Image< VectorPixelType, 3 > VectorImageType ;

  typedef itk::ImageRegionIterator< ScalarImageType > ScalarImageIteratorType ;
  typedef itk::ImageRegionIterator< ArrayImageType > ArrayImageIteratorType ;
  typedef itk::ImageRegionIterator< VectorImageType > VectorImageIteratorType ;

  itk::Size< 3 > size ;
  size.Fill(100) ;

  ScalarImageType::Pointer scalarImage = ScalarImageType::New() ;
  scalarImage->SetRegions(size) ;
  scalarImage->Allocate() ;

  int count = 0 ;
  ScalarImageIteratorType s_iter = 
    ScalarImageIteratorType(scalarImage, 
                            scalarImage->GetLargestPossibleRegion() ) ;
  while ( !s_iter.IsAtEnd() )
    {
    s_iter.Set(count) ;
    ++count ;
    ++s_iter ;
    }

  VectorImageType::Pointer vectorImage = VectorImageType::New() ;
  vectorImage->SetRegions(size) ;
  vectorImage->Allocate() ;
  count = 0 ;
  VectorImageType::PixelType vPixel ;
  VectorImageIteratorType v_iter = 
    VectorImageIteratorType(vectorImage,
                            vectorImage->GetLargestPossibleRegion()) ;
  while ( !v_iter.IsAtEnd() )
    {
    vPixel[0] = count ;
    vPixel[1] = count ;
    v_iter.Set( vPixel ) ;
    ++count ;
    ++v_iter ;
    }

  typedef DummyImageToListAdaptor< ScalarImageType, 
    FixedArrayMeasurementVectorSpecification< ScalarImageType> 
    > ScalarListType ;
  ScalarListType::Pointer sList = ScalarListType::New() ;
  sList->SetImage( scalarImage ) ;

  typedef DummyImageToListAdaptor< VectorImageType > VectorListType ;
  VectorListType::Pointer vList = VectorListType::New() ;
  vList->SetImage( vectorImage ) ;

  typedef itk::ScalarToArrayCastImageFilter< ScalarImageType, ArrayImageType > CasterType ;
  CasterType::Pointer caster = CasterType::New() ;
  caster->SetInput( scalarImage ) ;
  caster->Update() ;

  typedef itk::Statistics::ImageToListAdaptor< ArrayImageType > ImageListType ;
  ImageListType::Pointer imageList = ImageListType::New() ;
  imageList->SetImage( caster->GetOutput() ) ;

  std::cout << "Testing the each measurement values:" << std::endl ;
  for ( int i = 0 ; i < size[0] * size[1] * size[2] ; ++i )
    {
    if ( sList->GetMeasurementVector( i )[0] != vList->GetMeasurementVector( i )[0] 
         || sList->GetMeasurementVector( i )[0] != imageList->GetMeasurementVector( i )[0] )
      {
      std::cout << "ERROR: measurement mismatch!!!" << std::endl ;
      return EXIT_FAILURE ;
      }
    }
  std::cout << std::endl ;

  
  std::cout << "Scalar image (casted) pixel access performance test:" << std::endl ;
  time_t begin = clock() ;
  for ( int i = 0 ; i < size[0] * size[1] * size[2] ; ++i )
    {
    ArrayPixelType& temp =  imageList->GetMeasurementVector( i )  ;
    }
  time_t end = clock() ;

  std::cout << "time: " 
            << (double(end - begin) /CLOCKS_PER_SEC)
            << " seconds" << std::endl ;

  std::cout << "Scalar image pixel access performance test:" << std::endl ;
  begin = clock() ;
  for ( int i = 0 ; i < size[0] * size[1] * size[2] ; ++i )
    {
    ArrayPixelType& temp =  sList->GetMeasurementVector( i )  ;
    }
  end = clock() ;

  std::cout << "time: " 
            << (double(end - begin) /CLOCKS_PER_SEC) 
            << " seconds" << std::endl ;

  std::cout << "Vector image pixel access performance test:" << std::endl ;
  begin = clock() ;
  for ( int i = 0 ; i < size[0] * size[1] * size[2] ; ++i )
    {
    VectorPixelType& temp =  vList->GetMeasurementVector( i )  ;
    }
  end = clock() ;

  std::cout << "time: " 
            << (double(end - begin) /CLOCKS_PER_SEC) 
            << " seconds" << std::endl ;


  return EXIT_SUCCESS ;
}
