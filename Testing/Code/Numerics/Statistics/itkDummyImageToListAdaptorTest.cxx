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
#include "itkImageAdaptor.h"
#include "itkImageRegionIterator.h"
#include "itkPixelTraits.h"

#include "itkListSampleBase.h"
#include "itkImageToListAdaptor.h"
#include "itkScalarToArrayCastImageFilter.h"

#include "itkImage.h"
#include "itkObject.h"
#include "itkMacro.h"
#include "itkPixelTraits.h"

template < class TImage, class TMeasurementVector = ITK_TYPENAME TImage::PixelType >
class ITK_EXPORT DummyImageToListAdaptor : 
  public itk::Statistics::ListSampleBase< TMeasurementVector >
{
public:
  typedef DummyImageToListAdaptor Self ;
  typedef itk::Statistics::ListSampleBase< TMeasurementVector > Superclass ;

  typedef itk::SmartPointer< Self > Pointer ;
  typedef itk::SmartPointer< const Self > ConstPointer ;

  itkTypeMacro( DummyImageToListAdaptor, Object ) ;
  itkNewMacro( Self ) ;

  typedef typename TImage::PixelType PixelType ;
  typedef typename itk::PixelTraits< PixelType >::ValueType MeasurementType ;
  itkStaticConstMacro( MeasurementVectorSize, unsigned int, 
                       itk::PixelTraits< PixelType >::Dimension ) ;

  typedef TMeasurementVector MeasurementVectorType ;

  unsigned int Size() const 
  { return 1 ;} 
  
  float GetTotalFrequency() const 
  { return 1 ;}
  
  float GetFrequency(const unsigned long& id) const
  { return id ; }

  virtual MeasurementVectorType GetMeasurementVector(const unsigned long& id)
  {
    if( m_UseBuffer )
      {
      return *(reinterpret_cast< MeasurementVectorType* >
               (&(*m_ImageBuffer)[id]))  ;
      }
    else
      {
      return *(reinterpret_cast< MeasurementVectorType* >
               (&(m_Image->GetPixel( m_Image->ComputeIndex( id ))) ) )   ;
      }
  }
  
  void SetImage( TImage* image ) 
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

protected:
  DummyImageToListAdaptor()
  { m_Image = 0 ; }
  
  virtual ~DummyImageToListAdaptor() {}

  typename TImage::Pointer m_Image ;
  typename TImage::PixelContainer* m_ImageBuffer ;
  MeasurementVectorType m_TempVector ;
  bool m_UseBuffer ;

private:
  DummyImageToListAdaptor(const Self&) ; 
  void operator=(const Self&) ;

} ;


template < class TImage >
class ITK_EXPORT DummyScalarImageToListAdaptor : 
  public DummyImageToListAdaptor< TImage, itk::FixedArray< typename TImage::PixelType, 1> >
{
public:
  typedef DummyScalarImageToListAdaptor Self ;
  typedef DummyImageToListAdaptor< 
    TImage, itk::FixedArray< typename TImage::PixelType, 1> > Superclass ;

  typedef itk::SmartPointer< Self > Pointer ;
  typedef itk::SmartPointer< const Self > ConstPointer ;

  itkTypeMacro( DummyScalarImageToListAdaptor, DummyImageToListAdaptor ) ;
  itkNewMacro( Self ) ;

  typedef typename Superclass::MeasurementVectorType MeasurementVectorType ;

  MeasurementVectorType GetMeasurementVector(const unsigned long& id)
  {
    if( m_UseBuffer )
      {
      m_TempVector[0] = (*m_ImageBuffer)[id] ;
      }
    else
      {
      m_TempVector[0] = m_Image->GetPixel( m_Image->ComputeIndex( id ) )  ;
      }
    return m_TempVector  ;
  }
protected:
  DummyScalarImageToListAdaptor() {}
  virtual ~DummyScalarImageToListAdaptor() {}
} ; // end of class 

template< class TImage, class TCoordRep >
struct ImageJointDomainTraits
{
  typedef ImageJointDomainTraits Self ;

  typedef itk::PixelTraits< typename TImage::PixelType > PixelTraitsType ;
  itkStaticConstMacro(Size, 
                      unsigned int, 
                      TImage::ImageDimension +
                      PixelTraitsType::Dimension ) ;
  typedef itk::JoinTraits<
    TCoordRep, 
    typename PixelTraitsType::ValueType > JoinTraitsType ;

  typedef typename JoinTraitsType::ValueType MeasurementType ;

  typedef itk::FixedArray< MeasurementType, itkGetStaticConstMacro(Size) >
  MeasurementVectorType ;
} ; // end of ImageJointDomainTraits

template < class TImage, class TCoordRep >
class ITK_EXPORT DummyJointDomainImageToListAdaptor : 
  public DummyImageToListAdaptor< TImage,
                                  typename ImageJointDomainTraits< 
  TImage, TCoordRep >::MeasurementVectorType >
{
public:
  typedef DummyJointDomainImageToListAdaptor Self ;
  typedef DummyImageToListAdaptor< TImage,
                                   typename ImageJointDomainTraits< TImage, TCoordRep >::MeasurementVectorType > 
  Superclass ;
  
  typedef itk::SmartPointer< Self > Pointer ;
  typedef itk::SmartPointer< const Self > ConstPointer ;

  itkTypeMacro( DummyJointDomainImageToListAdaptor, DummyImageToListAdaptor ) ;
  itkNewMacro( Self ) ;

  typedef typename Superclass::MeasurementType MeasurementType ;
  typedef typename Superclass::MeasurementVectorType MeasurementVectorType ;

  itkStaticConstMacro(RangeDomainDimension, 
                      unsigned int, 
                      itk::PixelTraits< 
                      typename TImage::PixelType >::Dimension) ;
  typedef typename itk::FixedArray< 
    MeasurementType, 
    itkGetStaticConstMacro( RangeDomainDimension ) > 
  RangeDomainMeasurementVectorType ;
  
  MeasurementVectorType GetMeasurementVector(const unsigned long& id)
  {
    typename itk::Point<MeasurementType, TImage::ImageDimension> point ;
    typename TImage::IndexType index = m_Image->ComputeIndex( id ) ;
    m_Image->TransformIndexToPhysicalPoint( index, point ) ;
    
    for ( unsigned int i = 0 ; i < TImage::ImageDimension ; ++i )
      {
      m_TempVector[i] = point[i] ;
      }

    if( m_UseBuffer )
      {
      m_TempVector2 =  
        *(reinterpret_cast< RangeDomainMeasurementVectorType* >
          (&(*m_ImageBuffer)[id]))  ;
      }
    else
      {
      m_TempVector2 = 
        *(reinterpret_cast< RangeDomainMeasurementVectorType* >
          (&(m_Image->GetPixel( m_Image->ComputeIndex( id ) ) ) ) ) ;
      }

    for ( unsigned int i = TImage::ImageDimension ; i < MeasurementVectorType::Length ; ++i )
      {
      m_TempVector[i]= m_TempVector2[i - TImage::ImageDimension] ;
      }
    return m_TempVector ;
  }
protected:
  DummyJointDomainImageToListAdaptor() {}
  virtual ~DummyJointDomainImageToListAdaptor() {}
  RangeDomainMeasurementVectorType m_TempVector2 ;
} ; // end of class 

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
  size.Fill(10) ;

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

  typedef DummyScalarImageToListAdaptor< ScalarImageType > ScalarListType ;
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

  typedef DummyJointDomainImageToListAdaptor< VectorImageType, 
    float > JointDomainImageListType ;
  JointDomainImageListType::Pointer jointDomainImageList = 
    JointDomainImageListType::New() ;
  jointDomainImageList->SetImage( vectorImage ) ;

  std::cout << "Testing the each measurement values:" << std::endl ;
  for ( unsigned long i = 0 ; i < size[0] * size[1] * size[2] ; ++i )
    {
    if ( sList->GetMeasurementVector( i )[0] != 
         vList->GetMeasurementVector( i )[0] 
         || sList->GetMeasurementVector( i )[0] != 
         imageList->GetMeasurementVector( i )[0] 
         || sList->GetMeasurementVector( i )[0] != 
         jointDomainImageList->GetMeasurementVector( i )[3] 
      )
      {
      std::cout << "ERROR: measurement mismatch!!!" << std::endl ;
      return EXIT_FAILURE ;
      }
    }
  std::cout << std::endl ;

  
  std::cout << "Scalar image (casted) pixel access performance test:" << std::endl ;
  time_t begin = clock() ;
  for ( unsigned long i = 0 ; i < size[0] * size[1] * size[2] ; ++i )
    {
    imageList->GetMeasurementVector( i )  ;
    }
  time_t end = clock() ;

  std::cout << "time: " 
            << (double(end - begin) /CLOCKS_PER_SEC)
            << " seconds" << std::endl ;

  std::cout << "Scalar image pixel access performance test:" << std::endl ;
  begin = clock() ;
  for ( unsigned long i = 0 ; i < size[0] * size[1] * size[2] ; ++i )
    {
    sList->GetMeasurementVector( i )  ;
    }
  end = clock() ;

  std::cout << "time: " 
            << (double(end - begin) /CLOCKS_PER_SEC) 
            << " seconds" << std::endl ;

  std::cout << "Vector image pixel access performance test:" << std::endl ;
  begin = clock() ;
  for ( unsigned long i = 0 ; i < size[0] * size[1] * size[2] ; ++i )
    {
    vList->GetMeasurementVector( i )  ;
    }
  end = clock() ;

  std::cout << "time: " 
            << (double(end - begin) /CLOCKS_PER_SEC) 
            << " seconds" << std::endl ;

  std::cout << "Joint Domain Vector image pixel access performance test:" << std::endl ;
  begin = clock() ;
  for ( unsigned long i = 0 ; i < size[0] * size[1] * size[2] ; ++i )
    {
    std::cout << jointDomainImageList->GetMeasurementVector( i ) 
              << std::endl ;
    }
  end = clock() ;

  std::cout << "time: " 
            << (double(end - begin) /CLOCKS_PER_SEC) 
            << " seconds" << std::endl ;

  return EXIT_SUCCESS ;
}
