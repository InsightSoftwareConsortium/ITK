/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkListSampleToHistogramGenerator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkListSampleToHistogramGenerator_h
#define __itkListSampleToHistogramGenerator_h

#include "itkObject.h"
#include "itkListSampleBase.h"
#include "itkHistogram.h"
#include "itkStatisticsAlgorithm.h"
#include "itkDenseFrequencyContainer.h"

namespace itk{
namespace Statistics{

/** \class ListSampleToHistogramGenerator
 *  \brief Generates a Histogram using the data from the ListSample object
 *
 * This class is templated over the type of ListSample (which is a subclass of
 * ListSampleBase class), the type of measurement values in the histogram, and
 * The type of the frequency container that will be used to store frequency 
 * in the result histogram.
 *
 * User should plug in the input list sample and specify the number of bins
 * along each dimension. For example, if a measurement vector of 
 * the input list sample has three components, the histogram will be three 
 * dimensional histogram. So, the SetNumberOfBins() function will accepts 
 * itk::Size object with three components.
 * 
 * The upper bound and lower bound of the histogram will be automatically 
 * determined by the FindSampleBound funtion in itkStatisticsAlgorithm.h & 
 * .txx. The hitogram object's upper bound will be slightly bigger than
 * the uppper bound from the FindSampleBound to include the maximum values
 * in the Histogram object. To extend the upper bound of the histogram. this
 * class internally uses the intervals between two bins and the marginal scale
 * constant that you can set using the SetMarginalScale(float) funtion. 
 * The default marginal scale constant value is 100, which means that the 
 * upper bound of Histogram will be upper bound of the list sample + 
 * (bin interval) / 100 ( = marginal scale). 
 *
 * The result historam has equi-size bins along each axe.
 *
 * NOTE: The second template argument, THistogramMeasurement should be
 * float-point type (float or double). 
 *
 * <b> Recent API changes </b>
 * The class is templated over the list sample, the frequency measurement 
 * precision, the length of each measurement vector and optionally over the
 * type of frequency container.
 * 
 * \sa Histogram, ListSampleBase, FindSampleBound 
 */
template< class TListSample, 
          class THistogramMeasurement,  
          class TFrequencyContainer = DenseFrequencyContainer< float >, 
          unsigned int TMeasurementVectorLength = 1 >
class ITK_EXPORT ListSampleToHistogramGenerator :
    public Object
{
public:
  /** Standard typedefs */
  typedef ListSampleToHistogramGenerator Self;
  typedef Object Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ListSampleToHistogramGenerator, Object) ;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;

  /** the number of components in a measurement vector */
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TMeasurementVectorLength);

  typedef Histogram< THistogramMeasurement, 
                     itkGetStaticConstMacro(MeasurementVectorSize),
                     TFrequencyContainer > HistogramType ;

  typedef typename HistogramType::SizeType HistogramSizeType ;
  typedef typename HistogramType::MeasurementVectorType  MeasurementVectorType;

  /** plug in the ListSample object */
  void SetListSample(const TListSample* list)
    { 
    // Throw exception if the length of measurement vectors in the list is not
    // equal to the dimension of the histogram.
    if( list->GetMeasurementVectorSize() != MeasurementVectorSize )
      {
      itkExceptionMacro(<< "Length of measurement vectors in the list sample is "
        << list->GetMeasurementVectorSize() << " but histogram dimension is "
        << MeasurementVectorSize);
      }
    m_List = list ; 
    }

  void SetMarginalScale(float scale)
  { m_MarginalScale = scale ; }

  void SetNumberOfBins(HistogramSizeType sizes)
  { m_Sizes = sizes ; }

  const HistogramType* GetOutput() const
  { return m_Histogram ; }

  void Update() 
  { this->GenerateData() ; }

  itkSetMacro(AutoMinMax,bool);
  itkGetConstReferenceMacro(AutoMinMax,bool);

  void SetHistogramMin(const MeasurementVectorType & histogramMin)
    {
    m_HistogramMin = histogramMin;
    m_AutoMinMax = false;
    
    // Sanity check.. Check to see that container m_HistogramMin has the same
    // length as the length of measurement vectors in the list sample. And the
    // same length as the container over which the list sample is instantiated,
    // if fixed.
    MeasurementVectorTraits::Assert(m_HistogramMin, MeasurementVectorSize, 
        "Length Mismatch: ListSampleToHistogramGenerator::SetHistogramMin");
    if( m_List )
      {
      MeasurementVectorTraits::Assert(m_List->GetMeasurementVectorSize(), 
        MeasurementVectorSize, 
        "Length Mismatch: ListSampleToHistogramGenerator::SetHistogramMin");
      }
    }

  void SetHistogramMax(const MeasurementVectorType & histogramMax)
    {
    m_HistogramMax = histogramMax;
    m_AutoMinMax = false;
    
    // Sanity check.. Check to see that container m_HistogramMax has the same
    // length as the length of measurement vectors in the list sample. And the
    // same length as the container over which the list sample is instantiated,
    // if fixed.
    MeasurementVectorTraits::Assert(m_HistogramMax, MeasurementVectorSize, 
        "Length Mismatch: ListSampleToHistogramGenerator::SetHistogramMin");
    if( m_List )
      {
      MeasurementVectorTraits::Assert(m_List->GetMeasurementVectorSize(), 
        MeasurementVectorSize, 
        "Length Mismatch: ListSampleToHistogramGenerator::SetHistogramMin");
      }
    }


protected:
  ListSampleToHistogramGenerator() ;
  virtual ~ListSampleToHistogramGenerator() {}
  void GenerateData() ;
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  const TListSample* m_List ;
  typename HistogramType::Pointer m_Histogram ;
  HistogramSizeType m_Sizes ;
  float m_MarginalScale ;
  MeasurementVectorType m_HistogramMin;
  MeasurementVectorType m_HistogramMax;
  bool m_AutoMinMax;

} ; // end of class

} // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkListSampleToHistogramGenerator.txx"
#endif

#endif
