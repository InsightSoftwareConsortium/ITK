/*=========================================================================
  
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatHistogram.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkStatHistogram_h
#define __itkStatHistogram_h

#include <vector>

#include "itkMacro.h"
#include "itkSize.h"
#include "itkPoint.h"
#include "itkIndex.h"
#include "itkLightObject.h"

namespace itk{

/** \class Histogram 
 *  \brief This abstract class is a building block of histogram.
 *  DenseHistogram and SparseHistogram classes are derived from this class.
 */

template <class TBin, unsigned int HistogramDimension = 1, class TFeature = float> 
class ITK_EXPORT Histogram : public LightObject
{
public:
 /**
  * Standard "Self" typedef.
  */
  typedef Histogram  Self;

 /** 
  * Smart pointer typedef support 
  */
  typedef SmartPointer<Self>   Pointer;

 /**
  * Bin typedef support
  */
  typedef TBin BinType;

 /**
  * Feature typedef support
  */
  typedef TFeature FeatureType;

 /**
  * Dimension of the histogram
  */
  enum { Dimension = HistogramDimension };

 /**
  * Point typedef support
  */ 
  typedef Point<TFeature, HistogramDimension> PointType;
 
 /**
  * Index typedef support
  */ 
  typedef Index<HistogramDimension> IndexType;

 /**
  * Size typedef support
  */ 
  typedef Size<HistogramDimension> SizeType;

 /** 
  * Run-time type information (and related methods).
  */
  itkTypeMacro(Histogram, LightObject);

 /**
  * Method to return the index of histogram corresponding to feature
  */
  IndexType GetIndex(PointType point); 

 /**
  * Method to return dimension of histogram
  */
  //static unsigned int GetHistogramDimension() { return HistogramDimension; }
 
 /**
  * Method to get the number of bins of dimension d
  */
  unsigned long GetDimensionSize(unsigned int dimension) const
    { return this->m_Size[dimension]; }

 /**
  * Method to set the number of bins of dimension d 
  */
  void SetDimensionSize(unsigned int dimension, unsigned long size)
    { this->m_Size[dimension] = size; }

 /**
  * Method to set m_Size 
  */
  void SetSize(SizeType size)
    { this->m_Size = size; }

 /**
  * Method to get m_Size
  */
  SizeType GetSize() { return m_Size; }

 /**
  * Method to get minimum value of n th bin of dimension d
  */
  TFeature GetBinMin(unsigned int dimension, unsigned long nbin) const
    { return this->m_Min[dimension][nbin]; }

 /**
  * Method to get maximum value of n th bin of dimension d
  */
  TFeature GetBinMax(unsigned int dimension, unsigned long nbin) const
    { return this->m_Max[dimension][nbin]; }

 /**
  * Method to set minimum value of n th bin of dimension d
  */
  void SetBinMin(unsigned int dimension, unsigned long nbin, TFeature min)
    { this->m_Min[dimension][nbin] = min; }

 /**
  * Method to set maximum value of n th bin of dimension d
  */
  void SetBinMax(unsigned int dimension, unsigned long nbin, TFeature max)
    { this->m_Max[dimension][nbin] = max; }

 /**
  * Method to get the minimum of the bin corresponding to the gray level of 
  * dimension d.
  */
  TFeature GetBinMinFromValue(unsigned int dimension, const TFeature value ) const;

 /**
  * Method to get the maximum of the bin corresponding to the gray level of 
  * dimension d.
  */
  TFeature GetBinMaxFromValue(unsigned int dimension, const TFeature value ) const;

 /**
  * Method to get the minimum vector of a dimension 
  */
  const std::vector<TFeature> & GetDimensionMins(unsigned int dimension) const
    {  return m_Min[dimension]; }

 /**
  * Method to get the maximum vector of a dimension 
  */
  const std::vector<TFeature> & GetDimensionMaxs(unsigned int dimension) const
    {  return m_Max[dimension]; }

 /**
  * Method to get the minimum vector 
  */
  const std::vector< std::vector<TFeature> > & GetMins() const
    {  return m_Min; }

 /**
  * Method to get the maximum vector 
  */
  const std::vector< std::vector<TFeature> > & GetMaxs() const
    {  return m_Max; }

 /**
  * Method to get mins of each dimension for a point in the histogram
  */
  PointType GetHistogramMinFromValue(const PointType point); 

 /**
  * Method to get maxs of each dimension for a point in the histogram
  */
  PointType GetHistogramMaxFromValue(const PointType point); 

 /**
  * Method to get mins in the histogram by index 
  */
  PointType GetHistogramMinFromIndex(const IndexType index); 

 /**
  *  Method to get maxs in the histogram by index 
  */
  PointType GetHistogramMaxFromIndex(const IndexType index); 

 /**
  * Method to allocate histogram. The m_Size must be set a priori
  */
  virtual void Allocate() = 0;

 /**
  * Method to get the frequency of a bin from the histogram
  */
  virtual const TBin GetFrequency(const IndexType index)=0; 

 /**
  * Method to get the frequency corresponding to gray levels point
  */
  virtual const TBin GetFrequency(const PointType point)=0;

 /**
  * Method to set the frequency of histogram
  */
  virtual void SetFrequency(const IndexType index, const TBin value)=0;
 
 /**
  * Method to set the frequency corresponding to gray levels point
  */
  virtual void SetFrequency(const PointType point, const TBin value)=0;

 /**
  * Method to increase the frequency by one.  This function is convinent
  * to create histogram.
  */
  virtual void AddFrequency(const PointType point, const TBin value)=0;

 /**
  * Method to increase the frequency by one.  This function is convinent
  * to create histogram.
  */
  virtual void AddFrequency(const IndexType index, const TBin value)=0;
                           
 /**
  * Method to get the feature of histogram
  */
  TFeature GetFeature(const IndexType index, int dimension);

 /**
  * Method to get the feature of histogram
  */
  TFeature GetFeature(const PointType point, int dimension);

 /**
  * Method to get the feature of histogram
  */
  PointType GetFeature(const IndexType index);

 /**
  * Method to get the feature of histogram
  */
  PointType GetFeature(const PointType point);

protected:
 
  Histogram() {};
  virtual ~Histogram() {};
  Histogram(const Self&) {};
  void operator=(const Self&) {};

  // The number of bins for each each dimension
  SizeType m_Size;

  // lower bound of each bin
  std::vector< std::vector<TFeature> > m_Min;

  // upper bound of each bin
  std::vector< std::vector<TFeature> > m_Max;

private:

};

} // end of namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatHistogram.txx"
#endif

#endif
