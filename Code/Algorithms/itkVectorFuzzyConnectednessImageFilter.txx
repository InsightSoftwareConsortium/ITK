/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorFuzzyConnectednessImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkVectorFuzzyConnectednessImageFilter_txx
#define _itkVectorFuzzyConnectednessImageFilter_txx
#include "itkVectorFuzzyConnectednessImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkStatisticsImageFilter.h"
#include "itkConstNeighborhoodIterator.h"

namespace itk {

/*
 *
 */
template <class TInputImage, class TOutputImage>
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::VectorFuzzyConnectednessImageFilter()
  : m_CirclePointsLoc(0),
    m_CirclePointsNum(0),
    m_SuppressBckgFlag(0),
    m_NumberOfObjects(1),
    m_Threshold(0.0)
{
  m_HomoCovariance.SetIdentity();
  m_ObjectCovariances.resize(m_NumberOfObjects);
  m_ObjectMeans.resize(m_NumberOfObjects);
  m_ObjectSeeds.resize(m_NumberOfObjects);
  for(int i = 0;i<m_NumberOfObjects;i++)
    {
      m_ObjectCovariances[i].SetIdentity();
      m_ObjectMeans[i].Fill(0);
    }
}

/*
 *
 */
template <class TInputImage, class TOutputImage>
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::~VectorFuzzyConnectednessImageFilter()
{
  m_CirclePointsNum.resize(0);
  m_CirclePointsLoc.resize(0);
  m_ObjectSeeds.resize(0);
  m_ObjectMeans.resize(0);
  m_ObjectCovariances.resize(0);
}

/*
 *
 */
template <class TInputImage, class TOutputImage>
void
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::SetHomogeneityMatrix(const DoubleMatrixType homo_max)
{
  m_HomoCovariance = homo_max.GetInverse();
}


/*
 *
 */
template <class TInputImage, class TOutputImage>
void
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::SetObjectsMatrix(const DoubleMatrixType object_max,const int object_num)
{
  
  m_ObjectCovariances[object_num] = object_max.GetInverse();
}


/*
 *
 */
template <class TInputImage, class TOutputImage>
void 
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::SetObjectsMean(const DoubleVectorType mean, const int object_num)
{
  m_ObjectMeans[object_num] = mean;
}

/*
 *
 */
template <class TInputImage, class TOutputImage>
void 
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::SetObjectsSeed(const IndexType &seed, const int object_num)
{
  m_ObjectSeeds[object_num].push_front(seed);
}

/*
 *
 */
template <class TInputImage, class TOutputImage>
void 
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::Initialization()
{
  m_CirclePointsNum.resize(MAX_SCALE+1);
  m_CirclePointsLoc.resize(MAX_SCALE+1);

  m_ObjectSeeds.resize(m_NumberOfObjects);
  m_ObjectMeans.resize(m_NumberOfObjects);
  m_ObjectCovariances.resize(m_NumberOfObjects);
  for(int i = 0;i<m_NumberOfObjects;i++)
    {
      m_ObjectSeeds[i].clear();
      m_ObjectCovariances[i].Fill(0);
      m_ObjectMeans[i].Fill(0);
    }
}


/*
 *
 */
template <class TInputImage, class TOutputImage>
void 
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::ScalePrepare()
{
  double sum[VectorDimension];
  InputVectorType            value;
  double anisotropy_row,anisotropy_col;
  double const *spaceing;
  int ppptti1[2*(MAX_SCALE+5)][2*(MAX_SCALE+5)];
  TDVector location;

  spaceing = m_InputImage->GetSpacing();

  anisotropy_col = spaceing[0];
  anisotropy_row = spaceing[1];

  double tt1 = anisotropy_col;
  if(tt1>anisotropy_row)
    tt1 = anisotropy_row;

  anisotropy_col = anisotropy_col/tt1;
  anisotropy_row = anisotropy_row/tt1;

  for(int i=0;i<2*(MAX_SCALE+5);i++)
    {
      for(int j=0;j<2*(MAX_SCALE+5);j++)
        { ppptti1[i][j] = 0;}
    }
  for(int i = 0;i<=MAX_SCALE;i++)
    {m_CirclePointsNum[i] = 0;}

  int tti1 = MAX_SCALE + 5; 
  for (int k = 0; k <= MAX_SCALE; k++)
    {
      for (int i = -k - 2; i <= k + 2; i++)
        {
          for (int j = -k - 2; j <= k + 2; j++)
            {
              if (ppptti1[tti1 + i][tti1 + j] == 0)
                {
                  tt1 = sqrt(pow(((double) i) * anisotropy_row,2.0) + pow(((double) j) * anisotropy_col, 2.0));
                  if (tt1 <= ((double) k) + 0.5)
                    {
                      m_CirclePointsNum[k] = m_CirclePointsNum[k] + 1;
                      ppptti1[tti1 + i][tti1 + j] = 2;
                    }
                }
            }
        }

      m_CirclePointsLoc[k].resize(m_CirclePointsNum[k]);

      int tti2 = 0;
      for (int i = -k - 2; i <= k + 2; i++)
        { 
          for (int j = -k - 2; j <= k + 2; j++)
            {
              if (ppptti1[(MAX_SCALE+5) + i][(MAX_SCALE+5) + j] == 2)
                {
                  ppptti1[(MAX_SCALE+5) + i][(MAX_SCALE+5) + j] = 1;
                  location = j,i;
                  m_CirclePointsLoc[k][tti2] = location;
                  tti2 = tti2+1;
                }
            }
        }
    }

  for(unsigned int i = 0;i<VectorDimension;i++)
    {sum[i] = 0;}

  ImageRegionConstIterator<TInputImage> it(m_InputImage, m_InputImage->GetRequestedRegion());
  it.GoToBegin();
  while(!it.IsAtEnd())
    {
      value = it.Get();
      for(unsigned int i = 0;i<VectorDimension;i++)
        {sum[i] = sum[i] + value[i];}
      ++it;
    }
  int volume_size = 1;
  for(unsigned int i = 0;i<ImageDimension;i++)
    {volume_size = volume_size*static_cast<int>(m_Size[i]);}

  for(unsigned int i = 0;i<VectorDimension;i++)
    {m_Mean[i] = static_cast<int>(sum[i]/volume_size);}
}


template <class TInputImage, class TOutputImage>
void
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::Compute_Scale()
{
  const int Tolerance = 13;
  int flag;
  IndexType  index1,index2;
  InputVectorType value1,value2, mean_neighbor;
  double count_obj,count_nonobj,result;
  itk::Vector<double,VectorDimension> sum_vector;

  typename itk::ConstNeighborhoodIterator<InputImageType>::RadiusType radius;
  radius.Fill(0); 
  radius[0] = 1; // radius along x
  radius[1] = 1; // radius along y

  itk::ConstNeighborhoodIterator<InputImageType> iterN(radius,m_InputImage, m_InputImage->GetRequestedRegion());
  unsigned int neighborhoodSize = iterN.Size();  
  ImageRegionConstIterator <InputImageType> it(m_InputImage,m_InputImage->GetRequestedRegion());
 
  int volume_size = 1;
  for(unsigned int i = 0;i<ImageDimension;i++)
    {volume_size = volume_size*static_cast<int>(m_Size[i]);}

  m_ScaleArray.resize(volume_size);
  std::vector<char>::iterator itscale;

  it.GoToBegin();
  iterN.GoToBegin();
  itscale = m_ScaleArray.begin();

  for(;!it.IsAtEnd(); ++it,++itscale,++iterN)
    {
      value1 = it.Get();
      if((m_SuppressBckgFlag==1)&&(value1[0]<m_Mean[0]))
        *itscale = 1;
      else
        {
          sum_vector.Fill(0);
          for (int i = 0; i < neighborhoodSize; ++i)
            {
              value2 = iterN.GetPixel(i);
              for (unsigned int j = 0;j<VectorDimension; j++)
                {
                  sum_vector[j] += static_cast<double>(value2[j]);
                }
            }
          for (unsigned int j = 0;j<VectorDimension; j++)
            {
              mean_neighbor[j] = static_cast<typename InputVectorType::ValueType>(sum_vector[j]/(double)neighborhoodSize);
            }
          flag = 0;
          index1 = it.GetIndex();
          int scale_value;
          
          for ( scale_value = 1; scale_value < MAX_SCALE && !flag; scale_value++)
            {
              count_obj = 0.0;
              count_nonobj = 0.0;
              for (int i = 0; i < m_CirclePointsNum[scale_value]; i++)
                {
                  int x = index1[0] + m_CirclePointsLoc[scale_value][i][0];
                  int y = index1[1] + m_CirclePointsLoc[scale_value][i][1];
                  if (x < 0 || x >= m_Size[0])
                    {x = index1[0];}
                  if (y < 0 || y >= m_Size[1])
                    {y = index1[1];}
                  index2 = index1;
                  index2[1] = y;
                  index2[0] = x;
                  value2 = m_InputImage->GetPixel(index2);

                  for(unsigned int j=0;j<VectorDimension;j++)
                    {
                      value1[j] = vnl_math_abs( value2[j] - mean_neighbor[j]);
                    } 
                  for(unsigned int xx = 0;xx<VectorDimension;xx++)
                    {
                      sum_vector[xx] = 0;
                      for(unsigned int yy = 0;yy<VectorDimension;yy++)
                        {
                          sum_vector[xx] = sum_vector[xx] + value1[yy]*m_HomoCovariance[xx][yy];
                        }
                    }
                  result = 0; 
                  for(unsigned int xx = 0;xx<VectorDimension;xx++)
                    {
                      result = result + sum_vector[xx]*value1[xx];
                    }
                  count_obj = count_obj + exp(-0.5*result/9.0);
                  count_nonobj = count_nonobj + 1.0 - exp(-0.5*result/9.0);
                }

              if (100.0 * count_nonobj >= Tolerance * (count_nonobj + count_obj)) 
                {
                  *itscale = scale_value;
                  flag = 1;
                }
            }
          if (!flag)
            {*itscale = scale_value;}
        }
    }
  /* printf("Scale computation is done! \n"); */
}

template <class TInputImage, class TOutputImage>
void 
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::Compute_Filter()
{
  unsigned int i, j;
  int k,x,y;
  char iscale;
  double tt1,tt2,inv_k,count;
  double weight[MAX_SCALE][MAX_SCALE];
  IndexType index,index1;
  InputVectorType  value,zeroValue,mean;
  double sum[VectorDimension];

  m_FilterImage = InputImageType::New();
  index.Fill(0);
  typename InputImageType::RegionType region;

  region.SetSize(m_Size);
  region.SetIndex(index);

  m_FilterImage->SetRegions( region );
  m_FilterImage->SetSpacing(m_InputImage->GetSpacing());
  m_FilterImage->Allocate();

  ImageRegionConstIterator <InputImageType> it(m_InputImage,m_InputImage->GetRequestedRegion());
  ImageRegionIterator <TInputImage> itf(m_FilterImage,region);

  for(i = 0;i<MAX_SCALE;i++)
    {
      for(j = 0;j<MAX_SCALE;j++)
        {
          weight[i][j] = 0;
        }
    }
  for(i = 1;i<=MAX_SCALE;i++)
    {
      tt1 = (double)i*0.5;
      tt2 = -0.5 / pow(tt1, 2.0);
      for(j = 0;j<i;j++)
        {
          inv_k = exp(tt2 * pow((double)j, 2.0));
          weight[i-1][j] = inv_k;
        }
    }

  for( i = 0;i<VectorDimension;i++)
    {zeroValue[i] = 0;}

  typename std::vector<char>::iterator itscale;

  it.GoToBegin();
  itf.GoToBegin();
  itscale = m_ScaleArray.begin();
  for(;!it.IsAtEnd(); ++it,++itscale,++itf)
    {
      value = it.Get(); 
      index = it.GetIndex();
      iscale =  *itscale;
      count = 0.0;
      for(i = 0;i<VectorDimension;i++)
        {sum[i] = 0;}

      if((m_SuppressBckgFlag==1)&&(value[0]<m_Mean[0]))
        {itf.Set(zeroValue);}
      else
        {
          for (k = 0; k < iscale; k++)
            {
              tt1 = weight[iscale-1][k];
              for (i = 0; i < m_CirclePointsNum[k]; i++)
                {
                  x = index[0] + m_CirclePointsLoc[k][i][0];
                  y = index[1] + m_CirclePointsLoc[k][i][1];
                  if (x >= 0 && y >= 0 && x < m_Size[0] && y < m_Size[1]) 
                        {
                          index1 = index; 
                          index[1] = y;
                          index[0] = x;
                          value = m_InputImage->GetPixel(index1);

                          for(j=0;j<VectorDimension;j++)
                            {
                              sum[j] = sum[j] + tt1*value[j];
                            } 
                          count = count + tt1;
                        }
                    }
                }
              for(i = 0;i<VectorDimension;i++)
                {
                  mean[i] = static_cast<int>(sum[i]/count);
                }
              itf.Set(mean);
            }
        }
  m_ScaleArray.resize(0);
  /* printf("Filter computation is done! \n"); */
}


template <class TInputImage, class TOutputImage>
double
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::FuzzyAffinity(const InputVectorType value1, const InputVectorType value2, const int object)
{
  double result,homo_affinity,material,object_affinity1,object_affinity2;
  InputVectorType temp;
  itk::Vector<double,VectorDimension> temp_vector;

  /* Homogeneity feature-based affinity */
  for(unsigned int i = 0;i<VectorDimension;i++)
    {
      temp[i]  = vnl_math_abs(value1[i] - value2[i]);
    }
  for(unsigned int i = 0;i<VectorDimension;i++)
    {
      temp_vector[i] = 0;
      for(unsigned int j = 0;j<VectorDimension;j++)
        {
          temp_vector[i] = temp_vector[i] + ((double)temp[j])*m_HomoCovariance[i][j];
        }
    }
  result = 0;
  for(unsigned int i = 0;i<VectorDimension;i++)
    {
      result = result + temp_vector[i]*(double)temp[i];
    }
  homo_affinity = exp(-0.5*result);

  /* Object feature-based affinity */
  /* first voxel  */
  for(unsigned int i = 0;i<VectorDimension;i++)
    {
    temp[i] = static_cast< unsigned short>( 
        vnl_math_abs(value1[i]-m_ObjectMeans[object][i]) );
    }
  for(unsigned int i = 0;i<VectorDimension;i++)
    {
      temp_vector[i] = 0;
      for(unsigned int j = 0;j<VectorDimension;j++)
        {
          temp_vector[i] = temp_vector[i] + (double)temp[j]*m_ObjectCovariances[object][i][j];
        }
    }
  result = 0;
  for(unsigned int i = 0;i<VectorDimension;i++)
    {
      result = result + temp_vector[i]*(double)temp[i];
    } 
  object_affinity1 = exp(-0.5*result);


  /* second voxel */
  for(int i = 0;i<static_cast<int>(VectorDimension);i++)
    {
    temp[i] = static_cast< unsigned short >( 
        vnl_math_abs(value2[i] - m_ObjectMeans[object][i]) );
    }
  for(int i = 0;i<static_cast<int>(VectorDimension);i++)
    {
      temp_vector[i] = 0;
      for(int j = 0;j<static_cast<int>(VectorDimension);j++)
        {
          temp_vector[i] = temp_vector[i] + (double)temp[j]*m_ObjectCovariances[object][i][j];
        }
    }
  result = 0;
  for(int i = 0;i<static_cast<int>(VectorDimension);i++)
    {
      result = result + temp_vector[i]*(double)temp[i];
    }
  object_affinity2 = exp(-0.5*result);

  /* minimum one between them */
  if(object_affinity1>object_affinity2) 
    {material = object_affinity2;}
  else 
    {material = object_affinity1;}

  result = sqrt(homo_affinity*material);
  return (result);
}

template <class TInputImage, class TOutputImage>
void
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::Compute_Affinity(const int object)
{
  IndexType  index1,max_index;
  InputVectorType  value1,value2; 
  double tt2;
  AffinityVector zero_affinity,temp_affinity;
  unsigned short MAX_AFFINITY = 4096;

  ImageRegionIterator <InputImageType> itf(m_FilterImage,m_FilterImage->GetRequestedRegion());
  ImageRegionIterator <AffinityImageType> ita(m_AffinityImage,m_AffinityImage->GetRequestedRegion());


  zero_affinity.Fill(0);
  for(;!ita.IsAtEnd(); ++ita)
    {
      ita.Set(zero_affinity);
    }

  for(int i = 0;i<ImageDimension;i++)
    {
      max_index[i] = static_cast<typename IndexType::IndexValueType>(m_Size[i]-1);
    }
  for(int i = 0;i<ImageDimension;i++)
    {
      itf.GoToBegin();
      ita.GoToBegin();

      for(;!itf.IsAtEnd(); ++itf,++ita)
        {
          value1 = itf.Get();
          temp_affinity = ita.Get();
          if((m_SuppressBckgFlag==1)&&(value1[0]<m_Mean[0]))
            {ita.Set(zero_affinity);}
          else
            {
              index1 = itf.GetIndex();
              if(index1[i]<max_index[i])
                {
                  index1[i]++;
                  value2 = m_FilterImage->GetPixel(index1);
                  index1[i]--;

                  tt2 = FuzzyAffinity(value1,value2,object);
                  temp_affinity[i] = static_cast<unsigned short>( MAX_AFFINITY * tt2 );
                }
              else
                {temp_affinity = zero_affinity;}

              ita.Set(temp_affinity);
            }
        }
    }
  /* printf("Affinity computation is done! \n");  */
}

template <class TInputImage, class TOutputImage>
void
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::Fast_Tracking(const int object)
{
  std::vector<ListSeedType>        chash;
  typename ListSeedType::iterator  iter;
  IndexType                        current,max_index,index1,index2;
  AffinityVector                   vectorValue;
  unsigned short  topIndex = 4096;

  ImageRegionConstIterator<AffinityImageType> it;
  it = ImageRegionConstIterator<AffinityImageType>(m_AffinityImage,m_AffinityImage->GetRequestedRegion());

  chash.resize(topIndex+1);
  for(int i=0;i<=topIndex;i++)
    chash[i].clear();

  unsigned short  initialValue=0;
  m_FuzzyConnImage->FillBuffer( initialValue );

  /** object tracking...*/
  while(!m_ObjectSeeds[object].empty())
    {
      current = m_ObjectSeeds[object].back();
      m_ObjectSeeds[object].pop_back();
      m_FuzzyConnImage->SetPixel(current,topIndex);
      chash[topIndex].push_front(current);
    }

  SizeType  size;
  size = m_AffinityImage->GetLargestPossibleRegion().GetSize();

  for(int i = 0;i<static_cast<int>(ImageDimension);i++)
    {
      max_index[i] = static_cast<typename IndexType::IndexValueType>(size[i])-1;
    }
  while((topIndex>0) && (chash[topIndex].size() !=0))
    {
      current = chash[topIndex].back();
      chash[topIndex].pop_back();

      while((topIndex>0) && (chash[topIndex].size() == 0))
        {
          topIndex--;
        }
      unsigned short pmax,pmin,dimensionValue;

      pmax = m_FuzzyConnImage->GetPixel(current);
      for(int i = 0;i<ImageDimension;i++)
        {
          index1 = current;
          if(index1[i] < max_index[i])
            {
              index1[i]++;
              vectorValue = m_AffinityImage->GetPixel(current);
              dimensionValue = (vectorValue[i]);
              pmin = (pmax < dimensionValue ? pmax: dimensionValue);

              unsigned short value = m_FuzzyConnImage->GetPixel(index1);
              if (pmin > value)
                {
                  if (value == 0)
                    {
                      chash[pmin].push_front(index1);
                    } 
                  else
                    {
                      for( iter = chash[value].begin();iter !=chash[value].end();iter++)
                        {
                          index2 = *iter;
                          if(index2 == index1)
                            {
                              chash[value].erase(iter);
                              break;
                            }
                        }
                      chash[pmin].push_front(index1);
                    }

                  if (pmin>topIndex)
                    {topIndex = pmin;}
                  m_FuzzyConnImage->SetPixel(index1,pmin);
                }
            }
          index1 = current;
          if(index1[i]>0)
            {
              index1[i]--;
              vectorValue = m_AffinityImage->GetPixel(index1);
              dimensionValue = vectorValue[i];
              pmin = (pmax < dimensionValue ? pmax: dimensionValue);

              unsigned short value = m_FuzzyConnImage->GetPixel(index1);
              if (pmin > value)
                {
                  if (value == 0)
                    {
                      chash[pmin].push_front(index1);
                    }
                  else
                    {
                      for( iter = chash[value].begin();iter !=chash[value].end();iter++)
                        {
                          index2 = *iter;
                          if(index2 == index1)
                            {
                              chash[value].erase(iter);
                              break;
                            }
                        }
                      chash[pmin].push_front(index1);
                    }

                  if (pmin>topIndex)
                    {topIndex = pmin;}
                  m_FuzzyConnImage->SetPixel(index1,pmin);
                }
            }
        }
    }

}


template <class TInputImage, class TOutputImage>
void VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::GenerateData()
{

  typedef std::vector<unsigned short>  ShortVectorType;
  std::vector<ShortVectorType>         fuzzyScene;

  int                 volume_size;
  unsigned short      MAX_AFFINITY = 4096;

  m_InputImage = this->GetInput();
  m_Size = m_InputImage->GetLargestPossibleRegion().GetSize();

  typename AffinityImageType::RegionType region;
  IndexType index;
  index.Fill(0);

  region.SetSize(m_Size);
  region.SetIndex(index);

  m_AffinityImage = AffinityImageType::New();  
  m_AffinityImage->SetRegions( region );
  m_AffinityImage->Allocate();  

  typename UShortImageType::RegionType uregion;
  uregion.SetSize(m_Size);
  uregion.SetIndex(index);

  m_FuzzyConnImage = UShortImageType::New();  
  m_FuzzyConnImage->SetRegions( uregion );
  m_FuzzyConnImage->Allocate();


  ScalePrepare();
  Compute_Scale();
  Compute_Filter();

  volume_size = 1;
  for(int i = 0;i<ImageDimension;i++)
    {
      volume_size = volume_size*static_cast<int>(m_Size[i]);
    } 
  fuzzyScene.resize(m_NumberOfObjects);
  for (int i = 0;i<m_NumberOfObjects; i++)
    {
      fuzzyScene[i].resize(volume_size);
    }
  typedef ImageRegionIterator<UShortImageType> Iterator;
  typename ListSeedType::iterator iterList;  

  for (int i = 0;i<m_NumberOfObjects;i++)
    {
      /* compute affinity for different object */
      Compute_Affinity(i);
      Fast_Tracking(i);

      Iterator iter(m_FuzzyConnImage, m_FuzzyConnImage->GetBufferedRegion() );
      iter.GoToBegin();
      int k = 0; 
      while(!iter.IsAtEnd())
       {
         fuzzyScene[i][k] = iter.Get();
         k++;
         ++iter;
       }
    }

  m_SegmentObject = this->GetOutput(); 


  OutRegionType Outregion;
  Outregion.SetSize(m_Size);
  Outregion.SetIndex(index);
  m_SegmentObject->SetRegions( Outregion);
  m_SegmentObject->Allocate();

  ImageRegionIterator <OutputImageType> itS(this->m_SegmentObject,Outregion);
  itS.GoToBegin();

  OutputPixelType scale = static_cast<OutputPixelType>(NumericTraits<unsigned char>::max()/(m_NumberOfObjects));

  int j = 0;
  while(!itS.IsAtEnd())
    {
      if(m_NumberOfObjects == 1)
        {
          if( fuzzyScene[0][j] >= (MAX_AFFINITY * m_Threshold))
            {itS.Set(scale);}
          else
            {itS.Set(0);}
        }
      else
        { 
          int k = 0;
          for(int i = 1;i<m_NumberOfObjects;i++)
            {
              if (fuzzyScene[i][j]>fuzzyScene[k][j]) k = i;
            }
          itS.Set(static_cast<OutputPixelType>(k*scale));
        }
      ++itS;
      j++;
    }
  for (int i = 0;i<m_NumberOfObjects; i++)
    {
      fuzzyScene[i].resize(0);
    }
  fuzzyScene.resize(0);

}

template <class TInputImage, class TOutputImage>
void VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Number of objects = "  << m_NumberOfObjects << std::endl;
}

} // end namespace itk

#endif
