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

#include "itkImageRegionIteratorWithIndex.h"

namespace itk {

/*
 *
 */
template <class TInputImage, class TOutputImage>
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::VectorFuzzyConnectednessImageFilter()
  : m_SpherePointsLoc(0),
    m_SpherePointsNum(0),
    m_Objects(1),
    m_SelectedObject(0),
    m_ObjectsCovMatrix(0),
    m_ObjectsMean(0),
    m_ObjectsMaxDiff(0),
    m_ObjectsMap(0),
    m_ObjectsSeed(0)
{
}

/*
 *
 */
template <class TInputImage, class TOutputImage>
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::~VectorFuzzyConnectednessImageFilter()
{
  if (m_SpherePointsNum)
    {
    delete [] m_SpherePointsNum;
    m_SpherePointsNum = 0;
    }
  if (m_SpherePointsLoc)
    {
    delete [] m_SpherePointsLoc;
    m_SpherePointsLoc = 0;
    }
  if (m_ObjectsMean)
    {
    delete [] m_ObjectsMean;
    m_ObjectsMean = 0;
    }
  if (m_ObjectsSeed)
    {
    delete [] m_ObjectsSeed;
    m_ObjectsSeed = 0;
    }
  if (m_ObjectsCovMatrix)
    {
    delete [] m_ObjectsCovMatrix;
    m_ObjectsCovMatrix = 0;
    }
  if (m_ObjectsMap)
    {
    delete [] m_ObjectsMap;
    m_ObjectsMap = 0;
    }
  if (m_ObjectsMaxDiff)
    {
    delete [] m_ObjectsMaxDiff;
    m_ObjectsMaxDiff = 0;
    }
}

/*
 *
 */
template <class TInputImage, class TOutputImage>
void
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::SetObjectsMatrix(const MatrixType object_max,const int object_num)
{
  m_ObjectsCovMatrix[object_num] = object_max;
}


/*
 *
 */
template <class TInputImage, class TOutputImage>
void 
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::SetObjectsMean(const VDVector mean, const int object_num)
{
  m_ObjectsMean[object_num] = mean;
}

/*
 *
 */
template <class TInputImage, class TOutputImage>
void 
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::SetObjectsSeed(const IndexType &seed, const int object_num)
{
  m_ObjectsSeed[object_num].push_front(seed);
}

/*
 *
 */
template <class TInputImage, class TOutputImage>
void 
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::Initialization()
{
  m_SpherePointsNum = new int[8+1];
  m_SpherePointsLoc = new OffsetType[8+1];

  m_ObjectsMean = new VDVector[m_Objects];
  m_ObjectsSeed = new ListType[m_Objects];
  m_ObjectsCovMatrix = new MatrixType[m_Objects];
  m_ObjectsMap = new FloatType[m_Objects];
  m_ObjectsMaxDiff = new VDVector[m_Objects];
}


/*
 *
 */
template <class TInputImage, class TOutputImage>
void 
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::ScalePrepare()
{
  int i,j,k,tti1,tti2;
  double tt1;
  double anisotropy_row,anisotropy_col,anisotropy_slice;
  double const *spaceing;
  int ppptti1[2*(8+5)][2*(8+5)];
  IDVector location;

  m_InputImage = this->GetInput();

  spaceing = m_InputImage->GetSpacing();
  m_Size = m_InputImage->GetLargestPossibleRegion().GetSize();

  anisotropy_col = spaceing[0];
  anisotropy_row = spaceing[1];
  anisotropy_slice = spaceing[2];

  tt1 = anisotropy_col;
  if(tt1>anisotropy_row)
    tt1 = anisotropy_row;

  if(m_Size[2] > 1 && tt1 > anisotropy_slice)
    tt1 = anisotropy_slice;

  anisotropy_col = anisotropy_col/tt1;
  anisotropy_row = anisotropy_row/tt1;

  if(m_Size[2] > 1) 
    { anisotropy_slice = anisotropy_slice/tt1; }


  for(i=0;i<2*(8+5);i++)
    {
    for(j=0;j<2*(8+5);j++)
      {
      ppptti1[i][j] = 0;
      }
    }

  for(int i = 0;i<=8;i++)
    { m_SpherePointsNum[i] = 0; }

  tti1 = 8 + 5;

  for (k = 0; k <= 8; k++)
    {
    for (i = -k - 2; i <= k + 2; i++)
      {
      for (j = -k - 2; j <= k + 2; j++)
        {
        if (ppptti1[tti1 + i][tti1 + j] == 0)
          {
          tt1 = sqrt(pow(((double) i) * anisotropy_row,2.0) + 
                     pow(((double) j) * anisotropy_col, 2.0));
          if (tt1 <= ((double) k) + 0.5)
            {
            m_SpherePointsNum[k] = m_SpherePointsNum[k] + 1;
            ppptti1[tti1 + i][tti1 + j] = 2;
            }
          }
        }
      }

    m_SpherePointsLoc[k].resize(m_SpherePointsNum[k]);

    tti2 = 0;
    for (i = -k - 2; i <= k + 2; i++)
      {
      for (j = -k - 2; j <= k + 2; j++)
        {
        if (ppptti1[(8+5) + i][(8+5) + j] == 2)
          {
          ppptti1[(8+5) + i][(8+5) + j] = 1;
          location[0] = j;
          location[1] = i;
          location[2] = 0;
          m_SpherePointsLoc[k][tti2] = location;
          tti2 = tti2+1;
          }
        }
      }
    }


  m_MaskTotal = 0.0;
  for(i = -1;i <= 1;i++)
    {
    for(j = -1; j <= 1;j++)
      {
      m_Mask[i+1][j+1] = 0.0;
      }
    }

  for(i = -1;i<=1;i++)
    {
    for(j = -1; j <= 1;j++)
      {
      tt1 = pow(anisotropy_col * j, 2.0);
      tt1 = tt1 + pow(anisotropy_row * i, 2.0);
      tt1 = 1 / (1 + tt1);
      m_Mask[i + 1][j + 1] = tt1;
      m_MaskTotal = m_MaskTotal + tt1;
      }
    }

}

template <class TInputImage, class TOutputImage>
void 
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::Compute_LookupTable()
{

  const float HistThreshold = 0.90;
  MatrixType   HomogeneityCovarianceMatrix;
  itk::Vector<double,VectorDimension>    vectorA,vectorB;

  typedef std::vector<int>        VectorInt;

  VectorInt  *Histogram;
  VDVector            Hist_sum;
  int i,j,k,l,pslices,prow,pcol,tti1;
  IndexType            index1,index2;
  VDVector            value1,value2;
  float     result;   

  Histogram = new VectorInt[VectorDimension];

  m_InputImage = this->GetInput();
  m_Size = m_InputImage->GetLargestPossibleRegion().GetSize();

  pslices = m_Size[2];
  prow = m_Size[1];
  pcol = m_Size[0];

  for(i = 0;i<static_cast<int>(VectorDimension);i++)
    m_HomoMaxDiff[i] = 0;

  for(i = 0;i<pslices ;i++)
    {
    for(j = 0;j<prow;j++)
      {
      for(k = 0;k<pcol-1;k++)
        {
        index1[2] = index2[2] = i;
        index1[1] = index2[1] = j;
        index1[0] = k;
        index2[0] = k+1;
        value1 =  m_InputImage->GetPixel(index1);
        value2 =  m_InputImage->GetPixel(index2);

        for(l = 0;l<static_cast<int>(VectorDimension);l++)
          {
          if(vnl_math_abs(value1[l] - value2[l]) > m_HomoMaxDiff[l])
            {
            m_HomoMaxDiff[l] = vnl_math_abs(value1[l] - value2[l]);
            }
          }
        }
      }
    }

  for(i = 0;i<pslices ;i++)
    {
    for(j = 0;j<prow-1;j++)
      {
      for(k = 0;k<pcol;k++)
        {
        index1[2] = index2[2] = i;
        index1[1] = j;
        index2[1] = j+1;
        index1[0] = index2[0] = k;

        value1 =  m_InputImage->GetPixel(index1);
        value2 =  m_InputImage->GetPixel(index2);

        for(l = 0;l<static_cast<int>(VectorDimension);l++)
          if(vnl_math_abs(value1[l] - value2[l]) > m_HomoMaxDiff[l])
            m_HomoMaxDiff[l] = vnl_math_abs(value1[l] - value2[l]);
        }
      }
    }

  for(i = 0;i<pslices-1;i++)
    {
    for(j = 0;j<prow;j++)
      {
      for(k = 0;k<pcol;k++)
        {
        index1[2] = i;
        index2[2] = i+1;
        index1[1] = index2[1] = j;
        index1[0] = index2[0] = k;

        value1 =  m_InputImage->GetPixel(index1);
        value2 =  m_InputImage->GetPixel(index2);

        for(l = 0;l<static_cast<int>(VectorDimension);l++)
          if(vnl_math_abs(value1[l] - value2[l]) > m_HomoMaxDiff[l])
            m_HomoMaxDiff[l] = vnl_math_abs(value1[l] - value2[l]);
        }
      }
    }

  for(i = 0;i<static_cast<int>(VectorDimension);i++)
    {
    Histogram[i].resize(m_HomoMaxDiff[i]+1);
    }

  for(i = 0;i<static_cast<int>(VectorDimension);i++)
    {
    for(j = 0;j<=m_HomoMaxDiff[i];j++)
      {
      Histogram[i][j] = 0;
      }
    }

  for(i = 0;i<pslices ;i++)
    {
    for(j = 0;j<prow;j++)
      {
      for(k = 0;k<pcol-1;k++)
        {
        index1[2] = index2[2] = i;
        index1[1] = index2[1] = j;
        index1[0] = k;
        index2[0] = k+1;
        value1 =  m_InputImage->GetPixel(index1);
        value2 =  m_InputImage->GetPixel(index2);
        for(l = 0;l<static_cast<int>(VectorDimension);l++)
          {
          tti1 = vnl_math_abs(value1[l] - value2[l]);
          Histogram[l][tti1] = Histogram[l][tti1] +1;
          }
        }
      }
    }
  for(i = 0;i<pslices ;i++)
    {
    for(j = 0;j<prow-1;j++)
      {
      for(k = 0;k<pcol;k++)
        {
        index1[2] = index2[2] = i;
        index1[1] = j;
        index2[1] = j+1;
        index1[0] = index2[0] = k;

        value1 =  m_InputImage->GetPixel(index1);
        value2 =  m_InputImage->GetPixel(index2);

        for(l = 0;l<static_cast<int>(VectorDimension);l++)
          {
          tti1 = vnl_math_abs(value1[l] - value2[l]);
          Histogram[l][tti1] = Histogram[l][tti1] +1;
          }
        }
      }
    }
  for(i = 0;i<pslices-1;i++)
    {
    for(j = 0;j<prow;j++)
      {
      for(k = 0;k<pcol;k++)
        {
        index1[2] = i;
        index2[2] = i+1;
        index1[1] = index2[1] = j;
        index1[0] = index2[0] = k;

        value1 =  m_InputImage->GetPixel(index1);
        value2 =  m_InputImage->GetPixel(index2);

        for(l = 0;l<static_cast<int>(VectorDimension);l++)
          {
          tti1 = vnl_math_abs(value1[l] - value2[l]);
          Histogram[l][tti1] = Histogram[l][tti1] +1;
          }
        }
      }
    }

  for( i=0;i<static_cast<int>(VectorDimension); i++)
    {
    Hist_sum[i] = 0;
    for(j = 0;j<=m_HomoMaxDiff[i];j++)
      {
      Hist_sum[i] = Hist_sum[i] + Histogram[i][j];
      }
    }
  for(i = 0;i<static_cast<int>(VectorDimension);i++)
    {
    for(j=0;j<=m_HomoMaxDiff[i];j++)
      {
      tti1 = 0;
      m_FeaturesThreshold[i] = j;
      for(k=0;k<=j;k++)
        {
        tti1 = tti1+Histogram[i][k];
        }
      if ( ( static_cast<double>(tti1) / 
             static_cast<double>( Hist_sum[i]) ) >= HistThreshold )
        {
        break;
        }
      }
    }
  for(i = 0;i<static_cast<int>(VectorDimension);i++)
    {
    tti1 = 1;
    for(j=0;j<=i-1;j++)
      {
      tti1 = tti1*(m_FeaturesThreshold[j]+1); 
      }
    m_PowerValue[i] = tti1;
    }

  /*    To computer the homogeneity covariance matrix     */

  for (int x = 0;x < static_cast<int>(VectorDimension); x++)
    {
    for(int y = x;y < static_cast<int>(VectorDimension); y++)
      {
      HomogeneityCovarianceMatrix[x][y] = 0;
      double tt1 = 0;
      double tt2 = 0;
      int count = 0;
      for(i = 0;i<pslices ;i++)
        {
        for(j = 0;j<prow;j++)
          {
          for(k = 0;k<pcol-1;k++)
            {
            index1[2] = index2[2] = i;
            index1[1] = index2[1] = j;
            index1[0] = k;
            index2[0] = k+1;
            value1 =  m_InputImage->GetPixel(index1);
            value2 =  m_InputImage->GetPixel(index2);
            tt1 = (value1[x]-value2[x]) ;
            tt2 = (value1[y]-value2[y]) ;
            if((vnl_math_abs(tt1)<=m_FeaturesThreshold[x]) && 
               (vnl_math_abs(tt2)<=m_FeaturesThreshold[y]))
              {
              HomogeneityCovarianceMatrix[x][y] += tt1*tt2;
              count++;
              }
            }
          }
        }
      for(i = 0;i<pslices ;i++)
        {
        for(j = 0;j<prow-1;j++)
          {
          for(k = 0;k<pcol;k++)
            {
            index1[2] = index2[2] = i;
            index1[1] = j;
            index2[1] = j+1;
            index1[0] = index2[0] = k;
            value1 =  m_InputImage->GetPixel(index1);
            value2 =  m_InputImage->GetPixel(index2);
            tt1 = (value1[x]-value2[x]);
            tt2 = (value1[y]-value2[y]);

            if( vnl_math_abs(tt1)<=m_FeaturesThreshold[x] && 
                vnl_math_abs(tt2)<=m_FeaturesThreshold[y])
              {
              HomogeneityCovarianceMatrix[x][y] += tt1*tt2;
              count++;
              }
            }
          }
        }

      for(i = 0;i<pslices-1;i++)
        {
        for(j = 0;j<prow;j++)
          {
          for(k = 0;k<pcol;k++)
            {
            index1[2] = i;
            index2[2] = i+1;
            index1[1] = index2[1] = j;
            index1[0] = index2[0] = k;

            value1 =  m_InputImage->GetPixel(index1);
            value2 =  m_InputImage->GetPixel(index2);
            tt1 = (value1[x]-value2[x]);
            tt2 = (value1[y]-value2[y]);

            if( vnl_math_abs(tt1)<=m_FeaturesThreshold[x] && 
                vnl_math_abs(tt2)<=m_FeaturesThreshold[y])
              {
              HomogeneityCovarianceMatrix[x][y] += tt1*tt2;
              count++;
              }
            }
          }
        }
      HomogeneityCovarianceMatrix[x][y] = HomogeneityCovarianceMatrix[x][y] /(double)count;
      }
    }

  for (int x = 0;x < static_cast<int>(VectorDimension); x++)
    {
    for(int y = 0;y < x; y++)
      {
      HomogeneityCovarianceMatrix[x][y] = HomogeneityCovarianceMatrix[y][x];
      }
    }

  vnl_matrix<double>  HomoInverseMatrix = HomogeneityCovarianceMatrix.GetInverse();

  tti1 = 1;
  for(int x=0;x<static_cast<int>(VectorDimension);x++)
    {
    tti1 = tti1*(m_FeaturesThreshold[x]+1);
    }

  m_HomogeneityMap.resize(tti1);
  m_ScaleMap.resize(tti1);

  for(i = 0;i<tti1;i++)
    {
    k = i;
    for(j=0;j<static_cast<int>(VectorDimension);j++)
      {
      vectorA[j] = (k % (m_FeaturesThreshold[j]+1));
      k = k/(m_FeaturesThreshold[j]+1);
      }
    for(int x = 0;x<static_cast<int>(VectorDimension);x++)
      {
      vectorB[x] = 0;
      for(int y = 0;y<static_cast<int>(VectorDimension);y++)
        {
        vectorB[x] = vectorB[x] + vectorA[y]*HomoInverseMatrix[x][y];
        }
      }
    result = 0;
    for(int x = 0;x<static_cast<int>(VectorDimension);x++)
      {
      result = result + vectorB[x]*vectorA[x];
      }

    m_HomogeneityMap[i] = (float) exp(-0.5*result);
    m_ScaleMap[i] = (float) exp(-0.5*result/9.0);
    }

  /*  printf("Homogeneity map computation is done! \n"); */
  /*  printf("Scale map computation is done! \n");     */

  /*     compute object-based affinity look-up table  */

  for(l = 0;l<m_Objects;l++)
    {
    for(int x = 0;x<static_cast<int>(VectorDimension);x++)
      {
      m_ObjectsMaxDiff[l][x] = 0;
      for(i = 0;i<pslices;i++)
        {
        for(j = 0;j<prow;j++)
          {
          for(k = 0;k<pcol;k++)
            {
            index1[2] = i;
            index1[1] = j;
            index1[0] = k;
            value1 = m_InputImage->GetPixel(index1);

            if(vnl_math_abs(value1[x] - m_ObjectsMean[l][x])> m_ObjectsMaxDiff[l][x])
              {
              m_ObjectsMaxDiff[l][x] = vnl_math_abs(value1[x] - m_ObjectsMean[l][x]);
              }
            }
          }
        }
      }
    }

  for(l = 0;l<m_Objects;l++)
    {
    for(int x = 0;x<static_cast<int>(VectorDimension);x++)
      {
      m_ObjectsMaxDiff[l][x] = (int) m_ObjectsMaxDiff[l][x]/3;
      }
    }

  for(l = 0;l<m_Objects;l++)
    {
    tti1 = 1;
    for(int x = 0;x<static_cast<int>(VectorDimension);x++)
      {
      tti1 = tti1*(m_ObjectsMaxDiff[l][x]+1);
      }
    m_ObjectsMap[l].resize(tti1);

    vnl_matrix<double> ObjectInverseMatrix = m_ObjectsCovMatrix[l].GetInverse();

    for(i = 0;i<tti1;i++)
      {
      k = i;
      for(j=0;j<static_cast<int>(VectorDimension);j++)
        {
        vectorA[j] = (k % (m_ObjectsMaxDiff[l][j]+1));
        k = k/(m_ObjectsMaxDiff[l][j]+1);
        }

      for(int x = 0;x<static_cast<int>(VectorDimension);x++)
        {
        vectorB[x] = 0;
        for(int y = 0;y<static_cast<int>(VectorDimension);y++)
          vectorB[x] = vectorB[x] + vectorA[y]*ObjectInverseMatrix[x][y];
        }
      result = 0;
      for(int x = 0;x<static_cast<int>(VectorDimension);x++)
        result = result + vectorB[x]*vectorA[x];

      m_ObjectsMap[l][i] = exp(-0.5*result);
      }
    //  printf("Object %d map computation is done!\n",l);
    }

  for(i = 0;i<static_cast<int>(VectorDimension);i++)
    {
    Histogram[i].resize(0);
    }
  delete [] Histogram;
}

template <class TInputImage, class TOutputImage>
void 
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::Compute_Scale()
{
  const int Tolerance = 13;
  int i,j,k,x,y,z,pslices,prow,pcol;
  int flag,edge_flag;
  int tti1;
  IndexType  index1,index2;
  VDVector  value1,value2, mean;
  double mask_f[VectorDimension];
  double count_obj,count_nonobj;
  int slice,row,col;

  m_InputImage = this->GetInput();
  m_Size = m_InputImage->GetLargestPossibleRegion().GetSize();

  pslices = m_Size[2];
  prow = m_Size[1];
  pcol = m_Size[0];

  m_ScaleArray.resize(pslices*prow*pcol);

  for( slice = 0;slice<pslices;slice++)
    {
    for( row = 0;row<prow;row++)
      {
      for( col = 0;col<pcol;col++)
        {

        index1[2] = slice;
        index1[1] = row;
        index1[0] = col;
        value1 = m_InputImage->GetPixel(index1);

        flag = 0;
        edge_flag = 0;

        for(i = 0;i<static_cast<int>(VectorDimension);i++)
          mask_f[i] = 0.0;

        for (int yy = -1; yy <= 1; yy++)
          for (int xx = -1; xx <= 1; xx++)
            {
            x = xx + col;
            y = yy + row;

            if ((x >= 0) && (y >= 0)  && (x < pcol) && (y < prow) )
              {
              index2[2] = slice;
              index2[1] = y;
              index2[0] = x;
              value2 = m_InputImage->GetPixel(index2);

              for(i = 0;i<static_cast<int>(VectorDimension);i++)
                mask_f[i]  = mask_f[i] + m_Mask[yy + 1][xx + 1]*(double) value2[i];
              }
            else
              {
              for(i = 0;i<static_cast<int>(VectorDimension);i++)
                {
                mask_f[i]  = mask_f[i] +  m_Mask[yy + 1][xx + 1]*(double) value1[i];
                }
              }
            }

        for(i = 0;i<static_cast<int>(VectorDimension);i++)
          {
          mean[i] = (int) (mask_f[i]/m_MaskTotal + 0.5);
          }


        for (k = 1; k < 8 && !flag; k++)
          {
          count_obj = 0.0;
          count_nonobj = 0.0;
          for (i = 0; i < m_SpherePointsNum[k]; i++)
            {
            x = col + m_SpherePointsLoc[k][i][0];
            y = row + m_SpherePointsLoc[k][i][1];
            z = slice + m_SpherePointsLoc[k][i][2];
            if (x < 0 || x >= pcol)
              x = col;
            if (y < 0 || y >= prow)
              y = row;
            if (z < 0 || z >= pslices)
              z = slice;
            index1[2] = z;
            index1[1] = y;
            index1[0] = x;
            value1 = m_InputImage->GetPixel(index1);

            tti1 = 0;
            edge_flag = 0;

            int temp[VectorDimension];
            for(j=0;j<static_cast<int>(VectorDimension);j++)
              {
              temp[j] = vnl_math_abs( value1[j] - mean[j]);
              tti1 = tti1 + temp[j]*m_PowerValue[j];

              if(temp[j]>m_FeaturesThreshold[j])
                {
                edge_flag = 1;
                break;
                }
              }

            if(!edge_flag)
              {
              count_obj = count_obj + m_ScaleMap[tti1];
              count_nonobj = count_nonobj + 1.0 - m_ScaleMap[tti1];
              }
            else
              {
              count_obj = count_obj;
              count_nonobj = count_nonobj + 1.0;
              }
            }

          if (100.0 * count_nonobj >= Tolerance * (count_nonobj + count_obj)) 
            {
            m_ScaleArray[slice*prow*pcol + row * pcol + col] = k;
            flag = 1;
            }
          }

        if (!flag)
          {
          m_ScaleArray[slice*prow*pcol + row * pcol + col] = k;
          }
        }
      }
    }

  m_ScaleMap.resize(0);
  //printf("Scale computation is done! \n");
}


template <class TInputImage, class TOutputImage>
void 
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::Compute_Filter()
{

  int i,j,k,iscale;
  int x,y,z;
  int col, row, slice, pcol, prow, pslice;
  double tt1,tt2,inv_k,count;
  double weight[8][8];
  IndexType index;
  VDVector  value,mean;


  m_InputImage = this->GetInput();
  m_Size = m_InputImage->GetLargestPossibleRegion().GetSize();

  pslice = m_Size[2];
  prow = m_Size[1];
  pcol = m_Size[0];

  m_FilterImage = InputImageType::New();

  index.Fill(0);
  typename InputImageType::RegionType region;

  region.SetSize(m_Size);
  region.SetIndex(index);

  m_FilterImage->SetLargestPossibleRegion( region );
  m_FilterImage->SetBufferedRegion( region );
  m_FilterImage->SetRequestedRegion( region );
  m_FilterImage->SetSpacing(m_InputImage->GetSpacing());
  m_FilterImage->Allocate();


  for(i = 0;i<8;i++)
    {
    for(j = 0;j<8;j++)
      {
      weight[i][j] = 0;
      }
    }

  for(i = 1;i<=8;i++)
    {
    tt1 = (double)i*0.5;
    tt2 = -0.5 / pow(tt1, 2.0);
    for(j = 0;j<i;j++)
      {
      inv_k = exp(tt2 * pow((double)j, 2.0));
      weight[i-1][j] = inv_k;
      }
    }

  for (slice = 0; slice < pslice; slice++)
    {
    for (row = 0; row < prow; row++)
      {
      for (col = 0; col < pcol; col++)
        {
        iscale = (int) m_ScaleArray[slice * prow * pcol + row * pcol + col];
        count = 0.0;
        mean.Fill(0);

        for (k = 0; k < iscale; k++)
          {
          tt1 = weight[iscale-1][k];

          for (i = 0; i < m_SpherePointsNum[k]; i++)
            {

            x = col + m_SpherePointsLoc[k][i][0];
            y = row + m_SpherePointsLoc[k][i][1];
            z = slice + m_SpherePointsLoc[k][i][2];


            if (x >= 0 && y >= 0 && z >= 0 && x < pcol && y < prow && z < pslice) 
              {
              index[2] = z;
              index[1] = y;
              index[0] = x;

              value = m_InputImage->GetPixel(index);

              for(j=0;j<static_cast<int>(VectorDimension);j++)
                mean[j] = mean[j] + static_cast<int>(tt1*value[j]);
              count = count + tt1;
              }
            }
          }
        for(i = 0;i<static_cast<int>(VectorDimension);i++)
          mean[i] = static_cast<int>(mean[i]/count);

        index[2] = slice;
        index[1] = row;
        index[0] = col;
        m_FilterImage->SetPixel(index,mean);
        }
      }
    }

  //printf("Filter computation is done! \n");
}


template <class TInputImage, class TOutputImage>
void 
VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::Compute_Affinity()
{

  int i,j,k,pslices,prow,pcol;
  IndexType  index1,index2;
  VDVector  value1,value2, mean; 
  int tti1;
  double temp[VectorDimension];
  double tt2;
  int col,row,slice,col1,row1,slice1;

  m_InputImage = this->GetInput();
  m_Size = m_InputImage->GetLargestPossibleRegion().GetSize();

  pslices = m_Size[2];
  prow = m_Size[1];
  pcol = m_Size[0];

  m_Xaffinity.resize(pslices*prow*pcol);
  m_Yaffinity.resize(pslices*prow*pcol);
  m_Zaffinity.resize(pslices*prow*pcol);

  /*  compute homogeneity-based affinity  */
  for ( slice = 0; slice < pslices; slice++)
    {
    for ( row = 0; row < prow; row++)
      {
      for ( col = 0; col < pcol-1; col++)
        {
        index1[2] = index2[2] = slice;
        index1[1] = index2[1] = row;
        index1[0] = col;

        index2[0] = col + 1;

        value1 = m_FilterImage->GetPixel(index1);
        value2 = m_FilterImage->GetPixel(index2);

        int edge_flag = 0;
        tti1 = 0;

        for(j=0;j<static_cast<int>(VectorDimension);j++)
          {
          temp[j] = vnl_math_abs(value1[j]-value2[j]);
          if(temp[j] > m_FeaturesThreshold[j])
            {
            edge_flag = 1;
            break;
            }
          tti1 = tti1 + static_cast<int>(temp[j]*m_PowerValue[j]);
          }
        if(edge_flag)
          {
          tt2 = 0;
          }
        else
          {
          tt2 = m_HomogeneityMap[tti1];
          }

        m_Xaffinity[slice * prow * pcol + row * pcol + col]= (unsigned short) (4096 * tt2);
        }
      }
    }

  for (slice = 0; slice < pslices; slice++)
    {
    for (row = 0; row < prow-1; row++)
      {
      for (col = 0; col < pcol; col++)
        {
        index1[2] = index2[2] = slice;
        index1[0] = index2[0] = col;
        index1[1] = row;

        index2[1] = row + 1;

        value1 = m_FilterImage->GetPixel(index1);
        value2 = m_FilterImage->GetPixel(index2);

        int edge_flag = 0;
        tti1 = 0;

        for(j=0;j<static_cast<int>(VectorDimension);j++)
          {
          temp[j] = vnl_math_abs(value1[j]-value2[j]);
          if(temp[j] > m_FeaturesThreshold[j])
            {
            edge_flag = 1;
            break;
            }
          tti1 = tti1 + static_cast<int>(temp[j]*m_PowerValue[j]);
          }
        if(edge_flag)
          {
          tt2 = 0;
          }
        else
          {
          tt2 = m_HomogeneityMap[tti1];
          }
        m_Yaffinity[slice * prow * pcol + row * pcol + col] = (unsigned short) (4096 * tt2);
        }
      }
    }

  for (slice = 0; slice < pslices -1; slice++)
    {
    for (row = 0; row < prow; row++)
      {
      for (col = 0; col < pcol; col++)
        {
        index1[0] = index2[0] = col;
        index1[1] = index2[1] = row;
        index1[2] = slice;
        index2[2] = slice + 1;

        value1 = m_FilterImage->GetPixel(index1);
        value2 = m_FilterImage->GetPixel(index2);

        int edge_flag = 0;
        tti1 = 0;
        for(j=0;j<static_cast<int>(VectorDimension);j++)
          {
          temp[j] = vnl_math_abs(value1[j]-value2[j]);
          if(temp[j] > m_FeaturesThreshold[j])
            {
            edge_flag = 1;
            break;
            }
          tti1 = tti1 + static_cast<int>(temp[j]*m_PowerValue[j]);
          }
        if(edge_flag)
          {
          tt2 = 0;
          }
        else
          {
          tt2 = m_HomogeneityMap[tti1];
          }
        m_Zaffinity[slice * prow * pcol + row * pcol + col]= (unsigned short) (4096 * tt2);
        }
      }
    }

  m_HomogeneityMap.resize(0);

  /*   compute object-based affinity */

  VDVector* ObjectsOffset = new VDVector[m_Objects];

  for(i = 0;i<m_Objects; i++)
    {
    for(j = 0;j<static_cast<int>(VectorDimension); j++)
      {
      ObjectsOffset[i][j] = 1;
      for(k = 0;k<j;k++)
        {
        ObjectsOffset[i][j] = ObjectsOffset[i][j]*(m_ObjectsMaxDiff[i][k]+1);
        }
      }
    }

  m_Material.resize(pslices*prow*pcol);

  double *material,largest_material;
  material = new double[m_Objects];
  largest_material = 0;

  for (slice = 0; slice < pslices; slice++)
    {
    for (row = 0; row < prow; row++)
      {
      for (col = 0; col < pcol; col++)
        {
        for (int object = 0; object < m_Objects; object++)
          { 
          index1[2] = slice;
          index1[1] = row;
          index1[0] = col;

          value1 = m_FilterImage->GetPixel(index1);

          int edge_flag = 0;
          tti1 = 0;

          for(j=0;j<static_cast<int>(VectorDimension);j++)
            {
            temp[j] = static_cast<int>(
                vnl_math_abs(value1[j] - m_ObjectsMean[object][j])  );
            if (temp[j] > m_ObjectsMaxDiff[object][j])
              {
              edge_flag = 1;
              break;
              }
            tti1 = tti1 + static_cast<int>(temp[j]*ObjectsOffset[object][j]);
            }
          if(edge_flag)
            {
            material[object] = 0;
            }
          else
            {
            material[object] = m_ObjectsMap[object][tti1];
            }
          }

        double max_material = material[0];
        for (int object = 1; object < m_Objects; object++)
          {
          if (max_material < material[object])
            {
            max_material = material[object];
            }
          }
        m_Material[slice * prow * pcol + row * pcol + col] = max_material;
        if(largest_material < max_material)
          {
          largest_material = max_material;
          }
        }
      }
    }

  delete[] material;

  for (slice = 0; slice < pslices; slice++)
    {
    for (row = 0; row < prow; row++)
      {
      for (col = 0; col < pcol; col++)
        {
        m_Material[slice * prow * pcol + row * pcol + col] = 
          (double) 4096 * m_Material[slice * prow * pcol + row * pcol + col] / largest_material;
        }
      }
    }

  for(int object = 0;object<m_Objects;object++)
    {
    m_ObjectsMap[object].resize(0);
    }

  m_ScaleArray.resize(0);

  delete[] ObjectsOffset;

  /* combine homogeneity_based affinity and object_based affinity */

  for (slice = 0; slice < pslices; slice++)
    {
    for (row = 0; row < prow; row++)
      {
      for (col = 0; col < pcol - 1; col++)
        {
        slice1 = slice;
        row1 = row;
        col1 = col + 1;
        if(m_Material[slice * prow * pcol + row * pcol + col] < 
           m_Material[slice1 * prow * pcol + row1 * pcol + col1])
          {
          m_Xaffinity[slice * prow * pcol + row * pcol + col] = 
            (unsigned short)sqrt(m_Material[slice * prow * pcol + row * pcol + col] 
                                 * (double)m_Xaffinity[slice*prow * pcol+row*pcol+col]+0.5);
          }
        else
          {
          m_Xaffinity[slice * prow * pcol + row * pcol + col] = 
            (unsigned short) sqrt(m_Material[slice1 * prow * pcol + row1 * pcol + col1] 
                                  * (double)m_Xaffinity[slice*prow * pcol+row*pcol+col]+0.5);
          }
        }
      }
    }

  for (slice = 0; slice < pslices; slice++)
    {
    for (row = 0; row < prow-1; row++)
      {
      for (col = 0; col < pcol; col++)
        {
        slice1 = slice;
        row1 = row + 1;
        col1 = col;
        if(m_Material[slice * prow * pcol + row * pcol + col] < 
           m_Material[slice1 * prow * pcol + row1 * pcol + col1])
          {
          m_Yaffinity[slice * prow * pcol + row * pcol + col] = 
            (unsigned short)sqrt(m_Material[slice * prow * pcol + row * pcol + col] 
                                 * (double)m_Yaffinity[slice*prow * pcol+row*pcol+col]+0.5);
          }
        else
          {
          m_Yaffinity[slice * prow * pcol + row * pcol + col] = 
            (unsigned short) sqrt(m_Material[slice1 * prow * pcol + row1 * pcol + col1] 
                                  * (double)m_Yaffinity[slice*prow * pcol +row*pcol+col]+0.5);
          }
        }
      }
    }

  for (slice = 0; slice < pslices-1; slice++)
    {
    for (row = 0; row < prow; row++)
      {
      for (col = 0; col < pcol; col++)  
        {
        slice1 = slice+1;
        row1 = row;
        col1 = col;
        if(m_Material[slice * prow * pcol + row * pcol + col] < 
           m_Material[slice1 * prow * pcol + row1 * pcol + col1])
          {
          m_Zaffinity[slice * prow * pcol + row * pcol + col] = 
            (unsigned short)sqrt(m_Material[slice * prow * pcol + row * pcol + col] 
                                 * (double)m_Zaffinity[slice*prow*pcol + row*pcol+col]+0.5);
          }
        else
          {
          m_Zaffinity[slice * prow * pcol + row * pcol + col] = 
            (unsigned short)sqrt(m_Material[slice1 * prow * pcol + row1 * pcol + col1] 
                                 * (double)m_Zaffinity[slice*prow * pcol +row*pcol+col]+0.5);
          }
        }
      }
    }

  for (slice = 0; slice < pslices; slice++)
    {
    for (row = 0; row < prow; row++)
      {
      for (col = pcol - 1; col < pcol; col++)
        {
        m_Xaffinity[slice * prow * pcol + row * pcol + col] = 0;
        }
      }
    }

  for (slice = 0; slice < pslices; slice++)
    {
    for (row = prow - 1; row < prow; row++)
      {
      for (col = 0; col < pcol; col++)
        {
        m_Yaffinity[slice * prow * pcol + row * pcol + col] = 0;
        }
      }
    }

  for (slice = pslices - 1; slice < pslices; slice++)
    {
    for (row = 0; row < prow; row++)
      {
      for (col = 0; col < pcol; col++)
        {
        m_Zaffinity[slice * prow * pcol + row * pcol + col] = 0;
        }
      }
    }

  m_Material.resize(0);

  //printf("Affinity computation is done! \n");
}


template <class TInputImage, class TOutputImage>
void VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::FastTracking(int object_flag)
{

  int nbor[6][3] = 
  {
    { 1, 0, 0 },
    { 0, 1, 0 },
    { 0, 0, 1 },
    { -1, 0, 0},
    { 0, -1, 0},
    { 0, 0, -1}
  };

  ListType           chash[4096+1];
  typename ListType::iterator  iter;
  int                topIndex;
  int                pslices,prow,pcol;
  IndexType          current,index1,index2;
  unsigned short     affp[6];


  m_InputImage = this->GetInput();
  m_Size = m_InputImage->GetLargestPossibleRegion().GetSize();

  pslices = m_Size[2];
  prow = m_Size[1];
  pcol = m_Size[0];

  for(int i=0 ;i<=4096;i++)
    chash[i].clear();

  if(object_flag == 1)
    {
    /* object tracking...*/
    for( iter  = m_ObjectsSeed[m_SelectedObject-1].begin();
         iter != m_ObjectsSeed[m_SelectedObject-1].end();    iter++)
      {
      current = *iter;
      m_ObjectFuzzyScene->SetPixel(current,4096);
      chash[4096].push_front(current);
      }


    topIndex = 4096;
    while((topIndex>0) && (chash[topIndex].size() !=0))
      {
      current = chash[topIndex].back();
      chash[topIndex].pop_back();

      int x = current[0];
      int y = current[1];
      int z = current[2];

      int k = z*prow*pcol + y*pcol + x;

      affp[0] = m_Xaffinity[k];
      affp[1] = m_Yaffinity[k];
      affp[2] = m_Zaffinity[k];
      if(x>0)
        {
        affp[3] = m_Xaffinity[k-1];
        }
      if(y>0)
        {
        affp[4] = m_Yaffinity[k-pcol];
        }
      if(z>0)
        {
        affp[5] = m_Zaffinity[k-prow*pcol];
        }

      while((topIndex>0) && (chash[topIndex].size() == 0))
        {
        topIndex--;
        }

      unsigned short pmax,pmin,affn;

      pmax = m_ObjectFuzzyScene->GetPixel(current);

      for (int ei = 0; ei < 6; ei++)
        {
        int xx = current[0] + nbor[ei][0];
        int yy = current[1] + nbor[ei][1];
        int zz = current[2] + nbor[ei][2];

        if (xx >= 0 && xx < pcol &&
            yy >= 0 && yy < prow &&
            zz >= 0 && zz < pslices)
          {
          affn = affp[ei];
          pmin = (pmax < affn ? pmax: affn);

          index1[0] = xx;
          index1[1] = yy;
          index1[2] = zz;

          unsigned short value = m_ObjectFuzzyScene->GetPixel(index1);

          if (pmin > value)
            {
            if (value == 0)
              {
              chash[pmin].push_front(index1);
              if (pmin>topIndex) topIndex = pmin;
              }
            else
              {
              typename ListType::iterator iter;

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
              if (pmin>topIndex) topIndex = pmin;
              }

            m_ObjectFuzzyScene->SetPixel(index1,pmin);
            }
          }
        }
      }
    }
  else if(object_flag == 0)
    {
    /* background tracking ... */
    for(int i = 0;i<m_Objects;i++)
      {
      if(i != (m_SelectedObject-1))
        {
        for(iter = m_ObjectsSeed[i].begin();iter!=m_ObjectsSeed[i].end();iter++)
          {
          current = *iter;
          m_BackgroundFuzzyScene->SetPixel(current,4096);
          chash[4096].push_front(current);
          }
        }
      }


    topIndex = 4096;
    while((topIndex>0) && (chash[topIndex].size() !=0))
      {
      current = chash[topIndex].front();
      chash[topIndex].pop_front();

      while((topIndex>0) && (chash[topIndex].size() == 0))
        {
        topIndex--;
        }

      int x = current[0];
      int y = current[1];
      int z = current[2];

      int k = z*prow*pcol + y*pcol + x;

      affp[0] = m_Xaffinity[k];
      affp[1] = m_Yaffinity[k];
      affp[2] = m_Zaffinity[k];
      if(x>0)
        affp[3] = m_Xaffinity[k-1];
      if(y>0)
        affp[4] = m_Yaffinity[k-pcol];
      if(z>0)
        affp[5] = m_Zaffinity[k-prow*pcol];

      unsigned short pmax,pmin,affn;
      pmax = m_BackgroundFuzzyScene->GetPixel(current);

      for (int ei = 0; ei < 6; ei++)
        {
        int xx = current[0] + nbor[ei][0];
        int yy = current[1] + nbor[ei][1];
        int zz = current[2] + nbor[ei][2];

        if (xx >= 0 && xx < pcol &&
            yy >= 0 && yy < prow &&
            zz >= 0 && zz < pslices)
          {
          affn = affp[ei];
          pmin = (pmax < affn ? pmax: affn);

          index1[0] = xx;
          index1[1] = yy;
          index1[2] = zz;

          unsigned short value = m_BackgroundFuzzyScene->GetPixel(index1);

          if (pmin > value)
            {
            if (value == 0)
              {
              chash[pmin].push_front(index1);
              if (pmin>topIndex) topIndex = pmin;
              }
            else
              {
              typename ListType::iterator iter;
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
              if (pmin>topIndex) topIndex = pmin;
              }
            m_BackgroundFuzzyScene->SetPixel(index1,pmin);
            }
          }
        }
      }
    }
}


template <class TInputImage, class TOutputImage>
void VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::GenerateData()
{

  int prow,pcol;

  ScalePrepare();

  Compute_LookupTable();
  Compute_Scale();
  Compute_Filter();
  Compute_Affinity();

  m_InputImage = this->GetInput();
  m_SegmentObject = this->GetOutput(); 

  m_Size = m_InputImage->GetLargestPossibleRegion().GetSize();

  prow = m_Size[1];
  pcol = m_Size[0];

  IndexType index;
  index.Fill(0);

  typename UShortImage::RegionType region;
  region.SetSize(m_Size);
  region.SetIndex(index);
  m_ObjectFuzzyScene = UShortImage::New();  
  m_ObjectFuzzyScene->SetLargestPossibleRegion( region );
  m_ObjectFuzzyScene->SetBufferedRegion( region );
  m_ObjectFuzzyScene->SetRequestedRegion( region );
  m_ObjectFuzzyScene->Allocate();  

  m_BackgroundFuzzyScene = UShortImage::New();  
  m_BackgroundFuzzyScene->SetLargestPossibleRegion( region );
  m_BackgroundFuzzyScene->SetBufferedRegion( region );
  m_BackgroundFuzzyScene->SetRequestedRegion( region );
  m_BackgroundFuzzyScene->Allocate();

  RegionType region1;
  region1.SetSize(m_Size);
  region1.SetIndex(index);
  m_SegmentObject->SetLargestPossibleRegion( region1 );
  m_SegmentObject->SetBufferedRegion( region1 );
  m_SegmentObject->SetRequestedRegion( region1 );
  m_SegmentObject->Allocate();  

  ImageRegionIteratorWithIndex <UShortImage> it1(this->m_ObjectFuzzyScene,region);
  ImageRegionIteratorWithIndex <UShortImage> it2(this->m_BackgroundFuzzyScene,region);

  ImageRegionIteratorWithIndex <OutputImageType> it3(this->m_SegmentObject,region1);

  it1.GoToBegin();
  it2.GoToBegin();

  while(!it1.IsAtEnd())
    {
    it1.Set(0);
    it2.Set(0);
    ++it1;
    ++it2;
    }

  int object_flag = 1;
  FastTracking(object_flag);

  int count, old_count;

  if(m_Objects > 1)
    {
    object_flag = 0;
    FastTracking(object_flag);

    count = 0;
    old_count = 0;
    bool flag = 1;
    int iteration = 0; 


    while(flag)
      {
      old_count = count;
      count  = 0;
      flag = 0;
      iteration = iteration + 1;

      it1.GoToBegin();
      it2.GoToBegin();
      it3.GoToBegin();

      while(!it1.IsAtEnd())
        {
        if(it1.Get() > it2.Get())
          {
          it3.Set(1);

          count++;

          IndexType current = it1.GetIndex();
          if(current[0]>0)
            m_Xaffinity[current[2]*prow*pcol + current[1]*pcol + current[0]-1] = 0;
          if(current[1]>0)
            m_Yaffinity[current[2]*prow*pcol + (current[1]-1)*pcol + current[0]] = 0;
          if(current[2]>0)
            m_Zaffinity[(current[2]-1)*prow*pcol + current[1]*pcol + current[0]] = 0;
          }
        else
          it3.Set(0);
        ++it1;
        ++it2;
        ++it3;
        }
      if(count>old_count)
        {
        flag = 1;

        it2.GoToBegin();
        while(!it2.IsAtEnd())
          {
          it2.Set(0);
          ++it2;
          }
        FastTracking(0);  /* tracking background again */
        }
      }
    }
}

template <class TInputImage, class TOutputImage>
void VectorFuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Objects = "  << m_Objects  << std::endl;
  os << indent << "Selected Object = "  << m_SelectedObject  << std::endl;
}

} // end namespace itk

#endif
