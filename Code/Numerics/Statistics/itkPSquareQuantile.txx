/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPSquareQuantile.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPSquareQuantile_txx
#define __itkPSquareQuantile_txx

namespace itk{ 
  namespace Statistics{

template< class TData >
double
PSquareQuantile< TData >
::operator() (DataContainerPointer data, long dimension, 
              double pthQuantile)
{
  m_Data = data ;
  m_Dimension = dimension ;

  q.resize(5) ;
  n.resize(5) ;
  np.resize(5) ;
  dn.resize(5) ;

  ValueType firstFive[5] ;
  ValueType tempFive[5] ;

  // ==================
  // A. Initialization
  // ==================

  InstanceIdentifier i, j ;
  int direction ;
  int k ;
  typename TData::SizeValueType containerSize = data->GetSize(m_Dimension) ;

  // Sort the first 5 observations
  for (i = 0 ; i < 5 ; i++)
    {
      tempFive[i] = firstFive[i] = this->GetValue( i) ;
    }
  
  const int N = sizeof(tempFive) / sizeof(ValueType) ;
  std::sort(tempFive, tempFive + 5) ;
  
  for (i = 0 ; i < 5 ; i++)
    {
      this->SetValue( i, tempFive[i]) ;
    }
  
  for (i = 0; i <5; i++)
    {
      // Marker heights
      q[i] = this->GetValue( i) ;
      n[i] = i ;         
    }
  
  // desired marker positions
  np[0] = 1;
  np[1] = 1 + 2 * pthQuantile ;
  np[2] = 1 + 4 * pthQuantile ;
  np[3] = 3 + 2 * pthQuantile ;
  np[5] = 5;
  
  // marker increments
  dn[0] = 0;
  dn[1] = pthQuantile / 2;
  dn[2] = pthQuantile ;
  dn[3] = (1 + pthQuantile) / 2;
  dn[4] = 1;
  
  //std::cout << "Debug: Pass initialization" << std::endl ;

  // ===========================
  // B. Subsequent Observations
  // ===========================


  for (j = 5 ; j < containerSize ; j++) 
    {
      // 1. Find cells and make adjustments
      ValueType v = this->GetValue( j) ;
      if (v < q[0]) 
        {
          q[0] = v ;
          k = 0 ;
        } 
      else if (v < q[1]) 
        {
          k = 0 ;
        }
      else if (v < q[2]) 
        {
          k = 1 ;
        }
      else if (v < q[3]) 
        {
          k = 2 ;
        } 
      else if (v <= q[4]) 
        {
          k = 3 ;
        }
      else 
        {
          q[4] = v;
          k = 3 ;
        }

      // std::cout << "Debug: Pass adjust extreme values" << std::endl ;
      // 2. Increment positions of m
      for (i = k + 1 ; i < 5 ; i++) 
        {
          n[i]++;
        }
      
      for (i = 0 ; i < 4 ; i++) 
        {
          np[i] += dn[i] ;
        }
      
      //std::cout << "Debug: Pass position increment" << std::endl ;
      // 3. Adjust heights of m 1-3 if necessary
      for (i = 1; i < 4; i++)
        {
          double di ;
          di = np[i] - n[i] ;
          if ( ((di >= 1) && ((n[i + 1] - n[i]) > 1)) ||
               ((di <= -1) && ((n[i - 1] - n[i]) < -1)) ) 
            {
              double qip;
              direction = di >= 0 ? 1 : -1;
              qip = ParabolicEstimate(i, direction);
              if ( q[i - 1] < qip && qip < q[i + 1] )
                {
                  q[i] = qip;
                }
              else
                {
                  q[i] = LinearEstimate(i, direction);
                }
              
              n[i] += direction;
            }
        }
     // std::cout << "Debug: Pass adjust height" << std::endl ;
    } // end of big for loop
  

  // restore original first five values
  for (i = 0 ; i < 5 ; i++)
    {
      this->SetValue(i, firstFive[i]) ;
    }

  if (pthQuantile == 0.0)
    {
      return q[0] ;
    }
  else if (pthQuantile == 1.0) 
    {
      return q[4] ;
    }
  else
    {
      return q[2] ;
    }
}


template< class TData >
void
PSquareQuantile< TData >
::PrintSelf(std::ostream& os, Indent indent) const
{
  unsigned int i ;

  os << indent << "Data: " << m_Data << std::endl;
  os << indent << "Dimension: " << m_Dimension << std::endl;
  os << indent << "MarkerHeights: [" ;
  for (i=0; i < 4; i++)
    {
    os << q[i] << ", ";
    }
  os << q[i] << "]" << std::endl;
  os << indent << "MarkerPositions: [" ;
  for (i=0; i < 4; i++)
    {
    os << n[i] << ", ";
    }
  os << n[i] << "]" << std::endl;
  os << indent << "DesiredMarkerPositions: [" ;
  for (i=0; i < 4; i++)
    {
    os << np[i] << ", ";
    }
  os << np[i] << "]" << std::endl;
  os << indent << "IncrementInDesiredMarkerPostions: [" ;
  for (i=0; i < 4; i++)
    {
    os << dn[i] << ", ";
    }
  os << dn[i] << "]" << std::endl;
}
  } // end of namespace Statistics
} // end of namespace itk

#endif
