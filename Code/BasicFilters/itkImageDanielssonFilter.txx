/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageDanielssonFilter.txx itkImageDanielssonFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include <itkObjectFactory.h>
#include <itkSimpleImageRegionIterator.h>
#include "itkImageDanielssonFilter.h"

namespace itk
{

 /**
  *    Constructor
  */
template <class TInputImage,class TOutputImage>
ImageDanielssonFilter<TInputImage,TOutputImage>
::ImageDanielssonFilter()
{
  m_outputimage = TOutputImage::New();
  m_outputclosestpoints = TInputImage::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(2);
  this->ProcessObject::SetNthOutput(0,m_outputimage.GetPointer());
  this->ProcessObject::SetNthOutput(1,m_outputclosestpoints.GetPointer());
  m_SquaredDistance = false;
  if (TInputImage::ImageDimension == 2) {
    m_Metric = EIGHT;
  }
  else {
    m_Metric = SIX;
  }
  m_Dimension=TInputImage::ImageDimension;
  m_ClosestComputation= false;
}

 /**
  *  Set if the squared distance should be calculated
  */
template <class TInputImage,class TOutputImage>
void
ImageDanielssonFilter<TInputImage,TOutputImage>
::SetSquaredDistance(bool NewSquareDistance)
{
  m_SquaredDistance = NewSquareDistance;
}

 /**
  *  Get if the squared distance should be calculated
  */
template <class TInputImage,class TOutputImage>
bool
ImageDanielssonFilter<TInputImage,TOutputImage>
::GetSquareDistance()
{
  return m_SquaredDistance;
}


 /**
  *  Set the metric
  */
template <class TInputImage,class TOutputImage>
void
ImageDanielssonFilter<TInputImage,TOutputImage>
::SetMetric(unsigned char NewMetric)
{
  m_Metric = (Metric) NewMetric;
}

 /**
  *  Get the metric
  */
template <class TInputImage,class TOutputImage>
unsigned char
ImageDanielssonFilter<TInputImage,TOutputImage>
::GetMetric()
{
  return m_Metric;
}


 /**
  *  Set if ClosestPointsImage should be computed
  */
template <class TInputImage,class TOutputImage>
void
ImageDanielssonFilter<TInputImage,TOutputImage>
::SetClosestComputation(bool NewClosestComputation)
{
  m_ClosestComputation = NewClosestComputation;
}

 /**
  *  Get the state of ClosestPoints
  */
template <class TInputImage,class TOutputImage>
unsigned char
ImageDanielssonFilter<TInputImage,TOutputImage>
::GetClosestComputation()
{
  return m_ClosestComputation;
}


 /**
  *  Connect a source image to filter
  */
template <class TInputImage,class TOutputImage>
void
ImageDanielssonFilter<TInputImage,TOutputImage>
::SetInputImage( InputImagePointer inputimage)
{
  this->m_inputimage = inputimage;
  SetNthInput(0,m_inputimage.GetPointer());
}


 /**
  *  Return the distance map Image pointer
  */
template <class TInputImage,class TOutputImage>
ImageDanielssonFilter<TInputImage,TOutputImage>::OutputImagePointer 
ImageDanielssonFilter<TInputImage,TOutputImage>::GetOutput(void)
{
  return static_cast<TOutputImage*>(this->ProcessObject::GetOutput(0).GetPointer());
}


 /**
  *  Return Closest Points Map
  */
template <class TInputImage,class TOutputImage>
ImageDanielssonFilter<TInputImage,TOutputImage>::InputImagePointer 
ImageDanielssonFilter<TInputImage,TOutputImage>
::GetClosestPoints(void)
{
  return static_cast<TInputImage*>(this->ProcessObject::GetOutput(1).GetPointer());
}


 /**
  *  Executes filter. Performs Danielsson Computation
  */
template <class TInputImage,class TOutputImage>
void
ImageDanielssonFilter<TInputImage,TOutputImage>
::Execute(void)
{

 long size,sizex, sizey, sizez; /*different sizes of the input image*/
 short *dx, *dy, *dz;  /*distance arrays*/
 long i; /*counter*/

 /**
  * Set the (x,y,z) sizes of the input Image
  */

  TInputImage::RegionType m_input_region = m_inputimage->GetRequestedRegion();
  TInputImage::SizeType   input_image_size   = m_input_region.GetSize();

  sizex = input_image_size[0];
  sizey = input_image_size[1];
  sizez = input_image_size[2];

  if( m_Dimension == 2 ) {
    size = sizex*sizey;
	unsigned long Imagesize[] = {sizex,sizey};
  } 
  else {
    size = sizex*sizey*sizez;
    unsigned long Imagesize[] = {sizex,sizey,sizez};
  }

 
 /*define size*/

  double origin3D[3]  = {0, 0, 0};
  double spacing3D[3] = {1, 1, 1};
 
  TOutputImage::RegionType m_output_region = m_inputimage->GetRequestedRegion();

  m_outputimage->SetLargestPossibleRegion( m_inputimage->GetLargestPossibleRegion() );
  m_outputimage->SetBufferedRegion( m_inputimage->GetBufferedRegion() );
  m_outputimage->SetRequestedRegion( m_inputimage->GetRequestedRegion() );
  m_outputimage->SetOrigin(origin3D);
  m_outputimage->SetSpacing(spacing3D);
  
  m_outputimage->Allocate();

  TInputImage::RegionType m_outputclosestpoints_region = m_inputimage->GetRequestedRegion() ;

  m_outputclosestpoints->SetLargestPossibleRegion(m_inputimage->GetLargestPossibleRegion());
  m_outputclosestpoints->SetBufferedRegion( m_inputimage->GetBufferedRegion() );
  m_outputclosestpoints->SetRequestedRegion( m_inputimage->GetRequestedRegion() );

  m_outputclosestpoints->SetOrigin(origin3D);
  m_outputclosestpoints->SetSpacing(spacing3D);

  m_outputclosestpoints->Allocate();

  InputImageIterator itinput(m_inputimage,m_input_region);
  OutputImageIterator itoutput(m_outputimage,m_output_region);
 
  itoutput.Begin();
   /**
	* this function does the real work 
	*/
	Danielsson(m_inputimage,m_Metric,dx,dy,dz); 
    
	/**
	 *post processing, figure out the new image
	 */
  if(m_SquaredDistance) {
    for(i=0;i<size;i++) {
      if(m_Dimension == 3){
	    itoutput.Set((TOutputImage::PixelType)
				      (dx[i]*dx[i] +dy[i]*dy[i] +dz[i]*dz[i]));
	  }
	  else {
	    itoutput.Set((TOutputImage::PixelType) 
				        (dx[i]*dx[i] +dy[i]*dy[i]));
	  }
	  ++itoutput;
	}
  } 
  else {
    for(i=0;i<size;i++) {
	  if(m_Dimension == 3) {
	    itoutput.Set( (TOutputImage::PixelType) 
						(sqrt(dx[i]*dx[i] + dy[i]*dy[i] + dz[i]*dz[i]) ) );	
	  }
	  else {
	    itoutput.Set( (TOutputImage::PixelType) 
						                (sqrt(dx[i]*dx[i] +dy[i]*dy[i]) ) );
	  }  
	  ++itoutput; 
    }		
  }

  delete [] dx;
  delete [] dy;

  if(m_Dimension == 3) {
    delete [] dz;
  }

}


/**
 *update is called many times from within Danielsson function
 *It compairs the current distance map with a nearby
 *map (decided by ddx,ddy,ddz). If the nearby map has
 *a closer distance than 1+the current, the current
 *becomes 1+the nearby distance
 */
template <class TInputImage,class TOutputImage>
inline void
ImageDanielssonFilter<TInputImage,TOutputImage>
::update(
		 int &distance,			/*current distance*/
		 int *square_table,	   /*table of squares*/
	     short *&dx,short *&dy,short *&dz,	
		 int ddx,int ddy, int ddz, /*change in each d*/
		 int size,int sizex, int sizey, int sizez,
		 int index){
  int dindex; 
  dindex = index + ddx;
  dindex += (ddy * sizex);
  int newDistance;
  if(m_Dimension == 3) {
    dindex += (ddz * sizex * sizey);
	newDistance = square_table[dx[dindex]+ddx] 
			      + square_table[dy[dindex]+ddy] 
			      + square_table[dz[dindex]+ddz];
  }
  else {
    newDistance = square_table[dx[dindex]+ddx] 
		          +square_table[dy[dindex]+ddy];
  }
  if(newDistance < distance) {
    dx[index] = dx[dindex] + ddx;
	dy[index] = dy[dindex] + ddy;
	if(m_Dimension == 3) {
	  dz[index] = dz[dindex] + ddz;
	}
    distance = newDistance;
  }
}


/**
 * Filters distances throughout the image depending on the metric
 */
template <class TInputImage,class TOutputImage>
void
ImageDanielssonFilter<TInputImage,TOutputImage>
::Danielsson(const InputImagePointer InputImage,unsigned char metric,
			short *&dx,short *&dy,short *&dz
			)
{
  int x,y,z,index; 	/*these are counters*/
  int *sqtab;			/*this array holds the square table*/
  int *sq;			/*this array helps in the creation of sqtab*/
  int maxsize;			/*maxsize of sqtab*/
  int sizexminus1,sizeyminus1; /*prevents array overruns*/
  int dist;					/*current distance calculation*/

  TInputImage::RegionType InputImage_region = InputImage->GetLargestPossibleRegion();
  TInputImage::SizeType   InputImage_size   = InputImage_region.GetSize();

  int sizex = InputImage_size[0];
  int sizey = InputImage_size[1];
  int sizez = InputImage_size[2];

  if( (m_Dimension != 2) && (m_Dimension != 3) ) {
    std::cout << "This operation is for 2D-3D images only";
	return;
  }
  int size;
  long sizexy ;
  if(m_Dimension == 3) {
    sizexy = sizex*sizey,
	size = sizex*sizey*sizez;
  }
  else {
    size = sizex*sizey;
  }	

  /*Assume data is already Initialized in Execute()*/
  maxsize = sizex;
  if(sizey > maxsize) {
    maxsize = sizey;
  }
  if(m_Dimension == 3) {
    if(sizez > maxsize) {
	  maxsize = sizez;
	}
  }
  maxsize = 4 * maxsize;
  sqtab = new int[4*maxsize+1];

  try {
    sqtab = new int[4*maxsize+1];
    dx = new short[size+1];
	dy = new short[size+1];
	if(m_Dimension == 3) {
	  dz = new short[size+1];
  	}
  }
  catch (itk::ExceptionObject &e) { 
    std::cout << e << std::endl; 
  }

  /**
   * this is a clever way to fill up the array with 
   * an equal number of positives and negatives
   */
  sq = sqtab;
  sqtab = &sqtab[2*maxsize];
  for(index=(-2*maxsize);index<=(2*maxsize);index++) {
    sqtab[index] = index * index;
  }
 
  InputImageIterator itInputImage(InputImage,InputImage_region);

  itInputImage.Begin();

  for(index=0;index<size;index++) {
    TOutputImage::PixelType value = itInputImage.Get();
	if(value > 0) {
	  dx[index] = dy[index] = 0;
	  if (m_Dimension == 3) {
	    dz[index] = 0;
	  }
	}
	else {
	  dx[index] = dy[index] =  maxsize;
	  if (m_Dimension == 3) {
	    dz[index] = maxsize;
	  }
	}
	++itInputImage;
  }                          

  /**
   *2D Image
   */
  if (m_Dimension == 2) {
    /*2D forwward scan*/
    for(y=1;y<sizey;y++) {
   	  sizexminus1 = sizex - 1;
      index = y * sizex;
     	for(x=0;x<sizex;x++,index++) {
     	  dist = sqtab[dx[index]] + sqtab[dy[index]];
		  update(dist,sqtab,dx,dy,dz,0,-1,0,size,sizex,sizey,sizez,index);
       	  if(m_Metric == EIGHT) {
		    if(x > 0) {
			update(dist,sqtab,dx,dy,dz,-1,-1,0,size,sizex,sizey,sizez,index);
			}
			if(x < sizexminus1) {
			  update(dist,sqtab,dx,dy,dz,1,-1,0,size,sizex,sizey,sizez,index);
		    }
       	  }
		}
   	    index = y * sizex + 1;
     	for(x=1;x<sizex;x++,index++) {
     	  dist = sqtab[dx[index]] + sqtab[dy[index]];
	      update(dist,sqtab,dx,dy,dz,-1,0,0,size,sizex,sizey,sizez,index);
        }
  	    index = (y + 1)  * sizex - 2;
        for(x=sizex-2;x>=0;x--,index--) {
          dist = sqtab[dx[index]] + sqtab[dy[index]];
		  update(dist,sqtab,dx,dy,dz,1,0,0,size,sizex,sizey,sizez,index);
        }
      }
      
	/*2D backward scan*/
    for(y=(sizey-2);y>=0;y--) {
   	  sizexminus1 = sizex - 1;
	  index = y * sizex;
   	  for(x=0;x<sizex;x++,index++) {
        dist = sqtab[dx[index]] + sqtab[dy[index]];
		update(dist,sqtab,dx,dy,dz,0,1,0,size,sizex,sizey,sizez,index);
        if(m_Metric == EIGHT) {
		  if(x > 0) {
		    update(dist,sqtab,dx,dy,dz,-1,1,0,size,
							             sizex,sizey,sizez,index);
		  }
		  if(x < sizexminus1) {
		    update(dist,sqtab,dx,dy,dz,1,1,0,size,sizex,sizey,sizez,
											                   index);
		  }
	    }
	  }
      index = y * sizex + 1;
      for(x=1;x<sizex;x++,index++) {
        dist = sqtab[dx[index]]+ sqtab[dy[index]];
		update(dist,sqtab,dx,dy,dz,-1,0,0,size,sizex,sizey,sizez,index);
      } 	
  	  index = (y + 1)  * sizex - 2;
      for(x=sizex-2;x>=0;x--,index--) {
        dist = sqtab[dx[index]] + sqtab[dy[index]];
		update(dist,sqtab,dx,dy,dz,1,0,0,size,sizex,sizey,sizez,index);
      }   			
    }

    if(m_ClosestComputation == true) {

      TInputImage::RegionType m_outputclosestpoints_region = m_outputclosestpoints->GetRequestedRegion();
      InputImageIterator itClosestPoints(m_outputclosestpoints,m_outputclosestpoints_region);
  
      itClosestPoints.Begin();
      while( !itClosestPoints.IsAtEnd() ) {
        itClosestPoints.Set(0);
        ++itClosestPoints;
      }

      itClosestPoints.Begin();
	  for(y=0;y<sizey;y++) {
		index = y * sizex;
     	for(x=0;x<sizex;x++,index++) {
       	  if(((x+dx[index]) < 0) 
			 || ((x+dx[index]) > (sizex - 1)) 
			 ||((y+dy[index]) < 0) 
			 || ((y+dy[index]) > (sizey - 1))) {
		    continue;
		  }
       	  if(itClosestPoints.Get() == 0) {  
			itk::Index<TInputImage::ImageDimension> myindex ;
			myindex[0] = x+dx[index];
			myindex[1] = y+dy[index];
			itClosestPoints.Set(m_inputimage->GetPixel(myindex));	
		  }
		  ++itClosestPoints;
       	}
      }
    }
  }

/**
 * 3D Image
 */
  else {
    /*3D forward scan*/
    for(z=1;z<sizez;z++) {
   	  sizeyminus1 = sizey - 1;
   	  sizexminus1 = sizex - 1;
   	  for(y=0;y<sizey;y++) {
     	index = z * sizexy + y * sizex;
     	for(x=0;x<sizex;x++,index++) {
     	  dist = sqtab[dx[index]] + sqtab[dy[index]] + sqtab[dz[index]];
	      update(dist,sqtab,dx,dy,dz,0,0,-1,size,sizex,sizey,sizez,index);
       	  if(m_Metric != SIX) {
		    if(y > 0) {
			  update(dist,sqtab,dx,dy,dz,0,-1,-1,size,sizex,sizey,sizez,index);
			}
			if(y < sizeyminus1) {
			  update(dist,sqtab,dx,dy,dz,0,1,-1,size,sizex,sizey,sizez,index);
			}
		    if(x > 0) {
			  update(dist,sqtab,dx,dy,dz,-1,0,-1,size,sizex,sizey,sizez,index);
			}
			if(x < sizexminus1) {
			  update(dist,sqtab,dx,dy,dz,1,0,-1,size,sizex,sizey,sizez,index);
			}
            if(m_Metric == TWENTY_SIX) {
		      if(y > 0){
		        if(x > 0) {
			      update(dist,sqtab,dx,dy,dz,-1,-1,-1,size,sizex,sizey,sizez,index);
			    }
			    if(x < sizexminus1) {
			    update(dist,sqtab,dx,dy,dz,1,-1,-1,size,sizex,sizey,sizez,index);
                }
			  }
			  if(y < sizeyminus1){
			    if(x > 0) {
			      update(dist,sqtab,dx,dy,dz,-1,1,-1,size,sizex,sizey,sizez,index);
			    }
		        if(x < sizexminus1) {
			      update(dist,sqtab,dx,dy,dz,1,1,-1,size,sizex,sizey,sizez,index);
			    }
		      }
      	    }
       	  }
   	    }
      }

      for(y=1;y<sizey;y++) {
        index = z * sizexy + y * sizex;
   	    for(x=0;x<sizex;x++,index++) {
          dist = sqtab[dx[index]] + sqtab[dy[index]] + sqtab[dz[index]];
	      update(dist,sqtab,dx,dy,dz,0,-1,0,size,sizex,sizey,sizez,index);
     	  if(m_Metric != SIX) {
		    if(x > 0) {
		      update(dist,sqtab,dx,dy,dz,-1,-1,0,size,sizex,sizey,sizez,index);
		    }
		    if(x < sizexminus1) {
		      update(dist,sqtab,dx,dy,dz,+1,-1,0,size,sizex,sizey,sizez,index);
     	    }
		  }
   	    }
   	    index = z * sizexy + y * sizex + 1;
   	    for(x=1;x<sizex;x++,index++) {
          dist = sqtab[dx[index]] + sqtab[dy[index]] + sqtab[dz[index]];
		  update(dist,sqtab,dx,dy,dz,-1,0,0,size,sizex,sizey,sizez,index);
   	    }
  	    index = z * sizexy + (y + 1) * sizex - 2;
   	    for(x=(sizex-2);x>=0;x--,index--) {
   	      dist = sqtab[dx[index]] + sqtab[dy[index]] + sqtab[dz[index]];
		  update(dist,sqtab,dx,dy,dz,1,0,0,size,sizex,sizey,sizez,index);
        }
  	  }  
   	  for(y=(sizey-2);y>=0;y--) {
   	    index = z * sizexy + y * sizex;
   	    for(x=0;x<sizex;x++,index++) {
     	   dist = sqtab[dx[index]] + sqtab[dy[index]] + sqtab[dz[index]];
		  update(dist,sqtab,dx,dy,dz,0,1,0,size,sizex,sizey,sizez,index);
     	  if(m_Metric != SIX) {
		    if(x > 0) {
			  update(dist,sqtab,dx,dy,dz,-1,1,0,size,sizex,sizey,sizez,index);
		    }
		    if(x < sizexminus1) {
		      update(dist,sqtab,dx,dy,dz,1,1,0,size,sizex,sizey,sizez,index);
		    }
          }
   	    }
   	    index = z * sizexy + y * sizex + 1;
   	    for(x=1;x<sizex;x++,index++) {
          dist = sqtab[dx[index]] + sqtab[dy[index]] + sqtab[dz[index]];
	      update(dist,sqtab,dx,dy,dz,-1,0,0,size,sizex,sizey,sizez,index);
   	    }
        index = z * sizexy + (y + 1) * sizex - 2;
   	    for(x=(sizex-2);x>=0;x--,index--) {
          dist = sqtab[dx[index]] + sqtab[dy[index]] + sqtab[dz[index]];
	      update(dist,sqtab,dx,dy,dz,1,0,0,size,sizex,sizey,sizez,index);
  	    }
      }
    }

	/*3D backward scan*/

 	for(z=sizez-2;z>=0;z--) {
   	  sizeyminus1 = sizey - 1;
   	  sizexminus1 = sizex - 1;
   	  for(y=0;y<sizey;y++) {
   		index = z * sizexy + y * sizex;
   		for(x=0;x<sizex;x++,index++) {
     	  dist = sqtab[dx[index]] + sqtab[dy[index]]+ sqtab[dz[index]];
		  update(dist,sqtab,dx,dy,dz,0,0,1,size,sizex,sizey,sizez,index);
     	  if(m_Metric != SIX) {
		    if(y > 0) {
			  update(dist,sqtab,dx,dy,dz,0,-1,1,size,sizex,sizey,sizez,index);
			}
			if(y < sizeyminus1) {
			  update(dist,sqtab,dx,dy,dz,0,1,1,size,sizex,sizey,sizez,index);
            }
			if(x > 0) {
			  update(dist,sqtab,dx,dy,dz,-1,0,1,size,sizex,sizey,sizez,index);
			}
			if(x < sizexminus1) {
			  update(dist,sqtab,dx,dy,dz,1,0,1,size,sizex,sizey,sizez,index);
			}
     		if(m_Metric == TWENTY_SIX) {
			  if(y > 0){
			    if(x > 0) {
				  update(dist,sqtab,dx,dy,dz,-1,-1,1,size,sizex,sizey,sizez,index);
				}
				if(x < sizexminus1) {
				  update(dist,sqtab,dx,dy,dz,1,-1,1,size,sizex,sizey,sizez,index);
				}
			  }
			  if(y<sizeyminus1) {
			    if(x > 0) {
				  update(dist,sqtab,dx,dy,dz,-1,1,1,size,sizex,sizey,sizez,index);
				}
			    if(x < sizexminus1) {
				  update(dist,sqtab,dx,dy,dz,1,1,1,size,sizex,sizey,sizez,index);
				}
              }
     	    }
     	  }
   		}
   	  }
   	  for(y=1;y<sizey;y++) {
        index = z * sizexy + y * sizex;
     	for(x=0;x<sizex;x++,index++) {
       	  dist = sqtab[dx[index]]+ sqtab[dy[index]] + sqtab[dz[index]];
          update(dist,sqtab,dx,dy,dz,0,-1,0,size,sizex,sizey,sizez,index);
       	  if(m_Metric != SIX) {
		    if(x > 0) {
			  update(dist,sqtab,dx,dy,dz,-1,-1,0,size,sizex,sizey,sizez,index);
			}
	        if(x < sizexminus1) {
			  update(dist,sqtab,dx,dy,dz,1,-1,0,size,sizex,sizey,sizez,index);
			}
      	  }
    	}
     	index = z * sizexy + y * sizex + 1;
     	for(x=1;x<sizex;x++,index++) {
       	  dist = sqtab[dx[index]] + sqtab[dy[index]] + sqtab[dz[index]];
		  update(dist,sqtab,dx,dy,dz,-1,0,0,size,sizex,sizey,sizez,index);
     	}
    	index = z * sizexy + (y + 1) * sizex - 2;
     	for(x=(sizex-2);x>=0;x--,index--) {
       	  dist = sqtab[dx[index]] + sqtab[dy[index]] + sqtab[dz[index]];
		  update(dist,sqtab,dx,dy,dz,1,0,0,size,sizex,sizey,sizez,index);
     	}
      }
      for(y=(sizey-2);y>=0;y--) {
        index = z * sizexy + y * sizex;
     	for(x=0;x<sizex;x++,index++) {
       	  dist = sqtab[dx[index]] + sqtab[dy[index]] + sqtab[dz[index]];
		  update(dist,sqtab,dx,dy,dz,0,1,0,size,sizex,sizey,sizez,index);
      	  if(m_Metric != SIX) {
		    if(x > 0) {
			  update(dist,sqtab,dx,dy,dz,-1,1,0,size,sizex,sizey,sizez,index);
			}
			if(x < sizexminus1) {
			  update(dist,sqtab,dx,dy,dz,1,1,0,size,sizex,sizey,sizez,index);
			}
      	  }
     	}
     	index = z * sizexy + y * sizex + 1;
     	for(x=1;x<sizex;x++,index++) {
       	  dist = sqtab[dx[index]] + sqtab[dy[index]] + sqtab[dz[index]];
		  update(dist,sqtab,dx,dy,dz,-1,0,0,size,sizex,sizey,sizez,index);
    	}
     	index = z * sizexy + (y + 1) * sizex - 2;
     	for(x=(sizex-2);x>=0;x--,index--) {
       	  dist = sqtab[dx[index]] + sqtab[dy[index]] + sqtab[dz[index]];
		  update(dist,sqtab,dx,dy,dz,1,0,0,size,sizex,sizey,sizez,index);
     	}
	  }
	}

    if(m_ClosestComputation == true) {
      TInputImage::RegionType m_outputclosestpoints_region = m_outputclosestpoints->GetLargestPossibleRegion();
      InputImageIterator itClosestPoints(m_outputclosestpoints,m_outputclosestpoints_region);
      itClosestPoints.Begin();
      
	  while( !itClosestPoints.IsAtEnd() ) {
      itClosestPoints.Set(0);
      ++itClosestPoints;
      }

      itClosestPoints.Begin();
	  for(z=0;z<sizez;z++) {
	    for(y=0;y<sizey;y++) {
	      index = z * sizexy + y * sizex;
     	  for(x=0;x<sizex;x++,index++) {
       	    if(((x+dx[index]) < 0) 
			  || ((x+dx[index]) > (sizex - 1)) 
			  || ((y+dy[index]) < 0) 
			  || ((y+dy[index]) > (sizey - 1)) 
			  || ((z+dz[index]) < 0) 
			  || ((z+dz[index]) > (sizez - 1))) {
			  continue;
			}
       	    if(itClosestPoints.Get() == 0) { 
		      itk::Index<TInputImage::ImageDimension> myindex ;
		      myindex[0] = x+dx[index];
			  myindex[1] = y+dy[index];
			  myindex[2] = z+dz[index];
			  itClosestPoints.Set(m_inputimage->GetPixel(myindex));			
		    }
		    ++itClosestPoints;
       	  }
        }
	  }
    }

  }//end 3D Image

  delete [] sq;
}




} // end namespace itk