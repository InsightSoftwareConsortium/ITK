#ifndef _itkHybridFilter_txx
#define _itkHybridFilter_txx
#include "itkHybridFilter.h"

namespace itk
{


/**
 * Constructor
 */
template <class TInputImage, class TOutputImage, 
	class TInputMesh, class TOutputMesh>
HybridFilter<TInputImage,TOutputImage,TInputMesh,TOutputMesh>
::HybridFilter()
{
  m_IterNum = 0;
}



/**
 * Set the balloon force filter
 */
template <class TInputImage, class TOutputImage, 
	class TInputMesh, class TOutputMesh>
void
HybridFilter<TInputImage,TOutputImage,TInputMesh,TOutputMesh>
::SetBalloonForceFilter( BalloonForceFilterPointer	bffilter )
{
  m_BalloonForceFilter = bffilter;
}

/**
 * Set the gibbs prior filter
 */
template <class TInputImage, class TOutputImage, 
	class TInputMesh, class TOutputMesh>
void
HybridFilter<TInputImage,TOutputImage,TInputMesh,TOutputMesh>
::SetGibbsPriorFilter( GibbsPriorFilterPointer	gpfilter )
{
  m_GibbsPriorFilter = gpfilter;
}

/**
 * Set the gibbs prior filter input
 */
template <class TInputImage, class TOutputImage, 
	class TInputMesh, class TOutputMesh>
void
HybridFilter<TInputImage,TOutputImage,TInputMesh,TOutputMesh>
::SetGibbsInput()
{
  const typename TInputImage::Pointer   inputImage(    GetInput()   );
  m_GibbsPriorFilter->SetInput(inputImage);
}

/**
 * Send balloon force filter a new potential from the gibbs prior model
 */
template <class TInputImage, class TOutputImage, 
	class TInputMesh, class TOutputMesh>
void
HybridFilter<TInputImage,TOutputImage,TInputMesh,TOutputMesh>
::SetPotential( void )
{
  m_BalloonForceFilter->SetPotential(m_GibbsPriorFilter->GetOutput());
}

/**
 * Send balloon force filter a new potential from the gibbs prior model
 */
template <class TInputImage, class TOutputImage, 
	class TInputMesh, class TOutputMesh>
void
HybridFilter<TInputImage,TOutputImage,TInputMesh,TOutputMesh>
::SetObjectRegion( void )
{
  m_GibbsPriorFilter->SetTrainingImage(m_BalloonForceFilter->GetImageOutput());
}

/**
 * One iteration of gibbs prior model and the deformable model
 */
template <class TInputImage, class TOutputImage, 
	class TInputMesh, class TOutputMesh>
void
HybridFilter<TInputImage,TOutputImage,TInputMesh,TOutputMesh>
::Advance( void )
{
  if (m_IterNum != 0) m_GibbsPriorFilter->Modified();
  m_GibbsPriorFilter->Update();
  if (m_IterNum != 0) m_BalloonForceFilter->Modified();
  m_BalloonForceFilter->Update();
}

/**
 * Compute the output image
 */
template <class TInputImage, class TOutputImage, 
	class TInputMesh, class TOutputMesh>
void
HybridFilter<TInputImage,TOutputImage,TInputMesh,TOutputMesh>
::GenerateData()
{
  std::cout << "Hi, HybridFilter generating data ";
  std::cout << std::endl;

  const typename TInputImage::Pointer   inputImage(    GetInput()   );
        typename TOutputImage::Pointer  outputImage(   GetOutput()  );
		typename TOutputImage::Pointer	outputGp = m_GibbsPriorFilter->GetOutput();

  outputImage->SetLargestPossibleRegion( 
      inputImage->GetLargestPossibleRegion() );

  outputImage->SetBufferedRegion( 
      inputImage->GetBufferedRegion() );

  outputImage->SetRequestedRegion( 
      inputImage->GetRequestedRegion() );

  outputImage->Allocate();
  OutputImageIterator outit(outputImage,
	  inputImage->GetRequestedRegion());

  while (m_IterNum != 5) {
	Advance();
	m_IterNum++;
  }

  OutputImageIterator gpit(outputGp, outputGp->GetBufferedRegion());

  outit.Begin();
  gpit.Begin();

  while( !gpit.IsAtEnd() ) {
	outit.Set(gpit.Get());
	++outit;
	++gpit;
  }
}


} // end namespace itk

#endif
