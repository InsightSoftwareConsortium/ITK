/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMRegistrationFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for detail.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkFEMRegistrationFilter_txx_
#define _itkFEMRegistrationFilter_txx_

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif
 
#include "itkFEMRegistrationFilter.h"
#include "itkFEMElements.h"
#include "itkFEMLoadBC.h"

#include "itkRawImageIO.h"   
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkCastImageFilter.h"
#include "itkRecursiveMultiResolutionPyramidImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkDerivativeImageFilter.h"
#include "itkVectorNeighborhoodOperatorImageFilter.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkBSplineInterpolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.h"

#include <vnl/algo/vnl_determinant.h>

namespace itk {
namespace fem {



template<class TReference,class TTarget>
FEMRegistrationFilter<TReference,TTarget>::~FEMRegistrationFilter( )
{
}

template<class TReference,class TTarget>
FEMRegistrationFilter<TReference,TTarget>::FEMRegistrationFilter( )
{
  m_Load = 0;
  m_FileCount=0; 
 
  m_MinE=0;
  m_MinE=vnl_huge_val(m_MinE);  

  m_DescentDirection=positive;
  m_E.resize(1);
  m_Gamma.resize(1);
  m_Rho.resize(1);
  m_E[0]=1.; 
  m_Rho[0]=1.;
  m_CurrentLevel=0;
  m_Maxiters.resize(1);
  m_Maxiters[m_CurrentLevel]=1;  
  m_dT=1;
  m_Alpha=1.0;
  m_Temp=0.0;
  m_MeshPixelsPerElementAtEachResolution.resize(1);
  m_NumberOfIntegrationPoints.resize(1);
  m_NumberOfIntegrationPoints[m_CurrentLevel]=4;
  m_MetricWidth.resize(1);
  m_MetricWidth[m_CurrentLevel]=3;
  m_DoLineSearchOnImageEnergy=1;
  m_LineSearchMaximumIterations=100;
  m_UseMassMatrix=true;

  m_NumLevels=1;
  m_MaxLevel=1;
  m_MeshStep=2;
  m_MeshLevels=1;
  m_DoMultiRes=false;
  m_UseLandmarks=false;

  m_TotalIterations=0;

  m_ReadMeshFile=false;

  for (unsigned int i=0; i < ImageDimension; i++)
    {
    m_ImageScaling[i]=1;
    m_ImageSize[i]=0;
    m_ImageOrigin[i]=0;
    }
  m_FloatImage=NULL;
}


template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::RunRegistration()
{
  std::cout << "beginning registration\n";  

  // Solve the system in time 

  if (!m_DoMultiRes && m_Maxiters[m_CurrentLevel] > 0) 
    {
    SolverType mySolver;
    mySolver.SetDeltatT(m_dT);  
    mySolver.SetRho(m_Rho[m_CurrentLevel]);      
    mySolver.SetAlpha(m_Alpha); 
    
    CreateMesh((double)m_MeshPixelsPerElementAtEachResolution[m_CurrentLevel],mySolver); 
    ApplyLoads(mySolver,m_ImageSize);

    unsigned int ndofpernode=(m_Element)->GetNumberOfDegreesOfFreedomPerNode();
    unsigned int numnodesperelt=(m_Element)->GetNumberOfNodes()+1;
    unsigned int ndof=mySolver.GetNumberOfDegreesOfFreedom();
    unsigned int nzelts;
    if (!m_ReadMeshFile) nzelts=numnodesperelt*ndofpernode*ndof; 
    else nzelts=((2*numnodesperelt*ndofpernode*ndof > 25*ndof) ? 2*numnodesperelt*ndofpernode*ndof : 25*ndof);
    
    LinearSystemWrapperItpack itpackWrapper; 
    itpackWrapper.SetMaximumNonZeroValuesInMatrix(nzelts);
    itpackWrapper.SetMaximumNumberIterations(2*mySolver.GetNumberOfDegreesOfFreedom()); 
    itpackWrapper.SetTolerance(1.e-1);
//    itpackWrapper.JacobianSemiIterative(); 
    itpackWrapper.JacobianConjugateGradient(); 
    mySolver.SetLinearSystemWrapper(&itpackWrapper); 

    if( m_UseMassMatrix ) mySolver.AssembleKandM(); 
    else 
      {
      mySolver.InitializeForSolution();
      mySolver.AssembleK();
      }
    IterativeSolve(mySolver);

    GetVectorField(mySolver);
    WarpImage(m_RefImg); 
    }
  else if (m_Maxiters[m_CurrentLevel] > 0)
    {
    MultiResSolve();
    }
    
}



template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::SetReferenceImage(ImageType* R)
{
  m_RefImg=R;
  m_Rregion = m_RefImg->GetLargestPossibleRegion();
  m_ImageSize = m_RefImg->GetLargestPossibleRegion().GetSize();
  
  VectorType disp;
  for (unsigned int i=0; i < ImageDimension; i++) 
    {
    disp[i]=0.0;
    m_ImageOrigin[i]=0;
    m_FieldSize[i] = m_ImageSize[i];
    }

  m_FieldRegion.SetSize( m_FieldSize );
  m_Field = FieldType::New();
  m_Field->SetLargestPossibleRegion( m_FieldRegion );
  m_Field->SetBufferedRegion( m_FieldRegion );
  m_Field->Allocate(); 
 
  m_Wregion.SetSize( m_FieldSize );
  m_WarpedImage = ImageType::New();
  m_WarpedImage->SetLargestPossibleRegion( m_Wregion );
  m_WarpedImage->SetBufferedRegion( m_Wregion );
  m_WarpedImage->Allocate();

  FieldIterator m_FieldIter( m_Field, m_FieldRegion );
  m_FieldIter.GoToBegin();
  for( ; !m_FieldIter.IsAtEnd(); ++m_FieldIter )
    {
    m_FieldIter.Set(disp);
    }

}

template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::SetTargetImage(TargetImageType* T)
{
  m_TarImg=T;
  m_Tregion = m_TarImg->GetLargestPossibleRegion();
}


template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::ChooseMetric(float which)
{
  // Choose the similarity metric

  typedef itk::MeanSquaresImageToImageMetric<ImageType,TargetImageType> MetricType0;
  typedef itk::NormalizedCorrelationImageToImageMetric<ImageType,TargetImageType> MetricType1;
  typedef itk::PatternIntensityImageToImageMetric<ImageType,TargetImageType> MetricType2;
  typedef itk::MutualInformationImageToImageMetric<ImageType,TargetImageType> MetricType3;

  m_WhichMetric=(unsigned int) which;
  unsigned int whichmetric=(unsigned int) which;
  switch (whichmetric)
    {
    case 0:
      m_Metric=MetricType0::New();
      m_Metric->SetScaleGradient(m_Temp); // this is the default(?)
      std::cout << " Mean Square " << std::endl;
      break;
    case 1:
      m_Metric=MetricType1::New();
      m_Metric->SetScaleGradient(m_Temp); 
      std::cout << " Normalized Correlation " << std::endl;
      break;
    case 2:
      m_Metric=MetricType2::New();
      m_Metric->SetScaleGradient(m_Temp); 
      std::cout << " Pattern Intensity " << std::endl;
      break;
    case 3:
      m_Metric=MetricType3::New();
      m_Metric->SetScaleGradient(m_Temp); 
      std::cout << " Mutual Information " << std::endl;
      break;
    default:
      m_Metric=MetricType0::New();
      m_Metric->SetScaleGradient(m_Temp); 
      std::cout << " Mean Square  " <<std::endl;
    }

}

template<class TReference,class TTarget>
bool FEMRegistrationFilter<TReference,TTarget>::ReadConfigFile(const char* fname)
  // Reads the parameters necessary to configure the example & returns
  // false if no configuration file is found
{
  std::ifstream f;
  char buffer[80] = {'\0'};
  Float fbuf = 0.0;
  unsigned int ibuf = 0;
  unsigned int jj;

  std::cout << "Reading config file..." << fname << std::endl;
  f.open(fname);
  if (f)
    {
  
    this->DoMultiRes(true);

    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    this->m_NumLevels = (unsigned int) ibuf;

    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    this->m_MaxLevel = ibuf;
     
    // get the initial scales for the pyramid
    FEMLightObject::SkipWhiteSpace(f);
    for (jj=0; jj<ImageDimension; jj++)
      {
      f >> ibuf;
      m_ImageScaling[jj] = ibuf;
      }
    
    this->m_MeshPixelsPerElementAtEachResolution.resize(m_NumLevels);
    FEMLightObject::SkipWhiteSpace(f);
    for (jj=0; jj<this->m_NumLevels; jj++)
      {
      f >> ibuf;
      this->m_MeshPixelsPerElementAtEachResolution(jj) = ibuf;
      }
    
    FEMLightObject::SkipWhiteSpace(f);
    this->m_E.resize(m_NumLevels);
    for (jj=0; jj<this->m_NumLevels; jj++)
      {
      f >> fbuf;
      this->SetElasticity(fbuf,jj);
      }

    FEMLightObject::SkipWhiteSpace(f);
    this->m_Rho.resize(m_NumLevels);
    for (jj=0; jj<this->m_NumLevels; jj++)
      {
      f >> fbuf;
      this->SetRho(fbuf,jj);
      }

    FEMLightObject::SkipWhiteSpace(f);
    this->m_Gamma.resize(m_NumLevels);
    for (jj=0; jj<this->m_NumLevels; jj++)
      {
      f >> fbuf;
      this->SetGamma(fbuf,jj);
      }

    FEMLightObject::SkipWhiteSpace(f);
    this->m_NumberOfIntegrationPoints.resize(m_NumLevels);
    for(jj=0; jj< m_NumLevels; jj++)
      { 
      f >> ibuf;
      this->SetNumberOfIntegrationPoints(ibuf,jj);}

    FEMLightObject::SkipWhiteSpace(f);
    this->m_MetricWidth.resize(m_NumLevels);
    for(jj=0; jj< m_NumLevels; jj++)
      { 
      f >> ibuf;
      this->SetWidthOfMetricRegion(ibuf,jj);}
    
    FEMLightObject::SkipWhiteSpace(f);
    this->m_Maxiters.resize(m_NumLevels);
    for (unsigned int jj=0; jj<this->m_NumLevels; jj++)
      {
      f >> ibuf;
      this->SetMaximumIterations(ibuf,jj);
      }

    FEMLightObject::SkipWhiteSpace(f);
    float fbuf2=1.0;
    f >> fbuf;  
    f >> fbuf2;
    m_Temp=fbuf2;
    this->ChooseMetric(fbuf);

    FEMLightObject::SkipWhiteSpace(f);
    f >> fbuf;
    this->m_Alpha=fbuf;
    
    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    if (ibuf == 0)
      {
      this->SetDescentDirectionMinimize();
      }
    else
      {
      this->SetDescentDirectionMaximize();
      }

    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    this->DoLineSearch(ibuf); 
    
    FEMLightObject::SkipWhiteSpace(f);
    f >> fbuf;
    this->SetTimeStep(fbuf);

    FEMLightObject::SkipWhiteSpace(f);
    f >> fbuf;
    this->SetEnergyReductionFactor(fbuf);

    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    unsigned int dim = ibuf;

    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    this->m_ImageSize[0] = ibuf;

    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    this->m_ImageSize[1] = ibuf;

    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    if (dim == 3) this->m_ImageSize[2] = ibuf;

    FEMLightObject::SkipWhiteSpace(f);
    f >> buffer;
    this->SetReferenceFile(buffer);

    FEMLightObject::SkipWhiteSpace(f);
    f >> buffer;
    this->SetTargetFile(buffer);

    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    FEMLightObject::SkipWhiteSpace(f);
    f >> buffer;

    if (ibuf == 1)
      {
      this->UseLandmarks(true);
      this->SetLandmarkFile(buffer);
      }
    else
      {
      this->UseLandmarks(false);
      }

    FEMLightObject::SkipWhiteSpace(f);
    f >> buffer;
    this->SetResultsFile(buffer);

    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    FEMLightObject::SkipWhiteSpace(f);
    f >> buffer;

    if (ibuf == 1)
      {
      this->SetWriteDisplacements(true);
      this->SetDisplacementsFile(buffer);
      }
    else 
      {
      this->SetWriteDisplacements(false);
      }

    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    FEMLightObject::SkipWhiteSpace(f);
    f >> buffer;

    if (ibuf == 1)
      {
      this->m_ReadMeshFile=true;
      this->m_MeshFileName=buffer;
      }
    else
      { 
      this->m_ReadMeshFile=false;
      }

    f.close();
    std::cout << "Example configured. E " << m_E << " rho " << m_Rho << std::endl;
    return true;
    }
  else
    { 
    std::cout << "No configuration file specified...quitting.\n"; 
    return false;
    }  
}


template<class TReference,class TTarget>
int FEMRegistrationFilter<TReference,TTarget>::WriteDisplacementField(unsigned int index)
  // Outputs the displacement field for the index provided (0=x,1=y,2=z)
{
  // Initialize the caster to the displacement field
  typename IndexSelectCasterType::Pointer fieldCaster = IndexSelectCasterType::New();
  fieldCaster->SetInput( m_Field );
  fieldCaster->SetIndex( index );
  
  // Define the output of the caster
  typename FloatImageType::Pointer fieldImage = FloatImageType::New();
  fieldCaster->Update();
  fieldImage = fieldCaster->GetOutput();

  // Set up the output filename
  std::string outfile=m_DisplacementsFileName+static_cast<char>('x'+index)+std::string("vec.img");
  std::cout << "Writing displacements to " << outfile;

  // Write the single-index field to a file
  //   itk::ImageRegionIteratorWithIndex<FloatImageType> it( fieldImage, fieldImage->GetLargestPossibleRegion() );
  //   for (; !it.IsAtEnd(); ++it) { std::cout << it.Get() << "\t"; }
  typedef typename FloatImageType::PixelType FType;
  typedef RawImageIO<FType,ImageDimension> IOType;
  typename IOType::Pointer io = IOType::New();
  typedef ImageFileWriter<FloatImageType> WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetInput(fieldImage);
  writer->SetImageIO(io);
  writer->SetFileName(outfile.c_str());
  writer->Write();

  std::cout << "...done" << std::endl;
  return 0;
}



template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::WarpImage(ImageType* ImageToWarp)
{
  // -------------------------------------------------------
  std::cout << "Warping image" << std::endl;
  bool InverseWarp=true;


  if (InverseWarp)
    {
    bool InImage=true;
    FieldIterator m_FieldIter( m_Field, m_FieldRegion );
    m_FieldIter.GoToBegin();
    typename ImageType::IndexType rindex = m_FieldIter.GetIndex();
    typename ImageType::IndexType tindex = m_FieldIter.GetIndex();

    m_FieldIter.GoToBegin();  
    for( ; !m_FieldIter.IsAtEnd(); ++m_FieldIter )
      {
      rindex=m_FieldIter.GetIndex();
      tindex=m_FieldIter.GetIndex();
      VectorType disp=m_FieldIter.Get();

      for (unsigned int ii=0; ii < ImageDimension; ii++)
        { 
        tindex[ii]+=(long int)(disp[ii]+0.5);
        if ( tindex[ii] >= 0 && (unsigned int) tindex[ii] < (unsigned int) m_FieldSize[ii])  InImage=true;
        else 
          {
          InImage=false;
          ii=ImageDimension;
          }
        }

      if (InImage)
        {
        ImageDataType t = (ImageDataType) ImageToWarp->GetPixel(tindex);
        m_WarpedImage->SetPixel(rindex, t );
        }
      else
        {
        m_WarpedImage->SetPixel(rindex,0);
        }
      }
    }
  else 
    {
    typedef itk::WarpImageFilter<ImageType,ImageType,FieldType> WarperType;
    typename WarperType::Pointer warper = WarperType::New();

    typedef typename WarperType::CoordRepType CoordRepType;
    typedef itk::NearestNeighborInterpolateImageFunction<ImageType,CoordRepType>
      InterpolatorType0;
    typedef itk::LinearInterpolateImageFunction<ImageType,CoordRepType>
      InterpolatorType1;
    typedef itk::BSplineInterpolateImageFunction<ImageType,CoordRepType>
      InterpolatorType2;
    typename InterpolatorType1::Pointer interpolator = InterpolatorType1::New();
  
    warper = WarperType::New();
    warper->SetInput( ImageToWarp );
    warper->SetDeformationField( m_Field );
    warper->SetInterpolator( interpolator );
    warper->SetOutputSpacing( ImageToWarp->GetSpacing() );
    warper->SetOutputOrigin( ImageToWarp->GetOrigin() );
    typename ImageType::PixelType padValue = 1;
    warper->SetEdgePaddingValue( padValue );
    warper->Update();

    m_WarpedImage=warper->GetOutput();  
    }

}



template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::CreateMesh(double PixelsPerElement, Solver& mySolver)
{

  vnl_vector<double> MeshOriginV; MeshOriginV.resize(ImageDimension); 
  vnl_vector<double> MeshSizeV;   MeshSizeV.resize(ImageDimension); 
  vnl_vector<double> ImageSizeV;   ImageSizeV.resize(ImageDimension); 
  vnl_vector<double> ElementsPerDim;  ElementsPerDim.resize(ImageDimension); 
  for (unsigned int i=0; i<ImageDimension; i++)
    { 
    MeshSizeV[i]=(double)m_ImageSize[i]/(double)m_ImageScaling[i]; // FIX ME  make more general
    MeshOriginV[i]=(double)m_ImageOrigin[i];// FIX ME make more general
    ImageSizeV[i]=(double) m_ImageSize[i]+1;//to make sure all elts are inside the interpolation mesh
    ElementsPerDim[i]=MeshSizeV[i]/PixelsPerElement;
    }
  std::cout << " ElementsPerDim " << ElementsPerDim << std::endl;

  if (m_ReadMeshFile)
    {
    std::ifstream meshstream; 
    meshstream.open(m_MeshFileName.c_str());
    if (!meshstream)
      {
      std::cout<<"File "<<m_MeshFileName<<" not found!\n";
      return;
      }

    mySolver.Read(meshstream); 
    mySolver.GenerateGFN();
    itk::fem::MaterialLinearElasticity::Pointer m=dynamic_cast<MaterialLinearElasticity*>(mySolver.mat.Find(0));
   
    if (m)
      { 
      m->E=this->GetElasticity(m_CurrentLevel);  // Young modulus -- used in the membrane ///
      }   
    // now scale the mesh to the current scale
    Element::VectorType coord;  
    Node::ArrayType* nodes = &(mySolver.node);
    Node::ArrayType::iterator node=nodes->begin();
    m_Element=( *((*node)->m_elements.begin()));
    for(  node=nodes->begin(); node!=nodes->end(); node++) 
      {
      coord=(*node)->GetCoordinates();    
      for (unsigned int ii=0; ii < ImageDimension; ii++)
        { 
        coord[ii] = coord[ii]/(float)m_ImageScaling[ii];
        }
      (*node)->SetCoordinates(coord);  
      }
    }
  else if (ImageDimension == 2 && dynamic_cast<Element2DC0LinearQuadrilateral*>(m_Element) != NULL)
    {
    m_Material->E=this->GetElasticity(m_CurrentLevel);
    Generate2DRectilinearMesh(m_Element,mySolver,MeshOriginV,MeshSizeV,ElementsPerDim); 
    mySolver.GenerateGFN();
    }
  else if ( ImageDimension == 3 && dynamic_cast<Element3DC0LinearHexahedron*>(m_Element) != NULL) 
    {
    m_Material->E=this->GetElasticity(m_CurrentLevel);
  
    Generate3DRectilinearMesh(m_Element,mySolver,MeshOriginV,MeshSizeV,ElementsPerDim); 
    mySolver.GenerateGFN();
    }
  else 
    {  
    throw FEMException(__FILE__, __LINE__, "CreateMesh - wrong image or element type ");
    }
  
  mySolver.InitializeInterpolationGrid(ImageSizeV,MeshOriginV,MeshSizeV);

}


template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::ApplyLoads(SolverType& mySolver,ImageSizeType ImgSz)
{
  //
  // Apply the boundary conditions.  We pin the image corners.
  // First compute which elements these will be.
  ///

  LoadBC::Pointer l1;

  /* Pin image corners - we find the corners by using Origin and Size of the images
  unsigned int CornerCounter=0;
  Node::ArrayType* nodes = &(mySolver.node);
  Element::VectorType coord;  
  for(  Node::ArrayType::iterator node=nodes->begin(); node!=nodes->end(); node++) 
  {
    coord=(*node)->GetCoordinates();
    CornerCounter=0;
    for (unsigned int ii=0; ii < ImageDimension; ii++)
  { 
    if (coord[ii] == m_ImageOrigin[ii] || coord[ii] == ImgSz[ii]-1 ) CornerCounter++;
  }
  if (CornerCounter == ImageDimension) // the node is located at a true corner
  {
    unsigned int ndofpernode=(*((*node)->m_elements.begin()))->GetNumberOfDegreesOfFreedomPerNode();
    unsigned int numnodesperelt=(*((*node)->m_elements.begin()))->GetNumberOfNodes();
    unsigned int whichnode=0;
    std::cout << " corner coord " << coord << std::endl;
    for (unsigned int nn=0; nn<numnodesperelt; nn++)
    {
      if ((*node) == (*((*node)->m_elements.begin()))->GetNode(nn)) whichnode=nn;
    }
    // now, cycle through all elements in the node's element set and fix each dimension
    for (unsigned int jj=0; jj<ndofpernode; jj++)
    {
        l1=LoadBC::New();
      // now we get the element from the node -- we assume we need fix the dof only once
      // even if more than one element shares it.
        l1->m_element=( *((*node)->m_elements.begin())); 
        unsigned int localdof=whichnode*ndofpernode+jj;
        l1->m_dof=localdof; // FIXME should be correct for each element
        l1->m_value=vnl_vector<double>(1,0.0);
        mySolver.load.push_back( FEMP<Load>(&*l1) );
    }
  }
  }*/
  
  //Pin  one corner of image  
  unsigned int CornerCounter,ii,EdgeCounter=0;
  Node::ArrayType* nodes = &(mySolver.node);
  Element::VectorType coord;
  Node::ArrayType::iterator node=nodes->begin();
  bool EdgeFound;
  bool EdgeBool[ImageDimension];
  while(   node!=nodes->end() && EdgeCounter < ImageDimension ) 
    {
    coord=(*node)->GetCoordinates();
    CornerCounter=0;
    for (ii=0; ii < ImageDimension; ii++)
      { 
      EdgeBool[ii]=false;
      if (coord[ii] == m_ImageOrigin[ii] || coord[ii] == ImgSz[ii]-1 ) CornerCounter++;
      }
    if (CornerCounter == ImageDimension) // the node is located at a true corner
      {
      unsigned int ndofpernode=(*((*node)->m_elements.begin()))->GetNumberOfDegreesOfFreedomPerNode();
      unsigned int numnodesperelt=(*((*node)->m_elements.begin()))->GetNumberOfNodes();
      unsigned int whichnode;
     
      unsigned int maxnode=numnodesperelt-1;
      typedef typename Node::SetOfElements NodeEltSetType;
      for( NodeEltSetType::iterator elt=(*node)->m_elements.begin(); 
           elt!=(*node)->m_elements.end(); elt++) 
        {
        for (whichnode=0; whichnode<=maxnode; whichnode++)
          {
          Node::ConstPointer tnode=(*elt)->GetNode(whichnode); 
          coord=(tnode)->GetCoordinates();
          CornerCounter=0;
          for (ii=0; ii < ImageDimension; ii++)
            { 
            if ((coord[ii] == m_ImageOrigin[ii] && EdgeBool[ii]==false )|| 
                (coord[ii] == ImgSz[ii]-1       && EdgeBool[ii]==false )) 
              {
              CornerCounter++;
              }
            }
          if (CornerCounter == ImageDimension - 1) EdgeFound=true; else EdgeFound=false;
          if (EdgeFound)
            {
            for (unsigned int jj=0; jj<ndofpernode; jj++)
              {
              std::cout << " which node " << whichnode << std::endl; 
              std::cout << " edge coord " << coord << std::endl;
              for (ii=0; ii < ImageDimension; ii++)
                { 
                if (coord[ii] == m_ImageOrigin[ii] || coord[ii] == ImgSz[ii]-1 ) 
                  {
                  EdgeBool[ii]=false;
                  }
                }
              l1=LoadBC::New();
              // now we get the element from the node -- we assume we need fix the dof only once
              // even if more than one element shares it.
              l1->m_element= (*elt);  // Fixed bug TS 1/17/03 ( *((*node)->m_elements.begin())); 
              unsigned int localdof=whichnode*ndofpernode+jj;
              l1->m_dof=localdof; // FIXME should be correct for each element
              l1->m_value=vnl_vector<double>(1,0.0);
              mySolver.load.push_back( FEMP<Load>(&*l1) );
              }
            EdgeCounter++;
            }
          }
        }//end elt loop
      }
    node++;
    }//

  if (m_UseLandmarks)
    {
    // Landmark loads
    std::ifstream f;
    std::cout << m_LandmarkFileName << std::endl;
    std::cout << "Loading landmarks...";
    f.open(m_LandmarkFileName.c_str());
    if (f)
      {

      try
        { 
        mySolver.Read(f); 
        }
      catch (itk::ExceptionObject &err)
        { 
        std::cerr << "Exception: " << err;
        f.close();
        }
    
      f.close();
      std::cout << "done" << std::endl;
      }
    else
      {
      std::cout << "no landmark file specified." << std::endl;
      }
    }
  
  m_Load=FEMRegistrationFilter<TReference,TTarget>::ImageMetricLoadType::New();

  m_Load->SetReferenceImage(m_RefImg);
  m_Load->SetTargetImage(m_TarImg);
  
  m_Load->SetMetric(m_Metric);
  m_Load->InitializeMetric();
  m_Load->SetTemp(m_Temp);
  m_Load->SetGamma(m_Gamma[m_CurrentLevel]);
  typename ImageType::SizeType r;
  for (unsigned int i=0; i < ImageDimension; i++) r[i]=m_MetricWidth[m_CurrentLevel];
  m_Load->SetMetricRadius(r);
  m_Load->SetNumberOfIntegrationPoints(m_NumberOfIntegrationPoints[m_CurrentLevel]);
  m_Load->GN=static_cast<int>( mySolver.load.size() ) + 1; //NOTE SETTING GN FOR FIND LATER
  m_Load->SetSign((Float)m_DescentDirection);
  mySolver.load.push_back( FEMP<Load>(&*m_Load) );    
  m_Load=dynamic_cast<FEMRegistrationFilter<TReference,TTarget>::ImageMetricLoadType*> (&*mySolver.load.Find( static_cast<int>( mySolver.load.size() )));  
  
  return;
}



template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::IterativeSolve(SolverType& mySolver)
{
  m_MinE=10.e99;
  Float LastE , deltE;

  unsigned int iters=0;
  bool Done=false;
  unsigned int DLS=m_DoLineSearchOnImageEnergy;
  while ( !Done )
    {
    /*
   * Assemble the master force vector (from the applied loads)
   */
    // if (iters == 1) // for testing 
    if( m_UseMassMatrix ) mySolver.AssembleFforTimeStep(); else mySolver.AssembleF();

    /*
   * Solve the system of equations for displacements (u=K^-1*F)
   */
    mySolver.Solve();  
  
    //mySolver.PrintDisplacements(0);
  
    Float mint=1.0; //,ImageSimilarity=0.0;
    LastE=EvaluateResidual(mySolver,mint);
    deltE=(m_MinE-LastE);

    if ( DLS == 1 || (DLS==2 && deltE < 0.0) ) 
      {
      std::cout << " line search ";
      float tol = 1.0;//((0.01  < LastE) ? 0.01 : LastE/10.);
      LastE=this->GoldenSection(mySolver,tol,m_LineSearchMaximumIterations);
      deltE=(m_MinE-LastE);
      std::cout << " line search done " << std::endl;
      } 

    iters++;

    if ((LastE <= m_EnergyReductionFactor  || fabs(deltE) < 1.e-1 ) || iters >= m_Maxiters[m_CurrentLevel] 
        /*|| ImageSimilarity > LastISim*/) 
      {
      Done=true;
      }  
    mySolver.AddToDisplacements(mint);
    m_MinE=LastE;

    // uncomment to write out every deformation SLOW due to interpolating vector field everywhere.
    if ( /*(iters % 1) == 0 ||*/ Done ) {
    //GetVectorField(mySolver);
    //WarpImage(m_RefImg);
    //WriteWarpedImage(m_ResultsFileName.c_str());
    }//

    std::cout << " min E " << m_MinE <<  " delt E " << deltE <<  " iter " << iters << std::endl;
   
    m_TotalIterations++;
    } 
  
}



template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::GetVectorField(SolverType& mySolver)
{
  std::cout << " computing vector field " << std::endl;
  Element::ArrayType* el = &(mySolver.el);
  vnl_vector<double> Pos;  // solution at the point
  vnl_vector<double> Sol;  // solution at the local point
  vnl_vector<double> Gpt;  // global position given by local point
  FieldIterator m_FieldIter( m_Field, m_FieldRegion );
  m_FieldIter.GoToBegin();
  typename ImageType::IndexType rindex = m_FieldIter.GetIndex();

  Sol.resize(ImageDimension); 
  Gpt.resize(ImageDimension);
  if (ImageDimension == 2)
    {
    Element::ConstPointer eltp;


    for( ; !m_FieldIter.IsAtEnd(); ++m_FieldIter )
      {
      VectorType disp;  
// get element pointer from the solver elt pointer image
    

      rindex = m_FieldIter.GetIndex();
      for(unsigned int d=0; d<ImageDimension; d++)
        {
        Gpt[d]=(double)rindex[d]/(double)m_ImageScaling[d];
        }
      eltp=mySolver.GetElementAtPoint(Gpt);
      if (eltp)
        {
        eltp->GetLocalFromGlobalCoordinates(Gpt, Pos);
   
        unsigned int Nnodes= eltp->GetNumberOfNodes();
        typename Element::VectorType shapef(Nnodes);
        shapef = eltp->ShapeFunctions(Pos);
        Float solval;
        for(unsigned int f=0; f<ImageDimension; f++)
          {
          solval=0.0;
          for(unsigned int n=0; n<Nnodes; n++)
            {
            solval+=shapef[n] * mySolver.GetLS()->GetSolutionValue( eltp->GetNode(n)->GetDegreeOfFreedom(f) , mySolver.TotalSolutionIndex);
            }
          Sol[f]=solval;
          disp[f] =(Float) 1.0*Sol[f];//*((Float)m_ImageScaling[f]); 
          }
        m_Field->SetPixel(rindex, disp );
        }
      }
    }
  else if (ImageDimension==3)
    {

    Pos.resize(ImageDimension);
    for(  Element::ArrayType::iterator elt=el->begin(); elt!=el->end(); elt++) 
      {
  
      Float rstep=2.0/((double)m_MeshPixelsPerElementAtEachResolution[m_CurrentLevel]*(double)m_ImageScaling[0]);
      Float sstep=2.0/((double)m_MeshPixelsPerElementAtEachResolution[m_CurrentLevel]*(double)m_ImageScaling[1]);
      Float tstep=2.0/((double)m_MeshPixelsPerElementAtEachResolution[m_CurrentLevel]*(double)m_ImageScaling[2]);
      //std::cout << " r s t steps " << rstep << sstep << tstep << std::endl;
      for (double r=-1.0; r <= 1.0; r=r+rstep )
        {
        for (double s=-1.0; s <= 1.0; s=s+sstep )
          {
          for (double t=-1.0; t <= 1.0; t=t+tstep )
            {
            Pos[0]=r; 
            Pos[1]=s;
            Pos[2]=t; 
            //     for(  Element::ArrayType::iterator elt=el->begin(); elt!=el->end(); elt++) 
            //     {
  
            VectorType disp; 
            unsigned int Nnodes= (*elt)->GetNumberOfNodes();
            typename Element::VectorType shapef(Nnodes);

#define FASTHEX
#ifdef FASTHEX
//FIXME temporarily using hexahedron shape f for speed
            shapef[0] = (1 - r) * (1 - s) * (1 - t) * 0.125;
            shapef[1] = (1 + r) * (1 - s) * (1 - t) * 0.125;
            shapef[2] = (1 + r) * (1 + s) * (1 - t) * 0.125;
            shapef[3] = (1 - r) * (1 + s) * (1 - t) * 0.125;
            shapef[4] = (1 - r) * (1 - s) * (1 + t) * 0.125;
            shapef[5] = (1 + r) * (1 - s) * (1 + t) * 0.125;
            shapef[6] = (1 + r) * (1 + s) * (1 + t) * 0.125;
            shapef[7] = (1 - r) * (1 + s) * (1 + t) * 0.125;
#else
            shapef = (*elt)->ShapeFunctions(Pos);
#endif
            Float solval,posval;
            bool inimage=true;
            for(unsigned int f=0; f<ImageDimension; f++)
              {
              solval=0.0;
              posval=0.0;
              for(unsigned int n=0; n<Nnodes; n++)
                {
                posval+=shapef[n]*(((*elt)->GetNodeCoordinates(n))[f]);
                solval+=shapef[n] * mySolver.GetLS()->GetSolutionValue( (*elt)->GetNode(n)->GetDegreeOfFreedom(f) , mySolver.TotalSolutionIndex);
                }
              Sol[f]=solval;
              Gpt[f]=posval;

              Float x=Gpt[f];
              long int temp;
              if (x !=0) temp=(long int) 
                           ((x)*(Float)m_ImageScaling[f]+0.5); else temp=0;// round after scaling
              rindex[f]=temp;
              disp[f] =(Float) 1.0*Sol[f];//*((Float)m_ImageScaling[f]); 
              if ( temp < 0 || temp > (long int) m_FieldSize[f]-1)  inimage=false;
              }
//        if (fabs(r) == 1.0 && fabs(s)==1.0 && fabs(t)==1.0 ) std::cout << " rindex " << rindex << " sol " << Sol << std::endl;
            if (inimage) m_Field->SetPixel(rindex, disp );

//  }//end of other elt loop
            }}}//end of for loops
      } // end of elt array loop
    // Insure that the values are exact at the nodes. They won't necessarily be unless we use this code.
    /*Node::ArrayType* nodes = &(mySolver.node);
  Element::VectorType coord;  
  VectorType SolutionAtNode;
  for(  Node::ArrayType::iterator node=nodes->begin(); node!=nodes->end(); node++) 
  {
    coord=(*node)->GetCoordinates();
    for (unsigned int ii=0; ii < ImageDimension; ii++)
    { 
      if (coord[ii] != 0) rindex[ii]=(long int) (coord[ii]*(Float)m_ImageScaling[ii])-1; 
       else rindex[ii]=0;
      Float OldSol=mySolver.GetLinearSystemWrapper()->
        GetSolutionValue((*node)->GetDegreeOfFreedom(ii),mySolver.TotalSolutionIndex);
      SolutionAtNode[ii]=OldSol*((Float)m_ImageScaling[ii]);    
    }
    m_Field->SetPixel(rindex, SolutionAtNode );
  }*/
    }
}


template<class TReference,class TTarget>
typename FEMRegistrationFilter<TReference , TTarget>::FloatImageType* 
FEMRegistrationFilter<TReference,TTarget>::GetMetricImage(FieldType* Field)
{
  typename FloatImageType::RegionType metricRegion;
  metricRegion.SetSize( m_RefImg->GetLargestPossibleRegion().GetSize() );
  if (m_FloatImage == NULL)
    {
    m_FloatImage = FloatImageType::New();
    m_FloatImage->SetLargestPossibleRegion( metricRegion );
    m_FloatImage->SetBufferedRegion( metricRegion );
    m_FloatImage->Allocate(); 
    }
  
  m_Load=FEMRegistrationFilter<TReference,TTarget>::ImageMetricLoadType::New();
  m_Load->SetReferenceImage(m_RefImg);
  m_Load->SetTargetImage(m_TarImg);
  m_Load->SetMetric(m_Metric);
  m_Load->InitializeMetric();
  m_Load->SetTemp(m_Temp);
  m_Load->SetGamma(m_Gamma[m_CurrentLevel]);
  typename ImageType::SizeType r;
  for (unsigned int i=0; i < ImageDimension; i++) r[i]=m_MetricWidth[m_CurrentLevel];
  m_Load->SetMetricRadius(r);
  m_Load->SetNumberOfIntegrationPoints(m_NumberOfIntegrationPoints[m_CurrentLevel]);
  m_Load->SetSign((Float)m_DescentDirection);
 
   
  FieldIterator m_FieldIter( m_Field, m_FieldRegion );
  m_FieldIter.GoToBegin();
  typename ImageType::IndexType rindex = m_FieldIter.GetIndex();

  
  std:: cout << " get metric image " << std::endl;
  for( ; !m_FieldIter.IsAtEnd(); ++m_FieldIter )
    {
    rindex=m_FieldIter.GetIndex();
    typename FieldType::PixelType vector=m_Field->GetPixel(rindex);

    typename ImageMetricLoadType::VectorType invec;
    invec.resize(ImageDimension*2);
    for (unsigned int i=0; i<ImageDimension; i++)
      {
      invec[i]=(double) rindex[i];
      invec[i+ImageDimension]=(double) vector[i];
      }
    typename FloatImageType::PixelType mPix=m_Load->GetMetric(invec);
    
    m_FloatImage->SetPixel(rindex, mPix );

    }
  
  return m_FloatImage;

}

template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::ComputeJacobian()
{
  typename FloatImageType::RegionType m_JacobianRegion;
  m_JacobianRegion.SetSize( m_Field->GetLargestPossibleRegion().GetSize() );
  if (!m_FloatImage)
    {
    m_FloatImage = FloatImageType::New();
    m_FloatImage->SetLargestPossibleRegion( m_JacobianRegion );
    m_FloatImage->SetBufferedRegion( m_JacobianRegion );
    m_FloatImage->Allocate(); 
    }
 
  unsigned int row=0; 
  unsigned int col=0;

  Element::MatrixType jMatrix,idMatrix;
  jMatrix.resize(ImageDimension,ImageDimension);
  
  FieldIterator m_FieldIter( m_Field, m_FieldRegion );
  m_FieldIter.GoToBegin();
  typename ImageType::IndexType rindex = m_FieldIter.GetIndex();

  typename ImageType::IndexType difIndex[ImageDimension][2];
  
  std:: cout << " get jacobian " << std::endl;
  for( ; !m_FieldIter.IsAtEnd(); ++m_FieldIter )
    {
    rindex=m_FieldIter.GetIndex();
    typename ImageType::IndexType temp=rindex;

    Float dV;
    typename FieldType::PixelType dPix;
    for(row=0; row< ImageDimension;row++)
      {
      difIndex[row][0]=rindex;
      difIndex[row][1]=rindex;
      if (rindex[row] < m_FieldSize[row]-1) difIndex[row][0][row]=rindex[row]+1;
      if (rindex[row] > 0 )                 difIndex[row][1][row]=rindex[row]-1;
//      std::cout << " indices " << difIndex[row][0] << " " <<difIndex[row][1] << std::endl;
      dPix=m_Field->GetPixel(difIndex[row][1])-
        m_Field->GetPixel(difIndex[row][0]);
      for(col=0; col< ImageDimension;col++)
        {
        Float val;
        if (row == col) val=0.5*dPix[col]+1.0;
        else val = 0.5*dPix[col];
//        std::cout << " row " << row << " col " << col << " val " << val << std::endl;
        jMatrix.put(row,col,val);
        }
      }
    
    //the determinant of the jacobian matrix
    // std::cout << " get det " << std::endl;
    double det = vnl_determinant(jMatrix);
    m_FloatImage->SetPixel(rindex, det );

    }
  std:: cout << " mat val " << jMatrix << std::endl;


}


template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::WriteWarpedImage(const char* fname)
{

  // for image output
  std::ofstream fbin; 
  std::string exte=".img";
  std::string fnum;
  m_FileCount++;

  OStringStream buf;
  buf<<(m_FileCount+10);
  fnum=std::string(buf.str().c_str());

  std::string fullfname=(fname+fnum+exte);

  ImageIterator wimIter( m_WarpedImage,m_Wregion );

  fbin.open(fullfname.c_str(),std::ios::out | std::ios::binary);
  ImageDataType t=0;
  // for arbitrary dimensionality
  wimIter.GoToBegin();  
  for( ; !wimIter.IsAtEnd(); ++wimIter )
    {
    t=(ImageDataType) wimIter.Get();
    fbin.write(reinterpret_cast<const char*>(&t),sizeof(ImageDataType));
    }
  
}



template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::SampleVectorFieldAtNodes(SolverType& mySolver)
{

  std::cout << " upsampling vector field " << std::endl;
  // Here, we need to iterate through the nodes, get the nodal coordinates,
  // sample the VF at the node and place the values in the SolutionVector.
  unsigned int ii;
  Node::ArrayType* nodes = &(mySolver.node);
  Element::VectorType coord;  
  VectorType SolutionAtNode;

  FieldIterator m_FieldIter( m_Field, m_FieldRegion );
  m_FieldIter.GoToBegin();
  typename ImageType::IndexType rindex = m_FieldIter.GetIndex();

  for(  Node::ArrayType::iterator node=nodes->begin(); node!=nodes->end(); node++) 
    {
    coord=(*node)->GetCoordinates();
    bool inimage=true;
    for (ii=0; ii < ImageDimension; ii++)
      { 
      if (coord[ii] != 0) rindex[ii]=(long int) ((Float)coord[ii]*(Float)m_ImageScaling[ii]+0.5)-1;
      else rindex[ii]=0;
      if ( (long int)rindex[ii] < (long int)0 || (long int)rindex[ii]  > (long int)m_FieldSize[ii]-1 )
        { 
        inimage=false;      
        SolutionAtNode[ii]=0;
        }
      }
    if (inimage)   SolutionAtNode=m_Field->GetPixel(rindex);
    // Now put it into the solution!
    for (ii=0; ii < ImageDimension; ii++)
      { 
      Float Sol=SolutionAtNode[ii];///((Float)m_ImageScaling[ii]); // Scale back to current scale
//    std::cout << " the sol " << Sol << " at coord " << coord << " rind " << rindex << std::endl;
        mySolver.GetLinearSystemWrapper()->
          SetSolutionValue((*node)->GetDegreeOfFreedom(ii),Sol,mySolver.TotalSolutionIndex);    
        mySolver.GetLinearSystemWrapper()->
          SetSolutionValue((*node)->GetDegreeOfFreedom(ii),Sol,mySolver.SolutionTMinus1Index);    
      }
    }

}



template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::PrintVectorField()
{
  FieldIterator m_FieldIter( m_Field, m_FieldRegion );
  m_FieldIter.GoToBegin();
  unsigned int ct=0;
  while( !m_FieldIter.IsAtEnd() /*&& ct < 10*/ )
    {
    //VectorType disp;  for (unsigned int i=0; i<ImageDimension; i++) disp[i]=-9.9;
    std::cout << " field pix " << m_FieldIter.Get() << std::endl;
//    m_FieldIter.Set(disp);
    ++m_FieldIter; 
    ct++;
    }
}

template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::MultiResSolve()
{

//  Float LastScaleEnergy=0.0, ThisScaleEnergy=0.0;
  vnl_vector<Float> LastResolutionSolution;
//   Setup a multi-resolution pyramid
  typedef itk::RecursiveMultiResolutionPyramidImageFilter<FloatImageType,FloatImageType>
    PyramidType;
  typedef typename PyramidType::ScheduleType ScheduleType;
  typename PyramidType::Pointer pyramidR = PyramidType::New();
  typename PyramidType::Pointer pyramidT = PyramidType::New();

  typedef itk::CastImageFilter<ImageType,FloatImageType> CasterType1;
  typedef itk::CastImageFilter<FloatImageType,ImageType> CasterType2;
  typename CasterType1::Pointer Rcaster1 = CasterType1::New();
  typename CasterType1::Pointer Tcaster1 = CasterType1::New();
  typename CasterType2::Pointer Rcaster2;
  typename CasterType2::Pointer Tcaster2;

  Rcaster1->SetInput(m_RefImg); Rcaster1->Update();
  pyramidR->SetInput( Rcaster1->GetOutput());
  Tcaster1->SetInput(m_TarImg); Tcaster1->Update();
  pyramidT->SetInput( Tcaster1->GetOutput());

// set schedule by specifying the number of levels;
  pyramidR->SetNumberOfLevels( m_NumLevels );
  pyramidT->SetNumberOfLevels( m_NumLevels );

  ScheduleType SizeReductionR=pyramidR->GetSchedule();
  ScheduleType SizeReductionT=pyramidT->GetSchedule();
  for (unsigned int ii=0; ii<m_NumLevels; ii++) 
    for (unsigned int jj=0; jj<ImageDimension; jj++) 
      { 
      unsigned int scale=m_ImageScaling[jj]/(unsigned int)pow(2.0,(double)ii);
      if (scale < 1) scale=1;
      SizeReductionR[ii][jj]=scale;
      SizeReductionT[ii][jj]=scale;
      }
  pyramidR->SetSchedule(SizeReductionR); 
  pyramidT->SetSchedule(SizeReductionT);
  std::cout << " size change at this level " << SizeReductionR << std::endl;
  pyramidR->Update();
  pyramidT->Update();

  for (m_CurrentLevel=0; m_CurrentLevel<m_MaxLevel; m_CurrentLevel++)
    {
    pyramidR->GetOutput( m_CurrentLevel )->Update();
    pyramidT->GetOutput( m_CurrentLevel )->Update();
    
    typename ImageType::SizeType Isz=pyramidT->GetOutput( m_CurrentLevel )->GetLargestPossibleRegion().GetSize();

    for (unsigned int d=0; d < ImageDimension; d++)
      {
      m_ImageScaling[d]=SizeReductionR[m_CurrentLevel][d];
      }

    double MeshResolution=(double)this->m_MeshPixelsPerElementAtEachResolution(m_CurrentLevel);

    Rcaster2 = CasterType2::New();// Weird - don't know why but this worked
    Tcaster2 = CasterType2::New();// and declaring the casters outside the loop did not.

    SolverType SSS;

    SSS.SetDeltatT(m_dT);  
    SSS.SetRho(m_Rho[m_CurrentLevel]);     
    SSS.SetAlpha(m_Alpha);    

    CreateMesh(MeshResolution,SSS); 
    ApplyLoads(SSS,Isz);

    unsigned int ndofpernode=(m_Element)->GetNumberOfDegreesOfFreedomPerNode();
    unsigned int numnodesperelt=(m_Element)->GetNumberOfNodes()+1;
    unsigned int ndof=SSS.GetNumberOfDegreesOfFreedom();
    unsigned int nzelts;
    if (!m_ReadMeshFile) nzelts=numnodesperelt*ndofpernode*ndof; 
    else nzelts=((2*numnodesperelt*ndofpernode*ndof > 25*ndof) ? 2*numnodesperelt*ndofpernode*ndof : 25*ndof);
    
    LinearSystemWrapperItpack itpackWrapper; 
    itpackWrapper.SetMaximumNonZeroValuesInMatrix(nzelts);
    unsigned int maxits=2*SSS.GetNumberOfDegreesOfFreedom();
    itpackWrapper.SetMaximumNumberIterations(maxits); 
    itpackWrapper.SetTolerance(1.e-1);
    itpackWrapper.JacobianConjugateGradient(); 
    SSS.SetLinearSystemWrapper(&itpackWrapper); 

    m_Load=FEMRegistrationFilter<TReference,TTarget>::ImageMetricLoadType::New();

    Rcaster2->SetInput(pyramidR->GetOutput(m_CurrentLevel)); Rcaster2->Update();
    m_Load->SetReferenceImage(Rcaster2->GetOutput()); 
 
    Tcaster2->SetInput(pyramidT->GetOutput(m_CurrentLevel)); Tcaster2->Update();
    m_Load->SetTargetImage(Tcaster2->GetOutput());  

    m_Load->SetMetric(m_Metric);
    m_Load->InitializeMetric();
    m_Load->SetTemp(m_Temp);
    m_Load->SetGamma(m_Gamma[m_CurrentLevel]);
    ImageSizeType r;
    for (unsigned int dd=0; dd<ImageDimension; dd++) r[dd]=m_MetricWidth[m_CurrentLevel];
    m_Load->SetMetricRadius(r);
    m_Load->SetNumberOfIntegrationPoints(m_NumberOfIntegrationPoints[m_CurrentLevel]);
    m_Load->GN=SSS.load.size()+1; //NOTE SETTING GN FOR FIND LATER
    m_Load->SetSign((Float)m_DescentDirection);
    SSS.load.push_back( FEMP<Load>(&*m_Load) );    
    m_Load=dynamic_cast<FEMRegistrationFilter<TReference,TTarget>::ImageMetricLoadType*> (&*SSS.load.Find(SSS.load.size()));  
   
    if( m_UseMassMatrix ) SSS.AssembleKandM(); 
    else 
      {
      SSS.InitializeForSolution();
      SSS.AssembleK();
      }
 
    if ( m_CurrentLevel > 0) 
      {
      SampleVectorFieldAtNodes(SSS);
//        LastScaleEnergy=ThisScaleEnergy; ThisScaleEnergy=LastScaleEnergy; // need to resolve this
//        ThisScaleEnergy=SSS.EvaluateResidual(0.0);
      }
//    SSS.PrintDisplacements();

    IterativeSolve(SSS);

//      ThisScaleEnergy=SSS.EvaluateResidual(0.0);
//      std::cout << " E " << ThisScaleEnergy << std::endl;

    GetVectorField(SSS); 
    if ( m_CurrentLevel == m_MaxLevel-1) 
      { 
      //  m_RefImg=Tcaster2->GetOutput(); // for testing
      WarpImage(m_RefImg);     
      } 
      
    }// end image resolution loop

  //LinearSystemSolverType* temp=
  //    dynamic_cast<LinearSystemSolverType*>(mySolver.GetLinearSystemWrapper());
  //delete temp;

  return;
  
}

template<class TReference,class TTarget>
Element::Float FEMRegistrationFilter<TReference,TTarget>::EvaluateResidual(SolverType& mySolver,Float t)
{
  
  //Float defe=mySolver.GetDeformationEnergy(t);
  Float SimE=m_Load->EvaluateMetricGivenSolution(&(mySolver.el),t);

  Float maxsim=1.0;
  for (unsigned int i=0; i< ImageDimension; i++) maxsim*=(Float)m_ImageSize[i];
  if ( m_WhichMetric != 0) SimE=maxsim-SimE;
  //std::cout << " SimE " << SimE << " Def E " << defe << std::endl;
  return fabs((double)SimE);

}

template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::FindBracketingTriplet(SolverType& mySolver,Float* a, Float* b, Float* c)
{
  // in 1-D domain, we want to find a < b < c , s.t.  f(b) < f(a) && f(b) < f(c)
  //  see Numerical Recipes
 
  Float Gold=1.618034;
  Float Glimit=100.0;
  Float Tiny=1.e-20;
  
  Float ax = 0.0;
  Float bx = 1.0;
 
  Float fa = fabs( EvaluateResidual(mySolver, ax) );
  Float fb = fabs( EvaluateResidual(mySolver, bx) );
  
  Float ulim,u,r,q,fu,dum;

  if ( fb > fa ) 
    {
    dum=ax; ax=bx; bx=dum;
    dum=fb; fb=fa; fa=dum;
    }

  Float cx = bx+Gold*(bx-ax);  // first guess for c - the 3rd pt needed to bracket the min
  Float fc = fabs( EvaluateResidual(mySolver, cx) );

  
  while (fb > fc  /*&& fabs(ax) < 3. && fabs(bx) < 3. && fabs(cx) < 3.*/)
    {
    r=(bx-ax)*(fb-fc);
    q=(bx-cx)*(fb-fa);
    Float denom=(2.0*mySolver.GSSign(mySolver.GSMax(fabs(q-r),Tiny),q-r));
    u=(bx)-((bx-cx)*q-(bx-ax)*r)/denom;
    ulim=bx + Glimit*(cx-bx);
    if ((bx-u)*(u-cx) > 0.0)
      {
      fu=fabs(EvaluateResidual(mySolver, u));
      if (fu < fc)
        {
        ax=bx;
        bx=u;
        *a=ax; *b=bx; *c=cx;
        return;
        }
      else if (fu > fb)
        {
        cx=u;
        *a=ax; *b=bx; *c=cx;
        return;
        }
      
      u=cx+Gold*(cx-bx);
      fu=fabs(EvaluateResidual(mySolver, u));
      
      }
    else if ( (cx-u)*(u-ulim) > 0.0)
      {
      fu=fabs(EvaluateResidual(mySolver, u));
      if (fu < fc)
        {
        bx=cx; cx=u; u=cx+Gold*(cx-bx);
        fb=fc; fc=fu; fu=fabs(EvaluateResidual(mySolver, u));
        }

      }
    else if ( (u-ulim)*(ulim-cx) >= 0.0)
      {
      u=ulim;
      fu=fabs(EvaluateResidual(mySolver, u));
      }
    else
      {
      u=cx+Gold*(cx-bx);
      fu=fabs(EvaluateResidual(mySolver, u));
      }
    
    ax=bx; bx=cx; cx=u;
    fa=fb; fb=fc; fc=fu;

    }

  if ( fabs(ax) > 1.e3  || fabs(bx) > 1.e3 || fabs(cx) > 1.e3)
    {ax=-2.0;  bx=1.0;  cx=2.0; } // to avoid crazy numbers caused by bad bracket (u goes nuts)
  
  *a=ax; *b=bx; *c=cx;
}


template<class TReference,class TTarget>
Element::Float FEMRegistrationFilter<TReference,TTarget>::GoldenSection(SolverType& mySolver,Float tol,unsigned int MaxIters)
{
  // We should now have a, b and c, as well as f(a), f(b), f(c), 
  // where b gives the minimum energy position;

  Float ax, bx, cx;


  FindBracketingTriplet(mySolver,&ax, &bx, &cx);
  Float xmin,fmin;
  //if (fb!=0.0)
  Float f1,f2,x0,x1,x2,x3;

  Float R=0.6180339;
  Float C=(1.0-R);

  x0=ax;
  x3=cx;
  if (fabs(cx-bx) > fabs(bx-ax))
    {
    x1=bx;
    x2=bx+C*(cx-bx);
    } 
  else
    {
    x2=bx;
    x1=bx-C*(bx-ax);
    }
  f1=fabs(EvaluateResidual(mySolver, x1));
  f2=fabs(EvaluateResidual(mySolver, x2));
  unsigned int iters=0;
  while (fabs(x3-x0) > tol*(fabs(x1)+fabs(x2)) && iters < MaxIters)
    {
    iters++;
    if (f2 < f1)
      {
      x0=x1; x1=x2; x2=R*x1+C*x3;
      f1=f2; f2=fabs(EvaluateResidual(mySolver, x2));
      } 
    else
      {
      x3=x2; x2=x1; x1=R*x2+C*x0;
      f2=f1; f1=fabs(EvaluateResidual(mySolver, x1));
      }
    }
  if (f1<f2)
    {
    xmin=x1; 
    fmin=f1;
    } 
  else
    {
    xmin=x2;
    fmin=f2;
    }

  mySolver.SetEnergyToMin(xmin);
  std:: cout << " emin " << fmin <<  " at xmin " << xmin << std::endl;
  return fabs((double)fmin); 
}

template<class TReference,class TTarget>
void
FEMRegistrationFilter<TReference,TTarget>::
PrintSelf(std::ostream& os, Indent indent) const 
{ 
  Superclass::PrintSelf( os, indent );

  if (m_Load)
    {
    os << indent << "Load = " << m_Load;
    }
  else
    {
    os << indent << "Load = " << "(None)" << std::endl;
    }
}


}} // end namespace itk::fem

 
#endif
