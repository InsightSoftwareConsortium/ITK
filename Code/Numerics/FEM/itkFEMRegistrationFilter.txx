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
#include <fstream>

#include "itkImageFileWriter.h" 
#include "itkRawImageIO.h" 
#include "itkFEMRegistrationFilter.h"


namespace itk {
namespace fem {



template<class TReference,class TTarget>
FEMRegistrationFilter<TReference,TTarget>::~FEMRegistrationFilter( )
{
 
}

template<class TReference,class TTarget>
FEMRegistrationFilter<TReference,TTarget>::FEMRegistrationFilter( )
{
  m_FileCount=0; 
 
  m_MinE=vnl_huge_val(m_MinE);  

  m_DescentDirection=positive;
  m_E.resize(1);
  m_Rho.resize(1);
  m_E[0]=1.; 
  m_Rho[0]=1.;
  m_CurrentLevel=0;
  m_Maxiters.resize(1);
  m_Maxiters[m_CurrentLevel]=1;  
  m_dT=1;

  m_NumberOfIntegrationPoints.resize(1);
  m_NumberOfIntegrationPoints[m_CurrentLevel]=4;
  m_MetricWidth.resize(1);
  m_MetricWidth[m_CurrentLevel]=3;
  m_DoLineSearchOnImageEnergy=false;
  m_LineSearchFrequency=1;

  m_NumLevels=1;
  m_MaxLevel=1;
  m_MeshStep=2;
  m_MeshLevels=1;
  m_DoMultiRes=false;
  m_UseLandmarks=false;

  m_ReferenceFileName = NULL;
  m_TargetFileName = NULL;
  m_LandmarkFileName = NULL;
  m_Field=NULL;
  m_TotalIterations=0;

  m_ReadMeshFile=false;

  for (unsigned int i=0; i < ImageDimension; i++)
  {
     m_ImageScaling[i]=1;
     m_ImageSize[i]=0;
     m_ImageOrigin[i]=0;
  }
  
}


template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::RunRegistration()
{
    std::cout << "beginning registration\n";  

  // Solve the system in time 

  if (!m_DoMultiRes && m_Maxiters[m_CurrentLevel] > 0) 
  {
    m_Solver.SetDeltatT(m_dT);  
    m_Solver.SetRho(m_Rho[m_CurrentLevel]);      
    m_Solver.SetAlpha(m_Alpha); 
    
    CreateMesh((double)m_MeshElementsPerDimensionAtEachResolution[m_CurrentLevel],m_Solver); 
    m_Solver.GenerateGFN(); 
    ApplyLoads(m_Solver,m_ImageSize);


    LinearSystemWrapperItpack itpackWrapper; 
    itpackWrapper.SetMaximumNonZeroValuesInMatrix(25*m_Solver.GetNGFN());
    itpackWrapper.SetMaximumNumberIterations(2*m_Solver.GetNGFN()); 
    itpackWrapper.SetTolerance(1.e-13);
    itpackWrapper.JacobianSemiIterative(); 
    m_Solver.SetLinearSystemWrapper(&itpackWrapper); 

    m_Load=FEMRegistrationFilter<TReference,TTarget>::ImageMetricLoadType::New();

    m_Load->SetReferenceImage(m_RefImg);
    m_Load->SetTargetImage(m_TarImg);
    m_Load->SetMetric(m_Metric);
    m_Load->InitializeMetric();
    ImageSizeType r;
  for (unsigned int i=0; i<ImageDimension; i++) r[i]=m_MetricWidth[m_CurrentLevel];
    m_Load->SetMetricRadius(r);
    m_Load->SetNumberOfIntegrationPoints(m_NumberOfIntegrationPoints[m_CurrentLevel]);
    m_Load->GN=m_Solver.load.size()+1; //NOTE SETTING GN FOR FIND LATER
    m_Load->SetSign((Float)m_DescentDirection);
    m_Solver.load.push_back( FEMP<Load>(&*m_Load) );    
    m_Load=dynamic_cast<FEMRegistrationFilter<TReference,TTarget>::ImageMetricLoadType*> (&*m_Solver.load.Find(m_Solver.load.size()));  
  
 
    m_Solver.AssembleKandM();
 
    IterativeSolve(m_Solver);

    GetVectorField(m_Solver);
    WarpImage(m_RefImg); 
  }
  else if (m_Maxiters[m_CurrentLevel] > 0)
  {
    MultiResSolve();
  }
    

  std::cout<<"\n E " << m_E[m_CurrentLevel] << " dt " << m_dT << " rho " << m_Rho[m_CurrentLevel] << "\n";
  
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
void FEMRegistrationFilter<TReference,TTarget>::ChooseMetric(unsigned int whichmetric)
{
  // Choose the similarity metric

  typedef itk::MeanSquaresImageToImageMetric<ImageType,TargetImageType> MetricType0;
  typedef itk::NormalizedCorrelationImageToImageMetric<ImageType,TargetImageType> MetricType1;
  typedef itk::PatternIntensityImageToImageMetric<ImageType,TargetImageType> MetricType2;
  typedef itk::MutualInformationImageToImageMetric<ImageType,TargetImageType> MetricType3;

  
  switch (whichmetric){
    case 0:
      m_Metric=MetricType0::New();
      m_Metric->SetScaleGradient(1.0); // this is the default(?)
    std::cout << " Mean Square " << std::endl;
    break;
    case 1:
      m_Metric=MetricType1::New();
      m_Metric->SetScaleGradient(1.0); 
    std::cout << " Normalized Correlation " << std::endl;
    break;
    case 2:
      m_Metric=MetricType2::New();
      m_Metric->SetScaleGradient(1.0); 
    std::cout << " Pattern Intensity " << std::endl;
    break;
    case 3:
      m_Metric=MetricType3::New();
      m_Metric->SetScaleGradient(1.0); 
    std::cout << " Mutual Information " << std::endl;
    break;
    default:
      m_Metric=MetricType0::New();
      m_Metric->SetScaleGradient(1.0); 
    std::cout << " Mean Square " << std::endl;
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
  char* sbuf;
  unsigned int jj=0;

  std::cout << "Reading config file..." << fname << std::endl;
  f.open(fname);
  if (f) {
  
    this->DoMultiRes(true);

    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    this->m_NumLevels = (unsigned int) ibuf;

    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    this->m_MaxLevel = ibuf;
    
    this->m_MeshElementsPerDimensionAtEachResolution.resize(m_NumLevels);
    FEMLightObject::SkipWhiteSpace(f);
    for (jj=0; jj<this->m_NumLevels; jj++) {
      f >> ibuf;
      this->m_MeshElementsPerDimensionAtEachResolution(jj) = ibuf;
    }
    
    FEMLightObject::SkipWhiteSpace(f);
    this->m_E.resize(m_NumLevels);
    for (jj=0; jj<this->m_NumLevels; jj++) {
      f >> fbuf;
      this->SetElasticity(fbuf,jj);
    }

    FEMLightObject::SkipWhiteSpace(f);
    this->m_Rho.resize(m_NumLevels);
    for (jj=0; jj<this->m_NumLevels; jj++) {
      f >> fbuf;
      this->SetRho(fbuf,jj);
    }

    FEMLightObject::SkipWhiteSpace(f);
    this->m_NumberOfIntegrationPoints.resize(m_NumLevels);
    for(jj=0; jj< m_NumLevels; jj++){ 
    f >> ibuf;
    this->SetNumberOfIntegrationPoints(ibuf,jj);}

    FEMLightObject::SkipWhiteSpace(f);
    this->m_MetricWidth.resize(m_NumLevels);
    for(jj=0; jj< m_NumLevels; jj++){ 
    f >> ibuf;
    this->SetWidthOfMetricRegion(ibuf,jj);}
    
    FEMLightObject::SkipWhiteSpace(f);
    this->m_Maxiters.resize(m_NumLevels);
    for (unsigned int jj=0; jj<this->m_NumLevels; jj++) {
      f >> ibuf;
      this->SetMaximumIterations(ibuf,jj);
    }

    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    this->ChooseMetric(ibuf);

    FEMLightObject::SkipWhiteSpace(f);
    f >> fbuf;
    this->m_Alpha=fbuf;
    
    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    if (ibuf == 0) { this->SetDescentDirectionMinimize(); }
    else { this->SetDescentDirectionMaximize(); }

    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    this->DoLineSearch(ibuf); 
    
    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    m_LineSearchFrequency=ibuf;
    
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
    sbuf = new char[256];
    strcpy(sbuf, buffer);
    this->SetReferenceFile(sbuf);

    FEMLightObject::SkipWhiteSpace(f);
    f >> buffer;
    sbuf = new char[256];
    strcpy(sbuf, buffer);
    this->SetTargetFile(sbuf);

    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    FEMLightObject::SkipWhiteSpace(f);
    f >> buffer;

    if (ibuf == 1) {
      this->UseLandmarks(true);
    sbuf = new char[256];
    strcpy(sbuf, buffer);
      this->SetLandmarkFile(sbuf);
    }
    else { this->UseLandmarks(false); }

    FEMLightObject::SkipWhiteSpace(f);
    f >> buffer;
    sbuf = new char[256];
    strcpy(sbuf, buffer);
    this->SetResultsFile(sbuf);

    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    FEMLightObject::SkipWhiteSpace(f);
    f >> buffer;

    if (ibuf == 1) {
      this->SetWriteDisplacements(true);
      sbuf = new char[256];
      strcpy(sbuf, buffer);
      this->SetDisplacementsFile(sbuf);
    }
    else { this->SetWriteDisplacements(false); }

    FEMLightObject::SkipWhiteSpace(f);
    f >> ibuf;
    FEMLightObject::SkipWhiteSpace(f);
    f >> buffer;

    if (ibuf == 1) {
      this->m_ReadMeshFile=true;
      sbuf = new char[256];
      strcpy(sbuf, buffer);
      this->m_MeshFileName=sbuf;
    }
    else { this->m_ReadMeshFile=false; }

    f.close();
    std::cout << "Example configured. E " << m_E << " rho " << m_Rho << std::endl;
    return true;
  }
  else { 
    std::cout << "No configuration file specified...quitting.\n"; 
    return false;
  }  
}


template<class TReference,class TTarget>
int FEMRegistrationFilter<TReference,TTarget>::WriteDisplacementField(unsigned int index)
  // Outputs the displacement field for the index provided (0=x,1=y,2=z)
{
  // Initialize the caster to the displacement field
  IndexSelectCasterType::Pointer fieldCaster = IndexSelectCasterType::New();
  fieldCaster->SetInput( m_Field );
  fieldCaster->SetIndex( index );
  
  // Define the output of the caster
  FloatImageType::Pointer fieldImage = FloatImageType::New();
  fieldCaster->Update();
  fieldImage = fieldCaster->GetOutput();

  // Set up the output filename
  char* outfile = new char[strlen(m_DisplacementsFileName+10)];
  sprintf(outfile, "%s%c.raw", m_DisplacementsFileName, 'x'+index);
  std::cout << "Writing displacements to " << outfile;

  // Write the single-index field to a file
  //   itk::ImageRegionIteratorWithIndex<FloatImageType> it( fieldImage, fieldImage->GetLargestPossibleRegion() );
  //   for (; !it.IsAtEnd(); ++it) { std::cout << it.Get() << "\t"; }
  typedef typename FloatImageType::PixelType FType;
  typedef RawImageIO<FType,ImageDimension> IOType;
  IOType::Pointer io = IOType::New();
  typedef ImageFileWriter<FloatImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(fieldImage);
  writer->SetImageIO(io);
  writer->SetFileName(outfile);
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
      else m_WarpedImage->SetPixel(rindex,0);
    }
  }
  else 
  {
  typedef itk::WarpImageFilter<ImageType,ImageType,FieldType> WarperType;
  WarperType::Pointer warper = WarperType::New();

  typedef WarperType::CoordRepType CoordRepType;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,CoordRepType>
    InterpolatorType0;
  typedef itk::LinearInterpolateImageFunction<ImageType,CoordRepType>
    InterpolatorType1;
  typedef itk::BSplineInterpolateImageFunction<ImageType,CoordRepType>
    InterpolatorType2;
  InterpolatorType1::Pointer interpolator = InterpolatorType1::New();
  
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
void FEMRegistrationFilter<TReference,TTarget>::CreateMesh(double ElementsPerSide, Solver& mySolver)
{

  vnl_vector<double> MeshOriginV; MeshOriginV.resize(ImageDimension); 
  vnl_vector<double> MeshSizeV;   MeshSizeV.resize(ImageDimension); 
  vnl_vector<double> ElementsPerDimension;  ElementsPerDimension.resize(ImageDimension); 
  for (unsigned int i=0; i<ImageDimension; i++)
  { 
    MeshSizeV[i]=(double)m_ImageSize[i]/(double)m_ImageScaling[i]; // FIX ME  make more general
    MeshOriginV[i]=(double)m_ImageOrigin[i];// FIX ME make more general
    ElementsPerDimension[i]=(double)ElementsPerSide;
  }

  if (m_ReadMeshFile)
  {
  std::ifstream meshstream; 
  meshstream.open(m_MeshFileName);
  if (!meshstream)
  {
    std::cout<<"File "<<m_MeshFileName<<" not found!\n";
    return;
  }

  mySolver.Read(meshstream); 
  itk::fem::MaterialLinearElasticity::Pointer m=dynamic_cast<MaterialLinearElasticity*>(mySolver.mat.Find(0));
    if (m) { 
    m->E=this->GetElasticity(m_CurrentLevel);  // Young modulus -- used in the membrane ///
      //m->A=1.0;     // Crossection area ///
      //m->h=1.0;     // Crossection area ///
      //m->I=1.0;    // Moment of inertia ///
      //m->nu=0.; //.0;    // poissons -- DONT CHOOSE 1.0!!///
      //m->RhoC=1.0;
    }   
  }
  else if (ImageDimension == 2 && dynamic_cast<Element2DC0LinearQuadrilateral*>(m_Element) != NULL)
  {
    m_Material->E=this->GetElasticity(m_CurrentLevel);
    Generate2DRectilinearMesh(m_Element,mySolver,MeshOriginV,MeshSizeV,ElementsPerDimension); 
  }
  else if ( ImageDimension == 3 && dynamic_cast<Element3DC0LinearHexahedron*>(m_Element) != NULL) 
  {
    m_Material->E=this->GetElasticity(m_CurrentLevel);
  
    Generate3DRectilinearMesh(m_Element,mySolver,MeshOriginV,MeshSizeV,ElementsPerDimension); 
  }
  else 
  {  
    throw FEMException(__FILE__, __LINE__, "CreateMesh - wrong image or element type ");
  }
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
  
  //Pin  one corner image  
  unsigned int CornerCounter=0,ii=0,EdgeCounter=0;
  Node::ArrayType* nodes = &(mySolver.node);
  Element::VectorType coord;
  Node::ArrayType::iterator node=nodes->begin();
  bool CornerFound=false; bool EdgeFound=false;
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
      unsigned int whichnode=0;
     
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
        if (EdgeFound){
          for (unsigned int jj=0; jj<ndofpernode; jj++){
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
            l1->m_element=( *((*node)->m_elements.begin())); 
            unsigned int localdof=whichnode*ndofpernode+jj;
            l1->m_dof=localdof; // FIXME should be correct for each element
            l1->m_value=vnl_vector<double>(1,0.0);
            mySolver.load.push_back( FEMP<Load>(&*l1) );
          }
          EdgeCounter++;
        }
      }
      }//end elt loop
      CornerFound=true;
  }
    node++;
  }//

  if (m_UseLandmarks)
  {
  // Landmark loads
  std::ifstream f;
  std::cout << m_LandmarkFileName << std::endl;
  std::cout << "Loading landmarks...";
  f.open(m_LandmarkFileName);
  if (f) {

    try { 
      mySolver.Read(f); 
    }
    catch (itk::ExceptionObject &err) { 
      std::cerr << "Exception: " << err;
      f.close();
    }
    
    f.close();
    std::cout << "done" << std::endl;
  }
  else { std::cout << "no landmark file specified." << std::endl; }
  }
  
  m_Load=FEMRegistrationFilter<TReference,TTarget>::ImageMetricLoadType::New();

  m_Load->SetReferenceImage(m_RefImg);
  m_Load->SetTargetImage(m_TarImg);
  
  m_Load->SetMetric(m_Metric);
  m_Load->InitializeMetric();
  typename ImageType::SizeType r;
  for (unsigned int i=0; i < ImageDimension; i++) r[i]=m_MetricWidth[m_CurrentLevel];
  m_Load->SetMetricRadius(r);
  m_Load->SetNumberOfIntegrationPoints(m_NumberOfIntegrationPoints[m_CurrentLevel]);
  m_Load->GN=mySolver.load.size()+1; //NOTE SETTING GN FOR FIND LATER
  m_Load->SetSign((Float)m_DescentDirection);
  mySolver.load.push_back( FEMP<Load>(&*m_Load) );    
  m_Load=dynamic_cast<FEMRegistrationFilter<TReference,TTarget>::ImageMetricLoadType*> (&*mySolver.load.Find(mySolver.load.size()));  
  
  return;
}



template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::IterativeSolve(SolverType& mySolver)
{
  m_MinE=10.e9;
  Float LastE=9.e9 , deltE=1.e9;

  unsigned int iters=0;
  bool Done=false;
  unsigned int DLS=m_DoLineSearchOnImageEnergy;
  unsigned int LSF=m_LineSearchFrequency;
  while ( !Done ){
  /*
   * Assemble the master force vector (from the applied loads)
   */
   mySolver.AssembleFforTimeStep();

   /*
   * Solve the system of equations for displacements (u=K^-1*F)
   */
   mySolver.Solve();  
  
  
   Float mint=1.0; //,ImageSimilarity=0.0;
   if (DLS > 0 && (iters > 0 /*|| m_CurrentLevel !=0*/) && (iters % LSF) != 0) 
   {
     std::cout << " line search ";
     if (DLS == 1 ) LastE=mySolver.GoldenSection(1.e-1,10);
     else  LastE=mySolver.BrentsMethod(1.e-1,50);
     std::cout << " line search done " << std::endl;
   } else  LastE=mySolver.EvaluateResidual(mint);

   //ImageSimilarity=0.0;//m_Load->EvaluateMetricGivenSolution(&(mySolver.el), mint);
   deltE=(LastE-m_MinE);
   mySolver.AddToDisplacements(mint); 
   if ((LastE <= m_EnergyReductionFactor  /*|| deltE == 0.0 */) || iters > m_Maxiters[m_CurrentLevel] 
        /*|| ImageSimilarity > LastISim*/) 
   {
     Done=true;
   } 
   m_MinE=LastE;
   
   std::cout << " min E " << m_MinE << " delt E " << deltE <<  " iter " << iters << std::endl;
   iters++;
   if ( iters > m_Maxiters[m_CurrentLevel] -3) { DLS=1; LSF=m_Maxiters[m_CurrentLevel]*2;}
   
   // uncomment to write out every deformation SLOW due to interpolating vector field everywhere.
   //GetVectorField();
   //WarpImage(m_RefImg);
   //WriteWarpedImage(m_ResultsFileName);
   
   m_TotalIterations++;
  } 
  
}



template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::GetVectorField(SolverType& mySolver)
{
  std::cout << " computing vector field " << std::endl;
  Element::ArrayType* el = &(mySolver.el);
  vnl_vector_fixed<double,ImageDimension> Pos(0.0);  // solution at the point
  vnl_vector_fixed<double,ImageDimension> Sol(0.0);  // solution at the local point
  vnl_vector_fixed<double,ImageDimension> Gpt(0.0);  // global position given by local point
  FieldIterator m_FieldIter( m_Field, m_FieldRegion );
  m_FieldIter.GoToBegin();
  typename ImageType::IndexType rindex = m_FieldIter.GetIndex();
  for(  Element::ArrayType::iterator elt=el->begin(); elt!=el->end(); elt++) 
  {
  
// this code works only for 2 and 3 dimensions !!
  if (ImageDimension == 2) 
  {
    for (double r=-1.0; r <= 1.; r=r+ 2./(1.0* (double)m_ImageSize[0]/(double)m_MeshElementsPerDimensionAtEachResolution[m_CurrentLevel]) ){
    for (double s=-1.0; s <= 1.; s=s+ 2./(1.0* (double)m_ImageSize[1]/(double)m_MeshElementsPerDimensionAtEachResolution[m_CurrentLevel]) )
    {
      Pos[0]=r; 
      Pos[1]=s;

//      for(  Element::ArrayType::iterator elt=el->begin(); elt!=el->end(); elt++) 
//      {
        VectorType disp; 
        unsigned int Nnodes= (*elt)->GetNumberOfNodes();
        typename Element::VectorType shapef(Nnodes);

        Gpt=(*elt)->GetGlobalFromLocalCoordinates(Pos);
        Sol=(*elt)->InterpolateSolution(Pos,*(mySolver.GetLS()),mySolver.TotalSolutionIndex); // for total solution index
         
        bool inimage=true;
        for (unsigned int ii=0; ii < ImageDimension; ii++)
        { 
          Float x=Gpt[ii];
          long int temp;
          if (x !=0.0) temp=(long int) ((x)*(Float)m_ImageScaling[ii]+0.5);// BUG FIX ME
          else temp=0;
          rindex[ii]=temp;
          disp[ii] =(Float) 1.0*Sol[ii]*((Float)m_ImageScaling[ii]);
          if ( temp < 0 || temp > m_FieldSize[ii]-1)  inimage=false;
        }
     
        if (inimage) m_Field->SetPixel(rindex, disp );
    //} //other elt for
    }} // end for r,s loop
  } else if (ImageDimension==3){
    Float rstep=2.0*(double)m_MeshElementsPerDimensionAtEachResolution[m_CurrentLevel]/(double)m_ImageSize[0];
    Float sstep=2.0*(double)m_MeshElementsPerDimensionAtEachResolution[m_CurrentLevel]/(double)m_ImageSize[1];
    Float tstep=2.0*(double)m_MeshElementsPerDimensionAtEachResolution[m_CurrentLevel]/(double)m_ImageSize[2];
//    std::cout << " r s t steps " << rstep << sstep << tstep << std::endl;
    for (double r=-1.0; r <= 1.0; r=r+rstep ){
    for (double s=-1.0; s <= 1.0; s=s+sstep ){
    for (double t=-1.0; t <= 1.0; t=t+tstep ){
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
          if (x !=0) temp=(long int) ((x)*(Float)m_ImageScaling[f]+0.5); else temp=0;// round after scaling
          rindex[f]=temp;
          disp[f] =(Float) 1.0*Sol[f]*((Float)m_ImageScaling[f]); 
          if ( temp < 0 || temp > m_FieldSize[f]-1)  inimage=false;
        }
//        if (fabs(r) == 1.0 && fabs(s)==1.0 && fabs(t)==1.0 ) std::cout << " rindex " << rindex << " sol " << Sol << std::endl;
        if (inimage) m_Field->SetPixel(rindex, disp );

//  }//end of other elt loop
  }}}//end of for loops
  } // end of if image dim
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



template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::WriteWarpedImage(const char* fname)
{

  // for image output
  FILE *fbin; 
  std::string exte=".raw";
  std::string fnum;
  m_FileCount++;

  OStringStream buf;
  buf<<(m_FileCount+10);
  fnum=std::string(buf.str().c_str());

  std::string fullfname=(fname+fnum+exte);

  ImageIterator wimIter( m_WarpedImage,m_Wregion );

  fbin=fopen(fullfname.c_str(),"wb");
  ImageDataType t=0;
  // for arbitrary dimensionality
  wimIter.GoToBegin();  
  for( ; !wimIter.IsAtEnd(); ++wimIter )
  {
    t=(ImageDataType) wimIter.Get();
    fwrite(&t,sizeof(t),1,fbin); 
  }
  fclose(fbin);     

  
}



template<class TReference,class TTarget>
void FEMRegistrationFilter<TReference,TTarget>::SampleVectorFieldAtNodes(SolverType& mySolver)
{

  std::cout << " upsampling vector field " << std::endl;
  // Here, we need to iterate through the nodes, get the nodal coordinates,
  // sample the VF at the node and place the values in the SolutionVector.
  unsigned int ii=0;
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
      if ( rindex[ii] < 0 || rindex[ii]  > m_FieldSize[ii]-1  || inimage == false){ 
        inimage=false;      
      SolutionAtNode[ii]=0;
      }
    }
    if (inimage)   SolutionAtNode=m_Field->GetPixel(rindex);
    // Now put it into the solution!
    for (ii=0; ii < ImageDimension; ii++)
    { 
      Float Sol=SolutionAtNode[ii]/((Float)m_ImageScaling[ii]); // Scale back to current scale
    std::cout << " the sol " << Sol << " at coord " << coord << " rind " << rindex << std::endl;
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
  typedef PyramidType::ScheduleType ScheduleType;
  PyramidType::Pointer pyramidR = PyramidType::New();
  PyramidType::Pointer pyramidT = PyramidType::New();

  typedef itk::CastImageFilter<ImageType,FloatImageType> CasterType1;
  typedef itk::CastImageFilter<FloatImageType,ImageType> CasterType2;
  CasterType1::Pointer Rcaster1 = CasterType1::New();
  CasterType1::Pointer Tcaster1 = CasterType1::New();
  CasterType2::Pointer Rcaster2;
  CasterType2::Pointer Tcaster2;

  Rcaster1->SetInput(m_RefImg); Rcaster1->Update();
  pyramidR->SetInput( Rcaster1->GetOutput());
  Tcaster1->SetInput(m_TarImg); Tcaster1->Update();
  pyramidT->SetInput( Tcaster1->GetOutput());

// set schedule by specifying the number of levels;
   pyramidR->SetNumberOfLevels( m_NumLevels );
  pyramidT->SetNumberOfLevels( m_NumLevels );

//  ImageType::SizeType Isz=m_RefImg->GetLargestPossibleRegion().GetSize();
  ScheduleType SizeReductionR=pyramidR->GetSchedule();
  ScheduleType SizeReductionT=pyramidT->GetSchedule();
  //for (unsigned int ii=0; ii<m_NumLevels; ii++) for (unsigned int jj=0; jj<ImageDimension; jj++) 
  //{ SizeReductionR[ii][jj]=1;SizeReductionT[ii][jj]=1;}
  pyramidR->SetSchedule(SizeReductionR); pyramidT->SetSchedule(SizeReductionT);
  std::cout << " size change at this level " << SizeReductionR << std::endl;
  pyramidR->Update();
  pyramidT->Update();

/*  itk::RawImageIO<ImageDataType,ImageDimension>::Pointer io;
  itk::ImageFileWriter<ImageType>::Pointer writer;
  io = itk::RawImageIO<ImageDataType,ImageDimension>::New();
  writer = itk::ImageFileWriter<ImageType>::New();
  writer->SetImageIO(io);
  writer->SetFileName("E:\\Avants\\MetaImages\\junk64x64.raw");
  writer->SetInput(m_TarImg); writer->Write();*/
 
  for (m_CurrentLevel=0; m_CurrentLevel<m_MaxLevel; m_CurrentLevel++)
  {
    pyramidR->GetOutput( m_CurrentLevel )->Update();
    pyramidT->GetOutput( m_CurrentLevel )->Update();

//    Tcaster2->SetInput(pyramidT->GetOutput(m_CurrentLevel)); Tcaster2->Update(); writer->SetInput(Tcaster2->GetOutput()); writer->Write();
    
    typename ImageType::SizeType Isz=pyramidT->GetOutput( m_CurrentLevel )->GetLargestPossibleRegion().GetSize();

    for (unsigned int d=0; d < ImageDimension; d++)
    {
      m_ImageScaling[d]=SizeReductionR[m_CurrentLevel][d];
    }

      double MeshResolution=(double)this->m_MeshElementsPerDimensionAtEachResolution(m_CurrentLevel);

      Rcaster2 = CasterType2::New();// Weird - don't know why but this worked
      Tcaster2 = CasterType2::New();// and declaring the casters outside the loop did not.

      SolverType SSS;

      SSS.SetDeltatT(m_dT);  
      SSS.SetRho(m_Rho[m_CurrentLevel]);     
      SSS.SetAlpha(m_Alpha);    

      CreateMesh(MeshResolution,SSS); 
      SSS.GenerateGFN();
      ApplyLoads(SSS,Isz);

      LinearSystemWrapperItpack itpackWrapper; 
      itpackWrapper.SetMaximumNonZeroValuesInMatrix(25*SSS.GetNGFN());
      itpackWrapper.SetMaximumNumberIterations(2*SSS.GetNGFN()); 
      itpackWrapper.SetTolerance(1.e-13);
      itpackWrapper.JacobianSemiIterative(); 
      SSS.SetLinearSystemWrapper(&itpackWrapper); 

      m_Load=FEMRegistrationFilter<TReference,TTarget>::ImageMetricLoadType::New();

      Rcaster2->SetInput(pyramidR->GetOutput(m_CurrentLevel)); Rcaster2->Update();
      m_Load->SetReferenceImage(Rcaster2->GetOutput()); 
 
      Tcaster2->SetInput(pyramidT->GetOutput(m_CurrentLevel)); Tcaster2->Update();
      m_Load->SetTargetImage(Tcaster2->GetOutput());  

      m_Load->SetMetric(m_Metric);
      m_Load->InitializeMetric();
      ImageSizeType r;
      for (unsigned int dd=0; dd<ImageDimension; dd++) r[dd]=m_MetricWidth[m_CurrentLevel];
      m_Load->SetMetricRadius(r);
      m_Load->SetNumberOfIntegrationPoints(m_NumberOfIntegrationPoints[m_CurrentLevel]);
      m_Load->GN=SSS.load.size()+1; //NOTE SETTING GN FOR FIND LATER
      m_Load->SetSign((Float)m_DescentDirection);
      SSS.load.push_back( FEMP<Load>(&*m_Load) );    
      m_Load=dynamic_cast<FEMRegistrationFilter<TReference,TTarget>::ImageMetricLoadType*> (&*SSS.load.Find(SSS.load.size()));  
   
      SSS.AssembleKandM();
 
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
/*


void FEMRegistrationFilter<TReference,TTarget>::CreateLinearSystemSolver()
{
    // experiment with these values and choices for solver
    LinearSystemSolverType* itpackWrapper=new LinearSystemSolverType; 
    itpackWrapper->SetMaximumNonZeroValuesInMatrix(25*m_Solver.GetNGFN());
    itpackWrapper->SetMaximumNumberIterations(m_Solver.GetNGFN()); 
    itpackWrapper->SetTolerance(1.e-13);

    // select solution type
    // did converge in test
    
    itpackWrapper->JacobianSemiIterative(); // err 23 500 its
    m_Solver.SetLinearSystemWrapper(itpackWrapper); 

    // did not converge below here:  ordered best to worst
   // itpackWrapper->SymmetricSuccessiveOverrelaxationSuccessiveOverrelaxation(); // err 53
   // itpackWrapper->SymmetricSuccessiveOverrelaxationConjugateGradient();// err 43 
   // itpackWrapper->SuccessiveOverrelaxation(); // err 33 500 its

    // These methods failed!!
    //itpackWrapper->JacobianConjugateGradient();  // err 13 500 its
    //itpackWrapper->ReducedSystemSemiIteration(); // err 201 
    //itpackWrapper->ReducedSystemConjugateGradient(); // err 201 

    
}


*/

}} // end namespace itk::fem

 
#endif
