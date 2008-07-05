/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDiffeomorphicDemonsRegistrationFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif


/**
 * This is a small tool that shows how to use the diffeomorphic demons algorithm.
 * The user can choose if diffeomorphic, additive or compositive demons should be used.
 * The user can also choose the type of demons forces, or other parameters;
 *
 * \author Tom Vercauteren, INRIA & Mauna Kea Technologies
 *
 * contributed by Tom Vercauteren in his paper to the Insight Journal
 * http://hdl.handle.net/1926/510
 *
 */

#include "itkMultiResolutionPDEDeformableRegistration.h"
#include "itkFastSymmetricForcesDemonsRegistrationFilter.h"
#include "itkDiffeomorphicDemonsRegistrationFilter.h"
#include "itkWarpImageFilter.h"
#include "itkCommand.h"
#include "itkWarpJacobianDeterminantFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkWarpHarmonicEnergyCalculator.h"
#include "itkGridForwardWarpImageFilter.h"
#include "itkVectorCentralDifferenceImageFunction.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkHistogramMatchingImageFilter.h"


#include <getopt.h>
#include <iostream>

struct arguments
{
   std::string  fixedImageFile;  /* -f option */
   std::string  movingImageFile; /* -m option */
   std::string  inputFieldFile;  /* -b option */
   std::string  outputImageFile; /* -o option */
   std::string  outputFieldFile; /* -O option */
   std::string  trueFieldFile;   /* -r option */
   unsigned int numLevels;       /* -n option */
   std::vector<unsigned int> numIterations;   /* -i option */
   float sigmaDef;               /* -s option */
   float sigmaUp;                /* -g option */
   float maxStepLength;          /* -l option */
   unsigned int updateRule;      /* -a option */
   unsigned int gradientType;    /* -t option */
   bool useHistogramMatching;    /* -e option */
   unsigned int verbosity;       /* -v option */

   arguments () :
      fixedImageFile(""),
      movingImageFile(""),
      inputFieldFile(""),
      outputImageFile("output.mha"),
      outputFieldFile(""),
      trueFieldFile(""),
      numLevels(3u),
      sigmaDef(3.0f),
      sigmaUp(0.0f),
      maxStepLength(2.0f),
      updateRule(0u),
      gradientType(0u),
      useHistogramMatching(false),
      verbosity(0u)
   {
      numIterations = std::vector<unsigned int>(numLevels, 10u);
   }

   friend std::ostream& operator<< (std::ostream& o, const arguments& args)
   {
      std::ostringstream osstr;
      for (unsigned int i=0; i<args.numIterations.size(); ++i)
         osstr<<args.numIterations[i]<<" ";
      std::string iterstr = "[ " + osstr.str() + "]";

      std::string gtypeStr;
      switch (args.gradientType)
      {
      case 0:
         gtypeStr = "symmetrized (ESM for compositive)";
         break;
      case 1:
         gtypeStr = "fixed image (Thirion's vanilla forces)";
         break;
      case 2:
         gtypeStr = "warped moving image (Gauss-Newton for compositive)";
         break;
      case 3:
         gtypeStr = "mapped moving image (Gauss-Newton for additive)";
         break;
      default:
         gtypeStr = "unsuported";
      }

      std::string uruleStr;
      switch (args.updateRule)
      {
      case 0:
         uruleStr = "exponentialize and compose (Diffeomorphic)";
         break;
      case 1:
         uruleStr = "add (ITK basic implementation)";
         break;
      case 2:
         uruleStr = "compose (Thirion's proposal)";
         break;
      default:
         uruleStr = "unsuported";
      }
         
      return o
         <<"Arguments structure:"<<std::endl
         <<"  Fixed image file: "<<args.fixedImageFile<<std::endl
         <<"  Moving image file: "<<args.movingImageFile<<std::endl
         <<"  Input field file: "<<args.inputFieldFile<<std::endl
         <<"  Output image file: "<<args.outputImageFile<<std::endl
         <<"  Output field file: "<<args.outputFieldFile<<std::endl
         <<"  True field file: "<<args.trueFieldFile<<std::endl
         <<"  Number of multiresolution levels: "<<args.numLevels<<std::endl
         <<"  Number of demons iterations: "<<iterstr<<std::endl
         <<"  Deformation field sigma: "<<args.sigmaDef<<std::endl
         <<"  Update field sigma: "<<args.sigmaUp<<std::endl
         <<"  Maximum update step length: "<<args.maxStepLength<<std::endl
         <<"  Update rule: "<<uruleStr<<std::endl
         <<"  Type of gradient: "<<gtypeStr<<std::endl
         <<"  Use histogram matching: "<<args.useHistogramMatching<<std::endl
         <<"  Verbosity: "<<args.verbosity;
   }
};

static const char *optString = "f:m:b:o:O::r:n:i:s:g:l:a:t:ev::h?";

static const struct option longOpts[] = {
   { "fixed-image", required_argument, NULL, 'f' },
   { "moving-image", required_argument, NULL, 'm' },
   { "input-field", required_argument, NULL, 'b' },
   { "output-image", required_argument, NULL, 'o' },
   { "output-field", optional_argument, NULL, 'O' },
   { "true-field", required_argument, NULL, 'r' },
   { "num-levels", required_argument, NULL, 'n' },
   { "num-iterations", required_argument, NULL, 'i' },
   { "def-field-sigma", required_argument, NULL, 's' },
   { "up-field-sigma", required_argument, NULL, 'g' },
   { "max-step-length", required_argument, NULL, 'l' },
   { "update-rule", required_argument, NULL, 'a' },
   { "gradient-type", required_argument, NULL, 't' },
   { "use-histogram-matching", no_argument, NULL, 'e' },
   { "verbose", optional_argument, NULL, 'v' },
   { "help", no_argument, NULL, 'h' },
   { NULL, no_argument, NULL, 0 }
};

/* Display program usage, and exit.
 */
void display_usage( const std::string progname )
{   
   struct arguments defargs = arguments();

   std::ostringstream osstr;
   for (unsigned int i=0; i<defargs.numIterations.size(); ++i)
   {
      osstr<<defargs.numIterations[i]<<" ";
   }
   std::string iterstr = "[ " + osstr.str() + "]";
   
   std::cout<<std::endl;
   std::cout<<progname<<" - register 2 images using demons algorithm"<<std::endl;
   std::cout<<"Usage: "<<progname<<" [OPTION...]"<<std::endl;
   
   std::cout<<"  -f/--fixed-image=STRING    Fixed image filename - mandatory"<<std::endl;
   std::cout<<"  -m/--moving-image=STRING   Moving image filename - mandatory"<<std::endl;
   std::cout<<"  -b/--input-field=STRING    Input field filename - default: empty"<<std::endl;
   std::cout<<"  -o/--output-image=STRING   Output image filename - default: "<<defargs.outputImageFile<<std::endl;
   std::cout<<"  -O/--output-field(=STRING) Output field filename - default: OUTPUTIMAGENAME-field.mha"<<std::endl;
   std::cout<<"  -r/--true-field=STRING     True field filename - default: not used"<<std::endl;
   std::cout<<"  -n/--num-levels=UINT       number of multi-scale levels - default: "<<defargs.numLevels<<std::endl;
   std::cout<<"  -i/--num-iterations=UINTx...xUINT   number of demons iterations - default: "<<iterstr<<std::endl;
   std::cout<<"  -s/--def-field-sigma=FLOAT Smoothing sigma for the deformation field"<<std::endl
            <<"                             at each iteration - default: "<<defargs.sigmaDef<<std::endl;
   std::cout<<"  -g/--up-field-sigma=FLOAT  Smoothing sigma for the update field"<<std::endl
            <<"                             at each iteration - default: "<<defargs.sigmaUp<<std::endl;
   std::cout<<"  -l/--max-step-length=FLOAT Maximum length of an update vector"<<std::endl
            <<"                             (0: no restriction) - default: "<<defargs.maxStepLength<<std::endl;
   std::cout<<"  -a/--update-rule           Type of update rule. (0: s <- s o exp(u) (diffeomorphic),"<<std::endl
            <<"                             1: s <- s + u (ITK basic), 2: s <- s o (Id+u) (Thirion))"<<std::endl;
   std::cout<<"  -t/--gradient-type=UINT    type of gradient used for computing the demons force"<<std::endl
            <<"                             (0 is symmetrized, 1 is fixed image, 2 is warped moving image, 3 is mapped moving image) - default: "<<defargs.gradientType<<std::endl;
   std::cout<<"  -e/--use-histogram-matching  Use histogram matching (e.g. for different MRs)"<<std::endl;
   std::cout<<"  -v/--verbose(=UINT)        Verbosity - default: "<<defargs.verbosity<<"; without argurment: 1"<<std::endl;
   std::cout<<"  -h/--help                  Display this message and exit"<<std::endl;

   std::cout<<std::endl;
   std::cout<<"Copyright (c) 2006 Mauna Kea Technologies."<<std::endl;
   std::cout<<"Code: Tom Vercauteren."<<std::endl;
   std::cout<<"Report bugs to <tom@maunakeatech.com>."<<std::endl;
   
  exit( EXIT_FAILURE );
}



std::vector<unsigned int> parseUIntVector( const std::string & str)
{
   std::vector<unsigned int> vect;
   
   std::string::size_type crosspos = str.find('x',0);

   if (crosspos == std::string::npos)
   {
      // only one uint
      vect.push_back( static_cast<unsigned int>( atoi(str.c_str()) ));
      return vect;
   }

   // first uint
   vect.push_back( static_cast<unsigned int>(
                      atoi( (str.substr(0,crosspos)).c_str()  ) ));

   while(true)
   {
      std::string::size_type crossposfrom = crosspos;
      crosspos =  str.find('x',crossposfrom+1);

      if (crosspos == std::string::npos)
      {
         vect.push_back( static_cast<unsigned int>(
                            atoi( (str.substr(crossposfrom+1,str.length()-crossposfrom-1)).c_str()  ) ));
         return vect;
      }

      vect.push_back( static_cast<unsigned int>(
                         atoi( (str.substr(crossposfrom+1,crosspos)).c_str()  ) ));
   }
}



void parseOpts (int argc, char **argv, struct arguments & args)
{
   const std::string progname( "DemonsRegistration" );
   
   // Default values.
   args = arguments();

   std::vector<unsigned int> defiter = args.numIterations;
   args.numIterations.clear();

   if (argc == 1)
   {
      display_usage(progname);
   }
   
   int opt = 0; /* it's actually going to hold a char */
   int longIndex = 0;

   while ( (opt = getopt_long(argc, argv, optString, longOpts, &longIndex)) != -1 )
   {
      switch( opt ) {
      case 'f':
         if (! optarg) display_usage(progname);
         args.fixedImageFile = optarg;
         break;
      
      case 'm':
         if (! optarg) display_usage(progname);
         args.movingImageFile = optarg;
         break;

      case 'b':
         if (! optarg) display_usage(progname);
         else args.inputFieldFile = optarg;
         break;

      case 'o':
         if (! optarg) display_usage(progname);
         args.outputImageFile = optarg;
         break;

      case 'O':
         if (! optarg) args.outputFieldFile = "CHANGETHISSTRING";
         else args.outputFieldFile = optarg;
         break;

      case 'r':
         if (! optarg) display_usage(progname);
         else args.trueFieldFile = optarg;
         break;

      case 'n':
         if (! optarg) display_usage(progname);
         args.numLevels = static_cast<unsigned int>( atoi(optarg) );
         break;

      case 'i':
         if (! optarg) display_usage(progname);
         args.numIterations = parseUIntVector(std::string(optarg));
         break;
         
      case 's':
         if (! optarg) display_usage(progname);
         args.sigmaDef = atof(optarg);
         if ( args.sigmaDef<0.5 and args.sigmaDef>0.0 )
         {
            std::cout<<"Sigma is too small (min=0.5). We set it to 0.0 (no smoothing)."
                     <<std::endl<<std::endl;
            args.sigmaDef = 0.0;
         }
         break;

      case 'g':
         if (! optarg) display_usage(progname);
         args.sigmaUp = atof(optarg);
         if ( args.sigmaUp<0.5 and args.sigmaUp>0.0 )
         {
             std::cout<<"Sigma is too small (min=0.5). We set it to 0.0 (no smoothing)."
                 <<std::endl<<std::endl;
             args.sigmaUp = 0.0;
         }
         break;

      case 'l':
         if (! optarg) display_usage(progname);
         args.maxStepLength = atof(optarg);
         break;

      case 'a':
         if (! optarg) display_usage(progname);
         args.updateRule = static_cast<unsigned int>( atoi(optarg) );
         break;

      case 't':
         if (! optarg) display_usage(progname);
         args.gradientType = static_cast<unsigned int>( atoi(optarg) );
         break;

      case 'e':
         args.useHistogramMatching = true;
         break;
      
      case 'v':
         if (! optarg) args.verbosity++;
         else args.verbosity = static_cast<unsigned int>( atoi(optarg) );
         break;
      
      case 'h':  /* fall-through is intentional */
      case '?':   /* fall-through is intentional */
      default:
         display_usage(progname);
         break;
    }
   }

   if ( args.outputFieldFile=="CHANGETHISSTRING" )
   {
      
      unsigned int pos = args.outputImageFile.find(".");
      if ( pos < args.outputFieldFile.size() )
      {
         args.outputFieldFile = args.outputImageFile;
         args.outputFieldFile.replace(pos, args.outputFieldFile.size(), "-field.mha");
      }
      else
      {
         args.outputFieldFile = args.outputImageFile + "-field.mha";
      }
         
   }

   if ( args.numLevels ==0)
   {
      std::cout<<"The number of levels should be at least one."<<std::endl;
      display_usage(progname);
   }
   if ( args.numIterations.empty() )
   {
      // set a default number of iterations per level
      args.numIterations = std::vector<unsigned int>(args.numLevels, defiter[0]);
   }
   else if ( args.numLevels != args.numIterations.size() )
   {
      std::cout<<"The number of levels and the number of iterations do not match."<<std::endl;
      display_usage(progname);
   }

   if ( args.gradientType > 3 )
   {
      std::cout<<"The gradient type should be 0, 1, 2 or 3."<<std::endl;
      display_usage(progname);
   }

   if ( args.updateRule > 2 )
   {
      std::cout<<"The update rule should be 0, 1 or 2."<<std::endl;
      display_usage(progname);
   }
}



//  The following section of code implements a Command observer
//  that will monitor the evolution of the registration process.
//
template <class TPixel=float, unsigned int VImageDimension=3>
class CommandIterationUpdate : public itk::Command 
{
public:
   typedef  CommandIterationUpdate   Self;
   typedef  itk::Command             Superclass;
   typedef  itk::SmartPointer<Self>  Pointer;

   typedef itk::Image< TPixel, VImageDimension > InternalImageType;
   typedef itk::Vector< TPixel, VImageDimension >    VectorPixelType;
   typedef itk::Image<  VectorPixelType, VImageDimension > DeformationFieldType;

   typedef itk::DiffeomorphicDemonsRegistrationFilter<
      InternalImageType,
      InternalImageType,
      DeformationFieldType>   DiffeomorphicDemonsRegistrationFilterType;

   typedef itk::FastSymmetricForcesDemonsRegistrationFilter<
      InternalImageType,
      InternalImageType,
      DeformationFieldType>   FastSymmetricForcesDemonsRegistrationFilterType;

   typedef itk::MultiResolutionPDEDeformableRegistration<
      InternalImageType, InternalImageType,
      DeformationFieldType, TPixel >   MultiResRegistrationFilterType;

   typedef itk::WarpJacobianDeterminantFilter<
      DeformationFieldType, InternalImageType> JacobianFilterType;
   
   typedef itk::MinimumMaximumImageCalculator<InternalImageType> MinMaxFilterType;

   typedef itk::WarpHarmonicEnergyCalculator<DeformationFieldType>
      HarmonicEnergyCalculatorType;

   typedef itk::VectorCentralDifferenceImageFunction<DeformationFieldType>
      WarpGradientCalculatorType;

   typedef typename WarpGradientCalculatorType::OutputType WarpGradientType;
   
   itkNewMacro( Self );

private:
   std::ofstream m_Fid;
   bool m_headerwritten;
   typename JacobianFilterType::Pointer m_JacobianFilter;
   typename MinMaxFilterType::Pointer m_Minmaxfilter;
   typename HarmonicEnergyCalculatorType::Pointer m_HarmonicEnergyCalculator;
   typename DeformationFieldType::ConstPointer m_TrueField;
   typename WarpGradientCalculatorType::Pointer m_TrueWarpGradientCalculator;
   typename WarpGradientCalculatorType::Pointer m_CompWarpGradientCalculator;

public:
   void SetTrueField(const DeformationFieldType * truefield)
   {
      m_TrueField = truefield;

      m_TrueWarpGradientCalculator = WarpGradientCalculatorType::New();
      m_TrueWarpGradientCalculator->SetInputImage( m_TrueField );

      m_CompWarpGradientCalculator =  WarpGradientCalculatorType::New();
   }
   
   void Execute(itk::Object *caller, const itk::EventObject & event)
   {
      Execute( (const itk::Object *)caller, event);
   }

   void Execute(const itk::Object * object, const itk::EventObject & event)
   {
      if( !(itk::IterationEvent().CheckEvent( &event )) )
      {
         return;
      }

      typename DeformationFieldType::ConstPointer deffield = 0;
      unsigned int iter = -1;
      double metricbefore = -1.0;
      
      if ( const DiffeomorphicDemonsRegistrationFilterType * filter = 
           dynamic_cast< const DiffeomorphicDemonsRegistrationFilterType * >( object ) )
      {
         iter = filter->GetElapsedIterations() - 1;
         metricbefore = filter->GetMetric();
         deffield = const_cast<DiffeomorphicDemonsRegistrationFilterType *>
            (filter)->GetDeformationField();
      }
      else if ( const FastSymmetricForcesDemonsRegistrationFilterType * filter = 
           dynamic_cast< const FastSymmetricForcesDemonsRegistrationFilterType * >( object ) )
      {
         iter = filter->GetElapsedIterations() - 1;
         metricbefore = filter->GetMetric();
         deffield = const_cast<FastSymmetricForcesDemonsRegistrationFilterType *>
            (filter)->GetDeformationField();
      }
      else if ( const MultiResRegistrationFilterType * multiresfilter = 
           dynamic_cast< const MultiResRegistrationFilterType * >( object ) )
      {
         std::cout<<"Finished Multi-resolution iteration :"<<multiresfilter->GetCurrentLevel()-1<<std::endl;
         std::cout<<"=============================="<<std::endl<<std::endl;
      }
      else
      {
         return;
      }

      if (deffield)
      {
         std::cout<<iter<<": MSE "<<metricbefore<<" - ";

         double fieldDist = -1.0;
         double fieldGradDist = -1.0;
         double tmp;
         if (m_TrueField)
         {
            typedef itk::ImageRegionConstIteratorWithIndex<DeformationFieldType>
               FieldIteratorType;
            FieldIteratorType currIter(
               deffield, deffield->GetLargestPossibleRegion() );
            FieldIteratorType trueIter(
               m_TrueField, deffield->GetLargestPossibleRegion() );

            m_CompWarpGradientCalculator->SetInputImage( deffield );

            fieldDist = 0.0;
            fieldGradDist = 0.0;
            for ( currIter.GoToBegin(), trueIter.GoToBegin();
                  not currIter.IsAtEnd(); ++currIter, ++trueIter )
            {
               fieldDist += (currIter.Value() - trueIter.Value()).GetSquaredNorm();

               // No need to add Id matrix here as we do a substraction
               tmp = (
                  ( m_CompWarpGradientCalculator->EvaluateAtIndex(currIter.GetIndex())
                    -m_TrueWarpGradientCalculator->EvaluateAtIndex(trueIter.GetIndex())
                     ).GetVnlMatrix() ).frobenius_norm();
               fieldGradDist += tmp*tmp;
            }
            fieldDist = sqrt( fieldDist/ (double)(
                     deffield->GetLargestPossibleRegion().GetNumberOfPixels()) );
            fieldGradDist = sqrt( fieldGradDist/ (double)(
                     deffield->GetLargestPossibleRegion().GetNumberOfPixels()) );
            
            std::cout<<"d(.,true) "<<fieldDist<<" - ";
            std::cout<<"d(.,Jac(true)) "<<fieldGradDist<<" - ";
         }
         
         m_HarmonicEnergyCalculator->SetImage( deffield );
         m_HarmonicEnergyCalculator->Compute();
         const double harmonicEnergy
            = m_HarmonicEnergyCalculator->GetHarmonicEnergy();
         std::cout<<"harmo. "<<harmonicEnergy<<" - ";

         
         m_JacobianFilter->SetInput( deffield );
         m_JacobianFilter->UpdateLargestPossibleRegion();

        
         const unsigned int numPix = m_JacobianFilter->
            GetOutput()->GetLargestPossibleRegion().GetNumberOfPixels();
         
         TPixel* pix_start = m_JacobianFilter->GetOutput()->GetBufferPointer();
         TPixel* pix_end = pix_start + numPix;

         TPixel* jac_ptr;

         // Get percentage of det(Jac) below 0
         unsigned int jacBelowZero(0u);
         for (jac_ptr=pix_start; jac_ptr!=pix_end; ++jac_ptr)
         {
            if ( *jac_ptr<=0.0 ) ++jacBelowZero;
         }
         const double jacBelowZeroPrc = static_cast<double>(jacBelowZero)
            / static_cast<double>(numPix);
         

         // Get min an max jac
         const double minJac = *(std::min_element (pix_start, pix_end));
         const double maxJac = *(std::max_element (pix_start, pix_end));

         // Get some quantiles
         // We don't need the jacobian image
         // we can modify/sort it in place
         jac_ptr = pix_start + static_cast<unsigned int>(0.002*numPix);
         std::nth_element(pix_start, jac_ptr, pix_end);
         const double Q002 = *jac_ptr;

         jac_ptr = pix_start + static_cast<unsigned int>(0.01*numPix);
         std::nth_element(pix_start, jac_ptr, pix_end);
         const double Q01 = *jac_ptr;

         jac_ptr = pix_start + static_cast<unsigned int>(0.99*numPix);
         std::nth_element(pix_start, jac_ptr, pix_end);
         const double Q99 = *jac_ptr;

         jac_ptr = pix_start + static_cast<unsigned int>(0.998*numPix);
         std::nth_element(pix_start, jac_ptr, pix_end);
         const double Q998 = *jac_ptr;
         

         std::cout<<"max|Jac| "<<maxJac<<" - "
                  <<"min|Jac| "<<minJac<<" - "
                  <<"ratio(|Jac|<=0) "<<jacBelowZeroPrc<<std::endl;
         
         

         if (this->m_Fid.is_open())
         {
            if (not m_headerwritten)
            {
               this->m_Fid<<"Iteration"
                          <<", MSE before"
                          <<", Harmonic energy"
                          <<", min|Jac|"
                          <<", 0.2% |Jac|"
                          <<", 01% |Jac|"
                          <<", 99% |Jac|"
                          <<", 99.8% |Jac|"
                          <<", max|Jac|"
                          <<", ratio(|Jac|<=0)";
               
               if (m_TrueField)
               {
                  this->m_Fid<<", dist(warp,true warp)"
                             <<", dist(Jac,true Jac)";
               }
               
               this->m_Fid<<std::endl;
               
               m_headerwritten = true;
            }
            
            this->m_Fid<<iter
                       <<", "<<metricbefore
                       <<", "<<harmonicEnergy
                       <<", "<<minJac
                       <<", "<<Q002
                       <<", "<<Q01
                       <<", "<<Q99
                       <<", "<<Q998
                       <<", "<<maxJac
                       <<", "<<jacBelowZeroPrc;

            if (m_TrueField)
            {
               this->m_Fid<<", "<<fieldDist
                          <<", "<<fieldGradDist;
            }
            
            this->m_Fid<<std::endl;
         }
      }
   }
   
protected:   
   CommandIterationUpdate() :
      m_Fid( "metricvalues.csv" ),
      m_headerwritten(false)
   {
      m_JacobianFilter = JacobianFilterType::New();
      m_JacobianFilter->SetUseImageSpacing( true );
      m_JacobianFilter->ReleaseDataFlagOn();
      
      m_Minmaxfilter = MinMaxFilterType::New();

      m_HarmonicEnergyCalculator = HarmonicEnergyCalculatorType::New();

      m_TrueField = 0;
      m_TrueWarpGradientCalculator = 0;
      m_CompWarpGradientCalculator = 0;
   };

   ~CommandIterationUpdate()
   {
      this->m_Fid.close();
   }
};



template <unsigned int Dimension>
void DemonsRegistrationFunction( arguments args )
{
   // Declare the types of the images (float or double only)
   typedef float PixelType;
   typedef itk::Image< PixelType, Dimension >  ImageType;

   typedef itk::Vector< PixelType, Dimension >    VectorPixelType;
   typedef typename itk::Image
      < VectorPixelType, Dimension > DeformationFieldType;


   // Images we use
   typename ImageType::Pointer fixedImage = 0;
   typename ImageType::Pointer movingImage = 0;
   typename DeformationFieldType::Pointer inputDefField = 0;


   // Set up the file readers
   typedef itk::ImageFileReader< ImageType > FixedImageReaderType;
   typedef itk::ImageFileReader< ImageType > MovingImageReaderType;
   typedef itk::ImageFileReader< DeformationFieldType > FieldReaderType;

   {//for mem allocations
   
   typename FixedImageReaderType::Pointer fixedImageReader   = FixedImageReaderType::New();
   typename MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();
   
   fixedImageReader->SetFileName( args.fixedImageFile.c_str() );
   movingImageReader->SetFileName( args.movingImageFile.c_str() );


   // Update the reader
   try
   {
      fixedImageReader->Update();
      movingImageReader->Update();
   }
   catch( itk::ExceptionObject& err )
   {
      std::cout << "Could not read one of the input images." << std::endl;
      std::cout << err << std::endl;
      exit( EXIT_FAILURE );
   }

   if ( not args.inputFieldFile.empty() )
   {
      // Set up the file readers
      typename FieldReaderType::Pointer fieldReader = FieldReaderType::New();
      fieldReader->SetFileName(  args.inputFieldFile.c_str() );
      
      // Update the reader
      try
      {
         fieldReader->Update();
      }
      catch( itk::ExceptionObject& err )
      {
         std::cout << "Could not read the input field." << std::endl;
         std::cout << err << std::endl;
         exit( EXIT_FAILURE );
      }

      inputDefField = fieldReader->GetOutput();
      inputDefField->DisconnectPipeline();
   }

   

   if (!args.useHistogramMatching)
   {
      fixedImage = fixedImageReader->GetOutput();
      fixedImage->DisconnectPipeline();
      movingImage = movingImageReader->GetOutput();
      movingImage->DisconnectPipeline();
   }
   else
   {
      // match intensities
      ///\todo use inputDefField if any to get a better guess
      typedef typename itk::HistogramMatchingImageFilter
         <ImageType, ImageType> MatchingFilterType;
      typename MatchingFilterType::Pointer matcher = MatchingFilterType::New();

      matcher->SetInput( movingImageReader->GetOutput() );
      matcher->SetReferenceImage( fixedImageReader->GetOutput() );

      matcher->SetNumberOfHistogramLevels( 1024 );
      matcher->SetNumberOfMatchPoints( 7 );
      matcher->ThresholdAtMeanIntensityOn();

      // Update the matcher
      try
      {
         matcher->Update();
      }
      catch( itk::ExceptionObject& err )
      {
         std::cout << "Could not match the input images." << std::endl;
         std::cout << err << std::endl;
         exit( EXIT_FAILURE );
      }

      movingImage = matcher->GetOutput();
      movingImage->DisconnectPipeline();
      
      fixedImage = fixedImageReader->GetOutput();
      fixedImage->DisconnectPipeline();
   }

   }//end for mem allocations


   // Set up the demons filter output
   typename DeformationFieldType::Pointer defField = 0;

   {//for mem allocations
   
   // Set up the demons filter
   typedef typename itk::PDEDeformableRegistrationFilter
      < ImageType, ImageType, DeformationFieldType>   BaseRegistrationFilterType;
   typename BaseRegistrationFilterType::Pointer filter;

   switch (args.updateRule)
   {
   case 0:
   {
      // s <- s o exp(u) (Diffeomorphic demons)
      typedef typename itk::DiffeomorphicDemonsRegistrationFilter
         < ImageType, ImageType, DeformationFieldType>
         ActualRegistrationFilterType;
      typedef typename ActualRegistrationFilterType::GradientType GradientType;
      
      typename ActualRegistrationFilterType::Pointer actualfilter
         = ActualRegistrationFilterType::New();

      actualfilter->SetMaximumUpdateStepLength( args.maxStepLength );
      actualfilter->SetUseGradientType(
         static_cast<GradientType>(args.gradientType) );
      filter = actualfilter;

      break;
   }
   case 1:
   {
      // s <- s + u (ITK basic implementation)
      typedef typename itk::FastSymmetricForcesDemonsRegistrationFilter
         < ImageType, ImageType, DeformationFieldType>
         ActualRegistrationFilterType;
      typedef typename ActualRegistrationFilterType::GradientType GradientType;
      
      typename ActualRegistrationFilterType::Pointer actualfilter
         = ActualRegistrationFilterType::New();
      
      actualfilter->SetMaximumUpdateStepLength( args.maxStepLength );
      actualfilter->SetUseGradientType(
         static_cast<GradientType>(args.gradientType) );
      filter = actualfilter;

      break;
   }
   case 2:
   {
      // s <- s o (Id + u) (Diffeomorphic demons)
      // This is simply a crude diffeomorphic demons
      // where the exponential is computed in 0 iteration

      typedef typename itk::DiffeomorphicDemonsRegistrationFilter
         < ImageType, ImageType, DeformationFieldType>
         ActualRegistrationFilterType;
      typedef typename ActualRegistrationFilterType::GradientType GradientType;
      
      typename ActualRegistrationFilterType::Pointer actualfilter
         = ActualRegistrationFilterType::New();

      actualfilter->SetMaximumUpdateStepLength( args.maxStepLength );
      actualfilter->SetUseGradientType(
         static_cast<GradientType>(args.gradientType) );
      actualfilter->UseFirstOrderExpOn();
      filter = actualfilter;
      
      break;
   }
   default:
   {
      std::cout << "Unsupported update rule." << std::endl;
      exit( EXIT_FAILURE );
   }
   }

   if ( args.sigmaDef > 0.1 )
   {
      filter->SmoothDeformationFieldOn();
      filter->SetStandardDeviations( args.sigmaDef );
   }
   else
   {
      filter->SmoothDeformationFieldOff();
   }

   if ( args.sigmaUp > 0.1 )
   {
      filter->SmoothUpdateFieldOn();
      filter->SetUpdateFieldStandardDeviations( args.sigmaUp );
   }
   else
   {
      filter->SmoothUpdateFieldOff();
   }

   //filter->SetIntensityDifferenceThreshold( 0.001 );

   if ( args.verbosity > 0 )
   {      
      // Create the Command observer and register it with the registration filter.
      typename CommandIterationUpdate<PixelType, Dimension>::Pointer observer =
         CommandIterationUpdate<PixelType, Dimension>::New();

      if ( not args.trueFieldFile.empty() )
      {
         if (args.numLevels>1)
         {
            std::cout << "You cannot compare the results with a true filed in a multiresolution setting yet." << std::endl;
            exit( EXIT_FAILURE );
         }
         
         // Set up the file readers
         typename FieldReaderType::Pointer fieldReader = FieldReaderType::New();
         fieldReader->SetFileName(  args.trueFieldFile.c_str() );

         // Update the reader
         try
         {
            fieldReader->Update();
         }
         catch( itk::ExceptionObject& err )
         {
            std::cout << "Could not read the true field." << std::endl;
            std::cout << err << std::endl;
            exit( EXIT_FAILURE );
         }

         observer->SetTrueField( fieldReader->GetOutput() );
      }
      
      filter->AddObserver( itk::IterationEvent(), observer );
   }

   // Set up the multi-resolution filter
   typedef typename itk::MultiResolutionPDEDeformableRegistration<
      ImageType, ImageType, DeformationFieldType, PixelType >   MultiResRegistrationFilterType;
   typename MultiResRegistrationFilterType::Pointer multires = MultiResRegistrationFilterType::New();

   multires->SetRegistrationFilter( filter );
   multires->SetNumberOfLevels( args.numLevels );
   
   multires->SetNumberOfIterations( &args.numIterations[0] );

   multires->SetFixedImage( fixedImage );
   multires->SetMovingImage( movingImage );
   multires->SetInitialDeformationField( inputDefField );


   if ( args.verbosity > 0 )
   {
      // Create the Command observer and register it with the registration filter.
      typename CommandIterationUpdate<PixelType, Dimension>::Pointer multiresobserver =
         CommandIterationUpdate<PixelType, Dimension>::New();
      multires->AddObserver( itk::IterationEvent(), multiresobserver );
   }
   

   
   // Compute the deformation field
   try
   {
      multires->UpdateLargestPossibleRegion();
   }
   catch( itk::ExceptionObject& err )
   {
      std::cout << "Unexpected error." << std::endl;
      std::cout << err << std::endl;
      exit( EXIT_FAILURE );
   }


   // The outputs
   defField = multires->GetOutput();
   defField->DisconnectPipeline();

   }//end for mem allocations

   
   // warp the result
   typedef itk::WarpImageFilter
      < ImageType, ImageType, DeformationFieldType >  WarperType;
   typename WarperType::Pointer warper = WarperType::New();
   warper->SetInput( movingImage );
   warper->SetOutputSpacing( fixedImage->GetSpacing() );
   warper->SetOutputOrigin( fixedImage->GetOrigin() );
   warper->SetDeformationField( defField );

   
   // Write warped image out to file
   typedef PixelType OutputPixelType;
   typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
   typedef itk::CastImageFilter
      < ImageType, OutputImageType > CastFilterType;
   typedef itk::ImageFileWriter< OutputImageType >  WriterType;
   
   typename WriterType::Pointer      writer =  WriterType::New();
   typename CastFilterType::Pointer  caster =  CastFilterType::New();
   writer->SetFileName( args.outputImageFile.c_str() );
   caster->SetInput( warper->GetOutput() );
   writer->SetInput( caster->GetOutput()   );
   writer->SetUseCompression( true );
   
   try
   {
      writer->Update();
   }
   catch( itk::ExceptionObject& err )
   {
      std::cout << "Unexpected error." << std::endl;
      std::cout << err << std::endl;
      exit( EXIT_FAILURE );
   }
   
   
   // Write deformation field
   if (!args.outputFieldFile.empty())
   {
      // Write the deformation field as an image of vectors.
      // Note that the file format used for writing the deformation field must be
      // capable of representing multiple components per pixel. This is the case
      // for the MetaImage and VTK file formats for example.
      typedef itk::ImageFileWriter< DeformationFieldType > FieldWriterType;
      typename FieldWriterType::Pointer fieldWriter = FieldWriterType::New();
      fieldWriter->SetFileName(  args.outputFieldFile.c_str() );
      fieldWriter->SetInput( defField );
      fieldWriter->SetUseCompression( true );
      
      try
      {
         fieldWriter->Update();
      }
      catch( itk::ExceptionObject& err )
      {
         std::cout << "Unexpected error." << std::endl;
         std::cout << err << std::endl;
         exit( EXIT_FAILURE );
      }
   }


   
   // Create and write warped grid image
   if ( args.verbosity > 0 )
   {
      typedef itk::Image< unsigned char, Dimension > GridImageType;
      typename GridImageType::Pointer gridImage = GridImageType::New();
      gridImage->SetRegions( movingImage->GetRequestedRegion() );
      gridImage->SetOrigin( movingImage->GetOrigin() );
      gridImage->SetSpacing( movingImage->GetSpacing() );
      gridImage->Allocate();
      gridImage->FillBuffer(0);
      
      typedef itk::ImageRegionIteratorWithIndex<GridImageType> GridImageIteratorWithIndex;
      GridImageIteratorWithIndex itergrid = GridImageIteratorWithIndex(
         gridImage, gridImage->GetRequestedRegion() );

      const int gridspacing(8);
      for (itergrid.GoToBegin(); !itergrid.IsAtEnd(); ++itergrid)
      {
         itk::Index<Dimension> index = itergrid.GetIndex();

         if (Dimension==2 or Dimension==3)
         {
            // produce an xy grid for all z
            if ( (index[0]%gridspacing)==0 or
                 (index[1]%gridspacing)==0 )
            {
               itergrid.Set( itk::NumericTraits<unsigned char>::max() );
            }
         }
         else
         {
            unsigned int numGridIntersect = 0;
            for( unsigned int dim = 0; dim < Dimension; dim++ )
               numGridIntersect += ( (index[dim]%gridspacing)==0 );
            if (numGridIntersect >= (Dimension-1))
               itergrid.Set( itk::NumericTraits<unsigned char>::max() );
         }
      }

      typedef itk::WarpImageFilter
         < GridImageType, GridImageType, DeformationFieldType >  GridWarperType;
      typename GridWarperType::Pointer gridwarper = GridWarperType::New();
      gridwarper->SetInput( gridImage );
      gridwarper->SetOutputSpacing( fixedImage->GetSpacing() );
      gridwarper->SetOutputOrigin( fixedImage->GetOrigin() );
      gridwarper->SetDeformationField( defField );
      
      // Write warped grid to file
      typedef itk::ImageFileWriter< GridImageType >  GridWriterType;
   
      typename GridWriterType::Pointer      gridwriter =  GridWriterType::New();
      gridwriter->SetFileName( "WarpedGridImage.mha" );
      gridwriter->SetInput( gridwarper->GetOutput()   );
      gridwriter->SetUseCompression( true );
   
      try
      {
         gridwriter->Update();
      }
      catch( itk::ExceptionObject& err )
      {
         std::cout << "Unexpected error." << std::endl;
         std::cout << err << std::endl;
         exit( EXIT_FAILURE );
      }
   }


   // Create and write forewardwarped grid image
   if ( args.verbosity > 0 )
   {
      typedef itk::Image< unsigned char, Dimension > GridImageType;
      typedef itk::GridForwardWarpImageFilter<DeformationFieldType, GridImageType> GridForwardWarperType;

      typename GridForwardWarperType::Pointer fwWarper = GridForwardWarperType::New();
      fwWarper->SetInput(defField);
      fwWarper->SetForegroundValue( itk::NumericTraits<unsigned char>::max() );

      // Write warped grid to file
      typedef itk::ImageFileWriter< GridImageType >  GridWriterType;
   
      typename GridWriterType::Pointer      gridwriter =  GridWriterType::New();
      gridwriter->SetFileName( "ForwardWarpedGridImage.mha" );
      gridwriter->SetInput( fwWarper->GetOutput()   );
      gridwriter->SetUseCompression( true );
   
      try
      {
         gridwriter->Update();
      }
      catch( itk::ExceptionObject& err )
      {
         std::cout << "Unexpected error." << std::endl;
         std::cout << err << std::endl;
         exit( EXIT_FAILURE );
      }
   }



 
   // compute final metric
   if ( args.verbosity > 0 )
   {
      double finalSSD = 0.0;
      typedef itk::ImageRegionConstIterator<ImageType> ImageConstIterator;

      ImageConstIterator iterfix = ImageConstIterator(
         fixedImage, fixedImage->GetRequestedRegion() );
      
      ImageConstIterator itermovwarp = ImageConstIterator(
         warper->GetOutput(), fixedImage->GetRequestedRegion() );
      
      for (iterfix.GoToBegin(), itermovwarp.GoToBegin(); !iterfix.IsAtEnd(); ++iterfix, ++itermovwarp)
      {
         finalSSD += vnl_math_sqr( iterfix.Get() - itermovwarp.Get() );
      }

      const double finalMSE = finalSSD / static_cast<double>(
         fixedImage->GetRequestedRegion().GetNumberOfPixels() );
      std::cout<<"MSE fixed image vs. warped moving image: "<<finalMSE<<std::endl;
   }


   
   // Create and write jacobian of the deformation field
   if ( args.verbosity > 0 )
   {
      typedef itk::WarpJacobianDeterminantFilter
         <DeformationFieldType, ImageType> JacobianFilterType;
      typename JacobianFilterType::Pointer jacobianFilter = JacobianFilterType::New();
      jacobianFilter->SetInput( defField );
      jacobianFilter->SetUseImageSpacing( true );

      writer->SetFileName( "TransformJacobianDeteminant.mha" );
      caster->SetInput( jacobianFilter->GetOutput() );
      writer->SetInput( caster->GetOutput()   );
      writer->SetUseCompression( true );
      
      try
      {
         writer->Update();
      }
      catch( itk::ExceptionObject& err )
      {
         std::cout << "Unexpected error." << std::endl;
         std::cout << err << std::endl;
         exit( EXIT_FAILURE );
      }

      typedef itk::MinimumMaximumImageCalculator<ImageType> MinMaxFilterType;
      typename MinMaxFilterType::Pointer minmaxfilter = MinMaxFilterType::New();
      minmaxfilter->SetImage( jacobianFilter->GetOutput() );
      minmaxfilter->Compute();
      std::cout<<"Minimum of the determinant of the Jacobian of the warp: "
               <<minmaxfilter->GetMinimum()<<std::endl;
      std::cout<<"Maximum of the determinant of the Jacobian of the warp: "
               <<minmaxfilter->GetMaximum()<<std::endl;
   }

   
}


int itkDiffeomorphicDemonsRegistrationFilterTest( int argc, char *argv[] )
{
   struct arguments args;
   parseOpts (argc, argv, args);

   std::cout<<"Starting demons registration with the following arguments:"<<std::endl;
   std::cout<<args<<std::endl<<std::endl;

   // FIXME uncomment for debug only
   // itk::MultiThreader::SetGlobalDefaultNumberOfThreads(1);

   // Get the image dimension
   itk::ImageIOBase::Pointer imageIO =
      itk::ImageIOFactory::CreateImageIO(
         args.fixedImageFile.c_str(), itk::ImageIOFactory::ReadMode);
   imageIO->SetFileName(args.fixedImageFile.c_str());
   imageIO->ReadImageInformation();

   switch ( imageIO->GetNumberOfDimensions() )
   {
   case 2:
      DemonsRegistrationFunction<2>(args);
      break;
   case 3:
      DemonsRegistrationFunction<3>(args);
      break;
   default:
      std::cerr << "Unsuported dimension" << std::endl;
      exit( EXIT_FAILURE );
   }
  
  return EXIT_SUCCESS;
}
