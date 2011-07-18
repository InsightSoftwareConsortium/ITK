/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

/**
 *  Example on the use of the CurvatureFlowImageFilter
 *
 */

import org.itk.itkiobase.*;
import org.itk.itkimagefilterbase.*;
import org.itk.itkcurvatureflow.*;
import org.itk.itkimageintensity.*;


public class CurvatureFlowImageFilter
{
  public static void main( String argv[] )
  {
    itkImageFileReaderIUC2 reader = new itkImageFileReaderIUC2();
    itkImageFileWriterIUC2 writer = new itkImageFileWriterIUC2();

    itkCastImageFilterIUC2IF2 inputCast = new itkCastImageFilterIUC2IF2();

    itkCurvatureFlowImageFilterIF2IF2 filter = new itkCurvatureFlowImageFilterIF2IF2();

    itkRescaleIntensityImageFilterIF2IUC2 outputCast = new itkRescaleIntensityImageFilterIF2IUC2();

    inputCast.SetInput( reader.GetOutput() );
    filter.SetInput( inputCast.GetOutput() );
    outputCast.SetInput( filter.GetOutput() );
    writer.SetInput( outputCast.GetOutput() );

    outputCast.SetOutputMinimum(  (byte)0  );
    outputCast.SetOutputMaximum( (byte)255 );

    filter.SetNumberOfIterations( Short.parseShort( argv[2] ) );
    filter.SetTimeStep(           Float.parseFloat( argv[3] ) );

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );

    writer.Update();
  }

}
