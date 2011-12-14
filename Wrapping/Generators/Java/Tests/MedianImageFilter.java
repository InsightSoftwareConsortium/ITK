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
 *  Example on the use of the MedianImageFilter
 *
 */

import org.itk.itkcommon.*;
import org.itk.itkioimagebase.*;
import org.itk.itksmoothing.*;

public class MedianImageFilter
{
  public static void main( String argv[] )
  {
    System.out.println("MedianImageFilter Example");

    itkImageFileReaderIUC2 reader = new itkImageFileReaderIUC2();
    itkImageFileWriterIUC2 writer = new itkImageFileWriterIUC2();

    itkMedianImageFilterIUC2IUC2 filter = new itkMedianImageFilterIUC2IUC2();

    filter.SetInput( reader.GetOutput() );
    writer.SetInput( filter.GetOutput() );

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );


    int radius = Integer.parseInt( argv[2] );

    itkSize2 sizeRadius = new itkSize2();

    sizeRadius.SetElement( 0, radius );
    sizeRadius.SetElement( 1, radius );

    filter.SetRadius( sizeRadius );

    writer.Update();
  }

}
