import org.itk.io.*;
import org.itk.itkvtkglue.*;
import vtk.*;

// export LD_LIBRARY_PATH=/usr/local/lib/vtk-5.6/
// javac -cp ../../../build/lib/org.itk.itkvtkglue.jar:/usr/local/lib/vtk-5.6/java/vtk.jar:/home/glehmann/src/ITK/build/bin/org.itk.io.jar:/home/glehmann/src/ITK/build/bin/org.itk.simplefilters.jar itkVTKTest.java
// java -cp ../../../build/lib/org.itk.itkvtkglue.jar:/usr/local/lib/vtk-5.6/java/vtk.jar:/home/glehmann/src/ITK/build/bin/org.itk.io.jar:/home/glehmann/src/ITK/build/bin/org.itk.simplefilters.jar:. -Djava.library.path=/home/glehmann/src/ITK/build/bin/:../../../build/lib/:/usr/local/lib/vtk-5.6/ itkVTKTest ~/src/contrib-itk/binaryAttributeMorphology/images/cthead1.png out.tif

public class itkVTKTest {
  static
  {
    System.loadLibrary("vtkImagingJava");
  }

  public static void main(String argv[])
  {
    itkImageFileReaderIUC2 reader = new itkImageFileReaderIUC2();
    reader.SetFileName(argv[0]);

    itkImageToVTKImageFilterIUC2 i2v = new itkImageToVTKImageFilterIUC2();
    i2v.SetInput(reader.GetOutput());

    itkVTKImageToImageFilterIUC2 v2i = new itkVTKImageToImageFilterIUC2();
    v2i.SetInput(i2v.GetOutput());

    itkImageFileWriterIUC2 writer = new itkImageFileWriterIUC2();
    writer.SetInput(v2i.GetOutput());
    writer.SetFileName(argv[1]);
    writer.Update();
  }
}
