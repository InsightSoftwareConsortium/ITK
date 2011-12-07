import org.itk.io.*;
import org.itk.itkvtkglue.*;
import vtk.*;

// export LD_LIBRARY_PATH=/usr/local/lib/vtk-5.6/:/usr/lib/jvm/java-1.6.0-sun-1.6.0.22/jre/lib/amd64/xawt/
// javac -cp ../../../build/lib/org.itk.itkvtkglue.jar:/usr/local/lib/vtk-5.6/java/vtk.jar:/home/glehmann/src/ITK/build/bin/org.itk.io.jar:/home/glehmann/src/ITK/build/bin/org.itk.simplefilters.jar itkVTKDisplay.java
// java -cp ../../../build/lib/org.itk.itkvtkglue.jar:/usr/local/lib/vtk-5.6/java/vtk.jar:/home/glehmann/src/ITK/build/bin/org.itk.io.jar:/home/glehmann/src/ITK/build/bin/org.itk.simplefilters.jar:. -Djava.library.path=/home/glehmann/src/ITK/build/bin/:../../../build/lib/:/usr/local/lib/vtk-5.6/ itkVTKDisplay ~/src/contrib-itk/binaryAttributeMorphology/images/cthead1.png

public class itkVTKDisplay {
  static
  {
    System.loadLibrary("vtkRenderingJava");
  }

  public static void main(String argv[])
  {
    itkImageFileReaderIUC2 reader = new itkImageFileReaderIUC2();
    reader.SetFileName(argv[0]);

    itkImageToVTKImageFilterIUC2 i2v = new itkImageToVTKImageFilterIUC2();
    i2v.SetInput(reader.GetOutput());

    vtkRenderWindowInteractor renderWindowInteractor = new vtkRenderWindowInteractor();
    vtkImageViewer2 imageViewer = new vtkImageViewer2();
    imageViewer.SetInput(i2v.GetOutput());
    imageViewer.SetupInteractor( renderWindowInteractor );
    imageViewer.GetRenderer().ResetCamera();
    renderWindowInteractor.Initialize();
    renderWindowInteractor.Start();
  }
}
