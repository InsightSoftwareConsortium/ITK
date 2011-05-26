
%define itkver 2.8

Summary:        Extended language support for ITK
Name:                wrapitk
Version:        0.2
Release:        %mkrel 1
License:        BSDish
Group:                Sciences/Other
URL:                http://voxel.jouy.inra.fr/darcs/contrib-itk/WrapITK
#Source0:        http://voxel.jouy.inra.fr/darcs/contrib-itk/WrapITK/WrapITK-%{version}.tar.bz2
Source0:        http://voxel.jouy.inra.fr/darcs/contrib-itk/WrapITK/WrapITK.tar.bz2
Patch0:                wrapitk-reconstruction.patch.bz2
BuildRequires:        cmake >= 2.2
BuildRequires:        cableswig >= %{itkver}
BuildRequires:  python-numarray-devel
BuildRequires:  itk-devel >= %{itkver}
BuildRequires:  python-devel
BuildRequires:  tetex
BuildRequires:  tetex-latex
BuildRequires:  tetex-dvips
BuildRequires:  ghostscript
BuildRequires:  ImageMagick
BuildRequires:        vtk-devel >= 5.0
BuildRequires:        python-vtk >= 5.0
BuildRequires:  tcl tk
# needed for backport to 2006.0
%if %mdkversion >= 200610
BuildRequires:  tk-devel
BuildRequires:  tcl-devel
%endif
BuildRequires:        doxygen
BuildRequires:  tetex-latex
BuildRequires:  texinfo
BuildRoot:        %{_tmppath}/%{name}-%{version}-%{release}-buildroot
# for upgrade from package with ITK version
Epoch:          1

%description
ITK is an open-source software system to support the Visible Human Project.
Currently under active development, ITK employs leading-edge segmentation
and registration algorithms in two, three, and more dimensions.

The Insight Toolkit was developed by six principal organizations, three
commercial (Kitware, GE Corporate R&D, and Insightful) and three academic
(UNC Chapel Hill, University of Utah, and University of Pennsylvania).
Additional team members include Harvard Brigham & Women's Hospital,
University of Pittsburgh, and Columbia University. The funding for the
project is from the National Library of Medicine at the National Institutes
of Health. NLM in turn was supported by member institutions of NIH (see
sponsors).

%package  devel
Summary:        ITK header files for building C++ code
Group:                Development/C++
Requires:        cmake >= 2.2
Requires:        cableswig >= %{itkver}
Requires:        itk-devel >= %{itkver}

%description devel
ITK is an open-source software system to support the Visible Human Project.
Currently under active development, ITK employs leading-edge segmentation
and registration algorithms in two, three, and more dimensions.

The Insight Toolkit was developed by six principal organizations, three
commercial (Kitware, GE Corporate R&D, and Insightful) and three academic
(UNC Chapel Hill, University of Utah, and University of Pennsylvania).
Additional team members include Harvard Brigham & Women's Hospital,
University of Pittsburgh, and Columbia University. The funding for the
project is from the National Library of Medicine at the National Institutes
of Health. NLM in turn was supported by member institutions of NIH (see
sponsors).

%package -n python-itk
Summary:        Python bindings for ITK
Group:                Development/Python
Requires:        python
Requires:        itk >= %{itker}
Requires(pre):        itk >= %{itker}
Obsoletes:        itk-python
Provides:        itk-python

%description -n python-itk
ITK is an open-source software system to support the Visible Human Project.
Currently under active development, ITK employs leading-edge segmentation
and registration algorithms in two, three, and more dimensions.

The Insight Toolkit was developed by six principal organizations, three
commercial (Kitware, GE Corporate R&D, and Insightful) and three academic
(UNC Chapel Hill, University of Utah, and University of Pennsylvania).
Additional team members include Harvard Brigham & Women's Hospital,
University of Pittsburgh, and Columbia University. The funding for the
project is from the National Library of Medicine at the National Institutes
of Health. NLM in turn was supported by member institutions of NIH (see
sponsors).


%package -n python-itk-numarray
Summary:        Convert itk buffers to numarray objects
Group:                Development/Python
Requires:        python
Requires:        python-numarray
Requires:        python-itk = %{epoch}:%{version}

%description -n python-itk-numarray
Convert itk buffers to numarray objects


%package -n python-itkvtk
Summary:        Convert itk buffers to vtk ones
Group:                Development/Python
Requires:        python
Requires:        python-itk = %{epoch}:%{version}


%description -n python-itkvtk
Convert itk buffers to vtk ones


%package -n itkvtk-devel
Summary:        Convert itk buffers to vtk ones
Group:                Development/C++
Requires:        itk-devel


%description -n itkvtk-devel
Convert itk buffers to vtk ones


%package -n tcl-itk
Summary:        Tcl bindings for ITK
Group:                Development/Python
Requires:        tcl
Requires:        itk >= %{itker}
Requires(pre):        itk >= %{itker}
Obsoletes:        itk-tcl
Provides:        itk-tcl

%description -n tcl-itk
ITK is an open-source software system to support the Visible Human Project.
Currently under active development, ITK employs leading-edge segmentation
and registration algorithms in two, three, and more dimensions.

The Insight Toolkit was developed by six principal organizations, three
commercial (Kitware, GE Corporate R&D, and Insightful) and three academic
(UNC Chapel Hill, University of Utah, and University of Pennsylvania).
Additional team members include Harvard Brigham & Women's Hospital,
University of Pittsburgh, and Columbia University. The funding for the
project is from the National Library of Medicine at the National Institutes
of Health. NLM in turn was supported by member institutions of NIH (see
sponsors).


%prep

# %setup -q -n WrapITK-%{version}
%setup -q -n WrapITK

cd Modules/Morphology/
%patch0 -p1

%build

mkdir build
(
cd build

cmake -DCMAKE_INSTALL_PREFIX:PATH=%{_prefix} \
      -DWRAP_ITK_INSTALL_PREFIX:PATH=/%{_lib}/InsightToolkit/WrapITK/ \
      -DCMAKE_CXX_COMPILER:PATH=%{_bindir}/c++ \
      -DCMAKE_BUILD_TYPE:STRING=Release \
      -DCMAKE_SKIP_RPATH:BOOL=ON \
      -DWRAP_ITK_PYTHON:BOOL=ON \
      -DWRAP_ITK_TCL:BOOL=ON \
      -DWRAP_ITK_JAVA:BOOL=OFF \
      -DWRAP_unsigned_char:BOOL=ON \
      -DDOXYGEN_MAN_PATH:PATH=%{_mandir}/ \
      ..

%make
)

export LD_LIBRARY_PATH=`pwd`/build/bin:$LD_LIBRARY_PATH
export PYTHONPATH=`pwd`/build/Python:`pwd`/Python:$PYTHONPATH

# build the article
(
cd article
make
)

# build the doc
(
cd build/Python
mkdir -p doc
python make_doxygen_config.py doc
doxygen doxygen.config
)

# build the external projects
(
cd ExternalProjects/PyBuffer/
mkdir build
(
cd build

cmake -DCMAKE_INSTALL_PREFIX:PATH=%{_prefix} \
      -DCMAKE_BUILD_TYPE:STRING=Release \
      -DCMAKE_SKIP_RPATH:BOOL=ON \
      -DWrapITK_DIR:PATH=`pwd`/../../../build \
      ..

%make
)
)

(
cd ExternalProjects/MultiThreaderControl/
mkdir build
(
cd build

cmake -DCMAKE_INSTALL_PREFIX:PATH=%{_prefix} \
      -DCMAKE_BUILD_TYPE:STRING=Release \
      -DCMAKE_SKIP_RPATH:BOOL=ON \
      -DWrapITK_DIR:PATH=`pwd`/../../../build \
      ..

%make
)
)

(
cd ExternalProjects/ItkVtkGlue/
mkdir build
(
cd build

# disable tcl - it doesn't work yet

cmake -DCMAKE_INSTALL_PREFIX:PATH=%{_prefix} \
      -DCMAKE_BUILD_TYPE:STRING=Release \
      -DCMAKE_SKIP_RPATH:BOOL=ON \
      -DWrapITK_DIR:PATH=`pwd`/../../../build \
      -DBUILD_WRAPPERS:BOOL=ON \
      -DVTK_DIR:PATH=%{_libdir}/vtk-5.0 \
      -DWRAP_ITK_TCL:BOOL=OFF \
      ..

%make
)
)



%install
[ "$RPM_BUILD_ROOT" != "/" ] && rm -rf $RPM_BUILD_ROOT

(
cd build
make install DESTDIR=$RPM_BUILD_ROOT
)

# workaround not found library
mkdir -p $RPM_BUILD_ROOT/%{_sysconfdir}/ld.so.conf.d/
echo %{_libdir}/InsightToolkit/WrapITK/lib >> $RPM_BUILD_ROOT/%{_sysconfdir}/ld.so.conf.d/python-itk.conf

# install doc
mkdir -p $RPM_BUILD_ROOT/%{_mandir}
cp -r build/Python/doc/man3 $RPM_BUILD_ROOT/%{_mandir}
rm -f $RPM_BUILD_ROOT/%{_mandir}/man3/todo.3
rm -f $RPM_BUILD_ROOT/%{_mandir}/man3/itkBSplineDecompositionImageFilter.3
rm -f $RPM_BUILD_ROOT/%{_mandir}/man3/deprecated.3
rm -f $RPM_BUILD_ROOT/%{_mandir}/man3/BSplineUpsampleImageFilterBase.3


export LD_LIBRARY_PATH=`pwd`/build/bin:$LD_LIBRARY_PATH
export PYTHONPATH=`pwd`/build/Python:`pwd`/Python:$PYTHONPATH


# install the external projects
(
cd ExternalProjects/PyBuffer/build
make install DESTDIR=$RPM_BUILD_ROOT
)

(
cd ExternalProjects/MultiThreaderControl/build
make install DESTDIR=$RPM_BUILD_ROOT
)

(
cd ExternalProjects/ItkVtkGlue/build
make install DESTDIR=$RPM_BUILD_ROOT
)

%check

cd build
ctest
cd ../ExternalProjects/ItkVtkGlue/build
ctest
cd ../../MultiThreaderControl/build
ctest

%clean
rm -rf $RPM_BUILD_ROOT


%post -n python-itk -p /sbin/ldconfig

%postun -n python-itk -p /sbin/ldconfig


%files -n python-itk
%defattr(0644,root,root,0755)
%{_libdir}/InsightToolkit/WrapITK/Python
%{_libdir}/InsightToolkit/WrapITK/lib/*.py
%{_libdir}/InsightToolkit/WrapITK/lib/*Python.so
%{_libdir}/python%{pyver}/site-packages/WrapITK.pth
%{_sysconfdir}/ld.so.conf.d/python-itk.conf
%{_mandir}/man*/*
# exclude numarray files
%exclude %{_libdir}/InsightToolkit/WrapITK/Python/BufferConversion.py
%exclude %{_libdir}/InsightToolkit/WrapITK/Python/Configuration/BufferConversionConfig.py
%exclude %{_libdir}/InsightToolkit/WrapITK/lib/BufferConversionPython.py
%exclude %{_libdir}/InsightToolkit/WrapITK/lib/itkPyBuffer.py
%exclude %{_libdir}/InsightToolkit/WrapITK/lib/_BufferConversionPython.so
# exclude itkvtk files
%exclude %{_libdir}/InsightToolkit/WrapITK/Python/ItkVtkGlue.py
%exclude %{_libdir}/InsightToolkit/WrapITK/Python/Configuration/ItkVtkGlueConfig.py
%exclude %{_libdir}/InsightToolkit/WrapITK/lib/ItkVtkGluePython.py
%exclude %{_libdir}/InsightToolkit/WrapITK/lib/itkImageToVTKImageFilter.py
%exclude %{_libdir}/InsightToolkit/WrapITK/lib/itkVTKImageToImageFilter.py
%exclude %{_libdir}/InsightToolkit/WrapITK/lib/_ItkVtkGluePython.so

%doc article/*.pdf


%files -n python-itk-numarray
%defattr(0644,root,root,0755)
%{_libdir}/InsightToolkit/WrapITK/Python/BufferConversion.py
%{_libdir}/InsightToolkit/WrapITK/Python/Configuration/BufferConversionConfig.py
%{_libdir}/InsightToolkit/WrapITK/lib/BufferConversionPython.py
%{_libdir}/InsightToolkit/WrapITK/lib/itkPyBuffer.py
%{_libdir}/InsightToolkit/WrapITK/lib/_BufferConversionPython.so


%files -n python-itkvtk
%defattr(0644,root,root,0755)
%{_libdir}/InsightToolkit/WrapITK/Python/ItkVtkGlue.py
%{_libdir}/InsightToolkit/WrapITK/Python/itkvtk.py
%{_libdir}/InsightToolkit/WrapITK/Python/Configuration/ItkVtkGlueConfig.py
%{_libdir}/InsightToolkit/WrapITK/lib/ItkVtkGluePython.py
%{_libdir}/InsightToolkit/WrapITK/lib/itkImageToVTKImageFilter.py
%{_libdir}/InsightToolkit/WrapITK/lib/itkVTKImageToImageFilter.py
%{_libdir}/InsightToolkit/WrapITK/lib/_ItkVtkGluePython.so


%files -n tcl-itk
%defattr(0644,root,root,0755)
%attr(0755,root,root) %{_bindir}/itkwish
%{_libdir}/InsightToolkit/WrapITK/Tcl
%{_libdir}/InsightToolkit/WrapITK/bin/itkwish
%{_libdir}/InsightToolkit/WrapITK/lib/*Tcl.so

%files devel
%defattr(0644,root,root,0755)
%{_libdir}/InsightToolkit/WrapITK/Typedefs
%{_libdir}/InsightToolkit/WrapITK/Configuration
%{_libdir}/InsightToolkit/WrapITK/SWIG
%{_libdir}/InsightToolkit/WrapITK/WrapITKConfig.cmake
%{_datadir}/CMake/Modules/FindWrapITK.cmake

%files -n itkvtk-devel
%defattr(0644,root,root,0755)
%{_includedir}/InsightToolkit/BasicFilters/*
