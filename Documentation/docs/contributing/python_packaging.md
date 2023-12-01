Build ITK Python packages
======================================

We can generate Python wrappings and package them as Python wheels in order to conveniently distribute and use the ITK C++ library in Python scripts. In most cases community members can use the [general ITK Python distributions](https://pypi.org/project/itk/) on the Python Package Index (PyPI) for their projects.

```python
pip install itk
```

In some cases developers may need to build ITK Python wrappings for local testing without distribution. Visit the ITK Software Guide "Wrapping" section to learn more about that process.

Additionally, InsightSoftwareConsortium maintainers build ITK Python packages with each release candidate on Kitware build systems and upload them to
the [ITKPythonPackage GitHub releases page](https://github.com/InsightSoftwareConsortium/ITKPythonPackage/releases).

This section describes how to build Python packages for the main ITK libraries using the [ITKPythonPackage](https://github.com/insightSoftwareConsortium/ITKpythonpackage) project. Follow the steps below to reproduce the build process used to generate the official ITK Python release packages.

## Prerequisites

Building ITK Python wheels requires the following:
- CMake >= 3.16
- Git
- C++ Compiler (see [scikit-build platform specific requirements](https://scikit-build.readthedocs.io/en/latest/generators.html))
- Python >= 3.8

## Automated Platform Scripts

The following sections outline how to use the ITKPythonPackage project to build wheels on Linux, macOS, and Windows. Each script will fetch tagged ITK sources, build ITK with Python wrappings, and package binaries into the Python wheel archive format for distribution.

### Linux

On any linux distribution with docker and bash installed, running the script [dockcross-manylinux-build-wheels.sh](https://github.com/InsightSoftwareConsortium/ITKPythonPackage/blob/master/scripts/dockcross-manylinux-build-wheels.sh) will create 64-bit wheels for Python 3.x in the `dist` directory. A Python version to target may be passed as a trailing argument, or passing no arguments will target all default Python versions.

In addition, the environment variables `MANYLINUX_VERSION` and `IMAGE_TAG` may be set before calling the script to control the [dockcross](https://github.com/dockcross/dockcross) Docker image and target a specific platform type. See ITKPythonPackage for more information.

For example:

```bash
git clone https://github.com/InsightSoftwareConsortium/ITKPythonPackage.git
[...]

pushd ITKPythonPackage
export MANYLINUX_VERSION=2014
./scripts/dockcross-manylinux-build-wheels.sh cp38
[...]

ls -1 dist/
itk-5.3.0.dev20231108-cp38-cp38m-manylinux2014_x86_64.whl
```

### macOS

First, install the Python.org macOS Python distributions. This step requires sudo:

```bash
./scripts/macpython-install-python.sh
```

Then, run [macpython-build-wheels.sh](https://github.com/InsightSoftwareConsortium/ITKPythonPackage/blob/master/scripts/macpython-build-wheels.sh) to build the wheels. A Python version may be targetd by passing a trailing argument to the script. See ITKPythonPackage for environment variables used by `macpython-build-wheels.sh`.

```bash
git clone https://github.com/InsightSoftwareConsortium/ITKPythonPackage.git
[...]

./scripts/macpython-build-wheels.sh cp38
[...]

ls -1 dist/
itk-5.3.0.dev20231108-cp38-cp38m-macosx_10_9_x86_64.whl
```

### Windows

First, install Microsoft Visual Studio 2022 and ensure that Visual Studio, Git, Python, and CMake are part of the system `PATH` environment variable.

Open a PowerShell terminal as Administrator, and install Python:

```pwsh
PS C:\> Set-ExecutionPolicy Unrestricted
PS C:\> $pythonArch = "64"
PS C:\> iex ((new-object net.webclient).DownloadString('https://raw.githubusercontent.com/scikit-build/scikit-ci-addons/master/windows/install-python.ps1'))
```

Clone the ITKPythonPackage project into a short directory to avoid path length limitations on Windows. Also, it is necessary to disable antivirus checking on the C:\IPP directory. Otherwise, the build system conflicts with the antivirus when many files are created and deleted quickly, which can result in Access Denied errors. Windows 10 ships with an antivirus application, Windows Defender, that is enabled by default.

```pwsh
PS C:\Windows> cd C:\
PS C:\> git clone https://github.com/InsightSoftwareConsortium/ITKPythonPackage.git IPP
PS C:\> cd IPP
```

Then run the [windows_build_wheels.py](https://github.com/InsightSoftwareConsortium/ITKPythonPackage/blob/master/scripts/windows_build_wheels.py) script. The `--help` argument may be used to list available build options, including target Python versions and CMake options.

```pwsh
PS C:\IPP> .\scripts\windows_build_wheels.py
[...]

PS C:\IPP> ls dist
[...]
```

## Uploading to PyPI

InsightSoftwareConsortiums may upload ITK package wheels to PyPI using `twine`.

First, install dependencies:
```python
python -m pip install twine
```

Then, upload packages to the [PyPI testing server](https://test.pypi.org):
```python
python -m twine upload -r pypitest dist/*
```

After verifying distributions appear correctly on the testing server, upload the packages to the production PyPI server:

```python
python -m twine upload dist/*
```

The updated ITK Python wheels are now available for general installation:
```python
python -m pip install itk
```

## `ITKPythonBuilds` Archives

In addition to PyPI releases, the InsightSoftwareConsortium releases ITK build archives at [ITKPythonBuilds/releases](https://github.com/InsightSoftwareConsortium/ITKPythonBuilds/releases). Those archives contain ITK sources and binaries required for building ITK external modules with ITK Python wrappings.

Scripts are available in ITKPythonPackage to generate build archives on each platform after automated builds are generated above:

### Linux

```bash
./scripts/dockcross-manylinux-build-tarball.sh
```

### macOS

```bash
./scripts/macpython-build-tarball.sh
```

### Windows

```pwsh
.\scripts\windows-build-tarball.ps1
```
