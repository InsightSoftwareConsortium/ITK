# -*- coding: utf-8 -*-
from __future__ import print_function
from os import sys

try:
    from skbuild import setup
except ImportError:
    print('scikit-build is required to build from source.', file=sys.stderr)
    print('Please run:', file=sys.stderr)
    print('', file=sys.stderr)
    print('  python -m pip install scikit-build')
    sys.exit(1)

setup(
    name='itk-analyzeobjectmapiO',
    version='5.1.0',
    author='Hans J. Johnson',
    author_email='hans-johnson@uiowa.edu',
    packages=['itk'],
    package_dir={'itk': 'itk'},
    download_url=r'https://github.com/InsightSoftwareConsortium/itkAnalyzeObjectMap',
    description=r'ITK `ImageIO` class to read or write the AnalyzeObjectMap image format',
    long_description='itk-analyzeobjectmapio provides an `ImageIO` class to read or '
                     'write the AnalyzeObjectMap image format.',
    classifiers=[
        "License :: OSI Approved :: Apache Software License",
        "Programming Language :: Python",
        "Programming Language :: C++",
        "Development Status :: 4 - Beta",
        "Intended Audience :: Developers",
        "Intended Audience :: Education",
        "Intended Audience :: Healthcare Industry",
        "Intended Audience :: Science/Research",
        "Topic :: Scientific/Engineering",
        "Topic :: Scientific/Engineering :: Medical Science Apps.",
        "Topic :: Scientific/Engineering :: Information Analysis",
        "Topic :: Software Development :: Libraries",
        "Operating System :: Android",
        "Operating System :: Microsoft :: Windows",
        "Operating System :: POSIX",
        "Operating System :: Unix",
        "Operating System :: MacOS"
        ],
    license='Apache',
    keywords='ITK InsightToolkit AnalyzeObjectMap',
    url=r'https://github.com/InsightSoftwareConsortium/itkAnalyzeObjectMap',
    install_requires=[
        r'itk>=5.1.0'
    ]
    )
