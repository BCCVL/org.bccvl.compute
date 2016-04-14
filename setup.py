from setuptools import setup, find_packages

version = '1.11.0-rc2'

setup(
    name='org.bccvl.compute',
    version=version,
    description="BCCVL Compute Scripts",
    # long_description=open("README.txt").read() + "\n" +
    #                  open(os.path.join("docs", "HISTORY.txt")).read(),
    # Get more strings from
    # http://pypi.python.org/pypi?:action=list_classifiers
    classifiers=[
        "Framework :: Plone",
        "Programming Language :: Python",
    ],
    keywords='',
    author='',
    author_email='',
    url='http://svn.plone.org/svn/collective/',
    license='GPL',
    packages=find_packages('src'),
    package_dir={'': 'src'},
    namespace_packages=['org', 'org.bccvl'],
    include_package_data=True,
    zip_safe=False,
    test_suite='test_rscripts',
    install_requires=[
        'setuptools',  # distribute
        'zope.i18nmessageid',
    ],
    tests_require=[
        'numpy',
        'scipy',
        'Pillow',
        'scikit-image',
        'GDAL'
    ],
    extras_require={
        'test': [
            'org.bccvl.testsetup',
        ],
        'test_r': [
            'numpy',
            'scipy',
            'Pillow',
            'scikit-image',
            'GDAL'
        ]
    },
    entry_points="""
    # -*- Entry points: -*-
    #[z3c.autoinclude.plugin]
    #target = plone
    """,
    )
