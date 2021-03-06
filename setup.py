from setuptools import setup, find_packages


tests_require = [
    'scikit-image'
]


setup(
    name='org.bccvl.compute',
    setup_requires=['setuptools_scm'],
    use_scm_version=True,
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
    tests_require=tests_require,
    extras_require={
        'test': tests_require,
    }
)
