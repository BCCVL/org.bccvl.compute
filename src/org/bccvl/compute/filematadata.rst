

Demonstrate file metadata extractor:
====================================

First we'll need a simple data source and a transomgrifiec pipeline

    >>> config="""
    ... [transmogrifier]
    ... pipeline =
    ...     source
    ...     filemetadata
    ...     store
    ...
    ... [source]
    ... blueprint = gu.transmogrifier.jsonsource
    ... path = org.bccvl.compute.tests:data
    ...
    ... [filemetadata]
    ... blueprint = org.bccvl.compute.transmogrify.filemetadata
    ...
    ... [store]
    ... blueprint = gu.transmogrifier.tests.store
    ... key = _filemetadata
    ... """
    >>> registerConfig('gu.transmogrifier.tests.sample',
    ...                config)

    Run the chain.

    >>> transmogrifier('gu.transmogrifier.tests.sample')

    Check a few values

    >>> import pprint
    >>> pprint.pprint(plone.items)


    >>> item = plone.items[1]['mini_current_50to00_.3.zip']
    >>> subitem = item['mini_current_50to00_.3/bioclim_05.tif']
    >>> subitem['metadata']['srs']
    'EPSG:4326'
    >>> plone.items[2]['absence.csv']['headers']
    ['lon', 'lat']
    >>> plone.items[2]['absence.csv']['rows']
    400
    >>> plone.items[3]['occurrence.csv']['bounds']
    [-27.983333, 152.75786, -27.08149, 153.45]
