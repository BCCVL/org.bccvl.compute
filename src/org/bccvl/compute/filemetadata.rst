

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

    >>> filemd = dict(((item.keys()[0], item[item.keys()[0]])
    ...                for item in plone.items if item is not None))
    >>> item = filemd['mini_current_50to00_.3.zip']
    >>> subitem = item['mini_current_50to00_.3/bioclim_05.tif']
    >>> subitem['metadata']['srs']
    'EPSG:4326'
    >>> filemd['absence.csv']['headers']
    ['lon', 'lat']
    >>> filemd['absence.csv']['rows']
    400
    >>> filemd['occurrence.csv']['bounds']
    {'right': 153.45, 'bottom': -27.983333, 'top': -27.08149, 'left': 152.75786}
