import doctest
import unittest
import zope.component
import zope.security
import collective.transmogrifier
from collective.transmogrifier.tests import setUp as baseSetUp, tearDown
import gu.transmogrifier
import gu.transmogrifier.tests
from Products.Five import zcml
import org.bccvl.compute


def setUp(test):
    baseSetUp(test)
    from collective.transmogrifier.transmogrifier import Transmogrifier
    test.globs['transmogrifier'] = Transmogrifier(test.globs['plone'])

    zcml.load_config("meta.zcml", zope.security)
    zcml.load_config("meta.zcml", zope.component)
    zcml.load_config("meta.zcml", collective.transmogrifier)
    zcml.load_config("configure.zcml", collective.transmogrifier)
    zcml.load_config("configure.zcml", gu.transmogrifier)
    zcml.load_config('testing.zcml', gu.transmogrifier.tests)
    zcml.load_config('configure.zcml', org.bccvl.compute)

    # import logging
    # from zope.testing import loggingsupport
    # test.globs['handler'] = loggingsupport.InstalledHandler(
    #     'logger', level=logging.INFO)


def test_suite():
    import sys
    suite = unittest.findTestCases(sys.modules[__name__])
    suite.addTests((
        doctest.DocFileSuite(
            '../filemetadata.rst',
            setUp=setUp, tearDown=tearDown,
            optionflags=doctest.NORMALIZE_WHITESPACE | doctest.ELLIPSIS),
    ))
    return suite
