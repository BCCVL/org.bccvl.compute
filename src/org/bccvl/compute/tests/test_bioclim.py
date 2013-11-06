import unittest2 as unittest
from plone.app.uuid import utils
from org.bccvl.site.content.experiment import Experiment
from org.bccvl.site.content.dataset import Dataset
import os
import pkg_resources


def gen_mock_uuidToObject(data):

    def mock_uuidToObject(uuid):
        return data[uuid]

    return mock_uuidToObject


class MockFile(object):


    filename = None

    def __init__(self, fullpath):
        self.fullpath = fullpath
        self.filename = os.path.basename(fullpath)

    def open(self):
        return open(self.fullpath, 'r')


class Bioclim_Test(unittest.TestCase):

    def setUp(self):
        data = {}
        data['1'] = exp = Experiment()
        data['2'] = Dataset()
        data['3'] = Dataset()
        data['4'] = Dataset()
        exp.species_occurrence_dataset = '2'
        exp.species_absence_dataset = '3'
        exp.environmental_dataset = '4'
        data['2'].file = MockFile(pkg_resources.resource_filename(__name__, 'data/occurrence.csv'))
        data['3'].file = MockFile(pkg_resources.resource_filename(__name__, 'data/absence.csv'))
        data['4'].file = MockFile(pkg_resources.resource_filename(__name__, 'data/current_test.zip'))
        self.experiment = exp
        self.old_uuidToObject = utils.uuidToObject
        utils.uuidToObject = gen_mock_uuidToObject(data)
        # TODO mockup uuid2object

    def tearDown(self):
        self.experiment = None
        utils.uuidToObject = self.old_uuidToObject

    def test_bioclim(self):
        from org.bccvl.compute.bioclim import execute
        from org.bccvl.compute.utils import WorkEnvLocal
        execute(self.experiment, WorkEnvLocal)

    # def test_brt(self):
    #     from org.bccvl.compute.brt import execute
    #     execute(self.experiment, WorkEnvLocal)

        # 2. mock uuidToObject
        # 3. prepare climatitem, futureitem, occurenceitem, absencetiem
        # 3. prepare experiment object with above data
        # 4.

# R-script to generate test data
# require('raster')
# require('rgdal')
# require('SDMTools')
# biovars = c(1:18)
# box = extent(152.75722, 153.75722, -28.061551, -27.061551)
# for (var in biovars) {
#   r <- raster(paste('bio', var, '_311.tif', sep=''))
#   rc <- crop(r, box)
#   writeRaster(rc, paste('bioclim_', sprintf('%02d', var), '.tif', sep=''))
# }

# locs = read.csv(file="koala.csv", header=T)
# locs = locs[locs$lon > xmin(box) & locs$lon < xmax(box) & locs$lat > ymin(box) & locs$lat < ymax(box), ]
# write.csv(locs, file='occurrence.csv', row.names=FALSE)

# bkgd = raster(ext=box, nrows=20, ncols=20)
# bkgd[] = runif(20*20)
# bkgd = as.data.frame(as(bkgd, "SpatialGridDataFrame"))
# names(bkgd) <- c("layer", "lon","lat")
# write.csv(bkgd[, c("lon", "lat") ], file='absence.csv', row.names=FALSE)
