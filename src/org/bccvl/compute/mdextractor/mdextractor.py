
from io import BytesIO, StringIO
import csv
import zipfile
import uuid


#@implementer(IMetadataExtractor)
class MetadataExtractor(object):

    extractors = {}

    def from_string(self, data, mime_type):
        md = None
        if mime_type in self.extractors:
            md = self.extractors[mime_type].from_string(data)
        return md

    def from_file(self, path, mime_type):
        md = None
        if mime_type in self.extractors:
            md = self.extractors[mime_type].from_file(path)
        return md


# TODO: FilesystemExtractor .... size, etc....
class ZipExtractor(object):

    def from_fileob(self, fileob):
        ret = {}
        with zipfile.ZipFile(fileob, 'r') as zipf:
            # TODO: zipfile itself may have some metadata, e.g. comment
            for zipinfo in zipf.infolist():
                if zipinfo.filename.endswith('/'):
                    # skip directories
                    continue
                # interesting attributes on zipinfo
                #     compress_size, compress_type
                #     comment,
                md = {
                    'filename': zipinfo.filename,
                    'file_size': zipinfo.file_size,
                    'date_time': zipinfo.date_time,  # last mod?

                }
                ret[md['filename']] = md

                # all zip metadata collected, let's look at the data itself
                extractor = MetadataExtractor()
                # TODO: detect mime_type if possible first
                ret[md['filename']]['metadata'] = \
                    extractor.from_string(zipf.read(md['filename']),
                                          'image/tiff')

        return ret

    def from_file(self, path):
        fileob = open(path, 'r')
        return self.from_fileob(fileob)

    def from_string(self, data):
        bytesio = BytesIO(data)
        return self.from_fileob(bytesio)


class TiffExtractor(object):

    def _traverseXMP(self, xmp, _schema=u'', name=u''):
        """Generator which yields interesting values."""
        from libxmp.core import XMPIterator
        for xmpitem in XMPIterator(xmp, _schema, name, iter_justchildren=True):
            (schema, name, value, options) = xmpitem
            if options['IS_SCHEMA']:
                if schema == _schema:
                    # we asked for this nod, so skip it.
                    continue
                for subitem in self._traverseXMP(xmp, schema, name):
                    yield subitem
            yield xmpitem

    def from_file(self, path):
        return self._get_gdal_metadata(path)

    def from_string(self, data):
        from osgeo import gdal
        memname = '/vsimem/{0}'.format(str(uuid.uuid4()))
        try:
            gdal.FileFromMemBuffer(memname, data)

            ret = self._get_gdal_metadata(memname)
        finally:
            gdal.Unlink(memname)
        return ret

    def _get_gdal_metadata(self, filename):
        # let's do GDAL here ? if it fails do Hachoir
        from osgeo import gdal, osr
        ds = gdal.Open(filename, gdal.GA_ReadOnly)

        # TODO: get bounding box
        geotransform = ds.GetGeoTransform()
        projref = ds.GetProjectionRef()
        spref = osr.SpatialReference(projref)
        data = {
            'size': (ds.RasterXSize, ds.RasterYSize),
            'bands': ds.RasterCount,
            'projection': projref,  # WKT
            'srs': '{0}:{1}'.format(spref.GetAuthorityName('GEOGCS'),
                                    spref.GetAuthorityCode('GEOGCS')),
            'origin': (geotransform[0], geotransform[3]),
            'Pxiel Size': (geotransform[1], geotransform[5]),
        }

        data.update(ds.GetMetadata_Dict())
        data.update(ds.GetMetadata_Dict('EXIF'))
        from libxmp.core import XMPMeta
        xmp = ds.GetMetadata('xml:XMP') or []
        if xmp:
            data['xmp'] = {}
        for xmpentry in xmp:
            xmpmd = XMPMeta()
            xmpmd.parse_from_str(xmpentry)
            for xmpitem in self._traverseXMP(xmpmd):
                (schema, name, value, options) = xmpitem
                if options['IS_SCHEMA']:
                    continue
                if options['ARRAY_IS_ALT']:
                    # pick first element and move on
                    data['xmp'][name] = xmpmd.get_array_item(schema, name, 1)
                    continue
                # current item

                # ARRAY_IS_ALT .. ARRAY_IS_ALT_TEXT, pick first one (value is array + array is ordered)

                # -> array elements don't have special markers :(

                if options['ARRAY_IS_ALT']:
                    pass
                if options['HAS_LANG']:
                    pass
                if options['VALUE_IS_STRUCT']:
                    pass
                if options['ARRAY_IS_ALTTEXT']:
                    pass
                if options['VALUE_IS_ARRAY']:
                    pass
                if options['ARRAY_IS_ORDERED']:
                    pass

                #     -> ALT ARRAY_VALUE???
                # if options['VALUE_IS_ARRAY']:

                # else:
                data['xmp'][name] = value

        # EXIF could provide at least:
        #   width, height, bistpersample, compression, planarconfiguration,
        #   sampleformat, xmp-metadata (already parsed)

        # TODO: get driver metadata?
        #     ds.GetDriver().getMetadata()
        #     ds.GetDriver().ds.GetMetadataItem(gdal.DMD_XXX)

        # Extract GDAL metadata
        for numband in range(1, ds.RasterCount+1):
            band = ds.GetRasterBand(numband)
            (min, max, mean, stddev) = band.ComputeStatistics(False)
            banddata = {
                'data type': gdal.GetDataTypeName(band.DataType),
                # band.GetRasterColorTable().GetCount() ... color table with
                # count entries
                'min': min,
                'max': max,
                'mean': mean,
                'stddev': stddev,
                'color interpretation': gdal.GetColorInterpretationName(band.GetColorInterpretation()),
                'description': band.GetDescription(),
                'nodata': band.GetNoDataValue(),
                'size': (band.XSize, band.YSize),
                'index': band.GetBand(),
                #band.GetCategoryNames(), GetRasterCategoryNames() .. ?
                #band.GetScale()
            }
            banddata.update(band.GetMetadata())
            if not 'band' in data:
                data['band'] = []
            data['band'].append(banddata)

        ds = None

        # HACHOIR Tif extractor:
        # ret = {}
        # for field in parser:
        #     if field.name.startswith('ifd'):
        #         data = {
        #           'img_height': field['img_height']['value'].value,
        #           'img_width': field['img_width']['value'].value,
        #           'bits_per_sample': field['bits_per_sample']['value'].value,
        #           'compression': field['compression']['value'].display
        #         }
        #         ret = data
        return data


class CSVExtractor(object):

    def from_fileob(self, bytesio):
        csvreader = csv.reader(bytesio)
        headers = csvreader.next()
        bounds = [float("Inf"), float("Inf"),
                  float("-Inf"), float("-Inf")]
        species = set()
        count = 0
        lonidx = headers.index('lon')
        latidx = headers.index('lat')
        speciesidx = None
        if 'species' in headers:
            speciesidx = headers.index('species')
        for row in csvreader:
            count += 1
            lat, lon = float(row[latidx]), float(row[lonidx])
            bounds[0] = min(lat, bounds[0])
            bounds[1] = min(lon, bounds[1])
            bounds[2] = max(lat, bounds[2])
            bounds[3] = max(lon, bounds[3])
            if speciesidx is not None:
                species.add(row[speciesidx])

        data = {
            'headers': headers,
            'rows': count,
            'bounds': bounds,
            'species': species
        }

        return data

    def from_file(self, path):
        csvfile = open(path, 'r')
        return self.from_fileob(csvfile)

    def from_string(self, data):
        # collect header names, and number of rows.
        # assume we have occurrence / absence file.
        # and find bounding box coordinates
        csvfile = StringIO(data.decode('utf-8'))
        return self.from_fileob(csvfile)


class HachoirExtractor(object):

    def from_string(self, data):
        from hachoir_parser import guessParser
        from hachoir_core.stream import StringInputStream
        stream = StringInputStream(data)
        parser = guessParser(stream)
        from hachoir_metadata import extractMetadata
        ret = extractMetadata(parser)
        #formated = md.exportPlaintext(line_prefix=u"")

        import ipdb; ipdb.set_trace()
        return ret


MetadataExtractor.extractors = {
    'application/octet-stream': None,
    # basic file system extractor or use Hachoir to auto-detect?
    'application/zip': ZipExtractor(),
    'image/tiff': TiffExtractor(),
    'image/geotiff': TiffExtractor(),
    'text/csv': CSVExtractor(),
}

